{-# LANGUAGE OverloadedStrings, EmptyDataDecls, ExistentialQuantification, TemplateHaskell, MultiParamTypeClasses,
             DeriveDataTypeable, PackageImports, TypeFamilies #-}

import System.Random
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Traversable hiding (sequence)
import Snap hiding (forM)
import "mtl" Control.Monad.Reader hiding (forM)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import Text.Digestive
import Text.Digestive.Snap
import qualified Text.Digestive.Blaze.Html5 as F
import Snap.Snaplet.AcidState
import Control.Lens
import Data.SafeCopy
import Data.Typeable
import Snap.Http.Server.Config


-- Some State Functions... AcidState used to make it self-contained (but still easy).
data Blob = Blob { _blobVal :: (Text, Text) } deriving (Typeable)
makeLenses ''Blob
$(deriveSafeCopy 0 'base ''Blob)
data BlobState = BlobState (Map Int Blob) deriving (Typeable)
$(deriveSafeCopy 0 'base ''BlobState)
insertKey :: Int -> Blob -> Update BlobState ()
insertKey key value
    = do BlobState m <- get
         put (BlobState (M.insert key value m))
lookupKey :: Int -> Query BlobState (Maybe Blob)
lookupKey key
    = do BlobState m <- ask
         return (M.lookup key m)
lookupAll :: Query BlobState [(Int, Blob)]
lookupAll = do BlobState m <- ask
               return (M.assocs m)
$(makeAcidic ''BlobState ['insertKey, 'lookupKey, 'lookupAll])

-- Minimal snaplet setup
data App = App { _acid :: Snaplet (Acid BlobState) }
makeLenses ''App
instance HasAcid App BlobState where
  getAcidStore = (view snapletValue) . (view acid)
type AppHandler = Handler App App
app :: SnapletInit App App
app = makeSnaplet "app" "" Nothing $ do
    addRoutes [("", h)]
    a <- nestSnaplet "acid" acid $ acidInit (BlobState $ M.empty)
    return $ App a
main :: IO ()
main = serveSnaplet defaultConfig app

-- What we care about: Use of the library to serve a handler, and the instanced needed for data types.
h :: AppHandler ()
h = cmsData "/cms/blobs" getAllVals cr getKey emptyBlob upd
  where cr v = do key <- liftIO $ randomRIO (1,999999) -- NOTE(dbp 2014-06-16): For the love of god, do not use this in real applications!
                  update (InsertKey key v)
                  return key
        getAllVals = query LookupAll
        getKey key = query (LookupKey key)
        emptyBlob = Blob ("","")
        upd i v = do update (InsertKey i v)
                     return ()

-- NOTE(dbp 2014-06-24): Lenses make type checking hard... Would need
-- to add signatures to eliminate the silliness below.
instance CMS Blob where
  getFields = [ mkField (blobVal._1) (blobVal._1) "fst" Nothing
              , mkField (blobVal._2) (blobVal._2) "snd" Nothing
              ]
    where mkField l l2 nm v = Field v (mkField l l2 nm . Just) (view l) (mset l2) id id nm
          mset _ t Nothing = t
          mset s t (Just v) = set s v t

-- library Follows
data Field t = forall v. Field { getVal :: Maybe v
                               , setVal :: v -> Field t
                               , extractVal :: t -> v
                               , updateContainer :: t -> Maybe v -> t
                               , renderVal :: v -> Text
                               , parseVal :: Text -> v
                               , fieldName :: Text
                               }

class CMS t where
  getFields :: [Field t]

cmsData :: CMS t => ByteString
                 -> AppHandler [(Int, t)]
                 -> (t -> AppHandler Int)
                 -> (Int -> AppHandler (Maybe t))
                 -> t
                 -> (Int -> t -> AppHandler ())
                 -> AppHandler ()
cmsData base getAll create get empty update = route [(base, route [("/", ifTop cmsIndex)
                                                                  ,("/new", cmsNew)
                                                                  ,("/edit/:id", cmsEdit)
                                                                  ])]
  where cmsIndex = do all <- getAll
                      let fs = getFields
                      let html = do
                            H.p $ do H.a H.! A.href (H.toValue $ T.decodeUtf8 $ base <> "/new") $ "New"
                                     H.br
                            H.table $ do
                              H.tr $ forM_ fs (H.th . H.toHtml . fieldName)
                              forM_ all (\(i,e) -> H.tr $ do
                                H.td $ H.a H.! A.href (H.toValue $ T.decodeUtf8 $  base <> "/edit/" <> (B8.pack $ show i)) $ "Edit"
                                forM_ fs (\(Field _ _ ext _ ren _ _) -> H.td $ H.toHtml $ ren (ext e)))
                      writeLBS (renderHtml html)
        cmsNew = do let fs = getFields
                    let (form, html) = buildForm fs Nothing empty
                    r <- runForm "new" form
                    case r of
                      (view, Nothing) -> writeLBS (renderHtml $ html view)
                      (_, Just t) -> do id' <- create t
                                        redirect base
        cmsEdit = do (Just id') <- getParam "id"
                     let idInt = read $ B8.unpack id'
                     mi <- get idInt
                     case mi of
                       Nothing -> pass
                       Just t -> do let fs = getFields
                                    let (form, html) = buildForm fs (Just t) empty
                                    r <- runForm "edit" form
                                    case r of
                                      (view, Nothing) -> writeLBS (renderHtml $ html view)
                                      (_, Just t) -> do update idInt t
                                                        redirect base
        buildForm :: CMS t => [Field t] -> Maybe t -> t -> (Form H.Html AppHandler t, View H.Html -> H.Html)
        buildForm fields v emty = let form = build <$> sequenceA (map (\(Field get set extract upd render parse name) ->
                                                                        (\t -> set (parse t)) <$> name .: text (render . extract <$> v)) fields)
                                      html v = F.form v "" $ foldr (\f b ->
                                        (do H.p $ do
                                              F.label (fieldName f) v (H.toHtml $ fieldName f)
                                              F.inputText (fieldName f) v
                                            b)) (F.inputSubmit "Submit") fields
                                  in (form, html)
          where build fs = let start = fromMaybe emty v in
                               foldr (\(Field v _ _ upd _ _ _) b -> upd b v) start fs
