{-# LANGUAGE OverloadedStrings, EmptyDataDecls, ExistentialQuantification, TemplateHaskell, MultiParamTypeClasses,
             DeriveDataTypeable, PackageImports, TypeFamilies #-}

import System.Random
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Traversable
import Snap
import "mtl" Control.Monad.Reader
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8
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
$(makeAcidic ''BlobState ['insertKey, 'lookupKey])

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
h = cmsData "/cms/blobs" cr getKey emptyBlob upd
  where cr v = do key <- liftIO $ randomRIO (1,999999) -- NOTE(dbp 2014-06-16): For the love of god, do not use this in real applications!
                  update (InsertKey key v)
                  return key
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
                 -> (t -> AppHandler Int)
                 -> (Int -> AppHandler (Maybe t))
                 -> t
                 -> (Int -> t -> AppHandler ())
                 -> AppHandler ()
cmsData base create get empty update = route [(base, route [("/new", cmsNew)
                                                           ,("/edit/:id", cmsEdit)
                                                           ])]
  where cmsNew = do let fs = getFields
                    let (form, html) = buildForm fs Nothing empty
                    r <- runForm "new" form
                    case r of
                      (view, Nothing) -> writeLBS (renderHtml $ html view)
                      (_, Just t) -> do id' <- create t
                                        redirect $ B.concat [base, "/edit/", B8.pack (show id')]
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
                                                        redirect $ B.concat [base, "/edit/", B8.pack (show idInt)]
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
