{-# LANGUAGE OverloadedStrings, EmptyDataDecls, ExistentialQuantification #-}

import Data.Maybe
import Data.Traversable
import Snap
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8
import Text.Digestive
import Text.Digestive.Snap
import qualified Text.Digestive.Blaze.Html5 as F

data App
type AppHandler = Handler App App

data Issue = Issue { issueId :: Int, issueTitle :: Text, issueNumber :: Text }

h = cmsData "/cms/issue" cr upd del
  where cr = undefined
        upd = undefined
        del = undefined

data FormType

data Field t = forall v. Field { getVal :: v, setVal :: v -> Field t, extractVal :: t -> v,
                                 renderVal :: v -> Text, parseVal :: Text -> v,
                                 fieldName :: Text, formType :: FormType }

class CMS t where
  getFields :: [(Field t, t -> Field t -> t)]

cmsData :: CMS t => ByteString
                 -> (t -> AppHandler Int)
                 -> (Int -> AppHandler (Maybe t))
                 -> t
                 -> (t -> AppHandler ())
                 -> (t -> AppHandler ())
                 -> AppHandler ()
cmsData base create get empty update delete = route [(base, route [("/new", cmsNew)
                                                                  ,("/edit/:id", cmsEdit)])]
  where cmsNew = do let fs = getFields
                    let (form, html) = buildForm fs Nothing empty
                    r <- runForm "new" form
                    case r of
                      (view, Nothing) -> writeLBS (renderHtml $ html view)
                      (_, Just t) -> do id' <- create t
                                        redirect $ B.concat [base, "/show/", B8.pack (show id')]
        cmsEdit = do (Just id') <- getParam "id"
                     mi <- get (read $ B8.unpack id')
                     case mi of
                       Nothing -> pass
                       Just t -> do let fs = getFields
                                    let (form, html) = buildForm fs (Just t) empty
                                    r <- runForm "edit" form
                                    case r of
                                      (view, Nothing) -> writeLBS (renderHtml $ html view)
                                      (_, Just t) -> do update t
                                                        redirect $ B.concat [base, "/show/", B8.pack (show id')]
        buildForm :: [(Field t, t -> Field t -> t)] -> Maybe t -> t -> (Form H.Html AppHandler t, View H.Html -> H.Html)
        buildForm fields v emty = let form = build <$> sequenceA (map (\(Field get set extract render parse name typ, upd) ->
                                                                        (\t -> set (parse t)) <$> name .: text (render . extract <$> v)) fields)
                                      html v = F.form v "POST" $ foldr (\(f,_) b ->
                                        (H.p $ do
                                           F.label (fieldName f) v (H.toHtml $ fieldName f)
                                           F.inputText (fieldName f) v)) (F.inputSubmit "Submit") fields
                                  in (form, html)
          where build fs = let start = fromMaybe empty v in
                               foldr (\((_, upd), v) b -> upd b v) start (zip fields fs)
