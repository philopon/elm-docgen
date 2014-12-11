{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.Haskell.TH as TH

import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import System.FilePath
import System.Directory
import System.Process

import qualified Text.HTML.TagSoup as TS

import Data.Maybe(catMaybes)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as H

getExactDeps :: FilePath -> IO [(T.Text, T.Text)]
getExactDeps file = maybe [] id .
    (JSON.decode >=> JSON.parseMaybe parser) <$> L.readFile file
  where
    parser = JSON.withObject "not object" $
        mapM (\(p,v) -> (p,) <$> JSON.parseJSON v) . H.toList

getModules :: FilePath -> MaybeT IO (T.Text, [(T.Text, FilePath)])
getModules file = do
    (smy, src, mods) <- MaybeT $ (JSON.decode >=> JSON.parseMaybe parser) <$> L.readFile file
    let cands = [(T.pack m, takeDirectory file </> s </> foldr sep "" m <.> "elm") | s <- src, m <- mods]
    mfs <- liftIO $ filterM (doesFileExist . snd) cands
    return (smy, mfs)
  where
    parser = JSON.withObject "not object" $ \o ->
        (,,)
        <$> o JSON..: "summary"
        <*> o JSON..: "source-directories"
        <*> o JSON..: "exposed-modules"
    sep '.' b = pathSeparator : b
    sep a   b = a : b
        
getModuleDoc :: FilePath -> FilePath -> IO (Maybe JSON.Value)
getModuleDoc elmDoc file = bracket
    (createProcess (proc elmDoc [file]) {std_out = CreatePipe})
    (\(_, Just _, _, h) -> forkIO (() <$ waitForProcess h))
    process
  where
    process (_, Just o, _, _) = runMaybeT $ do
        JSON.Object val <- MaybeT $ JSON.decode <$> L.hGetContents o
        JSON.String com <- maybe mzero return $ H.lookup "comment" val
        let doc = loop Nothing $ T.lines com
        return . JSON.Object $ H.insert "comment" (JSON.toJSON doc) val
    process _ = error "getModuleDoc: no handle."

    loop :: Maybe T.Text -> [T.Text] -> [JSON.Value]
    loop before    []  = plain before []
    loop before (l:ls) = case T.span (`elem` "#@") l of
        ("######", v) -> plain before $ header 6 v : loop Nothing ls
        ("#####", v)  -> plain before $ header 5 v : loop Nothing ls
        ("####", v)   -> plain before $ header 4 v : loop Nothing ls
        ("###", v)    -> plain before $ header 3 v : loop Nothing ls
        ("##", v)     -> plain before $ header 2 v : loop Nothing ls
        ("#", v)      -> plain before $ header 1 v : loop Nothing ls
        ("@", T.break (== ' ') -> ("docs", v)) -> plain before $ docs v : loop Nothing ls
        _ -> loop (appendPlain before l) ls

    appendPlain Nothing  l = Just l
    appendPlain (Just b) l = Just $ T.concat [b, "\n", l]

    header lv b = JSON.object
        [ "tag"   JSON..= ("header" :: T.Text)
        , "level" JSON..= (lv :: Int)
        , "body"  JSON..= T.stripStart b
        ]

    docs v = JSON.object
        [ "tag"  JSON..= ("docs" :: T.Text)
        , "docs" JSON..= (JSON.toJSON . map T.strip . T.split (== ',')) v
        ]

    plain Nothing  = id
    plain (Just p) = (:) $ JSON.object
        [ "tag"  JSON..= ("plain" :: T.Text)
        , "body" JSON..= T.dropWhile (`elem` "\n\r") p
        ]

collectDoc :: FilePath -> FilePath -> IO L.ByteString
collectDoc elmDoc dir = getExactDeps (dir </> "exact-dependencies.json") >>=
    mapM (\(pkg, ver) -> runMaybeT $ do
        (smy, mfs) <- getModules (dir </> "packages" </> foldr sep "" (T.unpack pkg) </> T.unpack ver </> "elm-package.json")
        ms <- forM mfs $ \(_, file) -> MaybeT (getModuleDoc elmDoc file)
        return $ JSON.object ["package" JSON..= pkg, "summary" JSON..= smy, "modules" JSON..= ms]
        ) >>= return . JSON.encode . catMaybes
        
  where
    sep '/' b = pathSeparator : b
    sep a   b = a : b

main :: IO ()
main = do
    doc <- collectDoc "elm-doc" "elm-stuff"
    let style  = $(TH.runIO (readFile "./dist/main.css") >>= TH.stringE)
        script = $(TH.runIO (readFile "./dist/main.js") >>= TH.stringE)
        index  = $(TH.runIO (readFile "./html/index.html") >>= TH.stringE)
        processTag (TS.TagOpen  "style-place" a) = [TS.TagOpen  "style" a, TS.TagText style]
        processTag (TS.TagClose "style-place")   = [TS.TagClose "style"]
        processTag (TS.TagOpen  "db-script-place" a) = [TS.TagOpen  "script" a, TS.TagText doc]
        processTag (TS.TagClose "db-script-place")   = [TS.TagClose "script"]
        processTag (TS.TagOpen  "elm-script-place" a) = [TS.TagOpen  "script" a, TS.TagText script]
        processTag (TS.TagClose "elm-script-place")   = [TS.TagClose "script"]
        processTag a = [a]
        tag    = concatMap processTag $ TS.parseTags (index :: L.ByteString)
    L.putStrLn $ TS.renderTags tag
