{-# LANGUAGE OverloadedStrings #-}

module FileIO where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.FilePath
import           Control.Exception (throwIO)
import           Text.Pandoc

readMarkdownFile :: FilePath -> IO Pandoc
readMarkdownFile filepath = do
  txt <- T.readFile filepath 
  case runPure (readMarkdown pandocReaderOptions txt) of
    Left err ->
      throwIO $ userError (show err)
    Right doc ->
      return doc


pandocReaderOptions :: ReaderOptions
pandocReaderOptions
  = def { readerExtensions = pandocExtensions
        , readerStripComments = True
        }


writeMarkdownFile :: FilePath -> Pandoc -> IO ()
writeMarkdownFile filepath doc = do
  optTpl <- runIO (compileDefaultTemplate "markdown")
  case optTpl of
    Left err ->
      throwIO $ userError (show err)
    Right tpl -> do
      result <- runIO (writeMarkdown (writerOptions tpl) doc)
      case result of
        Left err ->
          throwIO $ userError (show err)
        Right text ->
          T.writeFile filepath text

writerOptions :: Template Text -> WriterOptions
writerOptions tpl
  = def
    { writerTemplate = Just tpl
    , writerExtensions = pandocExtensions
    , writerSetextHeaders = False
    }

  
-- add language extension to file name
addLanguage :: Text -> FilePath-> FilePath
addLanguage lang path =
  let dir  = takeDirectory path
      name = takeBaseName path
      ext  = takeExtension path
  in normalise (dir </> (name ++ "_" ++ T.unpack lang <.> ext))
