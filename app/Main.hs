{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Common ()

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.XSLT
import Text.XML.HXT.XSLT.Compilation
import System.Console.CmdArgs
import System.FilePath (takeExtension, takeBaseName, (</>), (<.>), hasDrive)
import Data.Strings (strStartsWith)
import System.Directory (listDirectory)
import Control.Monad (liftM2)


data Codegen = Codegen
    { input :: FilePath
    , output :: FilePath
    , mainNamespace :: String
    , scriptRoot :: FilePath
    } deriving (Show, Data, Typeable)

main :: IO ()
main = do
    cmdLine <- (cmdArgs args)
    case cmdLine of
        Codegen _ _ _ _ -> codegen cmdLine
    where
        args = Codegen
            { input = "." &= argPos 0 &= typDir &= opt "."
            , output = "codegen" &= argPos 1 &= typDir &= opt "codegen"
            , mainNamespace = "df" &= opt "df"
            , scriptRoot = "." &= typDir &= opt "."
            }

codegen :: Codegen -> IO ()
codegen cmdLine = do
    docs <- getLoweredXML cmdLine
    putStrLn ("Number of documents " ++ show (length docs))

getLoweredXML :: Codegen -> IO [XmlTree]
getLoweredXML cfg = do
    -- get list of source XML files
    files <- (map ((input cfg)</>)) <$> (filter fileFilter) <$> (listDirectory (input cfg))

    -- build list of XSLT filepaths
    transforms <- pure [ (scriptRoot cfg) </> (t <.> "xslt") | t <- ["lower-1", "lower-2"] ]

    -- parse & compile the XSLT stylesheets
    ss <- mapM ( \t -> runX ( constA (filepathURI t) >>> xsltCompileStylesheetFromURI ) ) transforms

    -- build the composite transform
    transform <- pure (seqA (map xsltApplyStylesheet (concat ss)))

    -- process the source XML files to lowered XML
    docs <- mapM (\t -> runX (readDocument [withValidate no] (filepathURI t) >>> transform)) files

    -- done
    return (concat docs)

        where
            fileFilter :: FilePath -> Bool
            fileFilter f = ((takeExtension f) == ".xml") && ((takeBaseName f) `strStartsWith` ((++".") (mainNamespace cfg)))
            filepathURI :: FilePath -> String
            filepathURI f =
                case (hasDrive f) of
                    True -> "file:///" ++ f
                    False -> f



