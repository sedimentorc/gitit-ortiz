module Pdflatex (plugin) where

-- This plugin allows you to include a LaTeX body in a page like this:
--
-- ~~~ { .pdflatex }
-- \begin{tikzpicture}[sibling distance=48pt]
--   \Tree [ .{$a \wedge b$}
--           [ .{$\neg a \vee c$} 
--             [ .{$\neg c \vee \neg b$}
--               [ .\node(n1){$\neg c1$}; {$\neg a$} \node(n2){$c$}; ]
--               [ .\node(m1){$\neg b$}; [ .{$a$} \node(m2){$b$}; ] ]
--             ]
--           ]
--         ]
--   \draw[->] (n2)..controls +(north east:1) and +(east:1)..(n1);
--   \draw[->] (m2)..controls +(east:1) and +(east:1)..(m1);
-- \end{tikzpicture}
-- ~~~
--
-- Study the skeleton of the LaTeX source file at the end of this file.
-- The "pdflatex", "pdfcrop", and "convert" executabless must be in the path.
-- The generated png file will be saved in the static img directory.
-- If no name is specified, a unique name will be generated from a hash
-- of the file contents.

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>), (<.>))
import System.IO (writeFile)
-- import Control.Monad.Trans (liftIO)
import System.Directory

import Network.Gitit.Interface


------------------------------------------------------------------------------
-- # latex eqn.tex
-- # dvips -q -f -e 0 -E -D 10000 -x 10000 -o eqn.ps eqn.dvi
-- # pstoedit -f plot-svg -dt -ssp eqn.ps eqn.svg
plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "pdflatex" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                           Just fn   -> ([Str fn], fn)  --used to say svg
                           Nothing   -> ([], uniqueName contents) --used to say svg
  
  curr <- liftIO getCurrentDirectory                         
  
  liftIO $ do
    let basePath = outfile  --staticDir cfg </> "/wikidata" </> 
    --------------------------------------------------------------------------
    -- $ cat > ${TEMP}.tex
    --------------------------------------------------------------------------
    let outfilePath = basePath <.> "tex"
    writeFile outfilePath (makebody contents)
    --------------------------------------------------------------------------
    -- $ latex ${TEMP}.tex
    --------------------------------------------------------------------------
    
-- 1. pdflatex filename (with or without extensions)
-- 2. bibtex filename (without extensions)
-- 3. pdflatex filename (with or without extensions)
-- 4. pdflatex filename (with or without extensions)
    
    let infilePath = outfilePath
    let outfilePath = basePath <.> "pdf"
    performUnixCommand "pdflatex" [infilePath]
    performUnixCommand "bibtex" [basePath]
    performUnixCommand "pdflatex" [infilePath]
    performUnixCommand "pdflatex" [infilePath]
    
    performUnixCommand "rm" [basePath<.>"aux"]
    performUnixCommand "rm" [basePath<.>"bbl"]
    performUnixCommand "rm" [basePath<.>"bib"]
    performUnixCommand "rm" [basePath<.>"blg"]
    performUnixCommand "rm" [basePath<.>"log"]
    performUnixCommand "rm" [basePath<.>"tex"]
    performUnixCommand "mv" [basePath<.>"pdf", "wikidata"]
    
    setCurrentDirectory "wikidata"
    performUnixCommand "git" ["add", outfile++".pdf"]
    performUnixCommand "git" ["commit", "-m", "\"compile pdflatex\""]

    
    --------------------------------------------------------------------------
    -- $ pdfcrop ${TEMP}.pdf
    --------------------------------------------------------------------------
    -- I commented out the following three lines
    --let infilePath = outfilePath
    --let outfilePath = outfile ++ "-crop" <.> "pdf"
    --performUnixCommand "pdfcrop" [infilePath]
    --------------------------------------------------------------------------
    -- $ pdf2ps ${TEMP}.pdf ${TEMP}.ps
    --------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = outfile ++ "-crop" <.> "ps"
    --performUnixCommand "pdf2ps" [infilePath,outfilePath]
    --------------------------------------------------------------------------
     -- $ dvips -q -f -e 0 -E -D 10000 -x 10000 -o ${TEMP}.ps ${TEMP}.dvi
    --------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = outfile <.> "ps"
    --performUnixCommand "dvips" ["-q", "-f", "-e", "0", "-E", "-D", "10000", "-x", "2000", "-o", outfilePath, infilePath]
    --------------------------------------------------------------------------
    -- $ pstoedit -f plot-svg -dt -ssp ${TEMP}.ps ${TEMP}.svg
    --------------------------------------------------------------------------
    --let infilePath = outfilePath
    --let outfilePath = basePath
    --performUnixCommand "pstoedit" ["-f", "plot-svg", "-dt", "-ssp", infilePath, outfilePath]
    --------------------------------------------------------------------------
    -- $ pdf2svg ${TEMP}.pdf ${TEMP}.svg
    --------------------------------------------------------------------------
    -- I commented out the following three lines
    -- let infilePath = outfilePath
    -- let outfilePath = basePath
    -- performUnixCommand "pdf2svg" [infilePath, outfilePath]
    --------------------------------------------------------------------------
    -- ### html
    --------------------------------------------------------------------------
--    return $ RawHtml ("<p><object type=\"image/svg+xml\" data=\""

--   return $ RawBlock (Format "html") ("<p><object type=\"image/svg+xml\" data=\""
--                      ++ ("/img" </> outfile)
--                      ++ "\"></object></p>")

-- <object data="myfile.pdf" type="application/pdf" width="100%" height="100%"> 
--  <p>It appears you don't have a PDF plugin for this browser.
--  No biggie... you can <a href="myfile.pdf">click here to
--  download the PDF file.</a></p>  
-- </object>

    setCurrentDirectory curr
 
                      
    return $ RawBlock (Format "html") ("<p><object data=\""++ outfile ++".pdf\""
                      ++ " type=\"application/pdf\" width=\"100%\" height=\"1000\">"
                      ++ " <p>It appears you don't have a PDF plugin for this browser."
                      ++ " No biggie... you can <a href=\""++ outfile ++".pdf\">click here to"
                      ++ " download the PDF file.</a>"++basePath++"</p>"
                      ++ " </object></p>")
  
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

performUnixCommand cmd args = do
  (exitcode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  if exitcode == ExitSuccess
    then return ()
    else error $ concat [ "\nError! "
                        , cmd
                        , " returned error status "
                        , show exitcode
                        , "\n  with stdout: \n"
                        , stdout
                        , "\n  with stdout: \n"
                        , stderr
                        ]

makebody xx
    = unlines
      [ 
       xx 
      ]
