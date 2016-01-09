module ImageTable (plugin) where

-- This plugin allows you to include a table of images in a page:
--



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

import Data.Text as T
import Data.Maybe
import Data.List


plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block

-- the next line sets the string "imagetable" as the reserved keyword that sets the entrypoint into this plugin
transformBlock (CodeBlock (_, classes, namevals) contents) | "imagetable" `elem` classes = do

  cfg <- askConfig


  
  curr <- liftIO getCurrentDirectory                         
  
  liftIO $ do
    let basePath = staticDir cfg </> "/wikidata" 
    --------------------------------------------------------------------------
    -- $ cat > ${TEMP}.tex
    --------------------------------------------------------------------------
    let mylines = Prelude.lines contents
    let mytext = Prelude.map T.pack mylines
    let myspliton = T.splitOn (T.pack "=")
    let mytextentries = Prelude.concat (Prelude.map myspliton mytext)
    let extractEven = Prelude.map snd . Prelude.filter (\(x,y) -> (mod x 2) == 0) . Prelude.zip [1..]
    let extractOdd = Prelude.map snd . Prelude.filter (\(x,y) -> (mod x 2) == 1) . Prelude.zip [1..]
    let mytextparms = extractOdd mytextentries
    let mystringparms = Prelude.map T.unpack mytextparms
    let mytextvalues = extractEven mytextentries
    let mystringvalues = Prelude.map T.unpack mytextvalues
    --------------------------------------------------------------------------
    -- $ latex ${TEMP}.tex
    --------------------------------------------------------------------------
    
-- 1. pdflatex filename (with or without extensions)
-- 2. bibtex filename (without extensions)
-- 3. pdflatex filename (with or without extensions)
-- 4. pdflatex filename (with or without extensions)



    setCurrentDirectory curr
    
    let parmstr=Prelude.unwords mystringparms
    let valuestr=Prelude.unwords mystringvalues
 
    let ind=elemIndex "path" mystringparms
    let mypath=mystringvalues!!(fromMaybe 0 ind) -- this line is slightly unsafe in that if there is no path field, it crashes.

    let files = getDirectoryContents mypath
    let allfiles = getFileList mypath

                  
    return $ RawBlock (Format "html") ("<p>your basepath is "++ basePath ++"</p>"
				      ++ "<p>your current directory is "++ curr ++"</p>"
                      ++ "<p>your entries are "++ parmstr ++"</p>"
                      ++ "<p>your values are "++ valuestr ++"</p>"
                      ++ "<p>your path is " ++ mypath ++"</p>"
                      )
                      -- ++ "<p>your files are " ++ allfiles ++"</p>"
                      --)

-- in case of failed execution, just return the input back into the page
transformBlock x = return x


-- FUNCTIONS BELOW

-- Function uniqueName
-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

-- Function perofmrUnixCommand
-- | Call a unix command
performUnixCommand cmd args = do
  (exitcode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  if exitcode == ExitSuccess
    then return ()
    else error $ Prelude.concat [ "\nError! "
                        , cmd
                        , " returned error status "
                        , show exitcode
                        , "\n  with stdout: \n"
                        , stdout
                        , "\n  with stdout: \n"
                        , stderr
                        ]

-- Function makebody
-- | extract the contents in the codeblock for the plugin
makebody xx
    = T.unlines --unlines is an inverse operation to lines. It joins lines, after appending a terminating newline to each.
      [ 
       xx 
      ]

-- Function extractEvery      
-- http://stackoverflow.com/questions/2026912/how-to-get-every-nth-element-of-an-infinite-list-in-haskell
extractEvery m = Prelude.map snd . Prelude.filter (\(x,y) -> (mod x m) == 0) . Prelude.zip [1..]

getFileList xx = do
  files <- getDirectoryContents xx
  return (Prelude.unwords files)
