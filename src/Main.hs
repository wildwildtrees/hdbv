module Main where

import Control.Exception
import Control.Monad
import GHC.IO.Exception 
import Text.Read
import Text.Show.Pretty
import qualified Data.Map as Map 
import System.Environment 
import System.Exit

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile filepath = catch (fmap Just . readFile $ filepath) (\e ->
  case ioe_type e of
    NoSuchThing -> pure Nothing
    _ -> throw e 
  ) 

filename = "database.dbv"

--debug = True
debug = False

debugOut string = when debug . putStrLn $ string


readMap :: String -> Map.Map String String
readMap string = do 
  let maybeMap = (readMaybe string) :: Maybe (Map.Map String String)
  maybe (Map.fromList []) id maybeMap 

main :: IO ()
main = do
  maybeContents <- maybeReadFile filename
  let contents = maybe "" id maybeContents 
  debugOut "contents:"
  debugOut contents 
  debugOut "Done"
  let hash = readMap contents 
  args <- getArgs 
  case args of
    ("get":var:[]) -> dbvGet var hash
    ("set":var:value:[]) -> dbvSet var value hash 
    _ -> exitFailure  
  return () 
    

dbvGet var hash = do
  let maybeValue = Map.lookup var hash
  case maybeValue of 
    Just value -> do
      putStr value
      exitSuccess 
    Nothing -> do
      exitFailure 

dbvSet var value hash = do 
  let newHash = Map.insert var value hash 
  writeFile filename . ppShow $ newHash 
  exitSuccess 



