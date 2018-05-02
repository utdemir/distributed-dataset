{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

--------------------------------------------------------------------------------
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.List
import Data.Conduit
import Data.Conduit.Zlib
import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid
import Control.Lens
import Data.Time.Calendar
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Conduit.JSON.NewlineDelimited as NDJ
import qualified Data.Conduit.Combinators as C
--------------------------------------------------------------------------------
import Network.Serverless.Execute
import Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

opts :: LambdaBackendOptions
opts = LambdaBackendOptions { _lboBucket = "serverless-batch"
                            , _lboPrefix = "testprefix"
                            , _lboStackPrefix = "serverlessbatchtest"
                            }

main :: IO ()
main = do
  initServerless
  withLambdaBackend opts $ \backend -> do
    asyncs <- mapM (\u -> threadDelay 10000 >> async (spawn backend u)) urls
    result <- mconcat <$> mapM wait asyncs
    M.toList (MM.getMonoidalMap result)
      & sortOn snd
      & reverse
      & take 10
      & mapM_ print

spawn :: Backend -> String -> IO (MM.MonoidalMap T.Text (Sum Int))
spawn backend url = do
  putStrLn $ "Spawning function for: " ++ url
  r <- execute
    backend
    (static Dict)
    (static processUrl `cap` cpure (static Dict) url)
  putStrLn $ "Got response from: " ++ url
  return $ MM.MonoidalMap r

urls :: [String]
urls =
  [ "http://data.gharchive.org/" ++ d ++ "-" ++ t ++ ".json.gz"
  | d <- showGregorian <$> [fromGregorian 2017 1 1 .. fromGregorian 2018 5 1]
  , t <- show <$> [(0::Int)..23]
  ]

processUrl :: String -> IO (M.Map T.Text (Sum Int))
processUrl str = do
  req <- parseRequest str
  res <- runConduitRes $
    httpSource req getResponseBody
      .| ungzip
      .| NDJ.valueParser
      .| C.foldMap processObject
  return $ MM.getMonoidalMap res

processObject :: Value -> MM.MonoidalMap T.Text (Sum Int)
processObject v = fromMaybe mempty $ do
  name <- v ^? key "actor" . key "login" . _String
  commits <- V.toList <$> v ^? key "payload" . key "commits" . _Array
  let msgs = catMaybes $ map (^? key "message" . _String) commits
      count = length $ filter (T.isInfixOf "monad" . T.toLower) msgs
  guard $ count > 0
  return $ MM.singleton name (Sum count)
