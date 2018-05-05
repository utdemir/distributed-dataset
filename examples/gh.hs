{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

--------------------------------------------------------------------------------
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.Async           (async, wait)
import           Control.Lens                       ((&), (^?), (.~))
import           Control.Monad                      (guard)
import           Data.Aeson                         (Value)
import           Data.Aeson.Lens                    (key, _Array, _String)
import           Data.Conduit                       (runConduitRes, (.|))
import qualified Data.Conduit.Combinators           as C
import qualified Data.Conduit.JSON.NewlineDelimited as NDJ
import           Data.Conduit.Zlib                  (ungzip)
import           Data.List                          (sortOn)
import qualified Data.Map                           as M
import qualified Data.Map.Monoidal                  as MM
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Monoid                        (Sum (Sum), mconcat)
import qualified Data.Text                          as T
import           Data.Time.Calendar                 (Day, fromGregorian,
                                                     showGregorian)
import           Data.Time.Clock.POSIX              (getPOSIXTime)
import qualified Data.Vector                        as V
import           Network.HTTP.Simple                (getResponseBody,
                                                     httpSource, parseRequest)
--------------------------------------------------------------------------------
import           Network.Serverless.Execute
import           Network.Serverless.Execute.Lambda
--------------------------------------------------------------------------------

opts :: LambdaBackendOptions
opts = lambdaBackendOptions "serverless-batch"
         & lboMemory .~ 1024

main :: IO ()
main = do
  initServerless
  withLambdaBackend opts $ \backend -> do
    asyncs <- mapM
      (\u -> threadDelay 10000 >> async (spawn backend u))
      allUrls
    result <- mconcat <$> mapM wait asyncs
    result
      & M.toList . MM.getMonoidalMap
      & reverse . sortOn snd
      & mapM_ print . take 50

spawn :: Backend -> String -> IO (MM.MonoidalMap T.Text (Sum Int))
spawn backend url = do
  putStrLn $ "Spawning function for: " ++ show url
  start <- getPOSIXTime

  r <- execute
    backend
    (static Dict)
    (static processUrl `cap` cpure (static Dict) url)

  end <- getPOSIXTime

  putStrLn $ "Got response from " ++ show url ++
               " in " ++ show (end - start) ++ " seconds."
  return $ MM.MonoidalMap r

--------------------------------------------------------------------------------

startDate :: Day
startDate = fromGregorian 2017 8 1

endDate :: Day
endDate = fromGregorian 2017 31 31

allUrls :: [String]
allUrls =
  [ "http://data.gharchive.org/" ++ d ++ "-" ++ t ++ ".json.gz"
  | d <- showGregorian <$> [startDate .. endDate]
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
      count = length $ filter (T.isInfixOf "cabal" . T.toLower) msgs
  guard $ count > 0
  return $ MM.singleton name (Sum count)
