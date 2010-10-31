module Log
(
	createLog,
	Loger(..)
)
where

import Control.Concurrent
import Data.DateTime


class Loger a where 
	doLog :: a -> String -> IO ()

data Log = Log (MVar [String]) FilePath (MVar Int)

instance Loger Log where
	doLog l@(Log mvar fpath s) message = do 
		modifyMVar_ mvar (\a -> return(a ++ [message]))
		forkIO $ writeToFileLogThread l
		return ()


writeToFileLogThread :: Log -> IO ()
writeToFileLogThread (Log mvar fpath s) = do
 dt <- getCurrentTime
 msgs <- modifyMVar mvar (\a -> return ([],a))
 v <- takeMVar s
 mapM_ (\msg -> appendFile fpath $ (show dt)++ ":" ++ msg++"\n") msgs
 putMVar s v
-- mapM_ (\msg -> print $ (show dt)++":"++ msg) msgs
 
createLog :: FilePath -> IO Log
createLog fp = do
	mvar <- newMVar []
	semaphor <- newMVar 1
	return $ Log mvar fp semaphor
