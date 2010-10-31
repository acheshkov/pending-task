module Types 
(
	Task(..),
	Key,
	Value
)
where


import Text.JSONb
import qualified Data.ByteString as B
import qualified Data.Trie as DT
import Monad
import Data.ByteString.Char8

type Key   = String
type Value = String
type RequestType = String

data Task = HTTPTask {
	getParams   ::  [(B.ByteString, B.ByteString)],
	postParams  ::  [(B.ByteString, B.ByteString)],
	rawPostData ::  B.ByteString,
	taskUrl         ::  B.ByteString,
	taskKey         ::  B.ByteString,
	requestType     ::  B.ByteString
} deriving (Show)

tryConstuctHTTPTask :: B.ByteString -> Maybe Task
tryConstuctHTTPTask str = do
	case decode str of
		Left _  -> fail ""
		Right (Object trie) -> do
			Object getP    <- DT.lookup (pack "getParams")   trie
			Object postP   <- DT.lookup (pack "postParams")  trie
			String rawData <- DT.lookup (pack "rawPostData") trie
			String url     <- DT.lookup (pack "url") trie
			String key     <- DT.lookup (pack "key") trie
			String rqType     <- DT.lookup (pack "requestType") trie
			g' <- return $ DT.toList getP
			p' <- return $ DT.toList postP
			g  <- case g' of 
				[] -> return []
				t1@((a,String s):xs) -> return t1
				otherwise -> fail ""
			p  <- case p' of 
				[] -> return []
				t2@((a,String s):xs) -> return t2
				otherwise -> fail ""
			let getParamsT  = Prelude.map (\(x,String y) -> (x,y)) g
			    postParamsT = Prelude.map (\(x,String y) -> (x,y)) p
			return $ HTTPTask  getParamsT postParamsT rawData url key rqType
