module Main(main) where

import System.Environment (getArgs)

import Control.Arrow (first, (>>>), (&&&), (***))
import Control.Arrow.ArrowList (constA)
import Control.Monad (when, forM_)
import Text.XML.HXT.Core (readDocument, getChildren, getText, isElem, XmlTree, XNode(..), deep, getName, localPart, hasName, ArrowXml, runLA, getAttrl, runX, withValidate, no)
import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
import Text.XML.HXT.Expat (withExpat)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Maybe (fromJust)

main = do
	args <- getArgs
	let src = head args
	rootElems <- runX $
		readDocument
  		[ withValidate no
  		, withExpat True
  		, withCurl []
  		]
  		src
      	-- >>> getChildren >>> isElem

	forM_ rootElems $ printElem ""


printElem tab el@(XN.NTree _ children) = do
	let name = XN.getQualifiedName el
	case name of
		Nothing -> putStr $ tab ++ "<"
		Just theName -> putStr $ tab ++ "<" ++ theName
	let attrVals = getAttrVals el
	forM_ attrVals $ \x -> putStrLn $ tab ++ "    " ++ (fst x) ++ "=\"" ++ (snd x) ++ "\""
	putStrLn $ (if (null attrVals) then "" else tab) ++ ">\n"
	forM_ (filter XN.isElem children) $ printElem (tab ++ "    ")
	putStrLn $ tab ++ "</" ++ (fromJust name) ++ ">"

--getName node@(NTree (XTag qName _) children) = 
--commandLoop :: ArrowXml cat => cat (NTree XNode) XmlTree
--commandLoop =
--	getChildren >>> isElem

getAttrVals :: XmlTree -> [(String, String)]
getAttrVals = runLA (getAttrl >>> getName &&& (getChildren >>> getText))
