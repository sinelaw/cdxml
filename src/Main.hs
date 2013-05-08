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

findMatching args = filter (\x -> (justOrEmpty $ XN.getQualifiedName x) == (head args))


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

	loop [rootElems]

	where loop elems = do
		forM_ (zip elems [1..]) $ \(name,index) -> putStrLn $ (justOrEmpty . XN.getQualifiedName $ name) ++ " " ++ (show index)
		cmdline <- readLn
		let (cmd:args) = words cmdline
		case cmd of
			--"cd" -> loop . getChildren . head . findMatching args $ elems
			_    -> return ()




printElem tab el@(XN.NTree _ children) = do
	when (XN.isElem el) (printActualElem tab el) 


printActualElem tab el@(XN.NTree _ children) = do
	let name = XN.getQualifiedName el
	putStr $ tab ++ "<" ++ (justOrEmpty name)
	let attrVals = getAttrVals el
	forM_ attrVals $ \x -> putStrLn $ tab ++ "    " ++ (fst x) ++ "=\"" ++ (snd x) ++ "\""
	putStrLn $ (if (null attrVals) then "" else tab) ++ ">\n"
	forM_ children $ printElem (tab ++ "    ")
	putStrLn $ tab ++ "</" ++ (justOrEmpty name) ++ ">"

--getName node@(NTree (XTag qName _) children) = 
--commandLoop :: ArrowXml cat => cat (NTree XNode) XmlTree
--commandLoop =
--	getChildren >>> isElem

justOrEmpty Nothing = ""
justOrEmpty (Just x) = x

getAttrVals :: XmlTree -> [(String, String)]
getAttrVals = runLA (getAttrl >>> getName &&& (getChildren >>> getText))
