module Main(main) where

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.Foldable (find)
import Control.Arrow ((>>>), (&&&))
--import Control.Arrow.ArrowList (constA)
import Control.Monad (when, forM_)
import Text.XML.HXT.Core (readDocument, getChildren, getText, XmlTree, getName, ArrowXml, runLA, getAttrl, runX, withValidate, no, XNode(..))
import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
import Text.XML.HXT.Expat (withExpat)
import qualified Text.XML.HXT.DOM.XmlNode as XN
--import Data.Maybe (fromJust)
--import Debug.Trace (trace)

--trace2 x = trace (show x) x

data Context = Context { getCurrentNode :: XmlTree
                       , getParentContext :: Context
                       }
             | Root

getContextName Root = ""
getContextName ctx = basePath ++ (justOrEmpty . XN.getQualifiedName . getCurrentNode $ ctx)
    where basePath = case (getContextName . getParentContext $ ctx) of 
                        "" -> ""
                        "/" -> "/"
                        path -> path ++ "/"

findMatching :: XN.XmlNode a => String -> [a] -> [a]
findMatching arg = filter (\x -> (justOrEmpty $ XN.getQualifiedName x) == arg)

getRootElems :: IO [XmlTree]
getRootElems = do
    args <- getArgs
    let src = head args
    runX $
        readDocument [ withValidate no
                     , withExpat True
                     , withCurl []
                     ]
                     src

main :: IO ()
main = do
    rootElems <- getRootElems
         -- >>> getChildren >>> isElem
    let root = head . filter XN.isElem $ rootElems
    loop (Context root Root)
    putStrLn "Exiting..."

loop :: Context -> IO ()
loop ctx = do
    let elem = getCurrentNode ctx
        parentCtx = getParentContext ctx
        elems = XN.getChildren elem
    putStr . getContextName $ ctx
    cmdline <- prompt "$ "
    when (length cmdline == 0) $ loop ctx

    let (cmd:args) = words cmdline
    case cmd of
        "exit" -> return ()
        "ls" -> do
            printElemChildren elems
            loop ctx
        "cd" -> commandCD ctx elems args
        _    -> loop ctx

commandCD :: Context -> [XmlTree] -> [String] -> IO ()
commandCD ctx elems args = case head args of
    "." -> loop ctx
    ".." -> case getParentContext ctx of
                Root -> loop ctx
                parentCtx -> loop parentCtx
    _ -> case findChild args elems of
            Nothing -> loop ctx
            Just child -> loop (Context child ctx)

findChild :: [String] -> [XmlTree] -> Maybe XmlTree
findChild cdArgs elems = find (const True) . findMatching (head cdArgs) $ elems

--printElemChildren :: [XmlTree] -> IO ()
printElemChildren :: XN.XmlNode a => [a] -> IO ()
printElemChildren elems = do
    forM_ (zip elems [(1::Integer)..]) $ 
        \(name,index) -> putStrLn $ (justOrEmpty . XN.getQualifiedName $ name) ++ " " ++ (show index)

printElem :: String -> XN.NTree XNode -> IO ()
printElem tab el@(XN.NTree _ children) = do
    when (XN.isElem el) (printActualElem tab el) 


printActualElem :: String -> XN.NTree XNode -> IO ()
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
--    getChildren >>> isElem

justOrEmpty :: Maybe String -> String
justOrEmpty Nothing = ""
justOrEmpty (Just x) = x

getAttrVals :: XmlTree -> [(String, String)]
getAttrVals = runLA (getAttrl >>> getName &&& (getChildren >>> getText))


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine