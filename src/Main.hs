{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import qualified Data.ByteString as BS
import Data.List.Utils (uniq)
import Data.String.Utils
import System.Environment
import System.FilePath.Posix
import System.Directory
import Network.Curl.Download
import Network.Curl.Opts
import Network.URI
import Text.HTML.TagSoup.Match()
import Text.HTML.TagSoup


main :: IO ()
main = do
    (arg:_) <- getArgs
    cwd <- getCurrentDirectory
    case parseURI arg of
        Nothing -> putStrLn $ "Invalid URI: " ++ arg
        Just uri -> let siteDir = fromMaybe "./" $ uriRegName <$> uriAuthority uri
                    in do createDirectoryIfMissing True siteDir
                          setCurrentDirectory siteDir
                          doMirror uri (cwd ++ "/" ++ siteDir)

doMirror :: URI -> FilePath -> IO ()
doMirror uri cwd = do
    let urlStr = uri2String uri
    result <- openURIString urlStr
    case result of
        Left err -> putStrLn $ "Download error: " ++ err
        Right doc -> 
            let 
                urlList = makeDlList uri $ parseTags doc
                dir = takeDirectory $ uriPath uri
            in do 
                forM_ (uniq urlList)
                    (\x -> do
                        isExists <- doesLocalExists x cwd
                        unless isExists $
                            if takeExtension (takeFileName x) `elem` forkFor
                                then case parseURI x of
                                       Just a -> void (putStrLn ("Fork for " ++ show x) >> forkIO (doMirror a cwd))
                                       Nothing -> return ()
                                else putStrLn ("Download " ++ x) >> urlToFileBs x cwd
                    )
                createDirectoryIfMissing True (cwd ++ dir) 
                writeFile (cwd ++ dir ++ "/" ++ normalizeFileName urlStr) doc


doesLocalExists :: String -> FilePath -> IO Bool
doesLocalExists uri cwd = 
 case parseURI uri of
  Nothing -> return False
  Just a -> doesFileExist $ normalise $ cwd ++ uriPath a


urlToFileBs :: String -> FilePath -> IO ()
urlToFileBs url cwd =
    case parseURI url of
        Nothing -> return ()
        Just uri -> do 
                    r <- openURIWithOpts [CurlFollowLocation True, CurlAutoReferer True, CurlTimeout 10, userAgent] url
                    let
                        cont = case r of
                                Left _ -> BS.empty
                                Right content -> content
                        dir = takeDirectory $ uriPath uri
                        file = normalizeFileName $ uriPath uri
                    createDirectoryIfMissing True (cwd ++ dir)
                    BS.writeFile (normalise $ cwd ++ dir ++ "/" ++ file) cont


strToLower :: String -> String
strToLower = map toLower

normalizePath :: String -> URI -> String
normalizePath name url = 
    if startswith "http" $ strToLower name 
        then name
        else pathToUrl name url

pathToUrl :: String -> URI -> String
pathToUrl name url = 
    if startswith "/" name
        then foldl (++) "" [scheme, user, reg, port, name]
        else foldl (++)  "" [scheme, user, reg, port, path, name]
    where
        scheme = uriScheme url ++ "//"
        user = fromMaybe "" $ uriUserInfo <$> uriAuthority url
        reg = fromMaybe "" $ uriRegName <$> uriAuthority url
        port = fromMaybe "" $ uriPort <$> uriAuthority url
        path = uriPath url

normalizeFileName :: String -> String
normalizeFileName name = 
    case takeFileName name of
        "" -> "index.html"
        file -> head $ split "#" file

makeDlList :: URI -> [Tag String] -> [String]
makeDlList url = foldl (\acc t -> 
    case t of
        TagOpen "img" _ -> normalizePath (fromAttrib "src" t) url : acc
        TagOpen "link" _ -> normalizePath (fromAttrib "href" t) url : acc
        TagOpen "script" _ -> if fromAttrib "src" t == ""
                                  then acc
                                  else normalizePath (fromAttrib "src" t) url : acc
        TagOpen "a" _ -> let 
                            scheme = uriScheme url ++ "//"
                            user = fromMaybe "" $ uriUserInfo <$> uriAuthority url
                            reg = fromMaybe "" $ uriRegName <$> uriAuthority url
                            port = fromMaybe "" $ uriPort <$> uriAuthority url
                            path = uriPath url
                            ref = normalizePath (fromAttrib "href" t) url
                          in if startswith (scheme ++ user ++ reg ++ port ++ path) ref
                                 then ref : acc
                                 else acc
        _ -> acc
    ) []

userAgent :: CurlOption
userAgent = CurlUserAgent "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.4 (KHTML, like Gecko) Chrome/22.0.1229.94 Safari/537.4"

uri2String :: URI -> String
uri2String uri =
    let Just a  = uriAuthority uri
    in foldl (++) "" [uriScheme uri, "//", uriUserInfo a, uriRegName a, uriPort a, uriPath uri, uriQuery uri, uriFragment uri]

forkFor :: [String]
forkFor = [".html", ".htm", ".php", ".cgi", ".pl", ".py", ".asp", ".shtml", ".yaws", ".jsp"]
