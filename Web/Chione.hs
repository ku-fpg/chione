{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs, FlexibleContexts #-}
module Web.Chione
         ( -- * main things
           clean
          -- * key directory names
         , build_dir
         , html_dir
         , admin_dir
         -- * Build target detection
         , findBuildTargets
         -- * Utils
         , makeHtmlRedirect
         -- * Link and URL issues
         , findLinks
         , LinkData(..)
         , getURLResponse
         , URLResponse(..)
         -- * Building content
         , generateStatus
         -- * KURE rewrites
         , findURL
         , mapURL
         , injectHTML
         , insertTeaser
        , module Web.Chione     -- include everything right now
        ) where

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath
import Development.Shake.Classes

import System.Directory hiding (doesFileExist)
import qualified System.Directory as Directory
import Control.Monad
import qualified Control.Exception as E
import System.Posix (getSymbolicLinkStatus, isDirectory)
import Control.Arrow
import Control.Applicative hiding ((*>))
import Data.List
import Data.Char
import Data.Time.Clock
import System.Exit

import Language.KURE.Walker
import Language.KURE.Debug

import qualified Language.KURE as KURE
import Language.KURE hiding (apply)

import System.Process

import Text.HTML.KURE

import Data.Monoid

import Control.Concurrent.ParallelIO.Local

-- | Name of location for all generated files.
-- Can always be removed safely, and rebuilt.
build_dir :: String
build_dir    = "_make"

-- | Name of location of our target HTML directory.
html_dir :: String
html_dir    = build_dir </> "html"

-- | Name of location of our admin HTML directory.
admin_dir :: String
admin_dir    = build_dir </> "admin"

-- | Name of location of our HTML contents directory.
contents_dir :: String
contents_dir    = build_dir </> "contents"


-- | 'findBuildTargets' looks to find the names and build instructions for
-- the final website. The first argument is the subdirectory to look into,
-- the second is the suffix to find.

findBuildTargets :: String -> String -> IO [String]
findBuildTargets subdir suffix = do
        contents <- getRecursiveContents subdir
        return $ filter ((subdir ++ "//*." ++ suffix) ?==) $ contents


-- (local to this module.)
-- From RWH, first edition, with handle from Johann Giwer.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = E.handle (\ E.SomeException {} -> return []) $ do       -- 5
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    s <- getSymbolicLinkStatus path
    if isDirectory s
      then getRecursiveContents path
      else return [path]
  return (concat paths)


------------------------------------------------------------------------------------

findURL :: (Monad m) => Translate Context m Attr String
findURL = do    (nm,val) <- attrT (,)
                cxt@(Context (c:_)) <- contextT
                tag <- KURE.apply getTag cxt c
                case (nm,[tag]) of
                   ("href","a":_)     -> return val
                   ("href","link":_)  -> return val
                   ("src","script":_) -> return val
                   ("src","img":_)    -> return val
                   _                  -> fail "no correct context"

mapURL :: (Monad m) => (String -> String) -> Rewrite Context m Attr
mapURL f = do   (nm,val) <- attrT (,)
                cxt@(Context (c:_)) <- contextT
                tag <- KURE.apply getTag cxt c
                case (nm,[tag]) of
                   ("href","a":_)     -> return $ attrC nm $ f val
                   ("href","link":_)  -> return $ attrC nm $ f val
                   ("src","script":_) -> return $ attrC nm $ f val
                   ("src","img":_)    -> return $ attrC nm $ f val
                   _                  -> fail "no correct context"


-- | Replace given id (2nd argument) with an HTML file (filename is first argument).
--
-- > let tr = inject "Foo.hs" "contents"
--
-- DEAD CODE
injectHTML :: String -> String -> R HTML
injectHTML fileName idName = extractR' $ prunetdR (promoteR (anyElementHTML fn))
  where
        fn :: T Element HTML
        fn = do nm <- getAttr "id"
                debugR 100 $ show ("inject",idName,nm)
                if nm == idName
                 then translate $ \ _ _ -> do
                        file <- liftActionFPGM $ readFile' fileName
                        return $ parseHTML fileName file
                        -- read the file
                 else fail "no match"


insertTeaser :: T Element HTML
insertTeaser = do
                    "a"       <- getTag
                    "teaser"  <- getAttr "class"
                    ('/':url) <- getAttr "href"
                    inside    <- getInner

                    let sub_content = contents_dir </> replaceExtension url "html"

                    inside_content <- contextfreeT $ \ _ -> liftActionFPGM $ do
                            need [ sub_content ]
                            sub_txt <- readFile' sub_content
                            let sub_html = parseHTML sub_content sub_txt
                            applyFPGM (extractT' (onetdT (promoteT findTeaser))
                                        <+ return (text ("Can not find teaser in " ++ sub_content)))
                                        sub_html

                    return $ mconcat
                           [ inside_content
                           , element "a" [ attr "href" ('/':url)
                                       , attr "class" "label"
                                       ]
                                       inside
                           ]

  where
          findTeaser :: T Element HTML
          findTeaser = do
                      "div" <- getTag
                      "teaser" <- getAttr "class"
                      getInner

-----------------------------------------------------------------------

-- Build a redirection page.

makeHtmlRedirect :: String -> String -> Action ()
makeHtmlRedirect out target = do
        writeFile' out $ "<meta http-equiv=\"Refresh\" content=\"0; url='" ++ target ++ "'\">\n"

-----------------------------------------------

data UploadedPage
        = NotAttempted  -- -
        | NotPresent    -- cross
        | Different     -- !
        | Same          -- tick
        deriving (Show,Enum)

data LinkData a = LinkData
        { ld_pageName :: String
        , ld_bytes :: Int               -- bytes in file
        , ld_wc    :: Int               -- words of text
        , ld_match :: UploadedPage      -- does the web version match?
        , ld_localURLs :: a
        , ld_remoteURLs :: a
        }
        deriving Show

instance Functor LinkData where
        fmap f (LinkData n by wc ma a b) = LinkData n by wc ma (f a) (f b)

-- | Reads an HTML file, finds all the local and global links.
-- The local links are normalize to the site-root.
findLinks :: String -> String -> IO (LinkData [String])
findLinks prefix name = do
        txt <- readFile (html_dir </> name)
        let tree = parseHTML name txt

        let urls = fromKureM error $ KURE.apply (extractT $ collectT $ promoteT' $ findURL) mempty tree

        let txt_lens = fromKureM error $ KURE.apply (extractT $ collectT $ promoteT' $ textT (length . words)) mempty tree

        -- now try get the remote version

        remote <- getURLContent $ prefix ++ "/" ++ name
        let rem_stat = case remote of
                Nothing -> NotPresent
                Just txt' | txt' == txt -> Same
                          | otherwise   -> Different

        -- What about ftp?
        let isRemote url = ("http://" `isPrefixOf` url)
                        || ("https://" `isPrefixOf` url)

        let locals = [ takeDirectory name </> url
                     | url <- urls
                     , not (isRemote url)
                     ]

        let globals = filter isRemote urls

        return $ LinkData name
                          (length txt)
                          (sum txt_lens)
                          rem_stat
                          locals
                          globals

data URLResponse
        = URLResponse { respCodes :: [Int], respTime :: Int }
        deriving Show

-- | Check external link for viability. Returns time in ms, and list of codes returned; redirections are followed.

getURLResponse :: String -> IO URLResponse
getURLResponse url | "http://scholar.google.com/" `isPrefixOf` url = return $ URLResponse [200] 999
getURLResponse url = do
      urlRep <- response1
      case respCodes urlRep of
         [n] | n `div` 100 == 4
             -> do urlRep' <- response2
                   return $ URLResponse ([n] ++ respCodes urlRep') (respTime urlRep + respTime urlRep')
         _ -> return urlRep
  where
      response1 = do
        tm1 <- getCurrentTime
        (res,out,err) <- readProcessWithExitCode "curl"
                                ["-A","Other","-L","-m","10","-s","--head",url]
                                ""
        tm2 <- getCurrentTime
        let code = concat
               $ map (\ case
                  ("HTTP/1.1":n:_) | all isDigit n  -> [read n :: Int]
                  _                                 -> [])
               $ map words
               $ lines
               $ filter (/= '\r')
               $ out
        return $ URLResponse code (floor (diffUTCTime tm2 tm1 * 1000))
      response2 = do
        tm1 <- getCurrentTime
        (res,out,err) <- readProcessWithExitCode "curl"
                                 ["-A","Other","-L","-m","10","-s",
                                  "-o","/dev/null","-i","-w","%{http_code}",
                                  url]
                                ""
        tm2 <- getCurrentTime
        let code = concat
               $ map (\ case
                  (n:_) | all isDigit n  -> [read n :: Int]
                  _                                 -> [])
               $ map words
               $ lines
               $ filter (/= '\r')
               $ out
        return $ URLResponse code (floor (diffUTCTime tm2 tm1 * 1000))

----------------------------------------------------------

getURLContent :: String -> IO (Maybe String)
getURLContent url = do
        (res,out,err) <- readProcessWithExitCode "curl"
                                 ["-A","Other","-L","-m","5","-s",
                                  url]
                                ""
        case res of
          ExitSuccess -> return (Just out)
          _           -> return Nothing


-- Case sensitive version of doesFileExist. Important on OSX, which ignores case
-- then break the web server will use case.
doesCasedFileExist :: String -> IO Bool
doesCasedFileExist file = do
        ok <- Directory.doesFileExist file
        if not ok then return False else recUp file
  where
          recUp "."  = return True
          recUp path = do
             files <- getDirectoryContents (takeDirectory path)
             if takeFileName path `elem` files then recUp (takeDirectory path) else return False


----------------------------------------------------------


generateStatus :: String -> [String] -> Action HTML
generateStatus prefix inp = do
        let files = [ nm0
                    | nm0 <- inp
                    , "//*.html" ?== nm0
                    ]

        need [ html_dir </> file
             | file <- files
             ]
        links <- liftIO $ withPool 32
              $ \ pool -> parallelInterleaved pool
              [ findLinks prefix file
              | file <- files
              ]

        good_local_links <- liftM concat $ sequence
                         [ do b <- liftIO $ doesCasedFileExist $ (html_dir </> file)
                              if b then return [file]
                                   else return []
                         | file <- nub (concatMap ld_localURLs links)
                         ]

-- sh -c 'curl -m 1 -s --head http://www.chalmers.se/cse/EN/people/persson-anders || echo ""'
-- -L <= redirect automatically
{-
        let classify (x:xs) = case words x of
                    ("HTTP/1.1":n:_) | all isDigit n -> classifyCode (read n) xs
                    _                                -> []
            classify _             = []

            classifyCode :: Int -> [String] -> String
            classifyCode n xs | n >= 300 && n < 400 = if again == unknown
                                                      then show n
                                                      else again
                  where again = classify xs

            classifyCode n _ = show n
-}

        let fake = False
        external_links <- liftIO $ withPool 32
                $ \ pool -> parallelInterleaved pool
                         [ do resp <- getURLResponse url
                              putStrLn $ "examining " ++ url ++ " ==> " ++ show resp
                              return (url,resp)
                         | url <- take 500 $ nub (concatMap ld_remoteURLs links)
                         ]

--        liftIO$ print $ external_links

{-
   curl -s --head http://www.haskell.org/
HTTP/1.1 307 Temporary Redirect
Date: Wed, 02 Jan 2013 02:51:59 GMT
Server: Apache/2.2.9 (Debian) PHP/5.2.6-1+lenny13 with Suhosin-Patch
Location: http://www.haskell.org/haskellwiki/Haskell
Vary: Accept-Encoding
Content-Type: text/html; charset=iso-8859-1

orange:fpg-web andy$ curl -s --head http://www.haskell.org/
-}

        let goodLinkCode :: URLResponse -> Bool
            goodLinkCode (URLResponse [] _) = False
            goodLinkCode (URLResponse xs _) = last xs == 200

            -- bad links get False, good links get true
        let findGoodLinks :: LinkData [String] -> LinkData [(Bool,String)]
            findGoodLinks link = link
                { ld_localURLs    = map (\ x -> (x `elem` good_local_links,x)) $ ld_localURLs link
                , ld_remoteURLs = map (\ url -> case lookup url external_links of
                                                       Nothing -> error "should never happen! (all links looked at)"
                                                       Just resp -> (goodLinkCode resp,url))
                                $ ld_remoteURLs link
                }

            markupCount :: [a] -> HTML
            markupCount = text . show . length

            markupCount' :: [(Bool,a)] -> HTML
            markupCount' xs = element "span" [attr "class" $ "badge " ++ label] $ text (show len)
                 where len = length $ filter (\ (b,_) -> not b) xs
                       label = if len == 0 then "badge-success" else "badge-important"

        let up txt tag = element "span" [attr "class" $ "badge badge-" ++ tag] txt

        let showUpload NotAttempted = text "-"
            showUpload NotPresent   = up tick "important"
              where
                 tick = element "i" [attr "class" "icon-remove icon-white"] $ zero --  icon-white"] $ zero
            showUpload Different    = up tick "warning"
              where
                 tick = element "i" [attr "class" "icon-refresh icon-white"] $ zero --  icon-white"] $ zero
            showUpload Same         = up tick "success"
              where
                 tick = element "i" [attr "class" "icon-ok icon-white"] $ zero --  icon-white"] $ zero

        let errorCount page = length
                       [ ()
                       | (False,_) <- ld_localURLs page ++ ld_remoteURLs page
                       ]

        let checked_links = map findGoodLinks links

            br = element "br" [] mempty

        let page_tabel = element "table" [] $ mconcat $
                        [ element "tr" [] $ mconcat
                          [ element "th" [] $ text $ "#"
                          , element "th" [] $ text $ "Page Name"
                          , element "th" [attr "style" "text-align: right"] $ text $ "size"
                          , element "th" [attr "style" "text-align: right"] $ text $ "words"
                          , element "th" [attr "style" "text-align: center"] $ text $ "up"
                          , element "th" [attr "style" "text-align: right"] $ mconcat [text "local",br,text "links"]
                          , element "th" [attr "style" "text-align: right"] $ mconcat [text "extern",br,text "links"]
                          , element "th" [attr "style" "text-align: right"] $ mconcat [text "local",br,text "fail"]
                          , element "th" [attr "style" "text-align: right"] $ mconcat [text "extern",br,text "fail"]
                          , element "th" [attr "style" "text-align: center"] $ mconcat [text "bad links"]
                          ]
                        ] ++
                        [ element "tr" [] $ mconcat
                          [ element "td" [attr "style" "text-align: right"] $ text $ show n
                          , element "td" []
                            $ element "a" [attr "href" (ld_pageName page) ]
                              $ text $ shorten 50 $ ld_pageName page
                          , element "td" [attr "style" "text-align: right"] $ text $ show $ ld_bytes page
                          , element "td" [attr "style" "text-align: right"] $ text $ show $ ld_wc page
                          , element "td" [attr "style" "text-align: center"] $ showUpload $ ld_match page
                          , element "td" [attr "style" "text-align: right"] $ markupCount $ ld_localURLs $ page
                          , element "td" [attr "style" "text-align: right"] $ markupCount $ ld_remoteURLs $ page
                          , element "td" [attr "style" "text-align: right"] $ markupCount' $ ld_localURLs $ page
                          , element "td" [attr "style" "text-align: right"] $ markupCount' $ ld_remoteURLs $ page
                          , element "td" [] $ mconcat
                                [ text bad <> br
                                | (False,bad) <- ld_localURLs page ++ ld_remoteURLs page
                                ]

                          ]
                        | (n,page) <- [1..] `zip`
                                ( sortBy (\ a b -> errorCount b `compare` errorCount a)
                                $ sortBy (\ a b -> ld_pageName a `compare` ld_pageName b)
                                $ checked_links
                                )

{-
                        | (n,page,page_bad,page_bad') <- zip4 [1..]
                                                    (map (fmap markupCount) links)
                                                    (map (fmap markupCount') bad_links)
                                                    bad_links
-}
                        ]

        let colorURLCode :: URLResponse -> HTML
            colorURLCode (URLResponse [] n) =
                    element "span" [attr "class" $ "badge badge-important"]
                    $ text $ if n > 3000
                             then "..."
                             else "!"
--                    $ element "i" [attr "class" "icon-warning-sign icon-white"]
--                      $ text "" -- intentionally

            colorURLCode resp@(URLResponse xs _) =
                    mconcat $ [ element "span" [attr "class" $ "badge " ++ label] $ text $ show x
                              | x <- xs
                              ]
                where label = if goodLinkCode resp
                              then "badge-success"
                              else "badge-important"

        let timing (_,URLResponse _ t1) (_,URLResponse _ t2) = t1 `compare` t2
        let correctness (_,u1) (_,u2) = goodLinkCode u1 `compare` goodLinkCode u2

        let link_tabel = element "table" [] $ mconcat $
                        [ element "tr" [] $ mconcat
                          [ element "th" [] $ text $ "#"
                          , element "th" [] $ text $ "External URL"
                          , element "th" [attr "style" "text-align: center"] $ mconcat [text "HTTP",br,text "code(s)"]
                          , element "th" [attr "style" "text-align: right"] $ mconcat [text "time",br,text "ms"]
                          ]
                        ] ++
                        [ element "tr" [] $ mconcat
                          [ element "td" [attr "style" "text-align: right"] $ text $ show n
                          , element "td" []
                            $ element "a" [attr "href" url ]
                              $ text $ shorten 72 $ url
                          , element "td" [attr "style" "text-align: right"]
                            $ colorURLCode resp
                          , element "td" [attr "style" "text-align: right"] $ text $ show tm
                          ]
                        | (n,(url,resp@(URLResponse _ tm))) <- zip [1..]
                                        $ sortBy correctness
                                        $ sortBy timing
                                        $ external_links
                        ]

        let f = element "div" [attr "class" "row"] . element "div" [attr "class" "span10  offset1"]

        let summary_table =
                  element "div" [attr "class" "row"] . element "div" [attr "class" "span4"]
                $ element "table" [] $ mconcat $
                        [ element "tr" [] $ mconcat
                          [ element "th" [] $ text title
                          , element "td" [attr "style" "text-align: right"] $ text $ txt ]
                        | (title,txt) <-
                                [ ("Local Pages", show $ length $ links)
                                , ("External Links",show $ length $ external_links)
                                , ("Broken Internal Links",show $ length $ nub
                                                          $ filter (not . fst)
                                                          $ concat
                                                          $ map ld_localURLs checked_links)
                                , ("Broken External Links",show $ length $
                                                           [ ()
                                                           | (_,url) <- external_links
                                                           , not (goodLinkCode url)])
                                ]

                        ]

        return $ f $ mconcat
                [ element "h2" [] $ text "Summary"
                , summary_table
                , element "h2" [] $ text "Pages"
                , page_tabel
                , element "h2" [] $ text "External URLs"
                , link_tabel
                ]

{-
findURL :: (Monad m) => Translate Context m Node String
findURL = promoteT $ do
                (nm,val) <- attrT (,)
                cxt@(Context (c:_)) <- contextT
                tag <- KURE.apply getTag cxt c
                case (nm,[tag]) of
                   ("href","a":_)     -> return val
                   ("href","link":_)  -> return val
                   ("src","script":_) -> return val
                   ("src","img":_)    -> return val
                   _                  -> fail "no correct context"
-}


shorten n xs | length xs < n = xs
              | otherwise     = take (n - 3) xs ++ "..."


---------------------------------------------------------



-- Call with the path top the wrapper template,
-- and
wrapTemplateFile :: String -> Int -> R HTML
wrapTemplateFile fullPath count = rewrite $ \ c inside -> do
        src <- liftActionFPGM $ readFile' fullPath
        let contents = parseHTML fullPath src
        let local_prefix nm = concat (take count (repeat "../")) ++ nm
        let normalizeTplURL nm
                -- should really check for ccs, js, img, etc.
                | "../" `isPrefixOf` nm = local_prefix (dropDirectory1 nm)
                | otherwise             = nm
        let fn = do
                "contents" <- getAttr "id"
                return inside
        let prog = extractR' (tryR (prunetdR (promoteR $ mapURL normalizeTplURL)))
               >>> extractR' (prunetdR (promoteR (anyElementHTML fn)))
        KURE.apply prog mempty contents

---------------------------------------------------------

makeStatus :: String -> String -> Rules ()
makeStatus prefix dir = ("_make" </> dir </> "status.html" ==) ?> \ out -> do
                contents :: [String] <- targetPages
                let contents' = filter (/= "status.html") $ contents
                status <- generateStatus prefix contents'
                writeFileChanged out $ show $ status

-------------------------------------------------------------------------

clean :: IO ()
clean = do
        b <- doesDirectoryExist build_dir
        when b $ do
           removeDirectoryRecursive build_dir
        return ()

-------------------------------------------------------------------------

data MyURL = MyURL String                 -- name of target (without the _make/html)
                 (Rules ())               -- The rule to build this target

instance Show MyURL where
        show = urlName

buildURL :: String -> (String -> Action ()) -> MyURL
buildURL target action = MyURL target $ (== (html_dir </> target)) ?> action

urlRules :: MyURL -> Rules ()
urlRules (MyURL _ rules) = rules

urlName :: MyURL -> String
urlName (MyURL name _) = name

copyPage :: String -> MyURL
copyPage urlFile = buildURL urlFile $ \ out -> do
        let src = dropDirectory1 $ dropDirectory1 $ out
        copyFile' src out

htmlPage :: String -> String -> R HTML -> MyURL
htmlPage htmlFile srcDir processor = buildURL htmlFile $ \ out -> do
        let srcName = build_dir </> srcDir </> dropDirectory1 (dropDirectory1 out)
--        liftIO $ print (htmlFile,srcDir,srcName,out)
        need [ srcName ]
        src <- readFile' srcName
        let contents = parseHTML srcName src
        page <- applyFPGM processor contents
        writeFile' out $ show $ page

getRedirect :: String -> Action String
getRedirect = askOracle . Redirect'

chioneRules :: [MyURL] -> Rules()
chioneRules urls = do
        mapM_ urlRules urls
        action $ liftIO $ print ("chioneRules", map (\ (MyURL file _) -> html_dir </> file) $ urls)
        action $ need $ map (\ (MyURL file _) -> html_dir </> file) $ urls
        addOracle $ \ Targets{} -> return $ map (\ (MyURL file _) -> file) $ urls
        return ()

newtype Targets = Targets () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- List of all page
targetPages :: Action [String]
targetPages = askOracle $ Targets ()

----------------------

newtype Redirect = Redirect' String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

addRedirectOracle :: [(String,String)] -> Rules ()
addRedirectOracle db = do
    addOracle $  \ (Redirect' htmlFile) ->
        case lookup htmlFile db of
          Just target -> return target
          Nothing     -> error $ "unknown redirection for file " ++ show htmlFile
    return ()


-- | needs addRedirectOracle
redirectPage :: String -> MyURL
redirectPage htmlFile = buildURL htmlFile $ \ out -> do
        target <- getRedirect htmlFile
        makeHtmlRedirect out target

----------------------

relativeURL :: Int -> String -> String
relativeURL n ('/':rest)
        | null (takeExtension rest) = replaceExtension (local_prefix </> rest) "html"
        | otherwise                 = local_prefix </> rest
  where local_prefix = concat (take n $ repeat "../")
relativeURL n other
  | "http://" `isPrefixOf` other
  || "https://" `isPrefixOf` other = other
  | otherwise                      = other

----------------------


divSpanExpand :: (String -> FPGM HTML) -> T Element HTML
divSpanExpand macro = do
         tag <- getTag
  --       () <- trace ("trace: " ++ tag) $ return ()
         guardMsg (tag == "div" || tag == "span") "wrong tag"
--         () <- trace ("trace: " ++ show tag) $ return ()
         cls <- getAttr "class"
---         () <- trace ("$$$$$$$$$$$$$$$$$ trace: " ++ show (tag,cls)) $ return ()
         constT $ macro cls

-----------------------------------------------


newtype FPGM a = FPGM { runFPGM :: IO (FPGMResult a) }

data FPGMResult a
        = FPGMResult a
        | FPGMFail String
        | forall r . FPGMAction (Action r) (r -> FPGM a)

-- for testint
applyFPGM'' :: forall a b . Translate Context FPGM a b -> a -> IO b
applyFPGM'' t a = do
        r <- runFPGM (KURE.apply t mempty a)
        case r of
          FPGMResult a -> return a
          FPGMFail msg  -> fail msg

applyFPGM :: forall a b . Translate Context FPGM a b -> a -> Action b
applyFPGM t a = do

        let loop (FPGMResult a) = return a
            loop (FPGMFail msg) =  fail $ "applyFPGM " ++ msg
            loop (FPGMAction act rest) = do
                              res <- act
                              run (rest res)

            run m = do res <- traced "apply-yah" $ runFPGM m
                       loop res

        run $ KURE.apply t mempty a

liftActionFPGM :: Action a -> FPGM a
liftActionFPGM m = FPGM $ return $ FPGMAction  m return

type T a b = Translate Context FPGM a b
type R a   = T a a

instance Monad FPGM where

        return = FPGM . return . FPGMResult

        m1 >>= k = FPGM $ do
                r <- runFPGM m1
                let f (FPGMResult a) = runFPGM (k a)
                    f (FPGMFail msg) = return (FPGMFail msg)
                    f (FPGMAction act rest) = return $ FPGMAction act (\ a -> rest a >>= k)
                f r

        fail = FPGM . return . FPGMFail

instance Functor FPGM where
        fmap f m = pure f <*> m

instance Applicative FPGM where
        pure a = return a
        af <*> aa = af >>= \ f -> aa >>= \ a -> return (f a)


instance MonadCatch FPGM where
        catchM m1 handle = FPGM $ do
                r <- runFPGM m1
                let f (FPGMResult a) = return (FPGMResult a)
                    f (FPGMFail msg) = runFPGM (handle msg)
                    f (FPGMAction act rest) = return (FPGMAction act rest)
                f r
--instance MonadIO FPGM where
--        liftIO m = FPGM (FPGMResult <$> m)

--------------------------------------------------

