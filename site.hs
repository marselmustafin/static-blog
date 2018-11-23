{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.List (sortBy, intercalate)
import System.FilePath (takeFileName)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Control.Applicative (Alternative (..))

static = do
  match "css/*" $ route idRoute >> compile compressCssCompiler
  match "images/*" idCopy
  where idCopy = route idRoute >> compile copyFileCompiler

main :: IO ()
main = hakyll $ do
  static
  let ldr ctx i = loadAndApplyTemplate "templates/default.html" ctx i >>= relativizeUrls
  match "index.org" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler >>= ldr defaultContext
  match "pgp.org" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler >>= ldr defaultContext
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= saveSnapshot "content"
      >>= ldr postCtx
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts)
                    <> constField "title" "Notes"
                    <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
        >>= ldr archiveCtx
  match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    field "nextPost" nextPostUrl <>
    field "prevPost" previousPostUrl <>
    dateField "date" "%F" <> defaultContext

previousPostUrl :: Item String -> Compiler String
previousPostUrl post = do
    posts <- getMatches "posts/*"
    let ident = itemIdentifier post
        sortedPosts = sortIdsByDate posts
        ident' = previousItem sortedPosts ident
    case ident' of
        Just i -> (fmap (maybe empty $ toUrl) . getRoute) i
        Nothing -> empty

nextPostUrl :: Item String -> Compiler String
nextPostUrl post = do
    posts <- getMatches "posts/*"
    let ident = itemIdentifier post
        sortedPosts = sortIdsByDate posts
        ident' = nextItem sortedPosts ident
    case ident' of
        Just i -> (fmap (maybe empty $ toUrl) . getRoute) i
        Nothing -> empty

nextItem :: Eq a => [a] -> a -> Maybe a
nextItem xs x =
    lookup x $ zip xs (tail xs)

previousItem :: Eq a => [a] -> a -> Maybe a
previousItem xs x =
    lookup x $ zip (tail xs) xs

urlOfPost :: Item String -> Compiler String
urlOfPost =
    fmap (maybe empty $ toUrl) . getRoute . itemIdentifier

sortIdsByDate :: [Identifier] -> [Identifier]
sortIdsByDate =
    sortBy byDate
  where
    byDate id1 id2 =
      let fn1 = takeFileName $ toFilePath id1
          fn2 = takeFileName $ toFilePath id2
          parseTime' fn = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
      in compare (parseTime' fn1 :: Maybe UTCTime) (parseTime' fn2 :: Maybe UTCTime)
