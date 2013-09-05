--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "home/*" $ do
       route idRoute
       compile $ pandocCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate bioCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" defaultContext
                >>= loadAndApplyTemplate "templates/disqus.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
--------------------------------------------------------------------------------

bioCtx :: Context String
bioCtx =
    field "bio" (\_ -> loadBody "home/bio.markdown") `mappend`
    defaultContext
