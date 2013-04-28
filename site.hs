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

    match "bio.markdown" $ do
       route $ setExtension "html"
       compile $ pandocCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate bioCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

bioCtx :: Context String
bioCtx =
    field "bio" (\_ -> loadBody "bio.markdown") `mappend`
--    (constField "bio" "This is my bio") `mappend`
    defaultContext

-- bio :: Compiler String
-- bio = do
--   bioMd <- load "bio/bio.md"
--   bioTpl <- loadBody "templates/bio.html"
--   bio <- applyTemplateList bioTpl defaultContext [bioMd]
--   return bio
