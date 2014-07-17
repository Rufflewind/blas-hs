module Main (main) where
import Control.Exception (Exception, tryJust)
import Control.Monad (forM_)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.IO.Error (isDoesNotExistError)
import System.Environment (getEnv)

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | Libraries.  Be sure to include all of the dependencies, otherwise Cabal
--   will complain with confusingly wrong error messages.
myExtraLibs :: [String]
myExtraLibs = []

-- | Library search directories.
myExtraLibDirs :: [String]
myExtraLibDirs = []

-- | C compiler options.
myCcOptions :: [String]
myCcOptions = []

-- | Linker options.
myLdOptions :: [String]
myLdOptions = []

-- | Header file.
myHeaderFile :: String
myHeaderFile = "cblas.h"

------------------------------------------------------------------------------

main :: IO ()
main = do
  defaultMainWithHooks simpleUserHooks
  -- todo: we need to check if Config is updated and re-run configure accordingly
    { postConf = myPostConf }

splitAtColon :: String -> [String]
splitAtColon s =
  let (l, r) = break (== ':') s in
  l : case r of
    []    -> []
    _ : t -> splitAtColon t

tryJustPred ::  Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
tryJustPred p m = do
  result <- tryJust (\ e -> if p e then Just () else Nothing) m
  return $ case result of
    Left () -> Nothing
    Right x -> Just x

lookupEnv :: String -> IO (Maybe String)
lookupEnv name = tryJustPred isDoesNotExistError $ getEnv name

getPathEnv :: String -> IO [String]
getPathEnv name = do
  env <- lookupEnv name
  return $ case env of
    Nothing -> []
    Just s  -> splitAtColon s

-- note that we have to use postConf (rather than confHook) because during
-- confHook the library configuration is still Nothing
myPostConf :: Args
           -> ConfigFlags
           -> PackageDescription
           -> LocalBuildInfo
           -> IO ()
myPostConf args cf pd lbi = do

  -- read environment variables
  libraryPath   <- getPathEnv "LIBRARY_PATH"
  ldLibraryPath <- getPathEnv "LD_LIBRARY_PATH"

  -- change the include file
  -- note: additionalIncludeDirs has no effect on c2hs!
  let c2hsFilenames = [ "src/Blas/Primitive/Safe.chs"
                      , "src/Blas/Primitive/Unsafe.chs" ]
      setHead (_ : xs) x     = x : xs
      setHead []       _     = []
      includeLine            = -- use "..." instead of <...> for flexibility
                               "#include \"" ++ myHeaderFile ++ "\""
      changeInclude contents = unlines $ setHead (lines contents) includeLine
      modifyFile filename f  = do
        contents <- readFile filename
        length contents `seq`          -- force evaluation before writing
          writeFile filename (f contents)
    in forM_ c2hsFilenames $ \ filename ->
         modifyFile filename changeInclude


  -- modify the package description
  let pd' = case library pd of
        Nothing  -> pd
        Just lib -> pd { library = Just lib' }
          where lib' = lib { libBuildInfo = bi' }
                bi   = libBuildInfo lib
                bi'  = bi { ccOptions    = ccOptions    bi ++ myLdOptions
                          , ldOptions    = ldOptions    bi ++ myLdOptions
                          , extraLibs    = extraLibs    bi ++ myExtraLibs
                          , extraLibDirs = extraLibDirs bi ++ myExtraLibDirs ++
                                           -- unsure if LD_LIBRARY_PATH should
                                           -- be included here
                                           libraryPath ++ ldLibraryPath }

  postConf simpleUserHooks args cf pd' lbi
