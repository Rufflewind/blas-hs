{-# LANGUAGE NoMonomorphismRestriction #-}
module Mod where
import Data.List (intercalate)
import Text.Printf (printf)

-- | Haskell module.
data Module =
  Module
  { mName     ::  String
  , mDoc      ::  String
  , mExts     :: [String]
  , mImports  :: [String]
  , mExports  :: [String]
  , mContents :: [String]
  }

-- | System include.
sysIncl :: String -> String
sysIncl = printf "#include <%s>\n"

-- | Local include.
locIncl :: String -> String
locIncl = printf "#include \"%s\"\n"

-- | Return @[]@ if the list @l@ is empty, otherwise @[l]@.
allOrNothing :: [a] -> [[a]]
allOrNothing l = if null l then [] else [l]

-- | Construct the header.
mkHeader :: Module -> [String]
mkHeader m =
  fmap (printf "{-# LANGUAGE %s #-}\n" . intercalate ", ")
       (allOrNothing $ mExts m)
  ++ [ printf "{-|\nModule: %s\n%s\n-}" (mName m) (mDoc m)
     , printf "module %s (%s) where\n" (mName m) $
              intercalate ", " $  mExports  m ]
  ++ fmap (printf "import %s\n") (mImports  m)

-- | Construct the module.
mkModule :: Module -> String
mkModule m = concat $ mkHeader m ++ mContents m
