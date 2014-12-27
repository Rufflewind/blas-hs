module Common ( module Common
              , module Prelude
              , module Control.Monad
              , module Data.Char
              , module Data.Functor
              , module Data.List
              , module Data.Monoid
              , module Text.Printf ) where
import Prelude hiding (Double, Float)
import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Functor ((<$>))
import Data.List (intercalate, intersperse)
import Data.Monoid ((<>), mconcat)
import Text.Printf (printf)

data Safety = Safe
            | Unsafe
            deriving (Bounded, Enum, Eq, Ord, Read, Show)

mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail _ _ []       = []
mapHeadTail f g (x : xs) = f x : (g <$> xs)

-- | Similar to @'break'@ but keeps track of the length of the first part.
breakWithLength :: (a -> Bool) -> [a] -> ((Int, [a]), [a])
breakWithLength _ []                = ((0, []), [])
breakWithLength f l@(x : xs) | f x  = ((0, []), l)
                             | True = ((n + 1, x : xs1), xs2)
  where ((n, xs1), xs2) = breakWithLength f xs

-- | Word wraps the string within the given width.
--   Existing newlines, indents, and multi-spaces are preserved.
wordWrap :: Int -> String -> [String]
wordWrap cap s = mconcat (wrapLine <$> lines s)

  where wrapLine l = (i <>) . intercalate " " . (snd <$>) <$>
                     wrapAtoms (cap - ni) (splitAtoms l')
          where ((ni, i), l') = breakWithLength (/= ' ') l

        wrapAtoms _ []               = []
        wrapAtoms n (t@(nw, _) : ts) = (t : ts1) : wrapAtoms n ts2
          where (ts1, ts2)    = grabAtoms (n - nw) ts

        grabAtoms _ []                             = ([], [])
        grabAtoms n xs@(x@(na, _) : xs') | n' >= 0 = (x : xs1, xs2)
                                         | True    = ([], xs)
          where n'            = n - na
                (xs1, xs2)    = grabAtoms n' xs'

-- | Split a line into "atoms", each of which is separated from another by a
--   single space (multiple spaces are treated as part of the atom).
--   Any space at the beginning or the end are removed.
--   Each pair returned contains the length of the atom and the atom itself.
splitAtoms :: String -> [(Int, String)]
splitAtoms = splitAtoms' . dropWhile (== ' ')

  where splitAtoms' [] = []
        splitAtoms' l  = atom : splitAtoms l' where (atom, l') = breakAtom l

        breakAtom l | ns > 1 = if na == 0
                               then ((nw, w), l3)
                               else ((nw + ns + na, w <> s <> a), l3)
                    | True   = ((nw, w), l2)
          where ((nw, w), l1) = breakWithLength (== ' ') l
                ((ns, s), l2) = breakWithLength (/= ' ') l1
                ((na, a), l3) = breakAtom l2

mkDocs :: [String] -> [String]
mkDocs docs = mapHeadTail modifyHead modifyTail docLines
  where docLines = intercalate [""] (wordWrap 72 <$> docs)
        modifyHead [] = "-- |"
        modifyHead s  = "-- | " <> s
        modifyTail [] = "--"
        modifyTail s  = "--   " <> s

mkDocs' :: [String] -> String
mkDocs' = unlines . mkDocs

writeModule :: FilePath                 -- ^ suffix of filename
            -> [String]                 -- ^ compiler extensions
            -> [String]                 -- ^ module name segments
            -> [String]                 -- ^ module documentation paragraphs
            -> [String]                 -- ^ export lines
            -> [String]                 -- ^ import lines (without "import ")
            -> String                   -- ^ module contents
            -> FilePath                 -- ^ prefix of filename
            -> IO ()
writeModule suffix exts name docs exps imps contents prefix =
  writeFile (prefix <> intercalate "/" name <> suffix) $
  "-- This file is auto-generated.  Do not edit directly.\n" <>
  mkDocs' ("" : docs) <> -- empty initial paragraph needed for Haddock to work
  (if null exts then "" else
   printf "{-# LANGUAGE %s #-}\n" (intercalate ", " exts)) <>
  printf "module %s (%s) where\n"
         (intercalate "." name) exps' <>
  mconcat (wrapImport <$> imps) <> "\n" <>
  contents
  where wrapImport s   = printf "import %s\n" s
        exps'          = case exps of
          []  -> ""
          [s] -> s
          _   -> printf "\n%s  " (unlines exps)
