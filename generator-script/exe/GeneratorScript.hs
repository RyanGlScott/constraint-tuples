{-# LANGUAGE NamedFieldPuns #-}
-- | The script that generates the source code for "Data.Tuple.Constraint" and
-- "Data.Tuple.Constraint.ClassNewtype". See the @GenCTuple.sh@ script for how
-- to invoke this.
module Main (main) where

import Data.List.Compat
import GHC.Exts (maxTupleSize)
import Prelude ()
import Prelude.Compat
import Options.Applicative

data Args = Args
  { output       :: FilePath
  , classNewtype :: Bool
  } deriving Show

argsParser :: Parser Args
argsParser = Args
  <$> strOption
      (  long "output"
      <> short 'o'
      <> metavar "PATH"
      <> help "The file to which to write the source code" )
  <*> switch
      (  long "class-newtype"
      <> help "Generate the source code for Data.Tuple.Constraint.ClassNewtype" )

main :: IO ()
main = execParser opts >>= generate
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc spiel
     <> header spiel )

    spiel = "Generate the source code for Data.Tuple.Constraint{,.ClassNewtype}"

generate :: Args -> IO ()
generate args@Args{output} =
  let sourceCode = unlines $ preamble args ++ decs args in
  writeFile output sourceCode

genCTuple :: Args -> Int -> String
genCTuple Args{classNewtype} n
  | n == 0    = "CTuple0"
  | otherwise = parens (concat (intersperse ", " cNums))
             ++ " => CTuple" ++ show n ++ " " ++ unwords cNums
  where
    parens :: String -> String
    parens s | n == 1    = s
             | otherwise = kindSig $ "(" ++ s ++ ")"

    kindSig :: String -> String
    kindSig s | classNewtype = "(" ++ s ++ " :: Constraint)"
              | otherwise    = s

    cNums :: [String]
    cNums = ['c':show i | i <- [1..n]]

haddocks :: Int -> [String]
haddocks i =
  [ "-- | A constraint tuple class with " ++ show i ++
    " argument" ++ pluralSuffix ++ "."
  ] ++
  if i == 0
  then [ "--"
       , "-- This class is only defined on GHC 7.8 or later."
       ]
  else []
  where
    pluralSuffix :: String
    pluralSuffix | i == 1
                 = ""
                 | otherwise
                 = "s"

preamble :: Args -> [String]
preamble Args{classNewtype} =
  [ "{-# LANGUAGE ConstraintKinds #-}"
  , "{-# LANGUAGE CPP #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE KindSignatures #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , "{-# LANGUAGE UndecidableInstances #-}"
  , "#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710"
  , "{-# LANGUAGE NullaryTypeClasses #-}"
  , "#endif"
  , "#if __GLASGOW_HASKELL__ >= 800"
  , "{-# LANGUAGE UndecidableSuperClasses #-}"
  , "#endif"
  ] ++ safeHaskell ++
  [ "-- | This module provides classes that emulate the behavior of GHC's constraint"
  , "-- tuple syntax. Unlike GHC's built-in constraint tuples, the classes in this"
  , "-- library can be partially applied."
  , "--"
  ] ++ haddockNote ++
  [ "module " ++ modName
  , "  ( -- * Constraint tuples"
  ] ++ exports ++
  [ "  ) where"
  , ""
  ] ++ imports
  where
    modName :: String
    modName = "Data.Tuple.Constraint" ++
              if classNewtype then ".ClassNewtype" else ""

    exports :: [String]
    exports = flip concatMap [0..maxTupleSize] $ \i ->
                case i of
                  0 -> [ "#if __GLASGOW_HASKELL__ >= 708"
                       , "    CTuple0,"
                       , "#endif"
                       ]
                  1 -> [ "    CTuple1" ]
                  _ -> [ "  , CTuple" ++ show i ]

    imports :: [String]
    imports | classNewtype
            = [ "import Data.Tuple.Constraint ( CTuple1"
              , "#if __GLASGOW_HASKELL__ >= 708"
              , "                             , CTuple0"
              , "#endif"
              , "                             )"
              , "#if __GLASGOW_HASKELL__ >= 800"
              , "import Data.Kind (Constraint)"
              , "#else"
              , "import GHC.Exts (Constraint)"
              , "#endif"
              , ""
              ]
            | otherwise
            = []

    safeHaskell :: [String]
    safeHaskell | classNewtype
                = [ "#if __GLASGOW_HASKELL__ >= 800"
                  , "{-# LANGUAGE Safe #-}"
                  , "#else"
                  , "{-# LANGUAGE Trustworthy #-}"
                  , "#endif"
                  ]
                | otherwise
                = [ "{-# LANGUAGE Safe #-}" ]

    haddockNote :: [String]
    haddockNote
      | classNewtype
      = [ "-- Unlike \"Data.Tuple.Constraint\", a @CTupleN@ class defined in this module"
        , "-- (where @N@ is greater than 1) compiles to a newtype around the corresponding"
        , "-- built-in constraint tuple type with @N@ arguments in Core. In contrast, a"
        , "-- @CTupleN@ class defined in \"Data.Tuple.Constraint\" compiles to a"
        , "-- dictionary data type with @N@ fields in Core."
        , "--"
        , "-- For most use cases, this distinction is of no practical consequence. One"
        , "-- scenario where you may benefit from using this module is when you are"
        , "-- interoperating with built-in constraint tuple syntax."
        , "-- For example, in this code:"
        , "--"
        , "-- @"
        , "-- data Dict :: Constraint -> Type where"
        , "--   Dict :: c => Dict c"
        , "--"
        , "-- foo :: CTuple2 a b => Dict (a, b)"
        , "-- foo = Dict"
        , "-- @"
        , "--"
        , "-- If you use the @CTuple2@ class from \"Data.Tuple.Constraint\" to define"
        , "-- @foo@, then in the Core for @foo@, the @a@ and @b@ must be extracted from"
        , "-- the @CTuple2@ dictionary before building the @Dict@ dictionary. On the other"
        , "-- hand, if you use the @CTuple@ class from this module, then no such"
        , "-- extraction is necessary, as the Core can simply cast the @CTuple2@"
        , "-- dictionary (which is a newtype) to the @(a, b)@ dictionary and use that to"
        , "-- construct a @Dict@ dictionary."
        ]
      | otherwise
      = []

decs :: Args -> [String]
decs args@Args{classNewtype} =
  flip concatMap [0..maxTupleSize] $ \i ->
    let cTuple = genCTuple args i in
    if classNewtype && (i == 0 || i == 1)
       then [] -- CTuple{0,1} are imported from Data.Tuple.Constraint
       else concat
              [ [ "#if __GLASGOW_HASKELL__ >= 708" | i == 0 ]
              , haddocks i
              , [ "class    " ++ cTuple
                , "instance " ++ cTuple
                ]
              , [ "#endif" | i == 0 ]
              , [ "" ]
              ]
