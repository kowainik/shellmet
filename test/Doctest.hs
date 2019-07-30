{-# LANGUAGE CPP #-}

module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
#if ( __GLASGOW_HASKELL__ >= 804 )
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest
        $ "-XInstanceSigs"
        : "-XOverloadedStrings"
        : "-XScopedTypeVariables"
        : sourceFiles
#else
main = pure ()
#endif
