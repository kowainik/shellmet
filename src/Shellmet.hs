{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
#if __GLASGOW_HASKELL__ >= 910
{-# LANGUAGE TypeOperators     #-}
#endif

{- |
Module                  : Shellmet
Copyright               : (c) 2019-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains neat utilities to be able to work with
shell commands in generic and simple way using just string literals.

>>> "echo" ["Hello", "World!"]
⚙  echo Hello 'World!'
Hello World!
-}

module Shellmet
    ( ($|)
    , ($^)
    , ($?)
    , isSuccess
    ) where

import Control.Exception (catch)
import Data.String (IsString (..))
import Data.Text (Text)
import System.Process (callProcess, readProcess, showCommandForUser)

import qualified Data.Text as T


{- | This instance is needed to provide functionality to call commands by using
simple string literals in 'IO' monad.

>>> "ls" ["-1", "test"]
⚙  ls -1 test
Doctest.hs
-}
instance (a ~ [Text], b ~ IO ()) => IsString (a -> b) where
    fromString :: String -> [Text] -> IO ()
    fromString cmd args = do
        let argStrs = map T.unpack args
        putStrLn $ "⚙  " ++ showCommandForUser cmd argStrs
        callProcess cmd argStrs
    {-# INLINE fromString #-}

{- | Run shell command with given options and return stripped stdout of the
executed command.

>>> "echo" $| ["Foo", "Bar"]
"Foo Bar"
-}
infix 5 $|
($|) :: FilePath -> [Text] -> IO Text
cmd $| args = T.strip . T.pack <$> readProcess cmd (map T.unpack args) ""
{-# INLINE ($|) #-}

{- | This operator runs shell command with given options but doesn't print the
command itself.

>>> "echo" $^ ["Foo", "Bar"]
Foo Bar
-}
infix 5 $^
($^) :: FilePath -> [Text] -> IO ()
cmd $^ args = callProcess cmd (map T.unpack args)
{-# INLINE ($^) #-}

{- | Do some IO actions when process failed with 'IOError'.

>>> "echo" ["0"] $? putStrLn "Command failed"
⚙  echo 0
0

>>> "exit" ["1"] $? putStrLn "Command failed"
⚙  exit 1
Command failed
-}
infixl 4 $?
($?) :: IO a -> IO a -> IO a
action $? handler = action `catch` \(_ :: IOError) -> handler
{-# INLINE ($?) #-}

{- | Returns the indicator of if the command succeded or not.

>>> isSuccess $ "echo" ["Hello world!"]
⚙  echo 'Hello world!'
Hello world!
True
-}
isSuccess :: IO a -> IO Bool
isSuccess action = (True <$ action) $? pure False
