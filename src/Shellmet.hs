{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{- | This module contains neat utilities to be able to work with
shell commands in generic and simple way using just strings.
-}

module Shellmet
       ( ($|)
       , ($?)
       ) where

import Control.Exception (catch)
import Data.String (IsString (..))
import Data.Text (Text)
import System.Process (callCommand, readProcess, showCommandForUser)

import qualified Data.Text as T


{- | This instance is needed to provide functionality to call commands by using
simple string literals in 'IO' monad.

>>> "ls" ["-1"]
⚙  ls -1
CHANGELOG.md
CONTRIBUTING.md
dist-newstyle
LICENSE
README.md
shellmet.cabal
src
stack.yaml
test
-}
instance (a ~ [Text], b ~ IO ()) => IsString (a -> b) where
    fromString :: String -> [Text] -> IO ()
    fromString cmd args = do
        let cmdStr = showCommandForUser cmd (map T.unpack args)
        putStrLn $ "⚙  " ++ cmdStr
        callCommand cmdStr

{- | Run shell command with given options and return stripped stdout of the
executed command.

>>> "echo" $| ["Foo", "Bar"]
"Foo Bar"
-}
infix 5 $|
($|) :: FilePath -> [Text] -> IO Text
cmd $| args = T.strip . T.pack <$> readProcess cmd (map T.unpack args) ""

{- | Do some IO actions when processed failed with error.

>>> "exit" ["0"] $? putStrLn "Command failed"
⚙  exit 0

>>> "exit" ["1"] $? putStrLn "Command failed"
⚙  exit 1
Command failed
-}
infixl 4 $?
($?) :: IO () -> IO () -> IO ()
action $? handler = action `catch` \(_ :: IOError) -> handler
