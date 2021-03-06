{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Tests.Integration.IOException1Spec where

import qualified Control.Exception as E
import qualified Prelude as P (writeFile, readFile)

import           Hydra.Prelude
import qualified Hydra.Domain                as D
import qualified Hydra.Language              as L
import qualified "hydra-free" Hydra.Runtime  as R
import qualified Hydra.Interpreters          as R
import           Hydra.Testing.Integrational
import           Hydra.Testing.Wrappers
import           Test.Hspec
import qualified GHC.IO.Exception as IOE

import           Hydra.TestData

-- Samples for the book

type NativeResult a = Either IOE.IOException a

-- Language-level error types:
data FSError
  = FileNotFound (Maybe FilePath)
  | OtherError String
type FSResult a = Either FSError a

-- Language itself:
data FileSystemF next where
  WriteFile :: FilePath -> String -> (FSResult () -> next) -> FileSystemF next
  ReadFile  :: FilePath -> (FSResult String -> next) -> FileSystemF next

instance Functor FileSystemF where
  fmap f (WriteFile p c next) = WriteFile p c (f . next)
  fmap f (ReadFile p next) = ReadFile p (f . next)

type FileSystem = Free FileSystemF

writeFile' filePath content = liftF $ WriteFile filePath content id
readFile' filePath = liftF $ ReadFile filePath id

-- Native error -> custom error:
fromNativeResult :: NativeResult a -> FSResult a
fromNativeResult (Right a) = Right a
fromNativeResult (Left ioException) = let
  fileName = IOE.ioe_filename ioException
  errType  = IOE.ioe_type ioException
  in case errType of
       IOE.NoSuchThing -> Left $ FileNotFound fileName
       _               -> Left $ OtherError $ show errType

interpretFileSystemF :: FileSystemF a -> IO a
interpretFileSystemF (WriteFile p c next) = do
  eRes <- try $ P.writeFile p c
  pure $ next $ fromNativeResult eRes
interpretFileSystemF (ReadFile p next) = error "Not implemented"

runFileSystem :: FileSystem a -> IO a
runFileSystem = foldFree interpretFileSystemF


spec :: Spec
spec = pure ()
