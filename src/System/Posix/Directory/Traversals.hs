{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}
module System.Posix.Directory.Traversals (

  getDirectoryContents

, allDirectoryContents
, allDirectoryContents'
, traverseDirectory

-- lower-level stuff
, readDirEnt
, packDirStream
, unpackDirStream

, realpath
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Posix.FilePath ((</>))
import System.Posix.Directory.Foreign

import qualified System.Posix as Posix
import System.IO.Error
import qualified Data.ByteString.Char8 as BS
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString as PosixBS
import System.Posix.Files.ByteString
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Exception

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca,allocaBytes)
import Foreign.Ptr
import Foreign.Storable

----------------------------------------------------------

-- | Get all files from a directory and its subdirectories.
--
-- Upon entering a directory, 'allDirectoryContents' will get all entries
-- strictly.  However the returned list is lazy in that directories will only
-- be accessed on demand.
allDirectoryContents :: RawFilePath -> IO [RawFilePath]
allDirectoryContents topdir = do
    namesAndTypes <- getDirectoryContents topdir
    let properNames = filter ((`notElem` [".", ".."]) . snd) namesAndTypes
    paths <- forM properNames $ \(typ,name) -> unsafeInterleaveIO $ do
        let path = topdir </> name
        case () of
            () | typ == dtDir -> allDirectoryContents path
               | typ == dtUnknown -> do
                    isDir <- isDirectory <$> getFileStatus path
                    if isDir
                        then allDirectoryContents path
                        else return [path]
               | otherwise -> return [path]
    return (topdir : concat paths)

-- | Get all files from a directory and its subdirectories strictly.
allDirectoryContents' :: RawFilePath -> IO [RawFilePath]
allDirectoryContents' = fmap reverse . traverseDirectory (\acc fp -> return (fp:acc)) []
-- this uses traverseDirectory because it's more efficient than forcing the
-- lazy version.

-- | Recursively apply the 'action' to the parent file or directory and all
-- files/subdirectories.
--
-- Like UNIX @find@, this includes the parent file/directory!
--
-- As for @find@, emitted file paths of subdirectories contain slashes,
-- starting with the parent directory.
--
-- This function allows for memory-efficient traversals.
traverseDirectory :: (MonadUnliftIO m) => (s -> RawFilePath -> m s) -> s -> RawFilePath -> m s
traverseDirectory act s0 topDirOrFile = toploop
  where
    toploop = do
        isDir <- liftIO $ isDirectory <$> getFileStatus topDirOrFile
        s' <- act s0 topDirOrFile
        if isDir then actOnDirContents topDirOrFile s' loop
                 else return s'
    loop typ path acc = do
        isDir <- liftIO $ case () of
            () | typ == dtDir     -> return True
               | typ == dtUnknown -> isDirectory <$> getFileStatus path
               | otherwise        -> return False
        if isDir
          then act acc path >>= \acc' -> actOnDirContents path acc' loop
          else act acc path

actOnDirContents :: (MonadUnliftIO m)
                 => RawFilePath
                 -> b
                 -> (DirType -> RawFilePath -> b -> m b)
                 -> m b
actOnDirContents pathRelToTop b f =
  modifyIOErrorUnliftIO
    ((`ioeSetFileName` (BS.unpack pathRelToTop)) .
     (`ioeSetLocation` "System.Posix.Directory.actOnDirContents")) $ do
    bracket
      (liftIO $ openDirStream pathRelToTop)
      (liftIO . Posix.closeDirStream)
      (\dirp -> loop dirp b)
 where
  loop dirp b' = do
    (typ,e) <- liftIO $ readDirEnt dirp
    if (e == "")
      then return b'
      else do
          if (e == "." || e == "..")
              then loop dirp b'
              else f typ (pathRelToTop </> e) b' >>= loop dirp

-- | `withRunInIO` lifted to `MonadUnliftIO`.
modifyIOErrorUnliftIO :: (MonadUnliftIO m) => (IOError -> IOError) -> m a -> m a
modifyIOErrorUnliftIO f action =
  withRunInIO $ \runInIO -> do
    modifyIOError f (runInIO action)

----------------------------------------------------------
-- dodgy stuff

type CDir = ()
type CDirent = ()

-- Posix doesn't export DirStream, so to re-use that type we need to use
-- unsafeCoerce.  It's just a newtype, so this is a legitimate usage.
-- ugly trick.
unpackDirStream :: DirStream -> Ptr CDir
unpackDirStream = unsafeCoerce

packDirStream :: Ptr CDir -> DirStream
packDirStream = unsafeCoerce

-- the __hscore_* functions are defined in the unix package.  We can import them and let
-- the linker figure it out.
--
-- In contrast to current `unix` we use `safe` calls for anything that
-- does file system IO, because it can take a substantial amount of time
-- on spinning disks or networked file systems, and `unsafe` calls block
-- a capability.
-- See https://github.com/haskell/unix/issues/34.
foreign import ccall safe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  c_name :: Ptr CDirent -> IO CString

foreign import ccall unsafe "__posixdir_d_type"
  c_type :: Ptr CDirent -> IO DirType

foreign import ccall "realpath"
  c_realpath :: CString -> CString -> IO CString

----------------------------------------------------------
-- less dodgy but still lower-level

readDirEnt :: DirStream -> IO (DirType, RawFilePath)
readDirEnt (unpackDirStream -> dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
       then do
         dEnt <- peek ptr_dEnt
         if (dEnt == nullPtr)
            then return (dtUnknown,BS.empty)
            else do
                 dName <- c_name dEnt >>= peekFilePath
                 dType <- c_type dEnt
                 c_freeDirEnt dEnt
                 return (dType, dName)
       else do
         errno <- getErrno
         if (errno == eINTR)
            then loop ptr_dEnt
            else do
                 let (Errno eo) = errno
                 if (eo == 0)
                    then return (dtUnknown,BS.empty)
                    else throwErrno "readDirEnt"

getDirectoryContents :: RawFilePath -> IO [(DirType, RawFilePath)]
getDirectoryContents path =
  modifyIOError ((`ioeSetFileName` (BS.unpack path)) .
                 (`ioeSetLocation` "System.Posix.Directory.Traversals.getDirectoryContents")) $ do
    bracket
      (PosixBS.openDirStream path)
      PosixBS.closeDirStream
      loop
 where
  loop dirp = do
     t@(_typ,e) <- readDirEnt dirp
     if BS.null e then return [] else do
       es <- loop dirp
       return (t:es)

-- | return the canonicalized absolute pathname
--
-- like canonicalizePath, but uses realpath(3)
realpath :: RawFilePath -> IO RawFilePath
realpath inp = do
    allocaBytes pathMax $ \tmp -> do
        void $ BS.useAsCString inp $ \cstr -> throwErrnoIfNull "realpath" $ c_realpath cstr tmp
        BS.packCString tmp
