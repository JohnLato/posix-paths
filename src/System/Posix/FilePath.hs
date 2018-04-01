{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}

-- | The equivalent of "System.FilePath" on raw (byte string) file paths.
--
-- Not all functions of "System.FilePath" are implemented yet. Feel free to contribute!
module System.Posix.FilePath (

  pathSeparator
, isPathSeparator
, searchPathSeparator
, isSearchPathSeparator
, extSeparator
, isExtSeparator

, splitExtension
, takeExtension
, replaceExtension
, dropExtension
, addExtension
, hasExtension
, (<.>)
, splitExtensions
, dropExtensions
, takeExtensions

, splitFileName
, takeFileName
, replaceFileName
, dropFileName
, takeBaseName
, replaceBaseName
, takeDirectory
, replaceDirectory
, combine
, (</>)
, splitPath
, joinPath
, splitDirectories

, hasTrailingPathSeparator
, addTrailingPathSeparator
, dropTrailingPathSeparator

, isRelative
, isAbsolute

, module System.Posix.ByteString.FilePath
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           System.Posix.ByteString.FilePath

import           Data.Char  (ord)
import           Data.Maybe (isJust)
import           Data.Word (Word8)

import           Control.Arrow (second)

-- $setup
-- >>> import Data.Char
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import qualified Data.ByteString as BS
-- >>> instance Arbitrary ByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ByteString where coarbitrary = coarbitrary . BS.unpack
--
-- >>> let _chr :: Word8 -> Char; _chr = chr . fromIntegral


-- | Path separator character
pathSeparator :: Word8
pathSeparator = fromIntegral $ ord '/'

-- | Check if a character is the path separator
--
-- prop> \n ->  (_chr n == '/') == isPathSeparator n
isPathSeparator :: Word8 -> Bool
isPathSeparator = (== pathSeparator)

-- | Search path separator
searchPathSeparator :: Word8
searchPathSeparator = fromIntegral $ ord ':'

-- | Check if a character is the search path separator
--
-- prop> \n -> (_chr n == ':') == isSearchPathSeparator n
isSearchPathSeparator :: Word8 -> Bool
isSearchPathSeparator = (== searchPathSeparator)

-- | File extension separator
extSeparator :: Word8
extSeparator = fromIntegral $ ord '.'

-- | Check if a character is the file extension separator
--
-- prop> \n -> (_chr n == '.') == isExtSeparator n
isExtSeparator :: Word8 -> Bool
isExtSeparator = (== extSeparator)

------------------------
-- extension stuff

-- | Split a 'RawFilePath' into a path+filename and extension
--
-- >>> splitExtension "file.exe"
-- ("file",".exe")
--
-- >>> splitExtension "file"
-- ("file","")
--
-- >>> splitExtension "/path/file.tar.gz"
-- ("/path/file.tar",".gz")
--
-- prop> \path -> uncurry (BS.append) (splitExtension path) == path
splitExtension :: RawFilePath -> (RawFilePath, ByteString)
splitExtension x = if BS.null basename
    then (x,BS.empty)
    else (BS.append path (BS.init basename),BS.cons extSeparator fileExt)
  where
    (path,file) = splitFileNameRaw x
    (basename,fileExt) = BS.breakEnd isExtSeparator file

-- | Get the final extension from a 'RawFilePath'
--
-- >>> takeExtension "file.exe"
-- ".exe"
--
-- >>> takeExtension "file"
-- ""
--
-- >>> takeExtension "/path/file.tar.gz"
-- ".gz"
takeExtension :: RawFilePath -> ByteString
takeExtension = snd . splitExtension

-- | Change a file's extension
--
-- prop> \path -> let ext = takeExtension path in replaceExtension path ext == path
replaceExtension :: RawFilePath -> ByteString -> RawFilePath
replaceExtension path ext = dropExtension path <.> ext

-- | Drop the final extension from a 'RawFilePath'
--
-- >>> dropExtension "file.exe"
-- "file"
--
-- >>> dropExtension "file"
-- "file"
--
-- >>> dropExtension "/path/file.tar.gz"
-- "/path/file.tar"
dropExtension :: RawFilePath -> RawFilePath
dropExtension = fst . splitExtension

-- | Add an extension to a 'RawFilePath'
--
-- >>> addExtension "file" ".exe"
-- "file.exe"
--
-- >>> addExtension "file.tar" ".gz"
-- "file.tar.gz"
--
-- >>> addExtension "/path/" ".ext"
-- "/path/.ext"
addExtension :: RawFilePath -> ByteString -> RawFilePath
addExtension file ext
    | BS.null ext = file
    | isExtSeparator (BS.head ext) = BS.append file ext
    | otherwise = BS.intercalate (BS.singleton extSeparator) [file, ext]


-- | Operator version of 'addExtension'
(<.>) :: RawFilePath -> ByteString -> RawFilePath
(<.>) = addExtension

-- | Check if a 'RawFilePath' has an extension
--
-- >>> hasExtension "file"
-- False
--
-- >>> hasExtension "file.tar"
-- True
--
-- >>> hasExtension "/path.part1/"
-- False
hasExtension :: RawFilePath -> Bool
hasExtension = isJust . BS.elemIndex extSeparator . takeFileName

-- | Split a 'RawFilePath' on the first extension
--
-- >>> splitExtensions "/path/file.tar.gz"
-- ("/path/file",".tar.gz")
--
-- prop> \path -> uncurry addExtension (splitExtensions path) == path
splitExtensions :: RawFilePath -> (RawFilePath, ByteString)
splitExtensions x = if BS.null basename
    then (path,fileExt)
    else (BS.append path basename,fileExt)
  where
    (path,file) = splitFileNameRaw x
    (basename,fileExt) = BS.break isExtSeparator file

-- | Remove all extensions from a 'RawFilePath'
--
-- >>> dropExtensions "/path/file.tar.gz"
-- "/path/file"
dropExtensions :: RawFilePath -> RawFilePath
dropExtensions = fst . splitExtensions

-- | Take all extensions from a 'RawFilePath'
--
-- >>> takeExtensions "/path/file.tar.gz"
-- ".tar.gz"
takeExtensions :: RawFilePath -> ByteString
takeExtensions = snd . splitExtensions

------------------------
-- more stuff

-- | Split a 'RawFilePath' into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path/file.txt"
-- ("path/","file.txt")
--
-- >>> splitFileName "path/"
-- ("path/","")
--
-- >>> splitFileName "file.txt"
-- ("./","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
splitFileName :: RawFilePath -> (RawFilePath, RawFilePath)
splitFileName x = if BS.null path
    then ("./", file)
    else (path,file)
  where
    (path,file) = splitFileNameRaw x

-- | Get the file name
--
-- >>> takeFileName "path/file.txt"
-- "file.txt"
--
-- >>> takeFileName "path/"
-- ""
takeFileName :: RawFilePath -> RawFilePath
takeFileName = snd . splitFileName

-- | Change the file name
--
-- prop> \path -> replaceFileName path (takeFileName path) == path
replaceFileName :: RawFilePath -> ByteString -> RawFilePath
replaceFileName x y = fst (splitFileNameRaw x) </> y

-- | Drop the file name
--
-- >>> dropFileName "path/file.txt"
-- "path/"
--
-- >>> dropFileName "file.txt"
-- "./"
dropFileName :: RawFilePath -> RawFilePath
dropFileName = fst . splitFileName

-- | Get the file name, without a trailing extension
--
-- >>> takeBaseName "path/file.tar.gz"
-- "file.tar"
--
-- >>> takeBaseName ""
-- ""
takeBaseName :: RawFilePath -> ByteString
takeBaseName = dropExtension . takeFileName

-- | Change the base name
--
-- >>> replaceBaseName "path/file.tar.gz" "bob"
-- "path/bob.gz"
--
-- prop> \path -> replaceBaseName path (takeBaseName path) == path
replaceBaseName :: RawFilePath -> ByteString -> RawFilePath
replaceBaseName path name = combineRaw dir (name <.> ext)
  where
    (dir,file) = splitFileNameRaw path
    ext = takeExtension file

-- | Get the directory, moving up one level if it's already a directory
--
-- >>> takeDirectory "path/file.txt"
-- "path"
--
-- >>> takeDirectory "file"
-- "."
--
-- >>> takeDirectory "/path/to/"
-- "/path/to"
--
-- >>> takeDirectory "/path/to"
-- "/path"
takeDirectory :: RawFilePath -> RawFilePath
takeDirectory x = case () of
    () | x == "/" -> x
       | BS.null res && not (BS.null file) -> file
       | otherwise -> res
  where
    res = fst $ BS.spanEnd isPathSeparator file
    file = dropFileName x

-- | Change the directory component of a 'RawFilePath'
--
-- prop> \path -> replaceDirectory path (takeDirectory path) `_equalFilePath` path || takeDirectory path == "."
replaceDirectory :: RawFilePath -> ByteString -> RawFilePath
replaceDirectory file dir = combineRaw dir (takeFileName file)

-- | Join two paths together
--
-- >>> combine "/" "file"
-- "/file"
-- >>> combine "/path/to" "file"
-- "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- "/absolute/path"
combine :: RawFilePath -> RawFilePath -> RawFilePath
combine a b | not (BS.null b) && isPathSeparator (BS.head b) = b
            | otherwise = combineRaw a b

-- | Operator version of combine
(</>) :: RawFilePath -> RawFilePath -> RawFilePath
(</>) = combine

-- | Split a path into a list of components:
--
-- >>> splitPath "/path/to/file.txt"
-- ["/","path/","to/","file.txt"]
--
-- prop> \path -> BS.concat (splitPath path) == path
splitPath :: RawFilePath -> [RawFilePath]
splitPath = splitter
  where
    splitter x
      | BS.null x = []
      | otherwise = case BS.elemIndex pathSeparator x of
            Nothing -> [x]
            Just ix -> case BS.findIndex (not . isPathSeparator) $ BS.drop (ix+1) x of
                          Nothing -> [x]
                          Just runlen -> uncurry (:) . second splitter $ BS.splitAt (ix+1+runlen) x

-- | Like 'splitPath', but without trailing slashes
--
-- >>> splitDirectories "/path/to/file.txt"
-- ["/","path","to","file.txt"]
-- >>> splitDirectories ""
-- []
splitDirectories :: RawFilePath -> [RawFilePath]
splitDirectories x
    | BS.null x = []
    | isPathSeparator (BS.head x) = let (root,rest) = BS.splitAt 1 x
                                    in root : splitter rest
    | otherwise = splitter x
  where
    splitter = filter (not . BS.null) . BS.split pathSeparator

-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- "path/to/file.txt"
joinPath :: [RawFilePath] -> RawFilePath
joinPath = foldr (</>) BS.empty


------------------------
-- trailing path separators

-- | Check if the last character of a 'RawFilePath' is '/', unless it's the
-- root.
--
-- >>> hasTrailingPathSeparator "/path/"
-- True
-- >>> hasTrailingPathSeparator "/"
-- False
hasTrailingPathSeparator :: RawFilePath -> Bool
hasTrailingPathSeparator x
    | BS.null x = False
    | x == "/"  = False
    | otherwise = isPathSeparator $ BS.last x

-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path/"
--
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
addTrailingPathSeparator :: RawFilePath -> RawFilePath
addTrailingPathSeparator x = if hasTrailingPathSeparator x
    then x
    else x `BS.snoc` pathSeparator

-- | Remove a trailing path separator
--
-- >>> dropTrailingPathSeparator "/path/"
-- "/path"
--
-- >>> dropTrailingPathSeparator "/"
-- "/"
dropTrailingPathSeparator :: RawFilePath -> RawFilePath
dropTrailingPathSeparator x = if hasTrailingPathSeparator x
    then BS.init x
    else x

------------------------
-- Filename/system stuff

-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
isAbsolute :: RawFilePath -> Bool
isAbsolute x
    | BS.length x > 0 = isPathSeparator (BS.head x)
    | otherwise = False

-- | Check if a path is relative
--
-- prop> \path -> isRelative path /= isAbsolute path
isRelative :: RawFilePath -> Bool
isRelative = not . isAbsolute

------------------------
-- internal stuff

-- Just split the input FileName without adding/normalizing or changing
-- anything.
splitFileNameRaw :: RawFilePath -> (RawFilePath, RawFilePath)
splitFileNameRaw x = BS.breakEnd isPathSeparator x

-- | Combine two paths, assuming rhs is NOT absolute.
combineRaw :: RawFilePath -> RawFilePath -> RawFilePath
combineRaw a b | BS.null a = b
                  | BS.null b = a
                  | isPathSeparator (BS.last a) = BS.append a b
                  | otherwise = BS.intercalate (BS.singleton pathSeparator) [a, b]

-- | we don't even attempt to fully normalize file paths, this is just enough
-- equality to test some operations.
--
_equalFilePath :: RawFilePath -> RawFilePath -> Bool
_equalFilePath a b = norm a == norm b
  where
    -- Drop trailing slash *after* we've dropped duplicate slashes,
    -- otherwise there might be trailing slashes left.
    norm = dropTrailingSlash . dropDups . dropInitialDot
    dropTrailingSlash path
        | BS.length path >= 2 && isPathSeparator (BS.last path) = BS.init path
        | otherwise = path
    dropInitialDot path
        | BS.length path >= 2 && BS.take 2 path == "./" = BS.drop 2 path
        | otherwise = path
    dropDups = joinPath . map f . splitPath
    f component
        | BS.isSuffixOf "//" component = f (BS.init component) -- there might be more slashes
        | otherwise = component
