{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}

module System.Posix.FilePath (
  pathSeparator
, isPathSeparator
, searchPathSeparator
, isSearchPathSeparator
, extSeparator
, isExtSeparator

, splitExtension
, takeExtension
-- , replaceExtension
, dropExtension
, addExtension
-- , hasExtension
, (<.>)
-- , splitExtensions
-- , dropExtensions
-- , takeExtensions

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
import           Data.Word (Word8)
import           Data.Char (ord)

import           Control.Arrow (second)

pathSeparator :: Word8
pathSeparator = fromIntegral $ ord '/'

isPathSeparator :: Word8 -> Bool
isPathSeparator = (== pathSeparator)

searchPathSeparator :: Word8
searchPathSeparator = fromIntegral $ ord ':'

isSearchPathSeparator :: Word8 -> Bool
isSearchPathSeparator = (== searchPathSeparator)

extSeparator :: Word8
extSeparator = fromIntegral $ ord '.'

isExtSeparator :: Word8 -> Bool
isExtSeparator = (== extSeparator)

------------------------
-- extension stuff

splitExtension :: RawFilePath -> (ByteString, ByteString)
splitExtension x = if BS.null basename
    then (x,"")
    else (BS.concat [path,BS.init basename],BS.cons extSeparator fileExt)
  where
    (path,file) = splitFileNameRaw x
    (basename,fileExt) = BS.breakEnd isExtSeparator file

takeExtension :: RawFilePath -> ByteString
takeExtension = snd . splitExtension

dropExtension :: RawFilePath -> RawFilePath
dropExtension = fst . splitExtension

addExtension :: RawFilePath -> ByteString -> RawFilePath
addExtension file ext
    | BS.null ext = file
    | isExtSeparator (BS.head ext) = BS.concat [file, ext]
    | otherwise = BS.concat [file, BS.singleton extSeparator, ext]


(<.>) :: RawFilePath -> ByteString -> RawFilePath
(<.>) = addExtension

------------------------
-- more stuff

splitFileName :: RawFilePath -> (RawFilePath, RawFilePath)
splitFileName x = if BS.null path
    then ("./", file)
    else (path,file)
  where
    (path,file) = splitFileNameRaw x

takeFileName :: RawFilePath -> RawFilePath
takeFileName = snd . splitFileName

replaceFileName :: RawFilePath -> ByteString -> RawFilePath
replaceFileName x y = fst (splitFileNameRaw x) </> y

dropFileName :: RawFilePath -> RawFilePath
dropFileName = fst . splitFileName

takeBaseName :: RawFilePath -> ByteString
takeBaseName = dropExtension . takeFileName

replaceBaseName :: RawFilePath -> ByteString -> RawFilePath
replaceBaseName path name = combineRaw dir (name <.> ext)
  where
    (dir,file) = splitFileNameRaw path
    ext = takeExtension file

takeDirectory :: RawFilePath -> RawFilePath
takeDirectory x = case () of
    () | x == "/" -> x
       | BS.null res && not (BS.null file) -> file
       | otherwise -> res
  where
    res = fst $ BS.spanEnd isPathSeparator file
    file = dropFileName x

replaceDirectory :: RawFilePath -> ByteString -> RawFilePath
replaceDirectory file dir = combineRaw dir (takeFileName file)

combine :: RawFilePath -> RawFilePath -> RawFilePath
combine a b | not (BS.null b) && isPathSeparator (BS.head b) = b
            | otherwise = combineRaw a b

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
(</>) = combine

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

splitDirectories :: RawFilePath -> [RawFilePath]
splitDirectories x
    | BS.null x = []
    | isPathSeparator (BS.head x) = let (root,rest) = BS.splitAt 1 x
                                    in root : splitter rest
    | otherwise = splitter x
  where
    splitter = filter (not . BS.null) . BS.split pathSeparator

joinPath :: [RawFilePath] -> RawFilePath
joinPath = foldr (</>) BS.empty


------------------------
-- trailing path separators

hasTrailingPathSeparator :: RawFilePath -> Bool
hasTrailingPathSeparator x
    | BS.null x = False
    | otherwise = isPathSeparator $ BS.last x

addTrailingPathSeparator :: RawFilePath -> RawFilePath
addTrailingPathSeparator x = if hasTrailingPathSeparator x
    then x
    else x `BS.snoc` pathSeparator

dropTrailingPathSeparator :: RawFilePath -> RawFilePath
dropTrailingPathSeparator x = if hasTrailingPathSeparator x
    then BS.init x
    else x

------------------------
-- Filename/system stuff

isAbsolute :: RawFilePath -> Bool
isAbsolute x
    | BS.length x > 0 = isPathSeparator (BS.head x)
    | otherwise = False

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
                  | isPathSeparator (BS.last a) = BS.concat [a, b]
                  | otherwise = BS.concat [a,BS.singleton pathSeparator, b]



