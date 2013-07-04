[![Build Status](https://secure.travis-ci.org/JohnLato/posix-paths.png?branch=master)](http://travis-ci.org/JohnLato/posix-paths)

posix-paths
===========

Haskell functions for working with posix paths

There are two primary modules, `System.Posix.FilePath` and
`System.Posix.Directory.Traversals`

`System.Posix.FilePath` provides an API for working with `RawFilePath`
paths (i.e.  bytestrings).  The API is largely compatible with the familiar
filepath module, although without windows-specific functionality (e.g. drives).

`System.Posix.Directory.Traversals` provides several functions to get the
contents of a directory structure.  These are generally much faster than
similar functions defined elsewhere.
