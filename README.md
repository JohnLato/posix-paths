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

Benchmarks
==========

Current benchmarks compare performance of a directory traversal based upon a
well-known Haskell example, the same algorithm implemented with RawFilePath
instead of FilePath, the unix "find" utility, and this package's
`allDirectoryContents`, `allDirectoryContents'`, and `traverseDirectory`.

Directory traversal of /tmpfs: http://johnlato.github.io/posix-paths/tmpfs.html

Directory traversal of /usr/local (~170k files):
http://johnlato.github.io/posix-paths/usrLocal.html
