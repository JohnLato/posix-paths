# 0.3.0.0

* Use `foreign import ccall safe` for `readdir()` to avoid blocking
  a capability; see https://github.com/haskell/unix/issues/34.
* Use `unliftio` to allow:
  * `MonadIO` based callback functions (see [Lifting `IO` to `MonadIO`](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance/#lifting-codeiocode-to-codemonadiocode) why that's important)
  * while still providing `bracket`
* Add `traverseDirectoryContents`.

# 0.2.1.6

* Relax base constraint for GHC 8.6.

# 0.2.1.5

* Relax base constraint for GHC 8.4.

# 0.2.1.4

* Fix QuickCheck failure on duplicate slashes (incorrect test). Fixes #22
