HUnitLang98.lhs  --  HUnit language support, generic Haskell 98 variant

Note: The Haskell system you use needs to find this file when looking
for module `HUnitLang`.

$Id: HUnitLang98.lhs,v 1.2 2002/02/14 19:27:56 heringto Exp $

> module HUnitLang
> (
>   Assertion,
>   assertFailure,
>   performTestCase
> )
> where


When adapting this module for other Haskell language systems, change
the imports and the implementations but not the interfaces.



Imports
-------

> import Data.List (isPrefixOf)
> import System.IO.Error (ioeGetErrorString)
> import Control.Exception (try)


Interfaces
----------

An assertion is an `IO` computation with trivial result.

> type Assertion = IO ()

`assertFailure` signals an assertion failure with a given message.

> assertFailure :: String -> Assertion

`performTestCase` performs a single test case.  The meaning of the
result is as follows:
  Nothing               test case success
  Just (True,  msg)     test case failure with the given message
  Just (False, msg)     test case error with the given message

> performTestCase :: Assertion -> IO (Maybe (Bool, String))


Implementations
---------------

> hunitPrefix = "HUnit:"

> hugsPrefix  = "IO Error: User error\nReason: "
> nhc98Prefix = "I/O error (user-defined), call to function `userError':\n  "
> -- GHC prepends no prefix to the user-supplied string.

> assertFailure msg = ioError (userError (hunitPrefix ++ msg))

> performTestCase action = do r <- try action
>                             case r of Right () -> return Nothing
>                                       Left  e  -> return (Just (decode e))
>  where
>   decode e = let s0 = ioeGetErrorString e
>                  (_, s1) = dropPrefix hugsPrefix  s0
>                  (_, s2) = dropPrefix nhc98Prefix s1
>              in            dropPrefix hunitPrefix s2
>   dropPrefix pref str = if pref `isPrefixOf` str
>                           then (True, drop (length pref) str)
>                           else (False, str)
