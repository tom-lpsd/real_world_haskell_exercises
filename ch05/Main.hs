module Main () where

import SimpleJSON
import Prettify (pretty)
import PrettyJSON (renderJValue)

main = putStrLn (pretty 40 (renderJValue (JObject [("foo", JNumber 1),
                                                   ("bar", JBool False),
                                                   ("baz", JString "あい")])))
