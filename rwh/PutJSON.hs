
module PutJSON where

import Data.List
import SimpleJSON


renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs js = intercalate ", " (map renderPair js)
        renderPair (s, val) = show s ++ ": " ++ renderJValue val

renderJValue (JArray a) = "[" ++ pairs a ++ "]"
  where pairs [] = ""
        pairs as = intercalate ", " (map renderJValue as)


putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
