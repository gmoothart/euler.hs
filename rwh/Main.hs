
module Main() where

import SimpleJSON
import PutJSON

main = do
    print (JObject [("foo", JNumber 1), ("bar", JBool False)])
