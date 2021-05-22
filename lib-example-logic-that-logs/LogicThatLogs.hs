-- | Example of program logic that abstracts away the concrete monad by
-- depending on a module signature.
module LogicThatLogs where

import Moo -- The Moo signature has been expanded through signature merging.
import Moo.Prelude

logic :: M ()
logic = do
    self logger 7 "this is a message"
    c <- call askCounter
    if c == 0 
        then call incCounter 1
        else pure ()
