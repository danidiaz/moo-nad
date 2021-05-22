-- | Example of program logic in the ReaderT-record-of-functions style, that
-- abstracts away the concrete reader-like monad by depending on a module
-- signature.
module LogicThatLogs where

-- The main Moo signature has been expanded through signature merging with a signature
-- local to this library.
import Moo 
import Moo.Prelude

-- | 'M' is a 'Monad'. Which one? We don't know yet, because this program logic
-- lives in an indefinite package.
logic :: M ()
logic = do
    -- Use 'self' for special-purpose HasX capabilities, like HasLogger.
    self logger 7 "this is a message"
    -- Use 'call' for record components located through "Control.Monad.Dep.Has.Has". 
    c <- call askCounter
    if c == 0 
        then call incCounter 1
        else pure ()
