module LogicThatLogs where

import Moo
import Moo.Prelude

logic :: M ()
logic = do
    call logger "this is a message"
    call logger "this is another message"
