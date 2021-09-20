module Error where

--                       line  column
data NodeInfo = NodeInfo Int   Int
    deriving (Show, Eq, Ord)

-- makes an error message using a NodeInfo and a general error message.
makeErrMsg :: NodeInfo -> String -> String -> String
makeErrMsg (NodeInfo line column) mid msg =
    "\n" ++ mid ++ ":" ++ show line ++ ":" ++ show column ++ "\n" ++ msg

makeErrMsgMid :: String -> String -> String
makeErrMsgMid mid msg =
    "\n" ++ mid ++ ":" ++ "\n" ++ msg

-- makes an error message using a NodeInfo and a general error message
--  and no module id.
makeErrMsgNI :: NodeInfo -> String -> String
makeErrMsgNI (NodeInfo line column) msg =
    "\n" ++ show line ++ ":" ++ show column ++ "\n" ++ msg
