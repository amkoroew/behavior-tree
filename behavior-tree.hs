module BehaviorTree where

data Result = Running
            | Success
            | Failure
              deriving (Show)

-- composite nodes
sequence' []    = Success
sequence' nodes = case head nodes of
                    Running -> Running
                    Failure -> Failure
                    _       -> sequence' (tail nodes)

selector' []     = Failure
selector' nodes  = case head nodes of
                    Running -> Running
                    Success -> Success
                    _       -> selector' (tail nodes)

-- decorator nodes
not' Running = Running
not' Failure = Success
not' Success = Failure

fail' _ = Failure

succeed' _ = Success
