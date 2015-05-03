module BehaviorTree where

data Result = Running
            | Success
            | Failure
              deriving (Show)

sequence' []    = Success
sequence' tasks = case head tasks of
                    Failure -> Failure
                    _       -> sequence' (tail tasks)
              
selector []     = Failure
selector tasks  = case (head tasks) of
                    Success -> Success
                    _       -> selector (tail tasks)
