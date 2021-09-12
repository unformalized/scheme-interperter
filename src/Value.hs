-- |

module Value where

data ListVal = Atom String
             | List [ListVal]
             | DottedList [ListVal] ListVal
             | Number Integer
             | String String
             | Bool Bool
