import Sugoi.Types (Problem(..))
import Sugoi.Main

main :: IO ()
main = questionerMain (Problem :: Problem Integer [Integer]) qs

qs :: [Integer]
qs = [2..10000]
