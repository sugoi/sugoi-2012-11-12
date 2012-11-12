import Sugoi.Types (Problem(..))
import Sugoi.Main

main :: IO ()
main = workerMain (Problem :: Problem Integer [Integer]) solve

solve :: Integer -> IO [Integer]
solve x = return [x]
