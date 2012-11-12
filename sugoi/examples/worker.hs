import Sugoi.Types (Problem(..))
import Sugoi.Main

main :: IO ()
main = workerMain solve

solve :: Integer -> IO [Integer]
solve x = return [x]
