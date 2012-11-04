import           Data.Conduit (($=),(=$=),(=$),($$),(>+>))
import qualified Data.Conduit as Pipe
import qualified Data.Conduit.List as Pipe
import           Data.Void (Void)


intSrc :: Pipe.Source IO Integer
intSrc = Pipe.sourceList [1..]

printer :: Pipe.Sink Integer IO ()
printer = Pipe.mapM_ print

main :: IO ()
main = do
  let
    a :: Pipe.Conduit () IO Void
    a = intSrc =$= Pipe.filter even =$= Pipe.isolate 10 =$= printer
  print "hi"