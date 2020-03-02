import Cardano.Prelude

import           System.Directory (listDirectory)
import           System.IO (BufferMode (..))
import qualified System.IO as IO
import           System.Process (callCommand)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering
  testCliMain

testCliMain ::  IO ()
testCliMain = do
  tests <- filter (`notElem` ["Core"]) <$> listDirectory "cardano-node/test/Test/Cardano/CLI/"
  mapM_ (\t -> callCommand $ "./cardano-node/test/Test/Cardano/CLI/" ++ t) tests
  pure ()
