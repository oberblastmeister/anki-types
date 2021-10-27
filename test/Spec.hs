import Anki.Types as Anki
import qualified Network.HTTP.Req as Req

main :: IO ()
main = do
  putStrLn "test request"
  res <- Req.runReq Req.defaultHttpConfig $ Anki.run Anki.SDeckNames Anki.Nil
  print res