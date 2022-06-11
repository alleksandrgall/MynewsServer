import TestHandler (withTestHandler)
import User (userSpec)

main :: IO ()
main = withTestHandler $ \h ->
  userSpec h
