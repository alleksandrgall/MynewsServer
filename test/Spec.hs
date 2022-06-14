import Category (categorySpec)
import TestHandler (withTestHandler)
import User (userSpec)

main :: IO ()
main = withTestHandler $ \h -> do
  userSpec h
  categorySpec h
