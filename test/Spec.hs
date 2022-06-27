import Article (articleSpec)
import Category (categorySpec)
import Image (imageSpec)
import Internal.TestHandler (withTestHandler)
import User (userSpec)

main :: IO ()
main = withTestHandler $ \h -> do
  userSpec h
  categorySpec h
  imageSpec h
  articleSpec h
