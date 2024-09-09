import           Data.Binary.Builder (append)
import           Html

myHtml =
  render
    (html_
       (head_ (tleaf title_ "<title>"))
       (body_ (append_ (tleaf p_ "<body>") (tleaf p_ "Paragraph2"))))

main = putStrLn myHtml
