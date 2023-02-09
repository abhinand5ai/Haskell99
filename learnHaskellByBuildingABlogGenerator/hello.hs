main = putStrLn myHtml

myHtml = makeHtml "My Page Title" (h1_ "My Page Content" <> p_ "Lets learn Haskell")

_html :: String -> String
_html body = "<html>" <> body <> "</html>"

_body :: String -> String
_body content = "<body>" <> content <> "</body>"

_head :: String -> String
_head x = "<head>" <> x <> "</head"

_title :: String -> String
_title =  _el "title" 

p_ :: String -> String
p_ = _el "p"

h1_ :: String -> String
h1_ = _el "h1"

_el :: String -> String -> String
_el = \tag -> \content -> "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

makeHtml :: String -> String -> String
makeHtml title body = _html ((_head . _title) title) <> _body body

wrapHtml :: String -> String
wrapHtml content = "<html><body>" <> content <> "</body></html>"
