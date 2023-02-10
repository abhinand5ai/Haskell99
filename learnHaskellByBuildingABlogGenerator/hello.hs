
_html :: String -> String
_html body = "<html>" <> body <> "</html>"

_body :: String -> String
_body content = "<body>" <> content <> "</body>"

_head :: String -> String
_head x = "<head>" <> x <> "</head"

_title :: String -> String
_title = _el "title"

_el :: String -> String -> String
_el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- _el = \tag -> \content -> "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

wrapHtml :: String -> String
wrapHtml content = "<html><body>" <> content <> "</body></html>"

newtype Html = Html String

newtype Structure = Structure String

append_ :: Structure -> Structure -> Structure
append_ (Structure x) (Structure y) = Structure (x <> y)

render :: Html -> String
render (Html x) = x

extractStruct :: Structure -> String
extractStruct (Structure x) = x

type Title = String

type Body = String

type Paragrah = String

type Heading1 = String

html_ :: Structure -> Structure -> Html
html_ head = Html . _el "html" . extractStruct . append_ head

title_ :: Title -> Structure
title_ = Structure . _el "title"

body_ :: Body -> Structure
body_ = Structure . _el "body"

h1_ :: Heading1 -> Structure
h1_ = Structure . _el "h1"

p_ :: Paragrah -> Structure
p_ = Structure . _el "p"

head_ :: String -> Structure
head_ = Structure . _el "head"

myHtml =
  render
    ( html_
        ( head_
            ( extractStruct
                (title_ "title")
            )
        )
        ( body_
            ( extractStruct
                ( append_
                    (head_ "My Heading")
                    ( append_
                        (p_ "Paragraph1")
                        (p_ "Paragraph2")
                    )
                )
            )
        )
    )

main = putStrLn myHtml