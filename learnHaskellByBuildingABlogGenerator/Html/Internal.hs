module Html.Internal where

newtype Html =
  Html String

newtype Structure =
  Structure String

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

_el :: String -> String -> String
_el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Structure -> Structure -> Html
html_ head = Html . _el "html" . extractStruct . append_ head

title_ :: Structure -> Structure
title_ = Structure . _el "title" . extractStruct

node :: String -> Structure -> Structure
node x = Structure . _el x . extractStruct

body_ :: Structure -> Structure
body_ = node "body"

h1_ :: Structure -> Structure
h1_ = node "h1"

p_ :: Structure -> Structure
p_ = node "p"

head_ :: Structure -> Structure
head_ = node "head"

li_ :: Structure -> Structure
li_ = node "li"

listNode :: String -> [Structure] -> Structure
listNode x = node x . foldl append_ (leaf "")

ul_ :: [Structure] -> Structure
ul_ = listNode "ul"

ol_ :: [Structure] -> Structure
ol_ = listNode "ol"

code_ :: Structure -> Structure
code_ = node "pre"

leaf :: String -> Structure
leaf = Structure . escape

tleaf :: (Structure -> Structure) -> String -> Structure
tleaf t = t . leaf

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<'  -> "&lt;"
          '>'  -> "&gt;"
          '&'  -> "&amp;"
          '"'  -> "&quot;"
          '\'' -> "&#39;"
          _    -> [c]
   in concatMap escapeChar
