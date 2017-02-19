{-# LANGUAGE OverloadedLists #-}

-- | This module has functions defined for common HTML elements. In
-- essence, you can replace @elAttr [] "div"@ with @div []@ which
-- makes the code look nicer and helps prevent typos at compile time
-- (ie "div" vs "dive").
module Reflex.UI.Elements where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Reflex
import qualified Reflex.Dom as Dom

type Attrs = Map Text Text

type Attr = (Text, Text)

-- | Helper for building elements with no children:
--   @
--   div [] empty -- <div></div>
--   @
empty :: Dom.DomBuilder t m => m ()
empty = return ()

-- * Common Attributes

class' :: Text -> Attr
class' = ("class",)

id' :: Text -> Attr
id' = ("id",)

-- * Elements

-- ** Content Sectioning

address :: Dom.DomBuilder t m => Attrs -> m a -> m a
address = Dom.elAttr "address"

article :: Dom.DomBuilder t m => Attrs -> m a -> m a
article = Dom.elAttr "article"

aside :: Dom.DomBuilder t m => Attrs -> m a -> m a
aside = Dom.elAttr "aside"

footer :: Dom.DomBuilder t m => Attrs -> m a -> m a
footer = Dom.elAttr "footer"

header :: Dom.DomBuilder t m => Attrs -> m a -> m a
header = Dom.elAttr "header"

hgroup :: Dom.DomBuilder t m => Attrs -> m a -> m a
hgroup = Dom.elAttr "hgroup"

nav :: Dom.DomBuilder t m => Attrs -> m a -> m a
nav = Dom.elAttr "nav"

section :: Dom.DomBuilder t m => Attrs -> m a -> m a
section = Dom.elAttr "section"

-- *** Headers

h1 :: Dom.DomBuilder t m => Attrs -> m a -> m a
h1 = Dom.elAttr "h1"

h2 :: Dom.DomBuilder t m => Attrs -> m a -> m a
h2 = Dom.elAttr "h2"

h3 :: Dom.DomBuilder t m => Attrs -> m a -> m a
h3 = Dom.elAttr "h3"

h4 :: Dom.DomBuilder t m => Attrs -> m a -> m a
h4 = Dom.elAttr "h4"

h5 :: Dom.DomBuilder t m => Attrs -> m a -> m a
h5 = Dom.elAttr "h5"

h6 :: Dom.DomBuilder t m => Attrs -> m a -> m a
h6 = Dom.elAttr "h6"

-- ** Text Content

dd :: Dom.DomBuilder t m => Attrs -> m a -> m a
dd = Dom.elAttr "dd"

div :: Dom.DomBuilder t m => Attrs -> m a -> m a
div = Dom.elAttr "div"

dl :: Dom.DomBuilder t m => Attrs -> m a -> m a
dl = Dom.elAttr "dl"

dt :: Dom.DomBuilder t m => Attrs -> m a -> m a
dt = Dom.elAttr "dt"

figcaption :: Dom.DomBuilder t m => Attrs -> m a -> m a
figcaption = Dom.elAttr "figcaption"

figure :: Dom.DomBuilder t m => Attrs -> m a -> m a
figure = Dom.elAttr "figure"

hr :: Dom.DomBuilder t m => Attrs -> m a -> m a
hr = Dom.elAttr "hr"

li :: Dom.DomBuilder t m => Attrs -> m a -> m a
li = Dom.elAttr "li"

main' :: Dom.DomBuilder t m => Attrs -> m a -> m a
main' = Dom.elAttr "main"

ol :: Dom.DomBuilder t m => Attrs -> m a -> m a
ol = Dom.elAttr "ol"

p :: Dom.DomBuilder t m => Attrs -> m a -> m a
p = Dom.elAttr "p"

pre :: Dom.DomBuilder t m => Attrs -> m a -> m a
pre = Dom.elAttr "pre"

ul :: Dom.DomBuilder t m => Attrs -> m a -> m a
ul = Dom.elAttr "ul"

-- ** Inline Text

a :: Dom.DomBuilder t m => Attrs -> m a -> m a
a = Dom.elAttr "a"

abbr :: Dom.DomBuilder t m => Attrs -> m a -> m a
abbr = Dom.elAttr "abbr"

b :: Dom.DomBuilder t m => Attrs -> m a -> m a
b = Dom.elAttr "b"

bdi :: Dom.DomBuilder t m => Attrs -> m a -> m a
bdi = Dom.elAttr "bdi"

bdo :: Dom.DomBuilder t m => Attrs -> m a -> m a
bdo = Dom.elAttr "bdo"

br :: Dom.DomBuilder t m => Attrs -> m a -> m a
br = Dom.elAttr "br"

cite :: Dom.DomBuilder t m => Attrs -> m a -> m a
cite = Dom.elAttr "cite"

code :: Dom.DomBuilder t m => Attrs -> m a -> m a
code = Dom.elAttr "code"

data' :: Dom.DomBuilder t m => Attrs -> m a -> m a
data' = Dom.elAttr "data"

dfn :: Dom.DomBuilder t m => Attrs -> m a -> m a
dfn = Dom.elAttr "dfn"

em :: Dom.DomBuilder t m => Attrs -> m a -> m a
em = Dom.elAttr "em"

i :: Dom.DomBuilder t m => Attrs -> m a -> m a
i = Dom.elAttr "i"

kbd :: Dom.DomBuilder t m => Attrs -> m a -> m a
kbd = Dom.elAttr "kbd"

mark :: Dom.DomBuilder t m => Attrs -> m a -> m a
mark = Dom.elAttr "mark"

q :: Dom.DomBuilder t m => Attrs -> m a -> m a
q = Dom.elAttr "q"

rp :: Dom.DomBuilder t m => Attrs -> m a -> m a
rp = Dom.elAttr "rp"

rt :: Dom.DomBuilder t m => Attrs -> m a -> m a
rt = Dom.elAttr "rt"

rtc :: Dom.DomBuilder t m => Attrs -> m a -> m a
rtc = Dom.elAttr "rtc"

ruby :: Dom.DomBuilder t m => Attrs -> m a -> m a
ruby = Dom.elAttr "ruby"

s :: Dom.DomBuilder t m => Attrs -> m a -> m a
s = Dom.elAttr "s"

samp :: Dom.DomBuilder t m => Attrs -> m a -> m a
samp = Dom.elAttr "samp"

small :: Dom.DomBuilder t m => Attrs -> m a -> m a
small = Dom.elAttr "small"

span :: Dom.DomBuilder t m => Attrs -> m a -> m a
span = Dom.elAttr "span"

strong :: Dom.DomBuilder t m => Attrs -> m a -> m a
strong = Dom.elAttr "strong"

sub :: Dom.DomBuilder t m => Attrs -> m a -> m a
sub = Dom.elAttr "sub"

sup :: Dom.DomBuilder t m => Attrs -> m a -> m a
sup = Dom.elAttr "sup"

time :: Dom.DomBuilder t m => Attrs -> m a -> m a
time = Dom.elAttr "time"

u :: Dom.DomBuilder t m => Attrs -> m a -> m a
u = Dom.elAttr "u"

var :: Dom.DomBuilder t m => Attrs -> m a -> m a
var = Dom.elAttr "var"

wbr :: Dom.DomBuilder t m => Attrs -> m a -> m a
wbr = Dom.elAttr "wbr"

-- ** Image and Multimedia

area :: Dom.DomBuilder t m => Attrs -> m a -> m a
area = Dom.elAttr "area"

audio :: Dom.DomBuilder t m => Attrs -> m a -> m a
audio = Dom.elAttr "audio"

img :: Dom.DomBuilder t m => Attrs -> m a -> m a
img = Dom.elAttr "img"

map :: Dom.DomBuilder t m => Attrs -> m a -> m a
map = Dom.elAttr "map"

track :: Dom.DomBuilder t m => Attrs -> m a -> m a
track = Dom.elAttr "track"

video :: Dom.DomBuilder t m => Attrs -> m a -> m a
video = Dom.elAttr "video"

-- ** Embedded Content

embed :: Dom.DomBuilder t m => Attrs -> m a -> m a
embed = Dom.elAttr "embed"

object :: Dom.DomBuilder t m => Attrs -> m a -> m a
object = Dom.elAttr "object"

param :: Dom.DomBuilder t m => Attrs -> m a -> m a
param = Dom.elAttr "param"

source :: Dom.DomBuilder t m => Attrs -> m a -> m a
source = Dom.elAttr "source"

-- ** Scripting

canvas :: Dom.DomBuilder t m => Attrs -> m a -> m a
canvas = Dom.elAttr "canvas"

noscript :: Dom.DomBuilder t m => Attrs -> m a -> m a
noscript = Dom.elAttr "noscript"

script :: Dom.DomBuilder t m => Attrs -> m a -> m a
script = Dom.elAttr "script"

-- ** Demarcating Edits

del :: Dom.DomBuilder t m => Attrs -> m a -> m a
del = Dom.elAttr "del"

ins :: Dom.DomBuilder t m => Attrs -> m a -> m a
ins = Dom.elAttr "ins"

-- ** Table Content

caption :: Dom.DomBuilder t m => Attrs -> m a -> m a
caption = Dom.elAttr "caption"

col :: Dom.DomBuilder t m => Attrs -> m a -> m a
col = Dom.elAttr "col"

colgroup :: Dom.DomBuilder t m => Attrs -> m a -> m a
colgroup = Dom.elAttr "colgroup"

table :: Dom.DomBuilder t m => Attrs -> m a -> m a
table = Dom.elAttr "table"

tbody :: Dom.DomBuilder t m => Attrs -> m a -> m a
tbody = Dom.elAttr "tbody"

td :: Dom.DomBuilder t m => Attrs -> m a -> m a
td = Dom.elAttr "td"

tfoot :: Dom.DomBuilder t m => Attrs -> m a -> m a
tfoot = Dom.elAttr "tfoot"

th :: Dom.DomBuilder t m => Attrs -> m a -> m a
th = Dom.elAttr "th"

thead :: Dom.DomBuilder t m => Attrs -> m a -> m a
thead = Dom.elAttr "thead"

tr :: Dom.DomBuilder t m => Attrs -> m a -> m a
tr = Dom.elAttr "tr"

-- ** Forms

button :: Dom.DomBuilder t m => Attrs -> m a -> m a
button = Dom.elAttr "button"

datalist :: Dom.DomBuilder t m => Attrs -> m a -> m a
datalist = Dom.elAttr "datalist"

fieldset :: Dom.DomBuilder t m => Attrs -> m a -> m a
fieldset = Dom.elAttr "fieldset"

form :: Dom.DomBuilder t m => Attrs -> m a -> m a
form = Dom.elAttr "form"

input :: Dom.DomBuilder t m => Attrs -> m a -> m a
input = Dom.elAttr "input"

label :: Dom.DomBuilder t m => Attrs -> m a -> m a
label = Dom.elAttr "label"

legend :: Dom.DomBuilder t m => Attrs -> m a -> m a
legend = Dom.elAttr "legend"

meter :: Dom.DomBuilder t m => Attrs -> m a -> m a
meter = Dom.elAttr "meter"

optgroup :: Dom.DomBuilder t m => Attrs -> m a -> m a
optgroup = Dom.elAttr "optgroup"

option :: Dom.DomBuilder t m => Attrs -> m a -> m a
option = Dom.elAttr "option"

output :: Dom.DomBuilder t m => Attrs -> m a -> m a
output = Dom.elAttr "output"

progress :: Dom.DomBuilder t m => Attrs -> m a -> m a
progress = Dom.elAttr "progress"

select :: Dom.DomBuilder t m => Attrs -> m a -> m a
select = Dom.elAttr "select"

textarea :: Dom.DomBuilder t m => Attrs -> m a -> m a
textarea = Dom.elAttr "textarea"

-- ** Interactive Elements

details :: Dom.DomBuilder t m => Attrs -> m a -> m a
details = Dom.elAttr "details"

dialogue :: Dom.DomBuilder t m => Attrs -> m a -> m a
dialogue = Dom.elAttr "dialogue"

menu :: Dom.DomBuilder t m => Attrs -> m a -> m a
menu = Dom.elAttr "menu"

menuitem :: Dom.DomBuilder t m => Attrs -> m a -> m a
menuitem = Dom.elAttr "menuitem"

summary :: Dom.DomBuilder t m => Attrs -> m a -> m a
summary = Dom.elAttr "summary"

