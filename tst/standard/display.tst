############################################################################
##
#W  standard/display.tst
#Y  Copyright (C) 2016                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/display.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# Test TikzString for a pbr
gap> TikzString(PBR([[-2, 2], [1, 2], [-1, 2]], [[-3], [1, 2], [- 3]]));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\begin{document}\n\\us\
etikzlibrary{arrows}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\dra\
w[semithick, -{>[width = 1.5mm, length = 2.5mm]}]}\n\\begin{tikzpicture}[\n  v\
ertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n  ghost/.style={c\
ircle, draw = none, inner sep = 0.14cm},\n  botloop/.style={min distance = 8mm\
, out = -70, in = -110},\n  toploop/.style={min distance = 8mm, out = 70, in =\
 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,3} {\n    \\node \
[vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n  \
}\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 0) {};\n\
    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n\
  \\arc (1) to (-2);\n  \\arc (1) .. controls (1.0666666666666667, 2.333333333\
3333335) and (0.93333333333333324, 2.3333333333333335) .. (2);\n\n  % arcs fro\
m vertex -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.0833333333333335)\
 and (1.2, 1.0833333333333335) .. (-3);\n\n  % arcs from vertex 2\n  \\arc (2)\
 .. controls (0.93333333333333324, 2.3333333333333335) and (1.0666666666666667\
, 2.3333333333333335) .. (1);\n  \\arc (2) edge [toploop] (2);\n\n  % arcs fro\
m vertex -2\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n\n  % arcs from verte\
x 3\n  \\arc (3) to (-1);\n  \\arc (3) .. controls (1.6000000000000001, 2.3333\
333333333335) and (1.7333333333333334, 2.3333333333333335) .. (2);\n\n  % arcs\
 from vertex -3\n  \\arc (-3) edge [botloop] (-3);\n\n\\end{tikzpicture}\n\\en\
d{document}"
gap> TikzString(PBR([[-2, 2], [1, 2], [-1, 2]], [[-3], [1, 2], [- 3]]), 
> rec(labels := true));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\begin{document}\n\\us\
etikzlibrary{arrows}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\dra\
w[semithick, -{>[width = 1.5mm, length = 2.5mm]}]}\n\\begin{tikzpicture}[\n  v\
ertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n  ghost/.style={c\
ircle, draw = none, inner sep = 0.14cm},\n  botloop/.style={min distance = 8mm\
, out = -70, in = -110},\n  toploop/.style={min distance = 8mm, out = 70, in =\
 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,3} {\n    \\node \
[vertex, label={[yshift=9mm]\\i}] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i\
) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vert\
ex, label={[yshift=-15mm,xshift=-0.5mm]-\\i}] at (\\i/1.5, 0) {};\n    \\node \
[ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1)\
 to (-2);\n  \\arc (1) .. controls (1.0666666666666667, 2.3333333333333335) an\
d (0.93333333333333324, 2.3333333333333335) .. (2);\n\n  % arcs from vertex -1\
\n  \\arc (-1) .. controls (1.4666666666666668, 1.0833333333333335) and (1.2, \
1.0833333333333335) .. (-3);\n\n  % arcs from vertex 2\n  \\arc (2) .. control\
s (0.93333333333333324, 2.3333333333333335) and (1.0666666666666667, 2.3333333\
333333335) .. (1);\n  \\arc (2) edge [toploop] (2);\n\n  % arcs from vertex -2\
\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n\n  % arcs from vertex 3\n  \\ar\
c (3) to (-1);\n  \\arc (3) .. controls (1.6000000000000001, 2.333333333333333\
5) and (1.7333333333333334, 2.3333333333333335) .. (2);\n\n  % arcs from verte\
x -3\n  \\arc (-3) edge [botloop] (-3);\n\n\\end{tikzpicture}\n\\end{document}\
"

# TikzString for a bipartition collection
gap> TikzString(PartitionMonoid(2));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\begin{document}\n\\be\
gin{center}\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n\
  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {\
$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n\
  %lines\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and labels\
\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fi\
ll(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \
\\draw(2, 2)--(2, 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{t\
ikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle\
(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(2, 0\
)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1\
, 2)--(2, 0);\n\n  %block number 2\n  %vertices and labels\n  \\fill(2, 2)circ\
le(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.125)\
;\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\draw(2, 2)--(1, 0);\
\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %blo\
ck number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.9\
4999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n \
 %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [\
above] {$2$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-\
2$};\n\n  %lines\n  \\draw(2, 2)--(2, 0);\n\n  %block number 3\n  %vertices an\
d labels\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\
\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpictur\
e}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n\
  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block\
 number 2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95,\
 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node\
 [below] {$-1$};\n\n  %lines\n  \\draw(2, 2)--(1, 0);\n\n  %block number 3\n  \
%vertices and labels\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [bel\
ow] {$-2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin\
{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circ\
le(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n\n  %lines\
\n\n  %block number 2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \
\\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1\
, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) n\
ode [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1, 0.75) and \
(2, 0.75) .. (2, 0.125);\n  \\draw(2, 2)--(2, 0);\n\\end{tikzpicture}\n\n\n\\b\
igskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and \
labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [\
above] {$1$};\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {\
$2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n  \
\\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\
\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n  \\d\
raw(1, 0.125) .. controls (1, 0.75) and (2, 0.75) .. (2, 0.125);\n  \\draw(1, \
2)--(1, 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture\
}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n \
 \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(2, 2)circle(.1\
25);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(2, 0)circle(.125);\n  \
\\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 1.875) .. contro\
ls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n  \\draw(2, 2)--(2, 0);\n\n  %block\
 number 2\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0\
.2) node [below] {$-1$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigsk\
ip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\f\
ill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\
\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fi\
ll(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n  \
\\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n  \\draw(\
1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and labels\n  \\fill(2, 0)cir\
cle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpict\
ure}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %\
vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996\
, 2.2) node [above] {$1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) nod\
e [below] {$-2$};\n\n  %lines\n  \\draw(1, 2)--(2, 0);\n\n  %block number 2\n \
 %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [\
above] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fi\
ll(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\\e\
nd{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block nu\
mber 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.949999\
99999999996, 2.2) node [above] {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1\
, -0.2) node [below] {$-1$};\n\n  %lines\n  \\draw(1, 2)--(1, 0);\n\n  %block \
number 2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, \
2.2) node [above] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and lab\
els\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  \
%lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n\
  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\dr\
aw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(1, 0)circle(.125);\
\n  \\draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\dra\
w(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1\
, 0.75) and (2, 0.75) .. (2, 0.125);\n  \\draw(1, 2)--(1, 0);\n\n  %block numb\
er 2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2)\
 node [above] {$2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\
\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(\
1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n\n\
  %lines\n\n  %block number 2\n  %vertices and labels\n  \\fill(2, 2)circle(.1\
25);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\n  %lines\n\n  %block number 3\
\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node \
[below] {$-1$};\n\n  %lines\n\n  %block number 4\n  %vertices and labels\n  \\\
fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\
\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block\
 number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.949\
99999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n  %\
vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [ab\
ove] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fill\
(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)cir\
cle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.\
125) .. controls (1, 0.75) and (2, 0.75) .. (2, 0.125);\n\\end{tikzpicture}\n\
\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertic\
es and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2)\
 node [above] {$1$};\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [a\
bove] {$2$};\n\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.2\
5) .. (2, 1.875);\n\n  %block number 2\n  %vertices and labels\n  \\fill(1, 0)\
circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\n  %block \
number 3\n  %vertices and labels\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.\
2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigski\
p\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fi\
ll(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\
\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\n  %l\
ines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n\
\n  %block number 2\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n  \\\
draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -\
0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1, 0.75\
) and (2, 0.75) .. (2, 0.125);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\
\\end{center}\\end{document}"

# Test TikzString for a bipartition
gap> TikzString(Bipartition([[1, 3], [2, -1], [-2, -3]]));
"\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1\
, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \
\\fill(3, 2)circle(.125);\n  \\draw(2.9500000000000002, 2.2) node [above] {$3$\
};\n\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1.1666666666666667) and (3,\
 1.1666666666666667) .. (3, 1.875);\n\n  %block number 2\n  %vertices and labe\
ls\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\\
fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n \
 \\draw(2, 2)--(1, 0);\n\n  %block number 3\n  %vertices and labels\n  \\fill(\
2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n  \\fill(3, 0)circ\
le(.125);\n  \\draw(3, -0.2) node [below] {$-3$};\n\n  %lines\n  \\draw(2, 0.1\
25) .. controls (2, 0.66666666666666663) and (3, 0.66666666666666663) .. (3, 0\
.125);\n\\end{tikzpicture}\n\n"
gap> TikzString(Bipartition([[1, 3], [2, -1], [-2, -3]]), 
> rec(colors := true, labels := true, beginDocument := true, 
>     endDocument := true));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\begin{document}\n\\be\
gin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill[red](1\
, 2)circle(.125);\n  \\draw[red](0.94999999999999996, 2.2) node [above] {$1$};\
\n  \\fill[red](3, 2)circle(.125);\n  \\draw[red](2.9500000000000002, 2.2) nod\
e [above] {$3$};\n\n  %lines\n  \\draw[red](1, 1.875) .. controls (1, 1.166666\
6666666667) and (3, 1.1666666666666667) .. (3, 1.875);\n\n  %block number 2\n \
 %vertices and labels\n  \\fill[green](2, 2)circle(.125);\n  \\draw[green](1.9\
5, 2.2) node [above] {$2$};\n  \\fill[green](1, 0)circle(.125);\n  \\draw[gree\
n](1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\draw[green](2, 2)--(1, 0);\n\
\n  %block number 3\n  %vertices and labels\n  \\fill[blue](2, 0)circle(.125);\
\n  \\draw[blue](2, -0.2) node [below] {$-2$};\n  \\fill[blue](3, 0)circle(.12\
5);\n  \\draw[blue](3, -0.2) node [below] {$-3$};\n\n  %lines\n  \\draw[blue](\
2, 0.125) .. controls (2, 0.66666666666666663) and (3, 0.66666666666666663) ..\
 (3, 0.125);\n\\end{tikzpicture}\n\n\\end{document}"

# Test DotString for a semigroup
gap> DotString(RegularBooleanMatMonoid(3));
"//dot\ndigraph  DClasses {\nnode [shape=plaintext]\nedge [color=black,arrowhe\
ad=none]\n1 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD \
COLSPAN=\"1\" BORDER = \"0\" > 1</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD></TR>\
\n</TABLE>>];\n2 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLB\
ORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\"0\"\
><TD COLSPAN=\"7\" BORDER = \"0\" > 2</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD>\
<TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</\
TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">\
*</TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGC\
OLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD \
BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gra\
y\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"\
gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</\
TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGC\
OLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><\
TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"\
gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">\
*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n3 [shape=box style=inv\
isible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSP\
ACING=\"0\" PORT=\"3\">\n<TR BORDER=\"0\"><TD COLSPAN=\"12\" BORDER = \"0\" > \
3</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOL\
OR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BG\
COLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD>\
<TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BG\
COLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gr\
ay\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BG\
COLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></T\
R>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gr\
ay\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BG\
COLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"g\
ray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<T\
R><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"g\
ray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*\
</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\
\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gr\
ay\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><T\
D BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD\
><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*<\
/TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\"\
>*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BG\
COLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD>\
<TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</\
TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"\
></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGC\
OLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD B\
GCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><T\
D BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n4 [sha\
pe=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDI\
NG=\"10\" CELLSPACING=\"0\" PORT=\"4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"6\" BO\
RDER = \"0\" > 4</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><\
TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n5 [shape=box st\
yle=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\"\
 CELLSPACING=\"0\" PORT=\"5\">\n<TR BORDER=\"0\"><TD COLSPAN=\"9\" BORDER = \"\
0\" > 5</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD\
 BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD>\
<TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></\
TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLO\
R=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGC\
OLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gra\
y\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"w\
hite\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><\
TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*\
</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD B\
GCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><T\
D BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOL\
OR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\
\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\
\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n</TABLE>>];\n6 [shape=box style=invi\
sible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPA\
CING=\"0\" PORT=\"6\">\n<TR BORDER=\"0\"><TD COLSPAN=\"6\" BORDER = \"0\" > 6<\
/TD></TR><TR><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*<\
/font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</f\
ont></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</fon\
t></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font>\
</TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></\
TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD\
></TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</\
font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</fo\
nt></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font\
></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font><\
/TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></T\
D><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD>\
</TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</f\
ont></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</fon\
t></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font>\
</TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></\
TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD\
><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD><\
/TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</fo\
nt></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font\
></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font><\
/TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></T\
D><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD>\
<TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD></\
TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</fon\
t></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font>\
</TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></\
TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD\
><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD><\
TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD></T\
R>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font\
></TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font><\
/TD><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></T\
D><TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD>\
<TD CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD><T\
D CELLPADDING=\"10\" BGCOLOR=\"white\"><font color=\"white\">*</font></TD></TR\
>\n</TABLE>>];\n7 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELL\
BORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"7\">\n<TR BORDER=\"0\
\"><TD COLSPAN=\"3\" BORDER = \"0\" > 7</TD></TR><TR><TD BGCOLOR=\"gray\">*</T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">\
*</TD></TR>\n</TABLE>>];\n8 [shape=box style=invisible label=<\n<TABLE BORDER=\
\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"8\">\n<TR B\
ORDER=\"0\"><TD COLSPAN=\"3\" BORDER = \"0\" > 8</TD></TR><TR><TD BGCOLOR=\"gr\
ay\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><T\
D BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD\
></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"gray\">*</TD></TR>\n</TABLE>>];\n9 [shape=box style=invisible label=<\n<TABL\
E BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"9\
\">\n<TR BORDER=\"0\"><TD COLSPAN=\"6\" BORDER = \"0\" > 9</TD></TR><TR><TD BG\
COLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD>\
</TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\
\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD B\
GCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><T\
D BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<T\
R><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD></TR>\n</TABLE>>];\n10 [shape=box style=invisible label=<\n<TABLE BOR\
DER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"10\">\n\
<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\" > 10</TD></TR><TR><TD BGCOLO\
R=\"gray\">*</TD></TR>\n</TABLE>>];\n2 -> 1\n3 -> 2\n4 -> 3\n5 -> 3\n6 -> 4\n6\
 -> 5\n7 -> 4\n7 -> 5\n8 -> 4\n8 -> 5\n9 -> 6\n9 -> 7\n9 -> 8\n10 -> 9\n }"
gap> DotString(RegularBooleanMatMonoid(2), rec(maximal := true));
"//dot\ndigraph  DClasses {\nnode [shape=plaintext]\nedge [color=black,arrowhe\
ad=none]\n1 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD \
COLSPAN=\"1\" BORDER = \"0\" > 1</TD></TR><TR><TD BGCOLOR=\"gray\">1</TD></TR>\
\n</TABLE>>];\n2 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLB\
ORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\"0\"\
><TD COLSPAN=\"3\" BORDER = \"0\" > 2</TD></TR><TR><TD BGCOLOR=\"gray\">1</TD>\
<TD BGCOLOR=\"gray\">1</TD><TD BGCOLOR=\"gray\">1</TD></TR>\n<TR><TD BGCOLOR=\
\"gray\">1</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">1</TD></TR>\n<T\
R><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">1</TD><TD BGCOLOR=\"gray\">1\
</TD></TR>\n</TABLE>>];\n3 [shape=box style=invisible label=<\n<TABLE BORDER=\
\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"3\">\n<TR B\
ORDER=\"0\"><TD COLSPAN=\"2\" BORDER = \"0\" > 3</TD></TR><TR><TD BGCOLOR=\"gr\
ay\">1</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><T\
D BGCOLOR=\"gray\">1</TD></TR>\n</TABLE>>];\n4 [shape=box style=invisible labe\
l=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\"\
 PORT=\"4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\" > 4</TD></TR><\
TR><TD BGCOLOR=\"gray\">C2</TD></TR>\n</TABLE>>];\n2 -> 1\n3 -> 2\n4 -> 3\n }"
gap> DotString(RegularBooleanMatMonoid(2), rec(number := false));
"//dot\ndigraph  DClasses {\nnode [shape=plaintext]\nedge [color=black,arrowhe\
ad=none]\n1 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"1\">\n<TR><TD BGCOLOR=\"gra\
y\">*</TD></TR>\n</TABLE>>];\n2 [shape=box style=invisible label=<\n<TABLE BOR\
DER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<\
TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">\
*</TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGC\
OLOR=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\"\
>*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n3 [shape=box style=invis\
ible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPAC\
ING=\"0\" PORT=\"3\">\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></\
TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</T\
ABLE>>];\n4 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"4\">\n<TR><TD BGCOLOR=\"gra\
y\">*</TD></TR>\n</TABLE>>];\n2 -> 1\n3 -> 2\n4 -> 3\n }"
gap> DotString(RegularBooleanMatMonoid(2), rec(normal := false));
"//dot\ndigraph  DClasses {\nnode [shape=plaintext]\nedge [color=black,arrowhe\
ad=none]\n1 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD \
COLSPAN=\"1\" BORDER = \"0\" > 1</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD></TR>\
\n</TABLE>>];\n2 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLB\
ORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\"0\"\
><TD COLSPAN=\"3\" BORDER = \"0\" > 2</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD>\
<TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<T\
R><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*\
</TD></TR>\n</TABLE>>];\n3 [shape=box style=invisible label=<\n<TABLE BORDER=\
\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"3\">\n<TR B\
ORDER=\"0\"><TD COLSPAN=\"2\" BORDER = \"0\" > 3</TD></TR><TR><TD BGCOLOR=\"gr\
ay\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><T\
D BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n4 [shape=box style=invisible labe\
l=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\"\
 PORT=\"4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\" > 4</TD></TR><\
TR><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n2 -> 1\n3 -> 2\n4 -> 3\n }"
gap> S := RegularBooleanMatMonoid(3);;
gap> DotString(S, rec(highlight := [rec(HClasses := [HClass(S, One(S))]), 
> rec(HClasses := [First(HClasses(S), x -> not IsGroupHClass(x))]),
> rec(HClasses := HClasses(First(DClasses(S), x -> not IsRegularDClass(x))))
> ]));
"//dot\ndigraph  DClasses {\nnode [shape=plaintext]\nedge [color=black,arrowhe\
ad=none]\n1 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD \
COLSPAN=\"1\" BORDER = \"0\" > 1</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD></TR>\
\n</TABLE>>];\n2 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLB\
ORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\"0\"\
><TD COLSPAN=\"7\" BORDER = \"0\" > 2</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD>\
<TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</\
TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">\
*</TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGC\
OLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD \
BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gra\
y\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"\
gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</\
TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGC\
OLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><\
TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"\
gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"#FF0000\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n3 [shape=box style=\
invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CEL\
LSPACING=\"0\" PORT=\"3\">\n<TR BORDER=\"0\"><TD COLSPAN=\"12\" BORDER = \"0\"\
 > 3</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BG\
COLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD>\
<TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></\
TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLO\
R=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGC\
OLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"\
gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD B\
GCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></\
TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"g\
ray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BG\
COLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"g\
ray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<T\
R><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"g\
ray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*\
</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\
\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gr\
ay\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><T\
D BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD\
><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*<\
/TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\"\
>*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BG\
COLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD>\
<TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</\
TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"\
></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGC\
OLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD B\
GCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><T\
D BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n4 [sha\
pe=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDI\
NG=\"10\" CELLSPACING=\"0\" PORT=\"4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"6\" BO\
RDER = \"0\" > 4</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><\
TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n</TABLE>>];\n5 [shape=box st\
yle=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\"\
 CELLSPACING=\"0\" PORT=\"5\">\n<TR BORDER=\"0\"><TD COLSPAN=\"9\" BORDER = \"\
0\" > 5</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD\
 BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD>\
<TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></\
TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLO\
R=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGC\
OLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"gra\
y\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<\
TR><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"w\
hite\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><\
TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*\
</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD B\
GCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><T\
D BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD></TR>\n<TR><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOL\
OR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\
\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"w\
hite\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\
\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\
\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n</TABLE>>];\n6 [shape=box style=invi\
sible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPA\
CING=\"0\" PORT=\"6\">\n<TR BORDER=\"0\"><TD COLSPAN=\"6\" BORDER = \"0\" > 6<\
/TD></TR><TR><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">\
*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">\
*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">\
*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">\
*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">\
*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">\
*</font></TD></TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color\
=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color\
=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color\
=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color\
=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color\
=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\"><font color\
=\"white\">*</font></TD></TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\">\
<font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\">\
<font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\">\
<font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\">\
<font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\">\
<font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\"#FF0000\">\
<font color=\"white\">*</font></TD></TR>\n<TR><TD CELLPADDING=\"10\" BGCOLOR=\
\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\
\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\
\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\
\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\
\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\" BGCOLOR=\
\"#FF0000\"><font color=\"white\">*</font></TD></TR>\n<TR><TD CELLPADDING=\"10\
\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\
\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\
\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\
\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\
\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLPADDING=\"10\
\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD></TR>\n<TR><TD CELLP\
ADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLP\
ADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLP\
ADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLP\
ADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLP\
ADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD><TD CELLP\
ADDING=\"10\" BGCOLOR=\"#FF0000\"><font color=\"white\">*</font></TD></TR>\n</\
TABLE>>];\n7 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDE\
R=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"7\">\n<TR BORDER=\"0\"><TD\
 COLSPAN=\"3\" BORDER = \"0\" > 7</TD></TR><TR><TD BGCOLOR=\"gray\">*</TD><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"whi\
te\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD\
 BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD>\
</TR>\n</TABLE>>];\n8 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" \
CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"8\">\n<TR BORDER=\
\"0\"><TD COLSPAN=\"3\" BORDER = \"0\" > 8</TD></TR><TR><TD BGCOLOR=\"gray\">*\
</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\
\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\
\">*</TD></TR>\n</TABLE>>];\n9 [shape=box style=invisible label=<\n<TABLE BORD\
ER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"9\">\n<T\
R BORDER=\"0\"><TD COLSPAN=\"6\" BORDER = \"0\" > 9</TD></TR><TR><TD BGCOLOR=\
\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\
\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"w\
hite\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></T\
D><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR\
=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCO\
LOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"\
></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"whit\
e\"></TD><TD BGCOLOR=\"gray\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD \
BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><\
TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\">*</T\
D></TR>\n</TABLE>>];\n10 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\
\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"10\">\n<TR BOR\
DER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\" > 10</TD></TR><TR><TD BGCOLOR=\"#88\
0000\">*</TD></TR>\n</TABLE>>];\n2 -> 1\n3 -> 2\n4 -> 3\n5 -> 3\n6 -> 4\n6 -> \
5\n7 -> 4\n7 -> 5\n8 -> 4\n8 -> 5\n9 -> 6\n9 -> 7\n9 -> 8\n10 -> 9\n }"

# DotString with option idempotentsemilattice
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> DotString(S, rec(idempotentsemilattice := true));
"//dot\ndigraph  DClasses {\nnode [shape=plaintext]\nedge [color=black,arrowhe\
ad=none]\n1 [shape=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER\
=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD \
COLSPAN=\"1\" BORDER = \"0\" > 1</TD></TR><TR><TD BGCOLOR=\"gray\" PORT=\"e8\"\
>*</TD></TR>\n</TABLE>>];\n2 [shape=box style=invisible label=<\n<TABLE BORDER\
=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR \
BORDER=\"0\"><TD COLSPAN=\"3\" BORDER = \"0\" > 2</TD></TR><TR><TD BGCOLOR=\"g\
ray\" PORT=\"e6\">*</TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD>\
</TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\" PORT=\"e4\">*</TD><\
TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"\
white\"></TD><TD BGCOLOR=\"gray\" PORT=\"e7\">*</TD></TR>\n</TABLE>>];\n3 [sha\
pe=box style=invisible label=<\n<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDI\
NG=\"10\" CELLSPACING=\"0\" PORT=\"3\">\n<TR BORDER=\"0\"><TD COLSPAN=\"3\" BO\
RDER = \"0\" > 3</TD></TR><TR><TD BGCOLOR=\"gray\" PORT=\"e3\">*</TD><TD BGCOL\
OR=\"white\"></TD><TD BGCOLOR=\"white\"></TD></TR>\n<TR><TD BGCOLOR=\"white\">\
</TD><TD BGCOLOR=\"gray\" PORT=\"e5\">*</TD><TD BGCOLOR=\"white\"></TD></TR>\n\
<TR><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"white\"></TD><TD BGCOLOR=\"gray\"\
 PORT=\"e2\">*</TD></TR>\n</TABLE>>];\n4 [shape=box style=invisible label=<\n<\
TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\
\"4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\" > 4</TD></TR><TR><TD\
 BGCOLOR=\"gray\" PORT=\"e1\">*</TD></TR>\n</TABLE>>];\n1 -> 2\n2 -> 3\n3 -> 4\
\nedge [color=blue,arrowhead=none,style=dashed]\n3:e2 -> 4:e1\n3:e3 -> 4:e1\n2\
:e4 -> 3:e2\n2:e4 -> 3:e3\n3:e5 -> 4:e1\n2:e6 -> 3:e3\n2:e6 -> 3:e5\n2:e7 -> 3\
:e2\n2:e7 -> 3:e5\n1:e8 -> 2:e4\n1:e8 -> 2:e6\n1:e8 -> 2:e7\n }"

# DotSemilatticeOfIdempotents
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> DotSemilatticeOfIdempotents(S);
"//dot\ngraph graphname {\n  node [shape=point]\nranksep=2;\nsubgraph cluster_\
1{\n8 \n}\nsubgraph cluster_2{\n6 4 7 \n}\nsubgraph cluster_3{\n3 5 2 \n}\nsub\
graph cluster_4{\n1 \n}\n2 -- 1\n3 -- 1\n4 -- 2\n4 -- 3\n5 -- 1\n6 -- 3\n6 -- \
5\n7 -- 2\n7 -- 5\n8 -- 4\n8 -- 6\n8 -- 7\n }"
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := false));;
gap> DotSemilatticeOfIdempotents(S);
"//dot\ngraph graphname {\n  node [shape=point]\nranksep=2;\nsubgraph cluster_\
1{\n1 \n}\nsubgraph cluster_2{\n2 3 5 \n}\nsubgraph cluster_3{\n4 6 7 \n}\nsub\
graph cluster_4{\n8 \n}\n2 -- 1\n3 -- 1\n4 -- 2\n4 -- 3\n5 -- 1\n6 -- 3\n6 -- \
5\n7 -- 2\n7 -- 5\n8 -- 4\n8 -- 6\n8 -- 7\n }"

# TexString for a transformation
gap> TexString(Transformation([1, 1, 1]));
"\\begin{pmatrix}\n  1 & 2 & 3 \\\\\n  1 & 1 & 1\n\\end{pmatrix}"
gap> TexString(Transformation([1, 1, 1]), 5);
"\\begin{pmatrix}\n  1 & 2 & 3 & 4 & 5 \\\\\n  1 & 1 & 1 & 4 & 5\n\\end{pmatri\
x}"
gap> TexString(Idempotents(FullTransformationMonoid(2)));
"\\begin{pmatrix}\n  1 & 2 \\\\\n  1 & 2\n\\end{pmatrix}\n\\begin{pmatrix}\n  \
1 & 2 \\\\\n  1 & 1\n\\end{pmatrix}\n\\begin{pmatrix}\n  1 & 2 \\\\\n  2 & 2\n\
\\end{pmatrix}"
gap> TexString(Transformation([1, 1, 1]), 2);
Error, Semigroups: TexString: usage,
the second argument (the degree) should be at least the degree of the first ar\
gument (a transformation),

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/display.tst");
