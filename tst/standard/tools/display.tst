############################################################################
##
#W  standard/tools/display.tst
#Y  Copyright (C) 2016-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## We don't use local variables in this test file because it doesn't play nice
## with something in this test file.

#
gap> START_TEST("Semigroups package: standard/tools/display.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# Test TikzString for a pbr
gap> TikzString(PBR([[-2, 2], [1, 2], [-1, 2]], [[-3], [1, 2], [- 3]]));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows\
}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\draw[semithick, -{>[wi\
dth = 1.5mm, length = 2.5mm]}]}\n\n\\begin{document}\n\\begin{tikzpicture}[\n \
 vertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n  ghost/.style=\
{circle, draw = none, inner sep = 0.14cm},\n  botloop/.style={min distance = 8\
mm, out = -70, in = -110},\n  toploop/.style={min distance = 8mm, out = 70, in\
 = 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,3} {\n    \\nod\
e [vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n\
  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 0) {};\
\n    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\
\n  \\arc (1) to (-2);\n  \\arc (1) .. controls (1.0666666666666667, 2.3333333\
333333335) and (0.93333333333333324, 2.3333333333333335) .. (2);\n\n  % arcs f\
rom vertex -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.083333333333333\
5) and (1.2, 1.0833333333333335) .. (-3);\n\n  % arcs from vertex 2\n  \\arc (\
2) .. controls (0.93333333333333324, 2.3333333333333335) and (1.06666666666666\
67, 2.3333333333333335) .. (1);\n  \\arc (2) edge [toploop] (2);\n\n  % arcs f\
rom vertex -2\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n\n  % arcs from ver\
tex 3\n  \\arc (3) to (-1);\n  \\arc (3) .. controls (1.6000000000000001, 2.33\
33333333333335) and (1.7333333333333334, 2.3333333333333335) .. (2);\n\n  % ar\
cs from vertex -3\n  \\arc (-3) edge [botloop] (-3);\n\n\\end{tikzpicture}\n\\\
end{document}"
gap> TikzString(PBR([[-2, 2], [1, 2], [-1, 2]], [[-3], [1, 2], [- 3]]),
> rec(labels := true));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows\
}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\draw[semithick, -{>[wi\
dth = 1.5mm, length = 2.5mm]}]}\n\n\\begin{document}\n\\begin{tikzpicture}[\n \
 vertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n  ghost/.style=\
{circle, draw = none, inner sep = 0.14cm},\n  botloop/.style={min distance = 8\
mm, out = -70, in = -110},\n  toploop/.style={min distance = 8mm, out = 70, in\
 = 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,3} {\n    \\nod\
e [vertex, label={[yshift=9mm]\\i}] at (\\i/1.5, 3) {};\n    \\node [ghost] (\
\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [v\
ertex, label={[yshift=-15mm,xshift=-0.5mm]-\\i}] at (\\i/1.5, 0) {};\n    \\no\
de [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc \
(1) to (-2);\n  \\arc (1) .. controls (1.0666666666666667, 2.3333333333333335)\
 and (0.93333333333333324, 2.3333333333333335) .. (2);\n\n  % arcs from vertex\
 -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.0833333333333335) and (1.\
2, 1.0833333333333335) .. (-3);\n\n  % arcs from vertex 2\n  \\arc (2) .. cont\
rols (0.93333333333333324, 2.3333333333333335) and (1.0666666666666667, 2.3333\
333333333335) .. (1);\n  \\arc (2) edge [toploop] (2);\n\n  % arcs from vertex\
 -2\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n\n  % arcs from vertex 3\n  \
\\arc (3) to (-1);\n  \\arc (3) .. controls (1.6000000000000001, 2.33333333333\
33335) and (1.7333333333333334, 2.3333333333333335) .. (2);\n\n  % arcs from v\
ertex -3\n  \\arc (-3) edge [botloop] (-3);\n\n\\end{tikzpicture}\n\\end{docum\
ent}"

# TikzString for a pbr collection
gap> x := PBR([[1], [], []], [[2], [-2, 1, 2], [1, 2, 3]]);;
gap> y := PBR([[-3, -2, -1, 2], [-3, -2, -1, 1, 2, 3], [-3, -2, -1, 1, 2, 3]],
> [[-3, -2, -1, 2], [-3, -2, -1, 1, 3], [-3, -2, -1, 1, 2, 3]]);;
gap> TikzString(Semigroup(x, y));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows\
}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\draw[semithick, -{>[wi\
dth = 1.5mm, length = 2.5mm]}]}\n\n\\begin{document}\n\\begin{center}\n\\begin\
{tikzpicture}[\n  vertex/.style={circle, draw, fill=black, inner sep =0.04cm},\
\n  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n  botloop/.style=\
{min distance = 8mm, out = -70, in = -110},\n  toploop/.style={min distance = \
8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,..\
.,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) at (\
\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at\
 (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % a\
rcs from vertex 1\n  \\arc (1) edge [toploop] (1);\n\n  % arcs from vertex -1\
\n  \\arc (-1) to (2);\n\n  % arcs from vertex 2\n\n  % arcs from vertex -2\n \
 \\arc (-2) edge [botloop] (-2);\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n\
\n  % arcs from vertex 3\n\n  % arcs from vertex -3\n  \\arc (-3) to (1);\n  \
\\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigskip\\b\
igskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=black, in\
ner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n\
  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.style\
={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \\for\
each \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\node [\
ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \
\\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0)\
 {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) to (-3);\n  \\arc (1) to (-2)\
;\n  \\arc (1) to (-1);\n  \\arc (1) .. controls (1.0666666666666667, 2.333333\
3333333335) and (0.93333333333333324, 2.3333333333333335) .. (2);\n\n  % arcs \
from vertex -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.08333333333333\
35) and (1.2, 1.0833333333333335) .. (-3);\n  \\arc (-1) .. controls (1.066666\
6666666667, 0.66666666666666674) and (0.93333333333333324, 0.66666666666666674\
) .. (-2);\n  \\arc (-1) edge [botloop] (-1);\n  \\arc (-1) to (2);\n\n  % arc\
s from vertex 2\n  \\arc (2) to (-3);\n  \\arc (2) to (-2);\n  \\arc (2) to (-\
1);\n  \\arc (2) .. controls (0.93333333333333324, 2.3333333333333335) and (1.\
0666666666666667, 2.3333333333333335) .. (1);\n  \\arc (2) edge [toploop] (2);\
\n  \\arc (2) .. controls (1.7333333333333334, 2.3333333333333335) and (1.6000\
000000000001, 2.3333333333333335) .. (3);\n\n  % arcs from vertex -2\n  \\arc \
(-2) .. controls (1.7333333333333334, 0.66666666666666674) and (1.600000000000\
0001, 0.66666666666666674) .. (-3);\n  \\arc (-2) edge [botloop] (-2);\n  \\ar\
c (-2) .. controls (0.93333333333333324, 0.66666666666666674) and (1.066666666\
6666667, 0.66666666666666674) .. (-1);\n  \\arc (-2) to (1);\n  \\arc (-2) to \
(3);\n\n  % arcs from vertex 3\n  \\arc (3) to (-3);\n  \\arc (3) to (-2);\n  \
\\arc (3) to (-1);\n  \\arc (3) .. controls (1.2, 1.9166666666666665) and (1.4\
666666666666668, 1.9166666666666665) .. (1);\n  \\arc (3) .. controls (1.60000\
00000000001, 2.3333333333333335) and (1.7333333333333334, 2.3333333333333335) \
.. (2);\n  \\arc (3) edge [toploop] (3);\n\n  % arcs from vertex -3\n  \\arc (\
-3) edge [botloop] (-3);\n  \\arc (-3) .. controls (1.6000000000000001, 0.6666\
6666666666674) and (1.7333333333333334, 0.66666666666666674) .. (-2);\n  \\arc\
 (-3) .. controls (1.2, 1.0833333333333335) and (1.4666666666666668, 1.0833333\
333333335) .. (-1);\n  \\arc (-3) to (1);\n  \\arc (-3) to (2);\n  \\arc (-3) \
to (3);\n\n\\end{tikzpicture}\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}[\n\
  vertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n  ghost/.style\
={circle, draw = none, inner sep = 0.14cm},\n  botloop/.style={min distance = \
8mm, out = -70, in = -110},\n  toploop/.style={min distance = 8mm, out = 70, i\
n = 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,3} {\n    \\no\
de [vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\
\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 0) {\
};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex\
 1\n  \\arc (1) edge [toploop] (1);\n\n  % arcs from vertex -1\n  \\arc (-1) t\
o (1);\n  \\arc (-1) to (2);\n\n  % arcs from vertex 2\n\n  % arcs from vertex\
 -2\n  \\arc (-2) edge [botloop] (-2);\n  \\arc (-2) to (1);\n  \\arc (-2) to \
(2);\n\n  % arcs from vertex 3\n\n  % arcs from vertex -3\n  \\arc (-3) to (1)\
;\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigsk\
ip\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=blac\
k, inner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14c\
m},\n  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.\
style={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \
\\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\n\
ode [ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n\
    \\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5\
, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) edge [toploop] (1);\n\n  \
% arcs from vertex -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.0833333\
333333335) and (1.2, 1.0833333333333335) .. (-3);\n  \\arc (-1) .. controls (1\
.0666666666666667, 0.66666666666666674) and (0.93333333333333324, 0.6666666666\
6666674) .. (-2);\n  \\arc (-1) edge [botloop] (-1);\n  \\arc (-1) to (1);\n  \
\\arc (-1) to (2);\n  \\arc (-1) to (3);\n\n  % arcs from vertex 2\n\n  % arcs\
 from vertex -2\n  \\arc (-2) .. controls (1.7333333333333334, 0.6666666666666\
6674) and (1.6000000000000001, 0.66666666666666674) .. (-3);\n  \\arc (-2) edg\
e [botloop] (-2);\n  \\arc (-2) .. controls (0.93333333333333324, 0.6666666666\
6666674) and (1.0666666666666667, 0.66666666666666674) .. (-1);\n  \\arc (-2) \
to (1);\n  \\arc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs from vertex 3\
\n\n  % arcs from vertex -3\n  \\arc (-3) edge [botloop] (-3);\n  \\arc (-3) .\
. controls (1.6000000000000001, 0.66666666666666674) and (1.7333333333333334, \
0.66666666666666674) .. (-2);\n  \\arc (-3) .. controls (1.2, 1.08333333333333\
35) and (1.4666666666666668, 1.0833333333333335) .. (-1);\n  \\arc (-3) to (1)\
;\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigsk\
ip\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=blac\
k, inner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14c\
m},\n  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.\
style={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \
\\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\n\
ode [ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n\
    \\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5\
, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) .. controls (1.0666666666\
666667, 2.3333333333333335) and (0.93333333333333324, 2.3333333333333335) .. (\
2);\n\n  % arcs from vertex -1\n  \\arc (-1) to (1);\n  \\arc (-1) to (2);\n  \
\\arc (-1) to (3);\n\n  % arcs from vertex 2\n  \\arc (2) .. controls (0.93333\
333333333324, 2.3333333333333335) and (1.0666666666666667, 2.3333333333333335)\
 .. (1);\n  \\arc (2) edge [toploop] (2);\n  \\arc (2) .. controls (1.73333333\
33333334, 2.3333333333333335) and (1.6000000000000001, 2.3333333333333335) .. \
(3);\n\n  % arcs from vertex -2\n  \\arc (-2) edge [botloop] (-2);\n  \\arc (-\
2) to (1);\n  \\arc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs from vertex\
 3\n  \\arc (3) .. controls (1.2, 1.9166666666666665) and (1.4666666666666668,\
 1.9166666666666665) .. (1);\n  \\arc (3) .. controls (1.6000000000000001, 2.3\
333333333333335) and (1.7333333333333334, 2.3333333333333335) .. (2);\n  \\arc\
 (3) edge [toploop] (3);\n\n  % arcs from vertex -3\n  \\arc (-3) to (1);\n  \
\\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigskip\\b\
igskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=black, in\
ner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n\
  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.style\
={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \\for\
each \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\node [\
ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \
\\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0)\
 {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) to (-3);\n  \\arc (1) to (-2)\
;\n  \\arc (1) to (-1);\n  \\arc (1) edge [toploop] (1);\n  \\arc (1) .. contr\
ols (1.0666666666666667, 2.3333333333333335) and (0.93333333333333324, 2.33333\
33333333335) .. (2);\n  \\arc (1) .. controls (1.4666666666666668, 1.916666666\
6666665) and (1.2, 1.9166666666666665) .. (3);\n\n  % arcs from vertex -1\n  \
\\arc (-1) .. controls (1.4666666666666668, 1.0833333333333335) and (1.2, 1.08\
33333333333335) .. (-3);\n  \\arc (-1) .. controls (1.0666666666666667, 0.6666\
6666666666674) and (0.93333333333333324, 0.66666666666666674) .. (-2);\n  \\ar\
c (-1) edge [botloop] (-1);\n  \\arc (-1) to (1);\n  \\arc (-1) to (2);\n  \\a\
rc (-1) to (3);\n\n  % arcs from vertex 2\n  \\arc (2) to (-3);\n  \\arc (2) t\
o (-2);\n  \\arc (2) to (-1);\n  \\arc (2) .. controls (0.93333333333333324, 2\
.3333333333333335) and (1.0666666666666667, 2.3333333333333335) .. (1);\n  \\a\
rc (2) edge [toploop] (2);\n  \\arc (2) .. controls (1.7333333333333334, 2.333\
3333333333335) and (1.6000000000000001, 2.3333333333333335) .. (3);\n\n  % arc\
s from vertex -2\n  \\arc (-2) .. controls (1.7333333333333334, 0.666666666666\
66674) and (1.6000000000000001, 0.66666666666666674) .. (-3);\n  \\arc (-2) ed\
ge [botloop] (-2);\n  \\arc (-2) .. controls (0.93333333333333324, 0.666666666\
66666674) and (1.0666666666666667, 0.66666666666666674) .. (-1);\n  \\arc (-2)\
 to (1);\n  \\arc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs from vertex 3\
\n  \\arc (3) to (-3);\n  \\arc (3) to (-2);\n  \\arc (3) to (-1);\n  \\arc (3\
) .. controls (1.2, 1.9166666666666665) and (1.4666666666666668, 1.91666666666\
66665) .. (1);\n  \\arc (3) .. controls (1.6000000000000001, 2.333333333333333\
5) and (1.7333333333333334, 2.3333333333333335) .. (2);\n  \\arc (3) edge [top\
loop] (3);\n\n  % arcs from vertex -3\n  \\arc (-3) edge [botloop] (-3);\n  \\\
arc (-3) .. controls (1.6000000000000001, 0.66666666666666674) and (1.73333333\
33333334, 0.66666666666666674) .. (-2);\n  \\arc (-3) .. controls (1.2, 1.0833\
333333333335) and (1.4666666666666668, 1.0833333333333335) .. (-1);\n  \\arc (\
-3) to (1);\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\
\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw\
, fill=black, inner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner \
sep = 0.14cm},\n  botloop/.style={min distance = 8mm, out = -70, in = -110},\n\
  toploop/.style={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and\
 labels\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {\
};\n    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1\
,...,3} {\n    \\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) \
at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) edge [toploop]\
 (1);\n\n  % arcs from vertex -1\n  \\arc (-1) to (1);\n  \\arc (-1) to (2);\n\
  \\arc (-1) to (3);\n\n  % arcs from vertex 2\n\n  % arcs from vertex -2\n  \
\\arc (-2) edge [botloop] (-2);\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n \
 \\arc (-2) to (3);\n\n  % arcs from vertex 3\n\n  % arcs from vertex -3\n  \\\
arc (-3) to (1);\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpict\
ure}\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, \
draw, fill=black, inner sep =0.04cm},\n  ghost/.style={circle, draw = none, in\
ner sep = 0.14cm},\n  botloop/.style={min distance = 8mm, out = -70, in = -110\
},\n  toploop/.style={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices\
 and labels\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, \
3) {};\n    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i i\
n {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\
\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) .. contro\
ls (1.0666666666666667, 2.3333333333333335) and (0.93333333333333324, 2.333333\
3333333335) .. (2);\n\n  % arcs from vertex -1\n  \\arc (-1) .. controls (1.46\
66666666666668, 1.0833333333333335) and (1.2, 1.0833333333333335) .. (-3);\n  \
\\arc (-1) .. controls (1.0666666666666667, 0.66666666666666674) and (0.933333\
33333333324, 0.66666666666666674) .. (-2);\n  \\arc (-1) edge [botloop] (-1);\
\n  \\arc (-1) to (1);\n  \\arc (-1) to (2);\n  \\arc (-1) to (3);\n\n  % arcs\
 from vertex 2\n  \\arc (2) .. controls (0.93333333333333324, 2.33333333333333\
35) and (1.0666666666666667, 2.3333333333333335) .. (1);\n  \\arc (2) edge [to\
ploop] (2);\n  \\arc (2) .. controls (1.7333333333333334, 2.3333333333333335) \
and (1.6000000000000001, 2.3333333333333335) .. (3);\n\n  % arcs from vertex -\
2\n  \\arc (-2) .. controls (1.7333333333333334, 0.66666666666666674) and (1.6\
000000000000001, 0.66666666666666674) .. (-3);\n  \\arc (-2) edge [botloop] (-\
2);\n  \\arc (-2) .. controls (0.93333333333333324, 0.66666666666666674) and (\
1.0666666666666667, 0.66666666666666674) .. (-1);\n  \\arc (-2) to (1);\n  \\a\
rc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs from vertex 3\n  \\arc (3) .\
. controls (1.2, 1.9166666666666665) and (1.4666666666666668, 1.91666666666666\
65) .. (1);\n  \\arc (3) .. controls (1.6000000000000001, 2.3333333333333335) \
and (1.7333333333333334, 2.3333333333333335) .. (2);\n  \\arc (3) edge [toploo\
p] (3);\n\n  % arcs from vertex -3\n  \\arc (-3) edge [botloop] (-3);\n  \\arc\
 (-3) .. controls (1.6000000000000001, 0.66666666666666674) and (1.73333333333\
33334, 0.66666666666666674) .. (-2);\n  \\arc (-3) .. controls (1.2, 1.0833333\
333333335) and (1.4666666666666668, 1.0833333333333335) .. (-1);\n  \\arc (-3)\
 to (1);\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\
\\bigskip\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fi\
ll=black, inner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep \
= 0.14cm},\n  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  to\
ploop/.style={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and lab\
els\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n\
    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...\
,3} {\n    \\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\
\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) edge [toploop] (1)\
;\n  \\arc (1) .. controls (1.0666666666666667, 2.3333333333333335) and (0.933\
33333333333324, 2.3333333333333335) .. (2);\n  \\arc (1) .. controls (1.466666\
6666666668, 1.9166666666666665) and (1.2, 1.9166666666666665) .. (3);\n\n  % a\
rcs from vertex -1\n  \\arc (-1) to (1);\n  \\arc (-1) to (2);\n  \\arc (-1) t\
o (3);\n\n  % arcs from vertex 2\n  \\arc (2) .. controls (0.93333333333333324\
, 2.3333333333333335) and (1.0666666666666667, 2.3333333333333335) .. (1);\n  \
\\arc (2) edge [toploop] (2);\n  \\arc (2) .. controls (1.7333333333333334, 2.\
3333333333333335) and (1.6000000000000001, 2.3333333333333335) .. (3);\n\n  % \
arcs from vertex -2\n  \\arc (-2) edge [botloop] (-2);\n  \\arc (-2) to (1);\n\
  \\arc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs from vertex 3\n  \\arc \
(3) .. controls (1.2, 1.9166666666666665) and (1.4666666666666668, 1.916666666\
6666665) .. (1);\n  \\arc (3) .. controls (1.6000000000000001, 2.3333333333333\
335) and (1.7333333333333334, 2.3333333333333335) .. (2);\n  \\arc (3) edge [t\
oploop] (3);\n\n  % arcs from vertex -3\n  \\arc (-3) to (1);\n  \\arc (-3) to\
 (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigskip\\bigskip\n\n\\b\
egin{tikzpicture}[\n  vertex/.style={circle, draw, fill=black, inner sep =0.04\
cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n  botloop/.st\
yle={min distance = 8mm, out = -70, in = -110},\n  toploop/.style={min distanc\
e = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \\foreach \\i in {\
1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) \
at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex\
] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n \
 % arcs from vertex 1\n  \\arc (1) edge [toploop] (1);\n  \\arc (1) .. control\
s (1.0666666666666667, 2.3333333333333335) and (0.93333333333333324, 2.3333333\
333333335) .. (2);\n  \\arc (1) .. controls (1.4666666666666668, 1.91666666666\
66665) and (1.2, 1.9166666666666665) .. (3);\n\n  % arcs from vertex -1\n  \\a\
rc (-1) .. controls (1.4666666666666668, 1.0833333333333335) and (1.2, 1.08333\
33333333335) .. (-3);\n  \\arc (-1) .. controls (1.0666666666666667, 0.6666666\
6666666674) and (0.93333333333333324, 0.66666666666666674) .. (-2);\n  \\arc (\
-1) edge [botloop] (-1);\n  \\arc (-1) to (1);\n  \\arc (-1) to (2);\n  \\arc \
(-1) to (3);\n\n  % arcs from vertex 2\n  \\arc (2) .. controls (0.93333333333\
333324, 2.3333333333333335) and (1.0666666666666667, 2.3333333333333335) .. (1\
);\n  \\arc (2) edge [toploop] (2);\n  \\arc (2) .. controls (1.73333333333333\
34, 2.3333333333333335) and (1.6000000000000001, 2.3333333333333335) .. (3);\n\
\n  % arcs from vertex -2\n  \\arc (-2) .. controls (1.7333333333333334, 0.666\
66666666666674) and (1.6000000000000001, 0.66666666666666674) .. (-3);\n  \\ar\
c (-2) edge [botloop] (-2);\n  \\arc (-2) .. controls (0.93333333333333324, 0.\
66666666666666674) and (1.0666666666666667, 0.66666666666666674) .. (-1);\n  \
\\arc (-2) to (1);\n  \\arc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs fro\
m vertex 3\n  \\arc (3) .. controls (1.2, 1.9166666666666665) and (1.466666666\
6666668, 1.9166666666666665) .. (1);\n  \\arc (3) .. controls (1.6000000000000\
001, 2.3333333333333335) and (1.7333333333333334, 2.3333333333333335) .. (2);\
\n  \\arc (3) edge [toploop] (3);\n\n  % arcs from vertex -3\n  \\arc (-3) edg\
e [botloop] (-3);\n  \\arc (-3) .. controls (1.6000000000000001, 0.66666666666\
666674) and (1.7333333333333334, 0.66666666666666674) .. (-2);\n  \\arc (-3) .\
. controls (1.2, 1.0833333333333335) and (1.4666666666666668, 1.08333333333333\
35) .. (-1);\n  \\arc (-3) to (1);\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\
\n\n\\end{tikzpicture}\n\n\\bigskip\\bigskip\n\n\\end{center}\\end{document}"

# TikzString for a bipartition collection
gap> TikzString(PartitionMonoid(2));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\n\\begin{document}\n\\\
begin{center}\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labe\
ls\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [abov\
e] {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\
\n\n  %lines\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and la\
bels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \
\\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\
\n  \\draw(2, 2)--(2, 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\beg\
in{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)ci\
rcle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(\
2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\dr\
aw(1, 2)--(2, 0);\n\n  %block number 2\n  %vertices and labels\n  \\fill(2, 2)\
circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.\
125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\draw(2, 2)--(1,\
 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  \
%block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw\
(0.94999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number \
2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) no\
de [above] {$2$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below]\
 {$-2$};\n\n  %lines\n  \\draw(2, 2)--(2, 0);\n\n  %block number 3\n  %vertice\
s and labels\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1\
$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpic\
ture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125)\
;\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(2, 2)circl\
e(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.125);\
\n  \\draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\dra\
w(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 1.875) .. controls (1\
, 1.25) and (2, 1.25) .. (2, 1.875);\n  \\draw(1, 0.125) .. controls (1, 0.75)\
 and (2, 0.75) .. (2, 0.125);\n  \\draw(1, 2)--(1, 0);\n\\end{tikzpicture}\n\n\
\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices\
 and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) n\
ode [above] {$1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below\
] {$-2$};\n\n  %lines\n  \\draw(1, 2)--(2, 0);\n\n  %block number 2\n  %vertic\
es and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {\
$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fill(1, 0)\
circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\\end{tikzp\
icture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n\
  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999\
996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n  %vertices an\
d labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\
\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %li\
nes\n  \\draw(2, 2)--(1, 0);\n\n  %block number 3\n  %vertices and labels\n  \
\\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\
\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %blo\
ck number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.9\
4999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n \
 %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [\
above] {$2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-\
1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n \
 %lines\n  \\draw(1, 0.125) .. controls (1, 0.75) and (2, 0.75) .. (2, 0.125);\
\n  \\draw(2, 2)--(2, 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\beg\
in{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)ci\
rcle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(\
2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(2, 0)cir\
cle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 1.\
875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n  \\draw(2, 2)--(2, 0\
);\n\n  %block number 2\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n\
  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\\
bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and\
 labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node \
[above] {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$\
-1$};\n\n  %lines\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices a\
nd labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$}\
;\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fill(2, 0)circ\
le(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpictu\
re}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %v\
ertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996,\
 2.2) node [above] {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node\
 [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] \
{$-2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1, 0.75) and (2, 0.75) ..\
 (2, 0.125);\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and la\
bels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\n\
  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\
\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\\
draw(0.94999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block num\
ber 2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2\
) node [above] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\
\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %li\
nes\n\n  %block number 4\n  %vertices and labels\n  \\fill(2, 0)circle(.125);\
\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\
\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices a\
nd labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) nod\
e [above] {$1$};\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above\
] {$2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\
\n\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.\
875);\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and labels\n \
 \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\
\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %blo\
ck number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.9\
4999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n \
 %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [\
above] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fi\
ll(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)c\
ircle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, \
0.125) .. controls (1, 0.75) and (2, 0.75) .. (2, 0.125);\n\\end{tikzpicture}\
\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vert\
ices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.\
2) node [above] {$1$};\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node \
[above] {$2$};\n\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1\
.25) .. (2, 1.875);\n\n  %block number 2\n  %vertices and labels\n  \\fill(1, \
0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\n  %bloc\
k number 3\n  %vertices and labels\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -\
0.2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigs\
kip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\\
fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$}\
;\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\n  %\
lines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n\
\n  %block number 2\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n  \\\
draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -\
0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1, 0.75\
) and (2, 0.75) .. (2, 0.125);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\
\\end{center}\\end{document}"

# Test TikzString for a bipartition
gap> TikzString(Bipartition([[1, 3], [2, -1], [-2, -3]]));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\n\\begin{document}\n\\\
begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2\
)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fi\
ll(3, 2)circle(.125);\n  \\draw(2.9500000000000002, 2.2) node [above] {$3$};\n\
\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1.1666666666666667) and (3, 1.1\
666666666666667) .. (3, 1.875);\n\n  %block number 2\n  %vertices and labels\n\
  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill\
(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\d\
raw(2, 2)--(1, 0);\n\n  %block number 3\n  %vertices and labels\n  \\fill(2, 0\
)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n  \\fill(3, 0)circle(.\
125);\n  \\draw(3, -0.2) node [below] {$-3$};\n\n  %lines\n  \\draw(2, 0.125) \
.. controls (2, 0.66666666666666663) and (3, 0.66666666666666663) .. (3, 0.125\
);\n\\end{tikzpicture}\n\n\\end{document}"
gap> TikzString(Bipartition([[1, 3], [2, -1], [-2, -3]]), 
> rec(colors := true, labels := true, beginDocument := true, 
>     endDocument := true));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\n\\begin{document}\n\\\
begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill[red]\
(1, 2)circle(.125);\n  \\draw[red](0.94999999999999996, 2.2) node [above] {$1$\
};\n  \\fill[red](3, 2)circle(.125);\n  \\draw[red](2.9500000000000002, 2.2) n\
ode [above] {$3$};\n\n  %lines\n  \\draw[red](1, 1.875) .. controls (1, 1.1666\
666666666667) and (3, 1.1666666666666667) .. (3, 1.875);\n\n  %block number 2\
\n  %vertices and labels\n  \\fill[green](2, 2)circle(.125);\n  \\draw[green](\
1.95, 2.2) node [above] {$2$};\n  \\fill[green](1, 0)circle(.125);\n  \\draw[g\
reen](1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\draw[green](2, 2)--(1, 0)\
;\n\n  %block number 3\n  %vertices and labels\n  \\fill[blue](2, 0)circle(.12\
5);\n  \\draw[blue](2, -0.2) node [below] {$-2$};\n  \\fill[blue](3, 0)circle(\
.125);\n  \\draw[blue](3, -0.2) node [below] {$-3$};\n\n  %lines\n  \\draw[blu\
e](2, 0.125) .. controls (2, 0.66666666666666663) and (3, 0.66666666666666663)\
 .. (3, 0.125);\n\\end{tikzpicture}\n\n\\end{document}"

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
Error, the 2nd argument (a pos. int.) is less than the degree of the 1st argum\
ent (a transformation)

# Tikz/DotLeft/RightCayleyDigraph
gap> TikzLeftCayleyDigraph(FullTransformationMonoid(2));
"\\begin{tikzpicture}[scale=1, auto, \n  vertex/.style={circle, draw, thick, f\
ill=white, minimum size=0.65cm},\n  edge/.style={arrows={-angle 90}, thick},\n\
  loop/.style={min distance=5mm,looseness=5,arrows={-angle 90},thick}]\n\n  % \
Vertices . . .\n  \\node [vertex] (a) at (0, 0) {};\n  \\node at (0, 0) {$a$};\
\n\n  \\node [vertex] (b) at (0, 0) {};\n  \\node at (0, 0) {$b$};\n\n  \\node\
 [vertex] (c) at (0, 0) {};\n  \\node at (0, 0) {$c$};\n\n  \\node [vertex] (c\
b) at (0, 0) {};\n  \\node at (0, 0) {$cb$};\n\n  % Edges . . .\n  \\path[->] \
(a) edge [loop]\n           node {$a$} (a);\n  \\path[->] (a) edge [edge] node\
 {$b$} (b);\n  \\path[->] (a) edge [edge] node {$c$} (c);\n  \\path[->] (b) ed\
ge [loop]\n           node {$a$} (b);\n  \\path[->] (b) edge [edge] node {$b$}\
 (a);\n  \\path[->] (b) edge [edge] node {$c$} (cb);\n  \\path[->] (c) edge [l\
oop]\n           node {$a$} (c);\n  \\path[->] (c) edge [loop]\n           nod\
e {$b$} (c);\n  \\path[->] (c) edge [loop]\n           node {$c$} (c);\n  \\pa\
th[->] (cb) edge [loop]\n           node {$a$} (cb);\n  \\path[->] (cb) edge [\
loop]\n           node {$b$} (cb);\n  \\path[->] (cb) edge [loop]\n           \
node {$c$} (cb);\n\\end{tikzpicture}"
gap> TikzRightCayleyDigraph(FullTransformationMonoid(2));
"\\begin{tikzpicture}[scale=1, auto, \n  vertex/.style={circle, draw, thick, f\
ill=white, minimum size=0.65cm},\n  edge/.style={arrows={-angle 90}, thick},\n\
  loop/.style={min distance=5mm,looseness=5,arrows={-angle 90},thick}]\n\n  % \
Vertices . . .\n  \\node [vertex] (a) at (0, 0) {};\n  \\node at (0, 0) {$a$};\
\n\n  \\node [vertex] (b) at (0, 0) {};\n  \\node at (0, 0) {$b$};\n\n  \\node\
 [vertex] (c) at (0, 0) {};\n  \\node at (0, 0) {$c$};\n\n  \\node [vertex] (c\
b) at (0, 0) {};\n  \\node at (0, 0) {$cb$};\n\n  % Edges . . .\n  \\path[->] \
(a) edge [loop]\n           node {$a$} (a);\n  \\path[->] (a) edge [edge] node\
 {$b$} (b);\n  \\path[->] (a) edge [edge] node {$c$} (c);\n  \\path[->] (b) ed\
ge [loop]\n           node {$a$} (b);\n  \\path[->] (b) edge [edge] node {$b$}\
 (a);\n  \\path[->] (b) edge [edge] node {$c$} (c);\n  \\path[->] (c) edge [lo\
op]\n           node {$a$} (c);\n  \\path[->] (c) edge [edge] node {$b$} (cb);\
\n  \\path[->] (c) edge [loop]\n           node {$c$} (c);\n  \\path[->] (cb) \
edge [loop]\n           node {$a$} (cb);\n  \\path[->] (cb) edge [edge] node {\
$b$} (c);\n  \\path[->] (cb) edge [edge] node {$c$} (c);\n\\end{tikzpicture}"
gap> DotRightCayleyDigraph(FullTransformationMonoid(2));
"//dot\ndigraph hgn{\nnode [shape=circle]\n1 [label=\"a\"]\n2 [label=\"b\"]\n3\
 [label=\"c\"]\n4 [label=\"cb\"]\n1 -> 1\n1 -> 2\n1 -> 3\n2 -> 2\n2 -> 1\n2 ->\
 3\n3 -> 3\n3 -> 4\n3 -> 3\n4 -> 4\n4 -> 3\n4 -> 3\n}\n"
gap> DotLeftCayleyDigraph(FullTransformationMonoid(2));
"//dot\ndigraph hgn{\nnode [shape=circle]\n1 [label=\"a\"]\n2 [label=\"b\"]\n3\
 [label=\"c\"]\n4 [label=\"cb\"]\n1 -> 1\n1 -> 2\n1 -> 3\n2 -> 2\n2 -> 1\n2 ->\
 4\n3 -> 3\n3 -> 3\n3 -> 3\n4 -> 4\n4 -> 4\n4 -> 4\n}\n"
gap> S := LeftZeroSemigroup(27);;
gap> DotLeftCayleyDigraph(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `DotString' on 1 arguments
gap> TikzLeftCayleyDigraph(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `TikzString' on 1 arguments
gap> DotRightCayleyDigraph(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `DotString' on 1 arguments
gap> TikzRightCayleyDigraph(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `TikzString' on 1 arguments

# Unbind local variables, auto-generated by etc/tst-unbind-local-vars.py
gap> Unbind(S);
gap> Unbind(x);
gap> Unbind(y);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/tools/display.tst");
