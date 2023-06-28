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
dth = 1.5mm, length = 2.5mm]}]}\n\n\\begin{document}\\begin{tikzpicture}[\n  v\
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
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows\
}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\draw[semithick, -{>[wi\
dth = 1.5mm, length = 2.5mm]}]}\n\n\\begin{document}\\begin{tikzpicture}[\n  v\
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

# TikzString for a pbr collection
gap> x := PBR([[1], [], []], [[2], [-2, 1, 2], [1, 2, 3]]);;
gap> y := PBR([[-3, -2, -1, 2], [-3, -2, -1, 1, 2, 3], [-3, -2, -1, 1, 2, 3]],
> [[-3, -2, -1, 2], [-3, -2, -1, 1, 3], [-3, -2, -1, 1, 2, 3]]);;
gap> TikzString(Semigroup(x, y));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows\
}\n\\usetikzlibrary{arrows.meta}\n\\newcommand{\\arc}{\\draw[semithick, -{>[wi\
dth = 1.5mm, length = 2.5mm]}]}\n\n\\begin{document}\\begin{center}\n\\begin{t\
ikzpicture}[\n  vertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n\
  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n  botloop/.style={m\
in distance = 8mm, out = -70, in = -110},\n  toploop/.style={min distance = 8m\
m, out = 70, in = 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,\
3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) at (\\\
i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\
\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arc\
s from vertex 1\n  \\arc (1) edge [toploop] (1);\n\n  % arcs from vertex -1\n \
 \\arc (-1) to (2);\n\n  % arcs from vertex 2\n\n  % arcs from vertex -2\n  \\\
arc (-2) edge [botloop] (-2);\n  \\arc (-2) to (1);\n  \\arc (-2) to (2);\n\n \
 % arcs from vertex 3\n\n  % arcs from vertex -3\n  \\arc (-3) to (1);\n  \\ar\
c (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigskip\\bigsk\
ip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=black, inner \
sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14cm},\n  bo\
tloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.style={mi\
n distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \\foreach\
 \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\node [ghos\
t] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n    \\nod\
e [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\
\n  }\n\n  % arcs from vertex 1\n  \\arc (1) to (-3);\n  \\arc (1) to (-2);\n \
 \\arc (1) to (-1);\n  \\arc (1) .. controls (1.0666666666666667, 2.3333333333\
333335) and (0.93333333333333324, 2.3333333333333335) .. (2);\n\n  % arcs from\
 vertex -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.0833333333333335) \
and (1.2, 1.0833333333333335) .. (-3);\n  \\arc (-1) .. controls (1.0666666666\
666667, 0.66666666666666674) and (0.93333333333333324, 0.66666666666666674) ..\
 (-2);\n  \\arc (-1) edge [botloop] (-1);\n  \\arc (-1) to (2);\n\n  % arcs fr\
om vertex 2\n  \\arc (2) to (-3);\n  \\arc (2) to (-2);\n  \\arc (2) to (-1);\
\n  \\arc (2) .. controls (0.93333333333333324, 2.3333333333333335) and (1.066\
6666666666667, 2.3333333333333335) .. (1);\n  \\arc (2) edge [toploop] (2);\n \
 \\arc (2) .. controls (1.7333333333333334, 2.3333333333333335) and (1.6000000\
000000001, 2.3333333333333335) .. (3);\n\n  % arcs from vertex -2\n  \\arc (-2\
) .. controls (1.7333333333333334, 0.66666666666666674) and (1.600000000000000\
1, 0.66666666666666674) .. (-3);\n  \\arc (-2) edge [botloop] (-2);\n  \\arc (\
-2) .. controls (0.93333333333333324, 0.66666666666666674) and (1.066666666666\
6667, 0.66666666666666674) .. (-1);\n  \\arc (-2) to (1);\n  \\arc (-2) to (3)\
;\n\n  % arcs from vertex 3\n  \\arc (3) to (-3);\n  \\arc (3) to (-2);\n  \\a\
rc (3) to (-1);\n  \\arc (3) .. controls (1.2, 1.9166666666666665) and (1.4666\
666666666668, 1.9166666666666665) .. (1);\n  \\arc (3) .. controls (1.60000000\
00000001, 2.3333333333333335) and (1.7333333333333334, 2.3333333333333335) .. \
(2);\n  \\arc (3) edge [toploop] (3);\n\n  % arcs from vertex -3\n  \\arc (-3)\
 edge [botloop] (-3);\n  \\arc (-3) .. controls (1.6000000000000001, 0.6666666\
6666666674) and (1.7333333333333334, 0.66666666666666674) .. (-2);\n  \\arc (-\
3) .. controls (1.2, 1.0833333333333335) and (1.4666666666666668, 1.0833333333\
333335) .. (-1);\n  \\arc (-3) to (1);\n  \\arc (-3) to (2);\n  \\arc (-3) to \
(3);\n\n\\end{tikzpicture}\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}[\n  v\
ertex/.style={circle, draw, fill=black, inner sep =0.04cm},\n  ghost/.style={c\
ircle, draw = none, inner sep = 0.14cm},\n  botloop/.style={min distance = 8mm\
, out = -70, in = -110},\n  toploop/.style={min distance = 8mm, out = 70, in =\
 110}]\n\n  % vertices and labels\n  \\foreach \\i in {1,...,3} {\n    \\node \
[vertex] at (\\i/1.5, 3) {};\n    \\node [ghost] (\\i) at (\\i/1.5, 3) {};\n  \
}\n\n  \\foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 0) {};\n\
    \\node [ghost] (-\\i) at (\\i/1.5, 0) {};\n  }\n\n  % arcs from vertex 1\n\
  \\arc (1) edge [toploop] (1);\n\n  % arcs from vertex -1\n  \\arc (-1) to (1\
);\n  \\arc (-1) to (2);\n\n  % arcs from vertex 2\n\n  % arcs from vertex -2\
\n  \\arc (-2) edge [botloop] (-2);\n  \\arc (-2) to (1);\n  \\arc (-2) to (2)\
;\n\n  % arcs from vertex 3\n\n  % arcs from vertex -3\n  \\arc (-3) to (1);\n\
  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigskip\
\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=black,\
 inner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14cm}\
,\n  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.st\
yle={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \\\
foreach \\i in {1,...,3} {\n    \\node [vertex] at (\\i/1.5, 3) {};\n    \\nod\
e [ghost] (\\i) at (\\i/1.5, 3) {};\n  }\n\n  \\foreach \\i in {1,...,3} {\n  \
  \\node [vertex] at (\\i/1.5, 0) {};\n    \\node [ghost] (-\\i) at (\\i/1.5, \
0) {};\n  }\n\n  % arcs from vertex 1\n  \\arc (1) edge [toploop] (1);\n\n  % \
arcs from vertex -1\n  \\arc (-1) .. controls (1.4666666666666668, 1.083333333\
3333335) and (1.2, 1.0833333333333335) .. (-3);\n  \\arc (-1) .. controls (1.0\
666666666666667, 0.66666666666666674) and (0.93333333333333324, 0.666666666666\
66674) .. (-2);\n  \\arc (-1) edge [botloop] (-1);\n  \\arc (-1) to (1);\n  \\\
arc (-1) to (2);\n  \\arc (-1) to (3);\n\n  % arcs from vertex 2\n\n  % arcs f\
rom vertex -2\n  \\arc (-2) .. controls (1.7333333333333334, 0.666666666666666\
74) and (1.6000000000000001, 0.66666666666666674) .. (-3);\n  \\arc (-2) edge \
[botloop] (-2);\n  \\arc (-2) .. controls (0.93333333333333324, 0.666666666666\
66674) and (1.0666666666666667, 0.66666666666666674) .. (-1);\n  \\arc (-2) to\
 (1);\n  \\arc (-2) to (2);\n  \\arc (-2) to (3);\n\n  % arcs from vertex 3\n\
\n  % arcs from vertex -3\n  \\arc (-3) edge [botloop] (-3);\n  \\arc (-3) .. \
controls (1.6000000000000001, 0.66666666666666674) and (1.7333333333333334, 0.\
66666666666666674) .. (-2);\n  \\arc (-3) .. controls (1.2, 1.0833333333333335\
) and (1.4666666666666668, 1.0833333333333335) .. (-1);\n  \\arc (-3) to (1);\
\n  \\arc (-3) to (2);\n  \\arc (-3) to (3);\n\n\\end{tikzpicture}\n\n\\bigski\
p\\bigskip\n\n\\begin{tikzpicture}[\n  vertex/.style={circle, draw, fill=black\
, inner sep =0.04cm},\n  ghost/.style={circle, draw = none, inner sep = 0.14cm\
},\n  botloop/.style={min distance = 8mm, out = -70, in = -110},\n  toploop/.s\
tyle={min distance = 8mm, out = 70, in = 110}]\n\n  % vertices and labels\n  \
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
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\n\\begin{document}\\be\
gin{center}\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\
\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above]\
 {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\
\n  %lines\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and labe\
ls\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\\
fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n \
 \\draw(2, 2)--(2, 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{\
tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circl\
e(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(2, \
0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(\
1, 2)--(2, 0);\n\n  %block number 2\n  %vertices and labels\n  \\fill(2, 2)cir\
cle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.125\
);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\draw(2, 2)--(1, 0)\
;\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %bl\
ock number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.\
94999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n\
  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node \
[above] {$2$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$\
-2$};\n\n  %lines\n  \\draw(2, 2)--(2, 0);\n\n  %block number 3\n  %vertices a\
nd labels\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\
\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpictur\
e}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n\
  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(2, 2)circle(.\
125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1, 0)circle(.125);\n \
 \\draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2\
, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1\
.25) and (2, 1.25) .. (2, 1.875);\n  \\draw(1, 0.125) .. controls (1, 0.75) an\
d (2, 0.75) .. (2, 0.125);\n  \\draw(1, 2)--(1, 0);\n\\end{tikzpicture}\n\n\n\
\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices a\
nd labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) nod\
e [above] {$1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] \
{$-2$};\n\n  %lines\n  \\draw(1, 2)--(2, 0);\n\n  %block number 2\n  %vertices\
 and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2\
$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fill(1, 0)ci\
rcle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\\end{tikzpic\
ture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  \
%vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.9499999999999999\
6, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n  %vertices and \
labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\
  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %line\
s\n  \\draw(2, 2)--(1, 0);\n\n  %block number 3\n  %vertices and labels\n  \\f\
ill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\\
end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block n\
umber 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999\
999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n  %ve\
rtices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [abov\
e] {$2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\
\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %li\
nes\n  \\draw(1, 0.125) .. controls (1, 0.75) and (2, 0.75) .. (2, 0.125);\n  \
\\draw(2, 2)--(2, 0);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{t\
ikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle\
(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill(2, 2\
)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(2, 0)circle(\
.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 1.875)\
 .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n  \\draw(2, 2)--(2, 0);\n\
\n  %block number 2\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n  \\\
draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigs\
kip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and lab\
els\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [abo\
ve] {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$}\
;\n\n  %lines\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and l\
abels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\
\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fill(2, 0)circle(\
.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpicture}\
\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vert\
ices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.\
2) node [above] {$1$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [b\
elow] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-\
2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1, 0.75) and (2, 0.75) .. (2\
, 0.125);\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and label\
s\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\n  %\
lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n \
 %block number 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\dra\
w(0.94999999999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number\
 2\n  %vertices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) n\
ode [above] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n \
 \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\
\n\n  %block number 4\n  %vertices and labels\n  \\fill(2, 0)circle(.125);\n  \
\\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bi\
gskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and l\
abels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [a\
bove] {$1$};\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$\
2$};\n  \\fill(1, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n \
 %lines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\
\n  \\draw(1, 2)--(1, 0);\n\n  %block number 2\n  %vertices and labels\n  \\fi\
ll(2, 0)circle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n\\e\
nd{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block nu\
mber 1\n  %vertices and labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.949999\
99999999996, 2.2) node [above] {$1$};\n\n  %lines\n\n  %block number 2\n  %ver\
tices and labels\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above\
] {$2$};\n\n  %lines\n\n  %block number 3\n  %vertices and labels\n  \\fill(1,\
 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle\
(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.125\
) .. controls (1, 0.75) and (2, 0.75) .. (2, 0.125);\n\\end{tikzpicture}\n\n\n\
\\bigskip\\bigskip\n\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices a\
nd labels\n  \\fill(1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) nod\
e [above] {$1$};\n  \\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above\
] {$2$};\n\n  %lines\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .\
. (2, 1.875);\n\n  %block number 2\n  %vertices and labels\n  \\fill(1, 0)circ\
le(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n\n  %block numb\
er 3\n  %vertices and labels\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) n\
ode [below] {$-2$};\n\n  %lines\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\
\n\\begin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(\
1, 2)circle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \
\\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n\n  %lines\
\n  \\draw(1, 1.875) .. controls (1, 1.25) and (2, 1.25) .. (2, 1.875);\n\n  %\
block number 2\n  %vertices and labels\n  \\fill(1, 0)circle(.125);\n  \\draw(\
1, -0.2) node [below] {$-1$};\n  \\fill(2, 0)circle(.125);\n  \\draw(2, -0.2) \
node [below] {$-2$};\n\n  %lines\n  \\draw(1, 0.125) .. controls (1, 0.75) and\
 (2, 0.75) .. (2, 0.125);\n\\end{tikzpicture}\n\n\n\\bigskip\\bigskip\n\n\\end\
{center}\\end{document}"

# Test TikzString for a bipartition
gap> TikzString(Bipartition([[1, 3], [2, -1], [-2, -3]]));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\n\\begin{document}\\be\
gin{tikzpicture}\n\n  %block number 1\n  %vertices and labels\n  \\fill(1, 2)c\
ircle(.125);\n  \\draw(0.94999999999999996, 2.2) node [above] {$1$};\n  \\fill\
(3, 2)circle(.125);\n  \\draw(2.9500000000000002, 2.2) node [above] {$3$};\n\n\
  %lines\n  \\draw(1, 1.875) .. controls (1, 1.1666666666666667) and (3, 1.166\
6666666666667) .. (3, 1.875);\n\n  %block number 2\n  %vertices and labels\n  \
\\fill(2, 2)circle(.125);\n  \\draw(1.95, 2.2) node [above] {$2$};\n  \\fill(1\
, 0)circle(.125);\n  \\draw(1, -0.2) node [below] {$-1$};\n\n  %lines\n  \\dra\
w(2, 2)--(1, 0);\n\n  %block number 3\n  %vertices and labels\n  \\fill(2, 0)c\
ircle(.125);\n  \\draw(2, -0.2) node [below] {$-2$};\n  \\fill(3, 0)circle(.12\
5);\n  \\draw(3, -0.2) node [below] {$-3$};\n\n  %lines\n  \\draw(2, 0.125) ..\
 controls (2, 0.66666666666666663) and (3, 0.66666666666666663) .. (3, 0.125);\
\n\\end{tikzpicture}\n\n\\end{document}"
gap> TikzString(Bipartition([[1, 3], [2, -1], [-2, -3]]),
> rec(colors := true, labels := true, beginDocument := true,
>     endDocument := true));
"%latex\n\\documentclass{minimal}\n\\usepackage{tikz}\n\n\\begin{document}\\be\
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
gap> GraphvizDClasses(RegularBooleanMatMonoid(3));
<graphviz digraph DClasses with 10 nodes and 14 edges>
gap> DotString(RegularBooleanMatMonoid(2), rec(maximal := true));
"//dot\ndigraph DClasses {\n\tnode [shape=plaintext] edge [color=black, arrowh\
ead=none] \n\t1 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\
\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \
\"0\">1</TD></TR><TR><TD BGCOLOR=\"gray\"><font color=\"black\">1</font></TD><\
/TR>\n</TABLE>>, shape=box, style=invisible]\n\t2 [label=<<TABLE BORDER=\"0\" \
CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\
\"0\"><TD COLSPAN=\"3\" BORDER = \"0\">2</TD></TR><TR><TD BGCOLOR=\"gray\"><fo\
nt color=\"black\">1</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">1</\
font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">1</font></TD></TR>\n<TR><\
TD BGCOLOR=\"gray\"><font color=\"black\">1</font></TD><TD BGCOLOR=\"white\"><\
font color=\"white\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">1\
</font></TD></TR>\n<TR><TD BGCOLOR=\"white\"><font color=\"white\">*</font></T\
D><TD BGCOLOR=\"gray\"><font color=\"black\">1</font></TD><TD BGCOLOR=\"gray\"\
><font color=\"black\">1</font></TD></TR>\n</TABLE>>, shape=box, style=invisib\
le]\n\t3 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLS\
PACING=\"0\" PORT=\"3\">\n<TR BORDER=\"0\"><TD COLSPAN=\"2\" BORDER = \"0\">3<\
/TD></TR><TR><TD BGCOLOR=\"gray\"><font color=\"black\">1</font></TD><TD BGCOL\
OR=\"white\"><font color=\"white\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"white\
\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\
\">1</font></TD></TR>\n</TABLE>>, shape=box, style=invisible]\n\t4 [label=<<TA\
BLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"\
4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\">4</TD></TR><TR><TD BGC\
OLOR=\"gray\"><font color=\"black\">C2</font></TD></TR>\n</TABLE>>, shape=box,\
 style=invisible]\n\t2 -> 1\n\t3 -> 2\n\t4 -> 3\n}\n"
gap> DotString(RegularBooleanMatMonoid(2), rec(number := false));
"//dot\ndigraph DClasses {\n\tnode [shape=plaintext] edge [color=black, arrowh\
ead=none] \n\t1 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\
\" CELLSPACING=\"0\" PORT=\"1\">\n<TR><TD BGCOLOR=\"gray\"><font color=\"black\
\">*</font></TD></TR>\n</TABLE>>, shape=box, style=invisible]\n\t2 [label=<<TA\
BLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"\
2\">\n<TR><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD><TD BGCOLOR=\
\"gray\"><font color=\"black\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\
\"black\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"gray\"><font color=\"black\">*\
</font></TD><TD BGCOLOR=\"white\"><font color=\"white\">*</font></TD><TD BGCOL\
OR=\"gray\"><font color=\"black\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"white\
\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\
\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD></TR>\
\n</TABLE>>, shape=box, style=invisible]\n\t3 [label=<<TABLE BORDER=\"0\" CELL\
BORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"3\">\n<TR><TD BGCOLOR\
=\"gray\"><font color=\"black\">*</font></TD><TD BGCOLOR=\"white\"><font color\
=\"white\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"white\"><font color=\"white\"\
>*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD></TR>\n<\
/TABLE>>, shape=box, style=invisible]\n\t4 [label=<<TABLE BORDER=\"0\" CELLBOR\
DER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"4\">\n<TR><TD BGCOLOR=\"\
gray\"><font color=\"black\">*</font></TD></TR>\n</TABLE>>, shape=box, style=i\
nvisible]\n\t2 -> 1\n\t3 -> 2\n\t4 -> 3\n}\n"
gap> DotString(RegularBooleanMatMonoid(2), rec(normal := false));
"//dot\ndigraph DClasses {\n\tnode [shape=plaintext] edge [color=black, arrowh\
ead=none] \n\t1 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\
\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \
\"0\">1</TD></TR><TR><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD><\
/TR>\n</TABLE>>, shape=box, style=invisible]\n\t2 [label=<<TABLE BORDER=\"0\" \
CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\
\"0\"><TD COLSPAN=\"3\" BORDER = \"0\">2</TD></TR><TR><TD BGCOLOR=\"gray\"><fo\
nt color=\"black\">*</font></TD><TD BGCOLOR=\"white\"><font color=\"white\">*<\
/font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD></TR>\n<TR>\
<TD BGCOLOR=\"white\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"gray\">\
<font color=\"black\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">\
*</font></TD></TR>\n<TR><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></T\
D><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD><TD BGCOLOR=\"gray\"\
><font color=\"black\">*</font></TD></TR>\n</TABLE>>, shape=box, style=invisib\
le]\n\t3 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLS\
PACING=\"0\" PORT=\"3\">\n<TR BORDER=\"0\"><TD COLSPAN=\"2\" BORDER = \"0\">3<\
/TD></TR><TR><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD><TD BGCOL\
OR=\"white\"><font color=\"white\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"white\
\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\
\">*</font></TD></TR>\n</TABLE>>, shape=box, style=invisible]\n\t4 [label=<<TA\
BLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"\
4\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\">4</TD></TR><TR><TD BGC\
OLOR=\"gray\"><font color=\"black\">*</font></TD></TR>\n</TABLE>>, shape=box, \
style=invisible]\n\t2 -> 1\n\t3 -> 2\n\t4 -> 3\n}\n"
gap> S := RegularBooleanMatMonoid(3);;
gap> GraphvizDClasses(S, rec(highlight := [rec(HClasses := [HClass(S, One(S))]),
> rec(HClasses := [First(HClasses(S), x -> not IsGroupHClass(x))]),
> rec(HClasses := HClasses(First(DClasses(S), x -> not IsRegularDClass(x))))
> ]));
<graphviz digraph DClasses with 10 nodes and 14 edges>

# DotString with option idempotentsemilattice
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> DotString(S, rec(idempotentsemilattice := true));
"//dot\ndigraph DClasses {\n\tnode [shape=plaintext] edge [color=black, arrowh\
ead=none] \n\t1 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\
\" CELLSPACING=\"0\" PORT=\"1\">\n<TR BORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \
\"0\">1</TD></TR><TR><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD><\
/TR>\n</TABLE>>, shape=box, style=invisible]\n\t2 [label=<<TABLE BORDER=\"0\" \
CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"2\">\n<TR BORDER=\
\"0\"><TD COLSPAN=\"3\" BORDER = \"0\">2</TD></TR><TR><TD BGCOLOR=\"gray\"><fo\
nt color=\"black\">*</font></TD><TD BGCOLOR=\"white\"><font color=\"white\">*<\
/font></TD><TD BGCOLOR=\"white\"><font color=\"white\">*</font></TD></TR>\n<TR\
><TD BGCOLOR=\"white\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"gray\"\
><font color=\"black\">*</font></TD><TD BGCOLOR=\"white\"><font color=\"white\
\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"white\"><font color=\"white\">*</font\
></TD><TD BGCOLOR=\"white\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"g\
ray\"><font color=\"black\">*</font></TD></TR>\n</TABLE>>, shape=box, style=in\
visible]\n\t3 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" \
CELLSPACING=\"0\" PORT=\"3\">\n<TR BORDER=\"0\"><TD COLSPAN=\"3\" BORDER = \"0\
\">3</TD></TR><TR><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD><TD \
BGCOLOR=\"white\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"white\"><fo\
nt color=\"white\">*</font></TD></TR>\n<TR><TD BGCOLOR=\"white\"><font color=\
\"white\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">*</font></TD\
><TD BGCOLOR=\"white\"><font color=\"white\">*</font></TD></TR>\n<TR><TD BGCOL\
OR=\"white\"><font color=\"white\">*</font></TD><TD BGCOLOR=\"white\"><font co\
lor=\"white\">*</font></TD><TD BGCOLOR=\"gray\"><font color=\"black\">*</font>\
</TD></TR>\n</TABLE>>, shape=box, style=invisible]\n\t4 [label=<<TABLE BORDER=\
\"0\" CELLBORDER=\"1\" CELLPADDING=\"10\" CELLSPACING=\"0\" PORT=\"4\">\n<TR B\
ORDER=\"0\"><TD COLSPAN=\"1\" BORDER = \"0\">4</TD></TR><TR><TD BGCOLOR=\"gray\
\"><font color=\"black\">*</font></TD></TR>\n</TABLE>>, shape=box, style=invis\
ible]\n\t1 -> 2\n\t2 -> 3\n\t3 -> 4\n}\n"

# DotSemilatticeOfIdempotents
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> DotSemilatticeOfIdempotents(S);
"//dot\ngraph semilattice {\n\tnode[shape=point] ranksep=2 \nsubgraph cluster_\
1 {\n\t8\n}\nsubgraph cluster_2 {\n\t6\n\t4\n\t7\n}\nsubgraph cluster_3 {\n\t3\
\n\t5\n\t2\n}\nsubgraph cluster_4 {\n\t1\n}\n\t2 -- 1\n\t3 -- 1\n\t4 -- 2\n\t4\
 -- 3\n\t5 -- 1\n\t6 -- 3\n\t6 -- 5\n\t7 -- 2\n\t7 -- 5\n\t8 -- 4\n\t8 -- 7\n\
\t8 -- 6\n}\n"
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := false));;
gap> DotSemilatticeOfIdempotents(S);
"//dot\ngraph semilattice {\n\tnode[shape=point] ranksep=2 \nsubgraph cluster_\
1 {\n\t1\n}\nsubgraph cluster_2 {\n\t2\n\t3\n\t5\n}\nsubgraph cluster_3 {\n\t4\
\n\t6\n\t7\n}\nsubgraph cluster_4 {\n\t8\n}\n\t2 -- 1\n\t3 -- 1\n\t4 -- 2\n\t4\
 -- 3\n\t5 -- 1\n\t6 -- 3\n\t6 -- 5\n\t7 -- 2\n\t7 -- 5\n\t8 -- 4\n\t8 -- 7\n\
\t8 -- 6\n}\n"

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
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [color\
=\"#ff00ff\", label=b, style=filled]\n\t2 [label=\"&#949;\"]\n\t3 [color=\"#00\
ff00\", label=a, style=filled]\n\t4 [label=ba]\n\t1 -> 2 [color=\"#00ff00\"]\n\
\t1 -> 3 [color=\"#ff00ff\"]\n\t2 -> 1 [color=\"#00ff00\"]\n\t2 -> 3 [color=\"\
#ff00ff\"]\n\t3 -> 4 [color=\"#00ff00\"]\n\t3 -> 3 [color=\"#ff00ff\"]\n\t4 ->\
 3 [color=\"#00ff00\"]\n\t4 -> 3 [color=\"#ff00ff\"]\n// legend context \n\tno\
de [shape=plaintext] \nsubgraph legend {\n\thead [label=<<table border=\"0\" c\
ellpadding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" \
port=\"port1\">a&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">b&nbs\
p;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellpadding=\"2\
\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port=\"port1\"\
>&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">&nbsp;</td></tr>\n</\
table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\thead:port1:e -> tail:port1:w [co\
lor=\"#00ff00\", constraint=false]\n\thead:port2:e\n\ttail:port2:w\n\thead:por\
t2:e -> tail:port2:w [color=\"#ff00ff\", constraint=false]\n}\n\tnode [shape=c\
ircle] node [shape=\"box\"] \n\n}\n"
gap> DotLeftCayleyDigraph(FullTransformationMonoid(2));
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [color\
=\"#ff00ff\", label=b, style=filled]\n\t2 [label=\"&#949;\"]\n\t3 [color=\"#00\
ff00\", label=a, style=filled]\n\t4 [label=ba]\n\t1 -> 2 [color=\"#00ff00\"]\n\
\t1 -> 3 [color=\"#ff00ff\"]\n\t2 -> 1 [color=\"#00ff00\"]\n\t2 -> 4 [color=\"\
#ff00ff\"]\n\t3 -> 3 [color=\"#00ff00\"]\n\t3 -> 3 [color=\"#ff00ff\"]\n\t4 ->\
 4 [color=\"#00ff00\"]\n\t4 -> 4 [color=\"#ff00ff\"]\n// legend context \n\tno\
de [shape=plaintext] \nsubgraph legend {\n\thead [label=<<table border=\"0\" c\
ellpadding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" \
port=\"port1\">a&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">b&nbs\
p;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellpadding=\"2\
\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port=\"port1\"\
>&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">&nbsp;</td></tr>\n</\
table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\thead:port1:e -> tail:port1:w [co\
lor=\"#00ff00\", constraint=false]\n\thead:port2:e\n\ttail:port2:w\n\thead:por\
t2:e -> tail:port2:w [color=\"#ff00ff\", constraint=false]\n}\n\tnode [shape=c\
ircle] node [shape=\"box\"] \n\n}\n"
gap> S := LeftZeroSemigroup(27);;
gap> DotLeftCayleyDigraph(S);
Error, the 1st argument (an out-regular digraph) must have out-degree at most \
24, found 27
gap> TikzLeftCayleyDigraph(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `TikzString' on 1 arguments
gap> DotRightCayleyDigraph(S);
Error, the 1st argument (an out-regular digraph) must have out-degree at most \
24, found 27
gap> TikzRightCayleyDigraph(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `TikzString' on 1 arguments

#
gap> S := Semigroup(Transformation([1, 2, 2]), Transformation([2, 2, 2]));
<transformation semigroup of degree 3 with 2 generators>
gap> IsMonoidAsSemigroup(S);
true
gap> Size(S);
2
gap> DotLeftCayleyDigraph(S);
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [label\
=\"&#949;\"]\n\t2 [color=\"#00ff00\", label=b, style=filled]\n\t1 -> 2 [color=\
\"#00ff00\"]\n\t2 -> 2 [color=\"#00ff00\"]\n// legend context \n\tnode [shape=\
plaintext] \nsubgraph legend {\n\thead [label=<<table border=\"0\" cellpadding\
=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port=\"por\
t1\">b&nbsp;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellp\
adding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port\
=\"port1\">&nbsp;</td></tr>\n</table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\th\
ead:port1:e -> tail:port1:w [color=\"#00ff00\", constraint=false]\n}\n\tnode [\
shape=circle] node [shape=\"box\"] \n\n}\n"

#
gap> F := FreeMonoid("c", "d");
<free monoid on the generators [ c, d ]>
gap> AssignGeneratorVariables(F);
gap> R := ParseRelations([c, d], "c^2=c, dc=d, cd^2=d^2, d^5=d^2");
[ [ c^2, c ], [ d*c, d ], [ c*d^2, d^2 ], [ d^5, d^2 ] ]
gap> S := F / R;
<fp monoid with 2 generators and 4 relations of length 20>
gap> DotLeftCayleyDigraph(S);
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [label\
=\"&#949;\"]\n\t2 [color=\"#00ff00\", label=c, style=filled]\n\t3 [color=\"#ff\
00ff\", label=d, style=filled]\n\t4 [label=cd]\n\t5 [label=\"d^2\"]\n\t6 [labe\
l=\"d^3\"]\n\t7 [label=\"d^4\"]\n\t1 -> 2 [color=\"#00ff00\"]\n\t1 -> 3 [color\
=\"#ff00ff\"]\n\t2 -> 2 [color=\"#00ff00\"]\n\t2 -> 3 [color=\"#ff00ff\"]\n\t3\
 -> 4 [color=\"#00ff00\"]\n\t3 -> 5 [color=\"#ff00ff\"]\n\t4 -> 4 [color=\"#00\
ff00\"]\n\t4 -> 5 [color=\"#ff00ff\"]\n\t5 -> 5 [color=\"#00ff00\"]\n\t5 -> 6 \
[color=\"#ff00ff\"]\n\t6 -> 6 [color=\"#00ff00\"]\n\t6 -> 7 [color=\"#ff00ff\"\
]\n\t7 -> 7 [color=\"#00ff00\"]\n\t7 -> 5 [color=\"#ff00ff\"]\n// legend conte\
xt \n\tnode [shape=plaintext] \nsubgraph legend {\n\thead [label=<<table borde\
r=\"0\" cellpadding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\
\"right\" port=\"port1\">c&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"por\
t2\">d&nbsp;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellp\
adding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port\
=\"port1\">&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">&nbsp;</td\
></tr>\n</table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\thead:port1:e -> tail:p\
ort1:w [color=\"#00ff00\", constraint=false]\n\thead:port2:e\n\ttail:port2:w\n\
\thead:port2:e -> tail:port2:w [color=\"#ff00ff\", constraint=false]\n}\n\tnod\
e [shape=circle] node [shape=\"box\"] \n\n}\n"

#
gap> F := FreeSemigroup("c", "d");
<free semigroup on the generators [ c, d ]>
gap> AssignGeneratorVariables(F);
gap> R := ParseRelations([c, d], "c^2=c, dc=d, cd^2=d^2, d^5=d^2");
[ [ c^2, c ], [ d*c, d ], [ c*d^2, d^2 ], [ d^5, d^2 ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 4 relations of length 20>
gap> DotLeftCayleyDigraph(S);
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [color\
=\"#00ff00\", label=c, style=filled]\n\t2 [color=\"#ff00ff\", label=d, style=f\
illed]\n\t3 [label=cd]\n\t4 [label=\"d^2\"]\n\t5 [label=\"d^3\"]\n\t6 [label=\
\"d^4\"]\n\t1 -> 1 [color=\"#00ff00\"]\n\t1 -> 2 [color=\"#ff00ff\"]\n\t2 -> 3\
 [color=\"#00ff00\"]\n\t2 -> 4 [color=\"#ff00ff\"]\n\t3 -> 3 [color=\"#00ff00\
\"]\n\t3 -> 4 [color=\"#ff00ff\"]\n\t4 -> 4 [color=\"#00ff00\"]\n\t4 -> 5 [col\
or=\"#ff00ff\"]\n\t5 -> 5 [color=\"#00ff00\"]\n\t5 -> 6 [color=\"#ff00ff\"]\n\
\t6 -> 6 [color=\"#00ff00\"]\n\t6 -> 4 [color=\"#ff00ff\"]\n// legend context \
\n\tnode [shape=plaintext] \nsubgraph legend {\n\thead [label=<<table border=\
\"0\" cellpadding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"r\
ight\" port=\"port1\">c&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\
\">d&nbsp;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellpad\
ding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port=\
\"port1\">&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">&nbsp;</td>\
</tr>\n</table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\thead:port1:e -> tail:po\
rt1:w [color=\"#00ff00\", constraint=false]\n\thead:port2:e\n\ttail:port2:w\n\
\thead:port2:e -> tail:port2:w [color=\"#ff00ff\", constraint=false]\n}\n\tnod\
e [shape=circle] node [shape=\"box\"] \n\n}\n"

#
gap> F := FreeMonoid("a", "b");
<free monoid on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := ParseRelations([a, b], "a^2=a, ba=b, ab^2=b^2, b^5=b^2");
[ [ a^2, a ], [ b*a, b ], [ a*b^2, b^2 ], [ b^5, b^2 ] ]
gap> S := F / R;
<fp monoid with 2 generators and 4 relations of length 20>
gap> DotLeftCayleyDigraph(S);
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [label\
=\"&#949;\"]\n\t2 [color=\"#00ff00\", label=a, style=filled]\n\t3 [color=\"#ff\
00ff\", label=b, style=filled]\n\t4 [label=ab]\n\t5 [label=\"b^2\"]\n\t6 [labe\
l=\"b^3\"]\n\t7 [label=\"b^4\"]\n\t1 -> 2 [color=\"#00ff00\"]\n\t1 -> 3 [color\
=\"#ff00ff\"]\n\t2 -> 2 [color=\"#00ff00\"]\n\t2 -> 3 [color=\"#ff00ff\"]\n\t3\
 -> 4 [color=\"#00ff00\"]\n\t3 -> 5 [color=\"#ff00ff\"]\n\t4 -> 4 [color=\"#00\
ff00\"]\n\t4 -> 5 [color=\"#ff00ff\"]\n\t5 -> 5 [color=\"#00ff00\"]\n\t5 -> 6 \
[color=\"#ff00ff\"]\n\t6 -> 6 [color=\"#00ff00\"]\n\t6 -> 7 [color=\"#ff00ff\"\
]\n\t7 -> 7 [color=\"#00ff00\"]\n\t7 -> 5 [color=\"#ff00ff\"]\n// legend conte\
xt \n\tnode [shape=plaintext] \nsubgraph legend {\n\thead [label=<<table borde\
r=\"0\" cellpadding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\
\"right\" port=\"port1\">a&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"por\
t2\">b&nbsp;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellp\
adding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port\
=\"port1\">&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">&nbsp;</td\
></tr>\n</table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\thead:port1:e -> tail:p\
ort1:w [color=\"#00ff00\", constraint=false]\n\thead:port2:e\n\ttail:port2:w\n\
\thead:port2:e -> tail:port2:w [color=\"#ff00ff\", constraint=false]\n}\n\tnod\
e [shape=circle] node [shape=\"box\"] \n\n}\n"

#
gap> F := FreeSemigroup("a", "b");
<free semigroup on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := ParseRelations([a, b], "a^2=a, ba=b, ab^2=b^2, b^5=b^2");
[ [ a^2, a ], [ b*a, b ], [ a*b^2, b^2 ], [ b^5, b^2 ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 4 relations of length 20>
gap> DotLeftCayleyDigraph(S);
"//dot\ndigraph hgn {\n\tnode [shape=circle] node [shape=\"box\"] \n\t1 [color\
=\"#00ff00\", label=a, style=filled]\n\t2 [color=\"#ff00ff\", label=b, style=f\
illed]\n\t3 [label=ab]\n\t4 [label=\"b^2\"]\n\t5 [label=\"b^3\"]\n\t6 [label=\
\"b^4\"]\n\t1 -> 1 [color=\"#00ff00\"]\n\t1 -> 2 [color=\"#ff00ff\"]\n\t2 -> 3\
 [color=\"#00ff00\"]\n\t2 -> 4 [color=\"#ff00ff\"]\n\t3 -> 3 [color=\"#00ff00\
\"]\n\t3 -> 4 [color=\"#ff00ff\"]\n\t4 -> 4 [color=\"#00ff00\"]\n\t4 -> 5 [col\
or=\"#ff00ff\"]\n\t5 -> 5 [color=\"#00ff00\"]\n\t5 -> 6 [color=\"#ff00ff\"]\n\
\t6 -> 6 [color=\"#00ff00\"]\n\t6 -> 4 [color=\"#ff00ff\"]\n// legend context \
\n\tnode [shape=plaintext] \nsubgraph legend {\n\thead [label=<<table border=\
\"0\" cellpadding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"r\
ight\" port=\"port1\">a&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\
\">b&nbsp;</td></tr>\n</table>>\n]\n\ttail [label=<<table border=\"0\" cellpad\
ding=\"2\" cellspacing=\"0\" cellborder=\"0\">\n<tr><td align=\"right\" port=\
\"port1\">&nbsp;</td></tr>\n<tr><td align=\"right\" port=\"port2\">&nbsp;</td>\
</tr>\n</table>>\n]\n\thead:port1:e\n\ttail:port1:w\n\thead:port1:e -> tail:po\
rt1:w [color=\"#00ff00\", constraint=false]\n\thead:port2:e\n\ttail:port2:w\n\
\thead:port2:e -> tail:port2:w [color=\"#ff00ff\", constraint=false]\n}\n\tnod\
e [shape=circle] node [shape=\"box\"] \n\n}\n"

# Unbind local variables, auto-generated by etc/tst-unbind-local-vars.py
gap> Unbind(S);
gap> Unbind(x);
gap> Unbind(y);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/tools/display.tst");
