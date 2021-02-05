##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##
##  Call this with GAP in the package directory:
##
##      gap makedoc.g
##

LoadPackage("GAPDoc");
LoadPackage("semigroups");

SemigroupsMakeDoc();
CopyHTMLStyleFiles("doc");
GAPDocManualLab("Semigroups");

QUIT;
