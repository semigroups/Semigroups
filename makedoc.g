##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##
##  Call this with GAP in the package directory:
##
##      gap makedoc.g
##

PACKAGE := "Semigroups";
PrintTo("VERSION", PackageInfo(PACKAGE)[1].Version);
LoadPackage("GAPDoc");
LoadPackage("semigroups");

SemigroupsMakeDoc();
CopyHTMLStyleFiles("doc");
GAPDocManualLab(PACKAGE);

QUIT;
