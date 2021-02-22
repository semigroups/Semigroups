##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##
##  Call this with GAP in the package directory:
##
##      gap makedoc.g
##

if not IsBoundGlobal("SemigroupsMakeDoc") then
  Read(Filename(DirectoriesPackageLibrary("semigroups", "gap"), "doc.g"));
fi;
SemigroupsMakeDoc(true);
