##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##
##  Call this with GAP in the package directory:
##
##      gap makedoc.g
##

if not IsDirectoryPath("gap")
    or not "doc.g" in DirectoryContents("gap") then
  Print("Error: GAP must be run from the package directory ",
        "when reading makedoc.g\n");
  FORCE_QUIT_GAP(1);
fi;
if IsBoundGlobal("SEMIGROUPS_DocXMLFiles") then
  MakeReadWriteGlobal("SEMIGROUPS_DocXMLFiles");
  UnbindGlobal("SEMIGROUPS_DocXMLFiles");
fi;
if IsBoundGlobal("SEMIGROUPS_MakeDoc") then
  MakeReadWriteGlobal("SEMIGROUPS_MakeDoc");
  UnbindGlobal("SEMIGROUPS_MakeDoc");
fi;
Read("gap/doc.g");
SEMIGROUPS_MakeDoc(DirectoryCurrent());
FORCE_QUIT_GAP();
