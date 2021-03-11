#############################################################################
##
##  doc.g
##  Copyright (C) 2021                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains the information required to build the Semigroups package
## documentation, it is used by makedoc.g.

BindGlobal("SEMIGROUPS_DocXMLFiles",
           ["../PackageInfo.g",
            "attr.xml",
            "attract.xml",
            "attrinv.xml",
            "bipart.xml",
            "blocks.xml",
            "boolmat.xml",
            "cong.xml",
            "conginv.xml",
            "conglatt.xml",
            "congpairs.xml",
            "congrees.xml",
            "congrms.xml",
            "conguniv.xml",
            "dual.xml",
            "display.xml",
            "elements.xml",
            "factor.xml",
            "ffmat.xml",
            "freeband.xml",
            "freeinverse.xml",
            "fropin.xml",
            "gree.xml",
            "grpffmat.xml",
            "ideals.xml",
            "io.xml",
            "isomorph.xml",
            "isorms.xml",
            "maximal.xml",
            "maxplusmat.xml",
            "normalizer.xml",
            "orbits.xml",
            "pbr.xml",
            "properties.xml",
            "semiact.xml",
            "semibipart.xml",
            "semiboolmat.xml",
            "semicons.xml",
            "semidp.xml",
            "semieunit.xml",
            "semiex.xml",
            "semiffmat.xml",
            "semifp.xml",
            "semigraph.xml",
            "semigroups.xml",
            "semigrp.xml",
            "semimaxplus.xml",
            "semipbr.xml",
            "semipperm.xml",
            "semiringmat.xml",
            "semitrans.xml",
            "trans.xml",
            "utils.xml",
            "word.xml"]);

BindGlobal("SemigroupsMakeDoc",
function(arg)
  local doquit, PACKAGE;
  if Length(arg) = 1 then
    doquit := arg[1];
  else
    doquit := false;
  fi;
  PACKAGE := "Semigroups";
  PrintTo(".VERSION", PackageInfo(PACKAGE)[1].Version, "\n");
  LoadPackage("GAPDoc");

  SetGapDocLaTeXOptions("utf8");
  MakeGAPDocDoc(Concatenation(PackageInfo(PACKAGE)[1]!.
                              InstallationPath, "/doc"),
                "main.xml", SEMIGROUPS_DocXMLFiles, PACKAGE, "MathJax",
                "../../..");

  CopyHTMLStyleFiles("doc");
  GAPDocManualLabFromSixFile(PACKAGE, "doc/manual.six");
  if doquit then
    FORCE_QUIT_GAP();
  fi;
end);
