#############################################################################
##
##  doc.g
##  Copyright (C) 2021-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains the information required to build the Semigroups package
## documentation, it is used by makedoc.g.

BindGlobal("SEMIGROUPS_DocXMLFiles",
           ["../PackageInfo.g",
            "acting.xml",
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
            "display.xml",
            "dual.xml",
            "elements.xml",
            "factor.xml",
            "ffmat.xml",
            "freeband.xml",
            "freeinverse.xml",
            "froidure-pin.xml",
            "grac.xml",
            "greens-generic.xml",
            "gren.xml",
            "grpffmat.xml",
            "grpperm.xml",
            "ideals.xml",
            "io.xml",
            "isomorph.xml",
            "isorms.xml",
            "main.xml",
            "maximal.xml",
            "maxplusmat.xml",
            "obsolete.xml",
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
            "semirms.xml",
            "semitrans.xml",
            "tietze.xml",
            "title.xml",
            "trans.xml",
            "utils.xml",
            "word.xml"]);

BindGlobal("SEMIGROUPS_MakeDoc",
function(pkgdir)
  local PKG, temp, version, args;

  PKG := "Semigroups";

  # Get the GAP version from PackageInfo.g and write it to .VERSION
  temp := SplitString(StringFile(Filename(pkgdir, "PackageInfo.g")), "\n");
  version := SplitString(First(temp, x -> StartsWith(x, "Version")), "\"")[2];
  PrintTo(Filename(pkgdir, ".VERSION"), version, "\n");

  args := [Filename(pkgdir, "doc"),
           "main.xml",
           SEMIGROUPS_DocXMLFiles,
           PKG,
           "MathJax",
           "../../.."];
  # If pdflatex is not available, but we call MakeGAPDocDoc implicitly asking
  # for GAPDoc to compile a PDF version of the manual, then GAPDoc fails to
  # create the doc/manual.six file, which we need later. This file however is
  # still created if we explicitly say that we don't want a PDF
  if Filename(DirectoriesSystemPrograms(), "pdflatex") = fail then
    Add(args, "nopdf");
  fi;
  LoadPackage("GAPDoc");
  SetGapDocLaTeXOptions("utf8");
  CallFuncList(MakeGAPDocDoc, args);
  CopyHTMLStyleFiles(Filename(pkgdir, "doc"));
  GAPDocManualLabFromSixFile(PKG, Filename(pkgdir, "doc/manual.six"));
end);
