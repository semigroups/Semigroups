#############################################################################
##
##  makedoc.g
##  Copyright (C) 2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

LoadPackage("AutoDoc");
LoadPackage("semigroups");

# Helper functions

RemovePrefixVersion := function(string)
  if StartsWith(string, ">=") then
    return string{[3 .. Length(string)]};
  fi;
  return string;
end;

UrlEntity := function(name, url)
  return StringFormatted("""<Alt Not="Text"><URL Text="{1}">{2}</URL></Alt>
  <Alt Only="Text"><Package>{1}</Package></Alt>""", name, url);
end;

PackageEntity := function(name)
  return UrlEntity(name, PackageInfo(name)[1].PackageWWWHome);
end;

Greens := function(name)
return StringFormatted("""<Alt Only="HTML">\(\mathscr{{{1}}}\)</Alt>
  <Alt Only="Text"><E>{1}</E></Alt>
  <Alt Only="LaTeX"><M>\mathcal{{{1}}}</M></Alt>""",
  name);
end;

MathOrCode := function(string)
  return StringFormatted("""<Alt Not="Text"><M>{1}</M></Alt>
  <Alt Only="Text"><C>{1}</C></Alt>""", string);
end;

XMLEntities := rec();

# Programmatically determined entities

PkgInfo := PackageInfo("semigroups")[1];

XMLEntities.VERSION := PkgInfo.Version;
XMLEntities.GAPVERS := RemovePrefixVersion(PkgInfo.Dependencies.GAP);

for Pkg in PkgInfo.Dependencies.NeededOtherPackages do
  entity_name := Concatenation(UppercaseString(Pkg[1]), "VERS");
  XMLEntities.(entity_name) := RemovePrefixVersion(Pkg[2]);
od;

ARCHIVENAME := SplitString(PkgInfo.ArchiveURL, "/");
ARCHIVENAME := Concatenation(ARCHIVENAME[Length(ARCHIVENAME)],
               PkgInfo.ArchiveFormats);
XMLEntities.ARCHIVENAME := ARCHIVENAME;

XMLEntities.SEMIGROUPS := PackageEntity("Semigroups");

for Pkg in PkgInfo.Dependencies.NeededOtherPackages do
  pkg_name := PackageInfo(Pkg[1])[1].PackageName;
  entity_name := UppercaseString(pkg_name);
  XMLEntities.(entity_name) := PackageEntity(pkg_name);
od;

# The files containing the xml of the doc

Files := Filtered(DirectoryContents("doc"),
                  x -> (not StartsWith(x, "."))
                       and (not StartsWith(x, "z-"))
                       and EndsWith(x, ".xml"));
Apply(Files, x -> Concatenation("doc/", x));
Add(Files, "PackageInfo.g");

# The scaffold files (chapters)

Includes := Filtered(DirectoryContents("doc"),
                     x -> StartsWith(x, "z-") and EndsWith(x, ".xml"));

# Hard coded entities

XMLEntities.LIBSEMIGROUPS := UrlEntity("libsemigroups",
  "https://libsemigroups.readthedocs.io/en/latest/");
XMLEntities.BLISS         := UrlEntity("bliss",
  "http://www.tcs.tkk.fi/Software/bliss/");

XMLEntities.H := Greens("H");
XMLEntities.R := Greens("R");
XMLEntities.L := Greens("L");
XMLEntities.D := Greens("D");
XMLEntities.J := Greens("J");

for thing in ["setX", "n", "p", "x", "y", "i", "j", "k", "r", "s"] do
  XMLEntities.(thing) := MathOrCode(thing);
od;

XMLEntities.bfn := """<Alt Not="Text"><M>\mathbf{n}</M></Alt>
<Alt Only="Text"><E>n</E></Alt>""";
XMLEntities.bbN := """<Alt Not="Text"><M>\mathbb{N}</M></Alt>
<Alt Only="Text"><E>N</E></Alt>""";

# The actual call to AutoDoc

AutoDoc( rec(
    gapdoc := rec(
        LaTeXOptions := rec( EarlyExtraPreamble := """
            \usepackage{a4wide}
            \newcommand{\bbZ}{\mathbb{Z}}
        """ ),
        main := "main",
        files := Files
    ),
    scaffold := rec(
        includes := Includes,
        bib := "bibliography.xml",
        entities := XMLEntities
    )
  )
);
