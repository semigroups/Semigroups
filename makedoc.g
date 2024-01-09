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
  if TestPackageAvailability(name) <> fail then
    return UrlEntity(PackageInfo(name)[1].PackageName,
                     PackageInfo(name)[1].PackageWWWHome);
  fi;
  return StringFormatted("<Package>{1}</Package>", name);
end;

Greens := function(name)
  local result;
  # Note that we don't use a multiline string to define the alternatives
  # because then they contain spaces like H -class.
  # Also we use multiline strings in StringFormatted, because otherwise the "
  # inside get mangled.
  result := StringFormatted
              ("""<Alt Only="HTML MathJax">\(\mathscr{{{1}}}\)</Alt>""", name);
  Append(result,
         StringFormatted("""<Alt Only="HTML noMathJax"><E>{1}</E></Alt>""",
                         name));
  Append(result,
         StringFormatted("""<Alt Only="Text"><E>{1}</E></Alt>""",
                         name));
  Append(result,
         StringFormatted("""<Alt Only="LaTeX"><M>\mathcal{{{1}}}</M></Alt>""",
                         name));
  return result;
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
ARCHIVENAME := Concatenation(Last(ARCHIVENAME), PkgInfo.ArchiveFormats);
XMLEntities.ARCHIVENAME := ARCHIVENAME;

XMLEntities.SEMIGROUPS := PackageEntity("Semigroups");

for Pkg in Concatenation(PkgInfo.Dependencies.NeededOtherPackages,
                         PkgInfo.Dependencies.SuggestedOtherPackages) do
  entity_name := UppercaseString(Pkg[1]);
  XMLEntities.(entity_name) := PackageEntity(Pkg[1]);
od;

# The files containing the xml of the doc

DocDir := DirectoriesPackageLibrary("semigroups", "doc")[1];
Files := Filtered(DirectoryContents(DocDir),
                  x -> (not StartsWith(x, "."))
                       and (not StartsWith(x, "z-"))
                       and EndsWith(x, ".xml"));
Apply(Files, x -> Concatenation("doc/", x));
Add(Files, "PackageInfo.g");
Sort(Files);

# The scaffold files (chapters)

Includes := Filtered(DirectoryContents(DocDir),
                     x -> StartsWith(x, "z-") and EndsWith(x, ".xml"));
Sort(Includes);

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

for thing in ["X", "n", "p", "x", "y", "i", "j", "k", "r", "s"] do
  XMLEntities.(thing) := MathOrCode(thing);
od;

XMLEntities.bfn := """<Alt Not="Text"><M>\mathbf{n}</M></Alt>
<Alt Only="Text"><E>n</E></Alt>""";
XMLEntities.bbN := """<Alt Not="Text"><M>\mathbb{N}</M></Alt>
<Alt Only="Text"><E>N</E></Alt>""";

# The actual call to AutoDoc

AutoDoc("semigroups", rec(
    autodoc := rec(scan_dirs := ["gap/tools/", "gap/attributes"]),
    gapdoc := rec(
        LaTeXOptions := rec(EarlyExtraPreamble := """
            \usepackage{a4wide}
            \newcommand{\bbZ}{\mathbb{Z}}
        """),
        main := "main",
        files := Files),

    scaffold := rec(
        includes := Includes,
        bib := "bibliography.xml",
        entities := XMLEntities)));

Unbind(RemovePrefixVersion);
Unbind(UrlEntity);
Unbind(PackageEntity);
Unbind(Greens);
Unbind(MathOrCode);
Unbind(XMLEntities);
Unbind(PkgInfo);
Unbind(Pkg);
Unbind(ARCHIVENAME);
Unbind(DocDir);
Unbind(Files);
Unbind(Includes);
