############################################################################
##
##  PackageInfo.g
##  Copyright (C) 2011-18                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY VERSION "3.0.16">
##  <!ENTITY GAPVERS "4.9.0">
##  <!ENTITY DIGRAPHSVERS "0.12.0">
##  <!ENTITY ORBVERS "4.8.0">
##  <!ENTITY IOVERS "4.5.1">
##  <!ENTITY GENSSVERS "1.6.5">
##  <!ENTITY ARCHIVENAME "semigroups-3.0.16">
##  <!ENTITY COPYRIGHTYEARS "2011-18">
##  <#/GAPDoc>

BindGlobal("_RecogsFunnyNameFormatterFunction",
function(st)
  if Length(st) = 0 then
    return st;
  else
    return Concatenation(" (", st, ")");
  fi;
end);

BindGlobal("_RecogsFunnyWWWURLFunction",
function(re)
  if IsBound(re.WWWHome) then
    return re.WWWHome;
  else
    return "";
  fi;
end);

_STANDREWS := Concatenation(["Mathematical Institute, ",
                             "North Haugh, ",
                             "St Andrews, ",
                             "Fife, ",
                             "KY16 9SS, ",
                             "Scotland"]);

SetPackageInfo(rec(
PackageName := "Semigroups",
Subtitle := "A package for semigroups and monoids",
Version := "3.0.16",
Date := "29/05/2018",
ArchiveFormats := ".tar.gz",

SourceRepository := rec(
    Type := "git",
    URL := Concatenation("https://github.com/gap-packages/", ~.PackageName),
),

IssueTrackerURL := Concatenation(~.SourceRepository.URL, "/issues"),
PackageWWWHome  := Concatenation("https://gap-packages.github.io/",
                                 ~.PackageName),
README_URL      := Concatenation(~.PackageWWWHome, "/README.md"),
PackageInfoURL  := Concatenation(~.PackageWWWHome, "/PackageInfo.g"),
ArchiveURL      := Concatenation(~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", "semigroups-", ~.Version),

Persons := [
  rec(
    LastName      := "Mitchell",
    FirstNames    := "J. D.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jdm3@st-and.ac.uk",
    WWWHome       := "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName     := "Burrell",
    FirstNames    := "S.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "sb235@st-andrews.ac.uk ",
    WWWHome       := "http://sburrell.nfshost.com",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName     := "Delgado",
    FirstNames    := "M.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "mdelgado@fc.up.pt",
    WWWHome       := "http://cmup.fc.up.pt/cmup/mdelgado/",
    Place         := "Porto",
    Institution   := "Universidade do Porto"),

  rec(
    LastName      := "East",
    FirstNames    := "J.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "j.east@uws.edu.au",
    WWWHome       := "http://goo.gl/MuiJu5",
    Place         := "Sydney",
    Institution   := "Western Sydney University"),

  rec(
    LastName      := "Egri-Nagy",
    FirstNames    := "A.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "attila@egri-nagy.hu",
    WWWHome       := "http://www.egri-nagy.hu",
    Place         := "Akita, Japan",
    Institution   := "Akita International University"),

  rec(
    LastName      := "Ham",
    FirstNames    := "N.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "nicholas.charles.ham@gmail.com",
    WWWHome       := "https://n-ham.github.io",
    Place         := "Hobart, Tasmania",
    Institution   := "University of Tasmania"),

  rec(
    LastName      := "Jonusas",
    FirstNames    := "J.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "jj252@st-and.ac.uk",
    PostalAddress := _STANDREWS,
    WWWHome       := "http://www-groups.mcs.st-andrews.ac.uk/~julius/",
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

   rec(
    LastName      := "Pfeiffer",
    FirstNames    := "M.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "markus.pfeiffer@morphism.de",
    WWWHome       := "https://www.morphism.de/~markusp/",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName     := "Russell",
    FirstNames    := "C.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "cr66@st-andrews.ac.uk",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Steinberg",
    FirstNames    := "B.",
    IsAuthor      := false,
    IsMaintainer  := false,
    WWWHome       := "http://www.sci.ccny.cuny.edu/~benjamin/"),

  rec(
    LastName      := "Smith",
    FirstNames    := "F.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "fls3@st-andrews.ac.uk",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Smith",
    FirstNames    := "J.",
    IsAuthor      := false,
    IsMaintainer  := false,
    WWWHome       := "http://math.sci.ccny.cuny.edu/people?name=Jhevon_Smith"),

  rec(
    LastName      := "Torpey",
    FirstNames    := "M.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "mct25@st-and.ac.uk",
    WWWHome       := "http://www-groups.mcs.st-and.ac.uk/~mct25/",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Wilson",
    FirstNames    := "W. A.",
    IsAuthor      := false,
    IsMaintainer  := false,
    Email         := "waw7@st-and.ac.uk",
    WWWHome       := "http://wilf.me",
    PostalAddress := _STANDREWS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews")],

Status := "deposited",

AbstractHTML := Concatenation(
  "<p>The Semigroups package is a GAP package containing ",
  "methods for semigroups, monoids, and inverse semigroups.  There are ",
  "particularly efficient methods for semigroups or ideals consisting of ",
  "transformations, partial permutations, bipartitions, partitioned binary ",
  "relations, subsemigroups of regular Rees 0-matrix semigroups, and matrices ",
  "of various semirings including boolean matrices, matrices over finite ",
  "fields, and certain tropical matrices.</p><p>",
  "Semigroups contains efficient methods for creating semigroups, ",
  "monoids, and inverse semigroup, calculating their Green's structure, ",
  "ideals, size, elements, group of units, small generating sets, testing ",
  "membership, finding the inverses of a regular element, factorizing ",
  "elements over the generators, and so on. It is possible to test if a ",
  "semigroup satisfies a particular property, such as if it is regular, ",
  "simple, inverse, completely regular, and a variety of further ",
  "properties.</p><p>",
  "There are methods for finding presentations for a semigroup, the ",
  "congruences of a semigroup, the normalizer of a semigroup in a permutation ",
  "group, the maximal subsemigroups of a finite semigroup, smaller degree ",
  "partial permutation representations, and the character tables of inverse ",
  "semigroups. There are functions for producing pictures of the Green's ",
  "structure of a semigroup, and for drawing graphical representations of ",
  "certain types of elements.</p>"),

PackageDoc := rec(
  BookName  := "Semigroups",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Semigroups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.9.0",
  NeededOtherPackages := [["orb", ">=4.8.0"],
                          ["io", ">=4.5.1"],
                          ["digraphs", ">=0.12.0"],
                          ["genss", ">=1.6.5"]],
  SuggestedOtherPackages := [["gapdoc", ">=1.5.1"]],

  ExternalConditions := []),

BannerString := Concatenation(
  "----------------------------------------------------------------------",
  "-------\n",
  "Loading  Semigroups ", ~.Version, "\n",
  "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
        " (", ~.Persons[1].WWWHome, ")\n",
  "with contributions by:\n",
  Concatenation(Concatenation(List(~.Persons{[2 .. Length(~.Persons) - 1]},
       p -> ["     ", p.FirstNames, " ", p.LastName,
       _RecogsFunnyNameFormatterFunction(
         _RecogsFunnyWWWURLFunction(p)), ",\n"]))),
  " and ", ~.Persons[Length(~.Persons)].FirstNames, " ",
  ~.Persons[Length(~.Persons)].LastName,
  _RecogsFunnyNameFormatterFunction(
    _RecogsFunnyWWWURLFunction(~.Persons[Length(~.Persons)])), ".\n",
  "-----------------------------------------------------------------------",
  "------\n"),

AvailabilityTest := function()
  local semigroups_so;
  semigroups_so := Filename(DirectoriesPackagePrograms("semigroups"),
                            "semigroups.so");
  if (not "semigroups" in SHOW_STAT()) and semigroups_so = fail then
    Info(InfoWarning, 1, "Semigroups: the kernel module is not compiled, ",
         "the package cannot be loaded.");
    return fail;
  fi;
  return true;
end,

Autoload := false,
TestFile := "tst/testinstall.tst",
Keywords := ["transformation semigroups", "partial permutations",
             "inverse semigroups", "Green's relations",
             "free inverse semigroup", "partition monoid", "bipartitions",
             "Rees matrix semigroups"]
));

MakeReadWriteGlobal("_RecogsFunnyWWWURLFunction");
MakeReadWriteGlobal("_RecogsFunnyNameFormatterFunction");
Unbind(_RecogsFunnyWWWURLFunction);
Unbind(_RecogsFunnyNameFormatterFunction);
Unbind(_STANDREWS);
