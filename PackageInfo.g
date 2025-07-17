############################################################################
##
##  PackageInfo.g
##  Copyright (C) 2011-2025                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

BindGlobal("_RecogsFunnyNameFormatterFunction",
function(st)
  if IsEmpty(st) then
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

_STANDREWSMATHS := Concatenation(["Mathematical Institute, North Haugh, ",
                                  "St Andrews, Fife, KY16 9SS, Scotland"]);
_STANDREWSCS := Concatenation(["Jack Cole Building, North Haugh, ",
                               "St Andrews, Fife, KY16 9SX, Scotland"]);

SetPackageInfo(rec(
PackageName := "Semigroups",
Subtitle := "A package for semigroups and monoids",
Version := "5.5.3",
Date := "17/07/2025",  # dd/mm/yyyy format
License := "GPL-3.0-or-later",

ArchiveFormats := ".tar.gz",

SourceRepository := rec(
    Type := "git",
    URL := Concatenation("https://github.com/semigroups/", ~.PackageName),
),

IssueTrackerURL := Concatenation(~.SourceRepository.URL, "/issues"),
PackageWWWHome  := Concatenation("https://semigroups.github.io/",
                                 ~.PackageName),
README_URL      := Concatenation(~.PackageWWWHome, "/README.md"),
PackageInfoURL  := Concatenation(~.PackageWWWHome, "/PackageInfo.g"),
ArchiveURL      := Concatenation(~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", "semigroups-", ~.Version),

Persons := [
  rec(
    LastName      := "Mitchell",
    FirstNames    := "James",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jdm3@st-andrews.ac.uk",
    WWWHome       := "https://jdbm.me",
    PostalAddress := _STANDREWSMATHS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Anagnostopoulou-Merkouri",
    FirstNames    := "Marina",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "marina.anagnostopoulou-merkouri@bristol.ac.uk",
    Place         := "Bristol",
    Institution   := "University of Bristol",
    WWWHome       := "https://marinaanagno.github.io"),

  rec(
    LastName      := "Breuer",
    FirstNames    := "Thomas",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "sam@math.rwth-aachen.de",
    WWWHome       := "https://www.math.rwth-aachen.de/~Thomas.Breuer/"),

  rec(
    LastName      := "Burrell",
    FirstNames    := "Stuart",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "stuartburrell1994@gmail.com",
    WWWHome       := "https://stuartburrell.github.io"),

  rec(
    LastName      := "Cirpons",
    FirstNames    := "Reinis",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "rc234@st-andrews.ac.uk",
    PostalAddress := _STANDREWSMATHS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews",
    WWWHome       := "https://reinisc.id.lv/"),

  rec(
    LastName      := "Conti-Leslie",
    FirstNames    := "Tom",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "tom.contileslie@gmail.com",
    WWWHome       := "https://tomcontileslie.com/"),

  rec(
    LastName      := "Edwards",
    FirstNames    := "Joseph",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "jde1@st-andrews.ac.uk",
    PostalAddress := _STANDREWSMATHS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews",
    WWWHome       := "https://github.com/Joseph-Edwards"),

  rec(
    LastName      := "Egri-Nagy",
    FirstNames    := "Attila",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "attila@egri-nagy.hu",
    WWWHome       := "http://www.egri-nagy.hu",
    Place         := "Akita, Japan",
    Institution   := "Akita International University"),

  rec(
    LastName      := "Elliott",
    FirstNames    := "Luke",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "le27@st-andrews.ac.uk",
    WWWHome       := "https://le27.github.io/Luke-Elliott/",
    PostalAddress := _STANDREWSMATHS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Flores Brito",
    FirstNames    := "Fernando",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "ffloresbrito@gmail.com"),

  rec(
    LastName      := "Froehlich",
    FirstNames    := "Tillman",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "trf1@st-andrews.ac.uk"),

  rec(
    LastName      := "Ham",
    FirstNames    := "Nick",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "nicholas.charles.ham@gmail.com",
    WWWHome       := "https://n-ham.github.io",
    Place         := "Hobart, Tasmania",
    Institution   := "University of Tasmania"),

  rec(
    LastName      := "Hancock",
    FirstNames    := "Robert",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "robert.hancock@maths.ox.ac.uk",
    WWWHome       := "https://sites.google.com/view/robert-hancock/"),

  rec(
    LastName      := "Horn",
    FirstNames    := "Max",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "mhorn@rptu.de",
    WWWHome       := "https://www.quendi.de/math",
    PostalAddress := Concatenation(
                       "Fachbereich Mathematik, ",
                       "RPTU Kaiserslautern-Landau, ",
                       "Gottlieb-Daimler-Straße 48, ",
                       "67663 Kaiserslautern, ",
                       "Germany"),
    Place         := "Kaiserslautern, Germany",
    Institution   := "RPTU Kaiserslautern-Landau"),

  rec(
    LastName      := "Jefferson",
    FirstNames    := "Christopher",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "caj21@st-andrews.ac.uk",
    WWWHome       := "https://heather.cafe/",
    PostalAddress := _STANDREWSCS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Jonusas",
    FirstNames    := "Julius",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "j.jonusas@gmail.com",
    WWWHome       := "http://julius.jonusas.work",
    Place         := "Brussels, Belgium"),

  rec(
    LastName      := "Nagpal",
    FirstNames    := "Chinmaya",
    IsAuthor      := true,
    IsMaintainer  := false),

  rec(
    LastName      := "Konovalov",
    FirstNames    := "Olexandr",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "obk1@st-andrews.ac.uk",
    WWWHome       := "https://olexandr-konovalov.github.io/",
    PostalAddress := _STANDREWSCS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

  rec(
    LastName      := "Konstantinidi",
    FirstNames    := "Artemis",
    IsAuthor      := true,
    IsMaintainer  := false),

  rec(
    LastName      := "Kwon",
    FirstNames    := "Hyeokjun",
    IsAuthor      := true,
    IsMaintainer  := false),

  rec(
    LastName      := "Pasechnik",
    FirstNames    := "Dima V.",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "dmitrii.pasechnik@cs.ox.ac.uk",
    WWWHome       := "http://users.ox.ac.uk/~coml0531/",
    PostalAddress := Concatenation(["Pembroke College, ",
                                    "St. Aldates, ",
                                    "Oxford OX1 1DW, ",
                                    "England"]),
    Place         := "Oxford",
    Institution   := "University of Oxford"),

  rec(
    LastName      := "Pfeiffer",
    FirstNames    := "Markus",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "markus.pfeiffer@morphism.de",
    WWWHome       := "https://markusp.morphism.de/"),

  rec(
    LastName      := "Russell",
    FirstNames    := "Christopher",
    IsAuthor      := true,
    IsMaintainer  := false),

  rec(
    LastName      := "Schmidt",
    FirstNames    := "Jack",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "jack.schmidt@uky.edu",
    WWWHome       := "https://www.ms.uky.edu/~jack/"),

  rec(
    LastName      := "Siccha",
    FirstNames    := "Sergio",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "sergio.siccha@gmail.com"),

  rec(
    LastName      := "Smith",
    FirstNames    := "Finn",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "fls3@st-andrews.ac.uk",
    PostalAddress := _STANDREWSMATHS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews",
    WWWHome       := "https://flsmith.github.io/"),

  rec(
    LastName      := "Spiers",
    FirstNames    := "Ben",
    IsAuthor      := true,
    IsMaintainer  := false),

  rec(
    LastName      := "Thiéry",
    FirstNames    := "Nicolas",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "nthiery@users.sf.net",
    WWWHome       := "https://nicolas.thiery.name/",
    Place         := "Paris",
    Institution   := "Université Paris Sud"),

  rec(
    LastName      := "Tsalakou",
    FirstNames    := "Maria",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "mt200@st-andrews.ac.uk",
    WWWHome       := "https://mariatsalakou.github.io/",
    PostalAddress := _STANDREWSMATHS,
    Place         := "St Andrews",
    Institution   := "University of St Andrews"),

   rec(
     LastName      := "Wensley",
     FirstNames    := "Chris",
     IsAuthor      := true,
     IsMaintainer  := false,
     Email         := "cdwensley.maths@btinternet.com"),

   rec(
     LastName      := "Whyte",
     FirstNames    := "Murray",
     IsAuthor      := true,
     IsMaintainer  := false,
     Email         := "mw231@st-andrews.ac.uk",
     PostalAddress := _STANDREWSMATHS,
     Place         := "St Andrews",
     Institution   := "University of St Andrews"),

   rec(
     LastName      := "Wilson",
     FirstNames    := "Wilf A.",
     IsAuthor      := true,
     IsMaintainer  := false,
     Email         := "gap@wilf-wilson.net",
     WWWHome       := "https://wilf.me"),

   rec(
     LastName      := "Yang",
     FirstNames    := "Tianrun",
     IsAuthor      := true,
     IsMaintainer  := false),

   rec(
     LastName      := "Young",
     FirstNames    := "Michael",
     IsAuthor      := true,
     IsMaintainer  := false,
     Email         := "mct25@st-andrews.ac.uk",
     WWWHome       := "https://mtorpey.github.io/",
     PostalAddress := _STANDREWSCS,
     Place         := "St Andrews",
     Institution   := "University of St Andrews"),

   rec(
     LastName      := "Zickgraf",
     FirstNames    := "Fabian",
     IsAuthor      := true,
     IsMaintainer  := false,
     Email         := "f.zickgraf@dashdos.com")],

Status := "deposited",

PackageDoc := rec(
  BookName  := "Semigroups",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Semigroups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.12.1",
  NeededOtherPackages := [["datastructures", ">=0.2.5"],
                          ["digraphs", ">=1.6.2"],
                          ["genss", ">=1.6.5"],
                          ["images", ">=1.3.1"],
                          ["IO", ">=4.5.1"],
                          ["orb", ">=4.8.2"]],
  SuggestedOtherPackages := [["GAPDoc", ">=1.6.3"],
                             ["AutoDoc", ">=2020.08.11"]],

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
  if not IsKernelExtensionAvailable("semigroups") then
    LogPackageLoadingMessage(PACKAGE_WARNING,
                             ["the kernel module is not compiled, ",
                              "the package cannot be loaded."]);
    return fail;
  fi;
  return true;
end,

Autoload := false,
TestFile := "tst/teststandard.g",
Keywords := ["transformation semigroups", "partial permutations",
             "inverse semigroups", "Green's relations",
             "free inverse semigroup", "partition monoid", "bipartitions",
             "Rees matrix semigroups"],

AutoDoc := rec(
    TitlePage := rec(
        Copyright := """&copyright; by J. D. Mitchell et al.<P/>
        &Semigroups; is free software; you can redistribute it and/or modify
        it, under the terms of the GNU General Public License, version 3 of
        the License, or (at your option) any later, version.""",
        Abstract := """
          The Semigroups package is a GAP package for semigroups, and monoids.
          There are particularly efficient methods for finitely presented
          semigroups and monoids, and for semigroups and monoids consisting of
          transformations, partial permutations, bipartitions, partitioned
          binary relations, subsemigroups of regular Rees 0-matrix semigroups,
          and matrices of various semirings including boolean matrices,
          matrices over finite fields, and certain tropical matrices.

          Semigroups contains efficient methods for creating semigroups,
          monoids, and inverse semigroups and monoids, calculating their
          Green's structure, ideals, size, elements, group of units, small
          generating sets, testing membership, finding the inverses of a
          regular element, factorizing elements over the generators, and so on.
          It is possible to test if a semigroup satisfies a particular
          property, such as if it is regular, simple, inverse, completely
          regular, and a large number of further properties.

          There are methods for finding presentations for a semigroup, the
          congruences of a semigroup, the maximal subsemigroups of a finite
          semigroup, smaller degree partial permutation representations, and
          the character tables of inverse semigroups. There are functions for
          producing pictures of the Green's structure of a semigroup, and for
          drawing graphical representations of certain types of elements.""",

        Acknowledgements := """
        The authors of the &SEMIGROUPS; package would like to thank:
        <List>
          <Mark>
            Manuel Delgado
          </Mark>
          <Item>
            who contributed to the function <Ref Oper = "DotString"/>.
          </Item>

          <Mark>
            Casey Donoven and Rhiannon Dougall
          </Mark>
          <Item>
            for their contribution to the development of the algorithms for
            maximal subsemigroups and smaller degree partial permutation
            representations.
          </Item>

          <Mark>
            James East
          </Mark>
          <Item>
            who contributed to the part of the package relating to
            bipartitions. We also thank the University of Western Sydney for
            their support of the development of this part of the package.
          </Item>

          <Mark>
            Zak Mesyan
          </Mark>
          <Item>
            who contributed to the code for graph inverse semigroups; see
            Section <Ref Sect="Graph inverse semigroups"/>.
          </Item>

          <Mark>
            Yann P&#233;resse and Yanhui Wang
          </Mark>
          <Item>
            who contributed to the attribute <Ref Attr = "MunnSemigroup"/>.
          </Item>

          <Mark>
            Jhevon Smith and Ben Steinberg
          </Mark>
          <Item>
            who contributed the function
            <Ref Attr = "CharacterTableOfInverseSemigroup"/>.
          </Item>
        </List>
        We would also like to acknowledge the support of: EPSRC grant number
        GR/S/56085/01; the Carnegie Trust for the Universities of Scotland
        for funding the PhD scholarships of Julius Jonu&#353;as and Wilf A.
        Wilson when they worked on this project; the Engineering and Physical
        Sciences Research Council (EPSRC) for funding the PhD scholarships of
        F. Smith (EP/N509759/1) and M. Young (EP/M506631/1) when they worked on
        this project.""")),

        AbstractHTML := ~.AutoDoc.TitlePage.Abstract));

MakeReadWriteGlobal("_RecogsFunnyWWWURLFunction");
MakeReadWriteGlobal("_RecogsFunnyNameFormatterFunction");
Unbind(_RecogsFunnyWWWURLFunction);
Unbind(_RecogsFunnyNameFormatterFunction);
Unbind(_STANDREWS);
