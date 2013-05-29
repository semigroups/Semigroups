#############################################################################
##
#W  PackageInfo.g
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY VERSION "1.0">
##  <!ENTITY ORBVERS "4.6">
##  <!ENTITY IOVERS "4.1">
##  <!ENTITY GRAPEVERS "4.5">
##  <!ENTITY ARCHIVENAME "semigroups-1.0">
##  <!ENTITY COPYRIGHTYEARS "2013">
##  <#/GAPDoc>

SetPackageInfo( rec(
PackageName := "Semigroups",
Subtitle := "Methods for Semigroups",
Version := "1.0",
Date := "06/2012",
ArchiveURL := 
          "https://bitbucket.org/zen154115/semigroups/downloads/semigroups-1.0",
ArchiveFormats := ".tar.gz",
Persons := [
  rec( 
    LastName      := "Mitchell",
    FirstNames    := "J. D.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jdm3@st-and.ac.uk",
    WWWHome       := "tinyurl.com/jdmitchell",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  )],

Status := "deposited",

README_URL := 
  "http://www-groups.mcs.st-and.ac.uk/~jamesm/semigroups/README",
PackageInfoURL := 
  "http://www-groups.mcs.st-and.ac.uk/~jamesm/semigroups/PackageInfo.g",

AbstractHTML := Concatenation( 
  "The Semigroups package is a ",
  "GAP  package for computing with semigroups ",
  "of transformations and partial permutations. Semigroups contains more ",
  "efficient methods than those available in the GAP library (and in many ",
  "cases more efficient than any other software) for creating semigroups of ",
  "transformations and partial permutations, calculating their Green's ",
  "classes, size, elements, group of units, minimal ideal, small generating ",
  "sets, testing membership, finding the inverses of a regular element, ",
  "factorizing elements over the generators, and many more. It is also ", 
  "possible to test if a semigroup ", 
  "satisfies a particular property, such as if it is regular, simple, " ,
  "inverse, completely regular, and a variety of further properties. ",
  "Several catalogues of examples are provided, such as generators for the ",
  "endomorphism monoids of every connected graph with at most 8 vertices ", 
  "and generators for the endomorphism monoids of the non-abelian groups with ",
  "order at most 64."),

PackageWWWHome := "http://www-groups.mcs.st-and.ac.uk/~jamesm/semigroups",
               
PackageDoc := rec(
  BookName  := "Semigroups",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",  
  SixFile   := "doc/manual.six",
  LongTitle := "Semigroups - Methods for semigroups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.5.4",#this must be changed to 4.7
  NeededOtherPackages := [["orb", ">=4.6"], ["io", ">=4.2"], 
  ["genss", ">=1.5"]],
  SuggestedOtherPackages := [["gapdoc", ">=1.5.1"], ["grape", ">=4.5"]], 
  ExternalConditions := []),
  AvailabilityTest := ReturnTrue, 
  Autoload := false,
  TestFile := "tst/testinstall.tst",
  Keywords := ["transformation semigroups", "partial permutations",
  "inverse semigroups", "Green's relations"]
));
