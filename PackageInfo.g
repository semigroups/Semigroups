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
##  <!ENTITY VERSION "0.99">
##  <!ENTITY ORBVERS "4.2">
##  <!ENTITY IOVERS "4.1">
##  <!ENTITY GRAPEVERS "4.5">
##  <!ENTITY ARCHIVENAME "citrus-0.99">
##  <!ENTITY COPYRIGHTYEARS "2011-12">
##  <#/GAPDoc>

SetPackageInfo( rec(
PackageName := "Citrus",
Subtitle := "Computing with Semigroups of Transformations and Partial Permutations",
Version := "0.99",
Date := "15/06/2012",
ArchiveURL := 
          "https://bitbucket.org/zen154115/citrus/downloads/citrus-0.99",
ArchiveFormats := ".tar.gz",
Persons := [
  rec( 
    LastName      := "Mitchell",
    FirstNames    := "J. D.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jdm3@st-and.ac.uk",
    WWWHome       := "http://tinyurl.com/jdmitchell",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  )],

Status := "deposited",

README_URL := 
  "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus/README",
PackageInfoURL := 
  "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus/PackageInfo.g",

AbstractHTML := Concatenation( 
  "The Citrus package is a ",
  "GAP  package for computing with semigroups ",
  "of transformations and partial permutations. Citrus contains more ",
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
  "endomorphism monoids of every connected graphs with at most 8 vertices ", 
  "and generators for the endomorphism monoids of the non-abelian groups with ",
  "order at most 64."),

PackageWWWHome := "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus",
               
PackageDoc := rec(
  BookName  := "Citrus",
  Archive := 
      "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus/citrus-0.99.tar.gz",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",  
  SixFile   := "doc/manual.six",
  LongTitle := "Citrus - ComputIng wiTh semigRUopS",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.5.3",
  NeededOtherPackages := [["orb", ">=4.2"], ["io", ">=4.1"]],
  SuggestedOtherPackages := [["gapdoc", ">=1.5.1"], ["grape", ">=4.5"]], 
  ExternalConditions := []),
  AvailabilityTest := ReturnTrue, 
  Autoload := false,
  TestFile := "tst/testinstall.tst",
  Keywords := ["transformation semigroups", "partial permutations",
  "inverse semigroups", "Green's relations"]
));
