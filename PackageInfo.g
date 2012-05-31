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
##  <!ENTITY VERSION "0.9">
##  <!ENTITY ORBVERS "4.2">
##  <!ENTITY IOVERS "4.1">
##  <!ENTITY GRAPEVERS "4.5">
##  <!ENTITY ARCHIVENAME "citrus-0.9">
##  <!ENTITY COPYRIGHTYEARS "2011-12">
##  <#/GAPDoc>

SetPackageInfo( rec(
PackageName := "Citrus",
Subtitle := "ComputIng wiTh semigRoUps and monoidS",
Version := "0.9",
Date := "31/05/2012",
ArchiveURL := 
          "https://bitbucket.org/zen154115/citrus/downloads/citrus-0.9",
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
  "The <span class=\"pkgname\">Citrus</span> package, is a ",
  "<span class=\"pkgname\">GAP</span>  package  for transformation", 
  "monoids and related objects."),

PackageWWWHome := "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus",
               
PackageDoc := rec(
  BookName  := "Citrus",
  Archive := 
      "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus/citrus-0.9.tar.gz",
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
# JDM can't think of sensible test for the binary :(
#function()  
#    if Filename(DirectoriesPackagePrograms("citrus"), "bin") = fail then
#      LogPackageLoadingMessage(PACKAGE_ERROR, "the binary is not available");
#    fi;
#    return true; 
#  end,
  Autoload := false,
  TestFile := "tst/testinstall.tst",
  Keywords := ["transformation semigroups", "partial permutations",
  "inverse semigroups", "Green's relations"]
));
