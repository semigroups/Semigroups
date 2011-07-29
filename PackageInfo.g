#############################################################################
##
#W  PackageInfo.g
#Y  Copyright (C) 2011                                      James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SetPackageInfo( rec(
PackageName := "Citrus",
Subtitle := "ComputIng with Transformation semigRoUps and monoidS",
Version := "0.1",
Date := "2011",
ArchiveURL := 
          "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus",
ArchiveFormats := ".tar.gz .tar.bz2",
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
Status := "undeposited",

README_URL := 
  "http://tinyurl.com/citrus/README.txt",
PackageInfoURL := 
  "http://tinyurl.com/citrus/PackageInfo.g",

AbstractHTML := Concatenation( 
  "The <span class=\"pkgname\">Citrus</span> package, is a ",
  "<span class=\"pkgname\">GAP</span>  package  for transformation", 
  "monoids and related objects."),

PackageWWWHome := "http://tinyurl.com/citrus",
               
PackageDoc := rec(
  BookName  := "Citrus",
  Archive := 
      "http://tinyurl.com/citrus",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",  
  SixFile   := "doc/manual.six",
  LongTitle := "Citrus - ComputIng with TransfoRmation semigrUopS",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.4.12",
  NeededOtherPackages := [["orb", "3.7"]],
  SuggestedOtherPackages := [["gapdoc", ">=1.1"]], 
  ExternalConditions := []),
  AvailabilityTest := ReturnTrue,
  Autoload := false,
  TestFile := "tst/testall.g",
  Keywords := ["transformation semigroups", "green's relations"]
));
