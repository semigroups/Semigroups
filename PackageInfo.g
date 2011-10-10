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
Version := "0.3",
Date := "10/10/2011",
ArchiveURL := 
          "https://bitbucket.org/zen154115/citrus/downloads/citrus0r3",
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
      "http://www-groups.mcs.st-and.ac.uk/~jamesm/citrus/citrus0r3.tar.gz",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",  
  SixFile   := "doc/manual.six",
  LongTitle := "Citrus - ComputIng with TransfoRmation semigrUopS",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.5",
  NeededOtherPackages := [["orb", "3.7"]],
  SuggestedOtherPackages := [["gapdoc", ">=1.3"]], 
  ExternalConditions := []),
  AvailabilityTest := ReturnTrue,
  Autoload := false,
  TestFile := "tst/testinstall.tst",
  Keywords := ["transformation semigroups", "green's relations"]
));
