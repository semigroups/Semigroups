#############################################################################
##
#W  PackageInfo.g
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

SetPackageInfo( rec(
PackageName := "MONOID",
Subtitle := "Computing with transformation semigroups and monoids",
Version := "4.0",
Date := "08/05/2010",
ArchiveURL := 
          "http://tinyurl.com/monoid4/monoid3r1p4",
ArchiveFormats := ".tar.gz .tar.bz2",
Persons := [
  rec( 
    LastName      := "Mitchell",
    FirstNames    := "James",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jdm3@st-and.ac.uk",
    WWWHome       := "http://tinyurl.com/monoid4",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  )],
Status := "deposited",

README_URL := 
  "http://tinyurl.com/monoid4/README.txt",
PackageInfoURL := 
  "http://tinyurl.com/monoid4/PackageInfo.g",

AbstractHTML := Concatenation( 
  "The <span class=\"pkgname\">MONOID</span> package, is a ",
  "<span class=\"pkgname\">GAP</span>  package  for transformation", 
  "monoids and related objects."),

PackageWWWHome := "http://tinyurl.com/monoid4",
               
PackageDoc := rec(
  BookName  := "MONOID",
  Archive := 
      "http://tinyurl.com/monoid4",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",  
  SixFile   := "doc/manual.six",
  LongTitle := "MONOID: computing with transformation semigroups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.4.12",
  NeededOtherPackages := [["orb", "3.6"]],
  SuggestedOtherPackages := [["gapdoc", ">=1.1"]], 
  ExternalConditions := []),
AvailabilityTest := ReturnTrue,
Autoload := false,
TestFile := "tst/testall.g",
Keywords := ["transformation semigroups", "green's relations"]

));