##
## PackageInfo.g
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##

SetPackageInfo( rec(
PackageName := "MONOID",
Subtitle := "Computing with transformation semigroups and monoids",
Version := "3.1.3",
Date := "07/11/2008",
ArchiveURL := 
          "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/monoid/monoid3r1p3",
ArchiveFormats := ".tar.gz",
Persons := [
  rec( 
    LastName      := "Mitchell",
    FirstNames    := "James",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "jdm3@st-and.ac.uk",
    WWWHome       := "http://www-groups.mcs.st-and.ac.uk/~jamesm",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  )],
Status := "deposited",

# CommunicatedBy := "Mike Atkinson (St. Andrews)",
# AcceptDate := " Tue Mar 13 16:32:52 GMT 2007 ",

README_URL := 
  "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/monoid/README.txt",
PackageInfoURL := 
  "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/monoid/PackageInfo.g",

AbstractHTML := 
  "The <span class=\"pkgname\">MONOID</span> package, is a <span class=\"pkgname\">GAP</span>  package  for transformation monoids and related objects.",

PackageWWWHome := "http://www-groups.mcs.st-and.ac.uk/~jamesm/monoid",
               
PackageDoc := rec(
  BookName  := "MONOID",
  Archive := 
      "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/monoid/",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",  
  SixFile   := "doc/manual.six",
  LongTitle := "MONOID: computing with transformation semigroups",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.4.10",
  NeededOtherPackages := [],
  SuggestedOtherPackages := [["genss", "0.96"], ["grape", ">=4.2"], ["gapdoc", ">=1.1"]],
  ExternalConditions := []),
AvailabilityTest := ReturnTrue,
BannerString := Concatenation( 
  "Loading MONOID ", ~.Version, "\n",
  "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
        " (", ~.Persons[1].WWWHome, ")\n",
 "For help, type: ?the monoid package \n" ),
Autoload := false,
TestFile := "tst/testall.g",
Keywords := ["transformation semigroups", "green's relations"]

));