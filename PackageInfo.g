############################################################################
##
#W  PackageInfo.g
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY VERSION "2.2">
##  <!ENTITY GAPVERS "4.7.6">
##  <!ENTITY ORBVERS "4.7.3">
##  <!ENTITY IOVERS "4.4.4">
##  <!ENTITY GRAPEVERS "4.5">
##  <!ENTITY GENSSVERS "1.5">
##  <!ENTITY ARCHIVENAME "semigroups-2.2">
##  <!ENTITY COPYRIGHTYEARS "2011-15">
##  <#/GAPDoc>

RecogsFunnyNameFormatterFunction := function(st)
  if Length(st) = 0 then 
      return st;
  else
      return Concatenation(" (",st,")");
  fi;
end;

RecogsFunnyWWWURLFunction := function(re)
  if IsBound(re.WWWHome) then
      return re.WWWHome;
  else
      return "";
  fi;
end;

SetPackageInfo( rec(
PackageName := "Semigroups",
Subtitle := "Methods for Semigroups",
Version := "2.2",
Date := "20/01/2015",
ArchiveURL := "http://tinyurl.com/jdmitchell/semigroups/semigroups-2.2",
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
  ),
  
  rec( 
    LastName      := "Delgado",
    FirstNames    := "Manuel",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "mdelgado@fc.up.pt",
    WWWHome       := "http://cmup.fc.up.pt/cmup/mdelgado/",
    Place         := "Porto",
    Institution   := "Universidade do Porto"
  ),
  
  rec( 
    LastName      := "East",
    FirstNames    := "J.",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "j.east@uws.edu.au",
    WWWHome       := "http://www.uws.edu.au/staff_profiles/uws_profiles/doctor_james_east",
    Place         := "Sydney",
    Institution   := "University of Western Sydney"
  ),
  
  rec(
    LastName      := "Egri-Nagy",
    FirstNames    := "Attila",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "attila@egri-nagy.hu",
    WWWHome       := "http://www.egri-nagy.hu",
    PostalAddress := Concatenation( [
                       "University of Hertfordshire\n",
                       "STRI\n",
                       "College Lane\n",
                       "AL10 9AB\n",
                       "United Kingdom" ] ),
    Place         := "Hatfield, Herts",
    Institution   := "UH"
  ),
  rec( 
    LastName      := "Jonusas",
    FirstNames    := "J.",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "jj252@st-and.ac.uk",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  ),
        
   rec(
    LastName      := "Pfeiffer",
    FirstNames    := "Markus",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "markus.pfeiffer@morphism.de",
    WWWHome       := "http://www.morphism.de/~markusp/",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  ), 

  rec( 
    LastName      := "Steinberg",
    FirstNames    := "B.",
    IsAuthor      := true,
    IsMaintainer  := false,
    WWWHome       := "http://www.sci.ccny.cuny.edu/~benjamin/",
  ),
  
  rec( 
    LastName      := "Smith",
    FirstNames    := "J.",
    IsAuthor      := true,
    IsMaintainer  := false,
    WWWHome       := "http://math.sci.ccny.cuny.edu/people?name=Jhevon_Smith",
  ),

  rec( 
    LastName      := "Torpey",
    FirstNames    := "M.",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "mct25@st-and.ac.uk",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  ),
  
  rec( 
    LastName      := "Wilson",
    FirstNames    := "W.",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "waw7@st-and.ac.uk",
    WWWHome       := "http://www-circa.mcs.st-and.ac.uk/~waw7",
    PostalAddress := Concatenation( [
                       "Mathematical Institute,",
                       " North Haugh,", " St Andrews,", " Fife,", " KY16 9SS,", 
                       " Scotland"] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  )],

Status := "deposited",

README_URL := 
  "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/semigroups/README",
PackageInfoURL := 
  "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/semigroups/PackageInfo.g",

AbstractHTML := 
"<p>The <strong class=\"pkg\">Semigroups</strong> package is a <strong class=\"pkg\">GAP</strong> package containing methods for semigroups, monoids, and inverse semigroups, principally of transformations, partial permutations, bipartitions, subsemigroups of regular Rees 0-matrix semigroups, free inverse semigroups, and free bands.</p>\n\n <p><strong class=\"pkg\">Semigroups</strong> contains more efficient methods than those available in the <strong class=\"pkg\">GAP</strong> library (and in many cases more efficient than any other software) for creating semigroups, monoids, and inverse semigroup, calculating their Green's structure, ideals, size, elements, group of units, small generating sets, testing membership, finding the inverses of a regular element, factorizing elements over the generators, and many more. It is also possible to test if a semigroup satisfies a particular property, such as if it is regular, simple, inverse, completely regular, and a variety of further properties.</p>\n\n <p>There are methods for finding congruences of certain types of semigroups, the normalizer of a semigroup in a permutation group, the maximal subsemigroups of a finite semigroup, and smaller degree partial permutation representations of inverse semigroups. There are functions for producing pictures of the Green's structure of a semigroup, and for drawing bipartitions.</p>", 

PackageWWWHome := "http://www-groups.mcs.st-andrews.ac.uk/~jamesm/semigroups.php",
               
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
  GAP := ">=4.7.6",
  NeededOtherPackages := [ ["orb", ">=4.7.3"], ["io", ">=4.4.4"] ],
  SuggestedOtherPackages := [["gapdoc", ">=1.5.1"], ["grape", ">=4.5"],
  ["genss", ">=1.5"]],
  ExternalConditions := []),

  BannerString := Concatenation(
  "----------------------------------------------------------------------",
  "-------\n",
  "Loading  Semigroups ", ~.Version, " - methods for semigroups\n",
  "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
        " (", ~.Persons[1].WWWHome, ")\n",
  "with contributions by:\n",
  Concatenation(Concatenation(List(~.Persons{[2..Length(~.Persons)-1]},
       p->["     ",p.FirstNames," ",p.LastName,
       RecogsFunnyNameFormatterFunction(
         RecogsFunnyWWWURLFunction(p)),",\n"]))),
  " and ",~.Persons[Length(~.Persons)].FirstNames," ",
  ~.Persons[Length(~.Persons)].LastName,
  RecogsFunnyNameFormatterFunction(
    RecogsFunnyWWWURLFunction(~.Persons[Length(~.Persons)])),".\n",
  "-----------------------------------------------------------------------",
  "------\n"
),

  AvailabilityTest := ReturnTrue, 
  Autoload := false,
  TestFile := "tst/testinstall.tst",
  Keywords := ["transformation semigroups", "partial permutations",
  "inverse semigroups", "Green's relations", "free inverse semigroup", 
  "partition monoid", "bipartitions", "Rees matrix semigroups"]
));
