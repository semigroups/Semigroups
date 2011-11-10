#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains functions with shorter names than some library functions
# commonly used in the citrus package. 

# new for 0.2! - CitrusDefaultMem - "for no argument"
#############################################################################
# Notes: for semigroups with 10000s to 100000s of elements (this is the 
# default).

InstallGlobalFunction(CitrusDefaultMem,
function()
  local len;
  len:=CitrusHashLen;
  len!.dclass_data:=1009;
  len!.kers:=1009;
  len!.rclass_data:=20287;
  len!.imgs:=503;
  len!.rclassreps_orb:=72379;
  Info(InfoWarning, 2, "for semigroups with tens to hundreds of thousands of",
   " elements");
  return true;
end);

# new for 0.2! - CitrusHiMem - "for no argument"
#############################################################################
# Notes: for semigroups with 100000s to millions of elements.

InstallGlobalFunction(CitrusHiMem,
function()
  local len;
  len:=CitrusHashLen;
  len!.dclass_data:=100003;
  len!.kers:=115979;
  len!.rclass_data:=649403;
  len!.imgs:=1009;
  len!.rclassreps_orb:=2316451;
  Info(InfoWarning, 2, "for semigroups with hundreds of thousands to millions",
   " of elements");
  return true;
end);

# new for 0.2! - CitrusLoMem - "for no argument"
#############################################################################
# Notes: for semigroups with 1000s to 10000s of elements.

InstallGlobalFunction(CitrusLoMem,
function()
  local len;
  len:=CitrusHashLen;
  len!.dclass_data:=211;
  len!.kers:=211;
  len!.rclass_data:=211;
  len!.imgs:=67;
  len!.rclassreps_orb:=383;
  Info(InfoWarning, 2, "for semigroups with thousands to tens of thousands of ",
  "elements");
  return true;
end);

# new for 0.2! - CitrusVeryLoMem - "for no argument"
#############################################################################
# Notes: for semigroups with 1s to 1000s of elements.

InstallGlobalFunction(CitrusVeryLoMem,
function()
  local len;
  len:=CitrusHashLen;
  len!.dclass_data:=11;
  len!.kers:=53;
  len!.rclass_data:=29;
  len!.imgs:=37;
  len!.rclassreps_orb:=53;
  Info(InfoWarning, 2, "for semigroups with less than a thousand elements");
  return true;
end);

# mod for 0.4! - CitrusMakeDoc - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusMakeDoc, 
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("citrus")[1]!.
   InstallationPath, "/doc"), "citrus.xml", 
   ["utils.xml", "greens.xml", "orbits.xml", "properties.xml",
     "transform.xml", "../PackageInfo.g"], "citrus", "MathJax");;
end);

# new for 0.4! - CitrusMathJaxDefault - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusMathJaxDefault, 
function()
GAPDoc2HTMLProcs.Head1MathJax:=Concatenation(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<!DOCTYPE html PUBLIC",
"\"-//W3C//DTD \ XHTML 1.0 Strict//EN\"\n",
"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dt\"",
"d\">\n\n<html xmlns=\"http://www.w3.org/1999/xhtml\"",
"xml:lang=\"en\">\n<head>\n<script",
" type=\"text/javascript\"\n",
"src=\"http://cdn.mathjax.org/mathjax/latest/MathJax",
".js?config=TeX-AMS-MML_HTMLorMML\">\n</script>\n<title>GAP (");
Info(InfoWarning, 1, "don't forget to run CitrusMakeDoc()");
end);

# new for 0.4! - CitrusMathJaxLocal - "for a path to the MathJax folder"
#############################################################################

InstallGlobalFunction(CitrusMathJaxLocal, 
function(arg)
  local path;

  if Length(arg)>0 then 
    path:= arg[1];
  else
    path:= "";
  fi;

  GAPDoc2HTMLProcs.Head1MathJax:=Concatenation(
  "<?xml version=\"1.0\"",
  "encoding=\"UTF-8\"?>\n\n<!DOCTYPE html PUBLIC \"-//W3C/\"",
  "/DTD XHTML 1.0 Strict//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1\"",
  "-strict.dtd\">\n\n<html xmlns=\"http://www.w3.org/1999/xhtml\"",
  "xml:lang=\"en\"\ >\n<head>\n<script type=\"text/javascript\"",
  "\n src=\"", path, "/MathJax/MathJax.js?config=default",
  "\">\n</script>\n<title>GAP\ (");
  Info(InfoWarning, 1, "don't forget to run CitrusMakeDoc()");
end);

# mod for 0.4! - CitrusTestAll - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestAll, 
function()
  Print(
  "Reading all .tst files in the directory citrus/tst/...\n\n"); 
  Read(Filename(DirectoriesPackageLibrary("citrus","tst"),"testall.g"));;
end);

# new for 0.1! - CitrusTestInstall - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestInstall, 
function()
  ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),
   "testinstall.tst"));;
end);

# new for 0.1! - CitrusTestManualExamples - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestManualExamples,
function()
  SizeScreen([80]); SetInfoLevel(InfoWarning, 0);
  TestManualExamples(Concatenation(PackageInfo("citrus")[1]!.
     InstallationPath, "/doc"), "citrus.xml", 
     ["utils.xml", "greens.xml", "orbits.xml", "properties.xml",
      "transform.xml", "../PackageInfo.g"]);
end);

# new for 0.1! - DClass - "for a trans. semi and trans. or Green's class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class or L-class or R-class.

InstallGlobalFunction(DClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensDClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensRClass(arg[1]) then 
    return DClassOfRClass(arg[1]);
  elif Length(arg)=1 and IsGreensLClass(arg[1]) then 
    return DClassOfLClass(arg[1]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return DClassOfHClass(arg[1]);
  fi;
  
  Info(InfoWarning, 1, "Usage: (trans. semigp. and trans.) or H-class or",
  " L-class or R-class.");
  return fail;
end);

# new for 0.1! - DClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(DClassNC,
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1])
   and IsTransformation(arg[2]) then
    return GreensDClassOfElementNC(arg[1], arg[2]);
  fi;

  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 0.1! - Degree - "for a transformation"
#############################################################################
# Notes: returns DegreeOfTransformation.

InstallOtherMethod(Degree, "for a transformation",
[IsTransformation], f-> DegreeOfTransformation(f));

# new for 0.1! - Degree - "for a transformation semigroup"
#############################################################################
# Notes: returns DegreeOfTransformationSemigroup.

InstallOtherMethod(Degree, "for a transformation semigroup",
[IsTransformationSemigroup], s-> DegreeOfTransformationSemigroup(s));

# new for 0.1! - Generators - "for a semigroup or monoid"
############################################################################
# Notes: returns the monoid generators of a monoid, and the semigroup 
# generators of a semigroup. 

InstallOtherMethod(Generators, "for a semigroup or monoid",
[IsSemigroup],
function(s)

  if IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

# new for 0.1! - HClass - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(HClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensHClassOfElement(arg[1], arg[2]);
  fi;

  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 0.1! - HClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(HClassNC, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensHClassOfElementNC(arg[1], arg[2]);
  fi;

  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 0.1! - LClass - "for a trans. semi. and trans. or H-class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(LClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensLClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return LClassOfHClass(arg[1]);
  fi;
  
  Info(InfoWarning, 1, "Usage: (trans. semigp. and trans.) or H-class.");
  return fail;
end);

# new for 0.1! - LClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(LClassNC, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensLClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 0.1! - RandomTransformationSemigroup 
#############################################################################

InstallGlobalFunction(RandomTransformationSemigroup,
function(m,n)
  return Semigroup(Set(List([1..m], x-> RandomTransformation(n))));
end);

# new for 0.1! - RandomTransformationSemigroup 
###########################################################################
InstallGlobalFunction(RandomTransformationMonoid,
function(m,n)
  return Monoid(Set(List([1..m], x-> RandomTransformation(n))));
end);

# new for 0.1! - Rank - "for a transformation"
#############################################################################
# Notes: returns RankOfTransformation. 

InstallOtherMethod(Rank, "for a transformation",
[IsTransformation], f-> RankOfTransformation(f));

# new for 0.1! - RClass - "for a trans. semi. and trans. or H-class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(RClass, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensRClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return RClassOfHClass(arg[1]);
  fi;
  
  Info(InfoWarning, 1, "Usage: (trans. semigp. and trans.) or H-class.");
  return fail;
end);

# new for 0.1! - RClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(RClassNC, 
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2]) then 
    return GreensRClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Info(InfoWarning, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

#EOF
