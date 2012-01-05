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

# mod for 0.4! - CitrusMakeDoc - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusMakeDoc, 
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("citrus")[1]!.
   InstallationPath, "/doc"), "citrus.xml", 
   ["utils.xml", "greens.xml", "orbits.xml", "properties.xml",
     "transform.xml", "../PackageInfo.g"], "citrus", "MathJax");;
  return;
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
Info(InfoCitrus, 1, "don't forget to run CitrusMakeDoc()");
return;
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
  Info(InfoCitrus, 1, "don't forget to run CitrusMakeDoc()");
  return;
end);

# mod for 0.4! - CitrusTestAll - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestAll, 
function()
  Print(
  "Reading all .tst files in the directory citrus/tst/...\n\n"); 
  Read(Filename(DirectoriesPackageLibrary("citrus","tst"),"testall.g"));;
  return;
end);

# new for 0.1! - CitrusTestInstall - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestInstall, 
function()
  ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),
   "testinstall.tst"));;
  return;
end);

# new for 0.1! - CitrusTestManualExamples - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestManualExamples,
function()
  local InfoLevelInfoWarning, InfoLevelInfoCitrus;
  SizeScreen([80]); 
  InfoLevelInfoWarning:=InfoLevel(InfoWarning);
  InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);
  SetInfoLevel(InfoWarning, 0);
  SetInfoLevel(InfoCitrus, 0);

  TestManualExamples(Concatenation(PackageInfo("citrus")[1]!.
     InstallationPath, "/doc"), "citrus.xml", 
     ["utils.xml", "greens.xml", "orbits.xml", "properties.xml",
      "transform.xml", "../PackageInfo.g"]);
  
  SetInfoLevel(InfoWarning, InfoLevelInfoWarning);
  SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);
  Unbind(InfoLevelInfoCitrus); Unbind(InfoLevelInfoWarning);
  return;
end);

# new for 0.5! - CitrusReadTestManualExamples - "for no argument" 
#############################################################################

InstallGlobalFunction(CitrusReadTestManualExamples, 
function()
  local ex, tst, i;

  ex:=ManualExamples("~/citrus/doc/", "citrus.xml",  [ "utils.xml",
  "greens.xml", "orbits.xml", "properties.xml", 
  "transform.xml", "../PackageInfo.g" ], "Single" );;

  for i in [1..Length(ex)] do 
    Print("*** Example ", i, " ***\n");
    tst:=ReadTestExamplesString(ex[i]);
  od;

  return true;
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
  
  Info(InfoCitrus, 1, "Usage: (trans. semigp. and trans.) or H-class or",
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

  Info(InfoCitrus, 1, "Usage: trans. semigp. and trans.");
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

  Info(InfoCitrus, 1, "Usage: trans. semigp. and trans.");
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

  Info(InfoCitrus, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# mod for 0.5! - LClass - "for a trans. semi. and trans. or H-class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(LClass, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) or
  IsGreensDClass(arg[1])) and IsTransformation(arg[2]) then 
    return GreensLClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return LClassOfHClass(arg[1]);
  fi;
  
  Info(InfoCitrus, 1, "Usage: (trans. semigp. or D-class  and trans.) or ",
  "H-class.");
  return fail;
end);

# new for 0.1! - LClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(LClassNC, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) or 
    IsGreensDClass(arg[1])) and IsTransformation(arg[2]) then 
    return GreensLClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Info(InfoCitrus, 1, "Usage: (trans. semigp. or D-class) and trans.");
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
  
  Info(InfoCitrus, 1, "Usage: (trans. semigp. and trans.) or H-class.");
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
  
  Info(InfoCitrus, 1, "Usage: trans. semigp. and trans.");
  return fail;
end);

# new for 0.5! - ReadCitrus - "for a string and optional pos. int."
#############################################################################

InstallGlobalFunction(ReadCitrus, 
function(arg)
  local read_line, file, i, line;
  
  if not IsString(arg[1]) then 
    Error("the first argument must be a string,");
  fi;

  #####

  read_line:=function(line)
    local m, n, r, dom, out, f, i, j;
    
    m:=Int([line[1]]);            # block size <10
    n:=Int(line{[2..m+1]});       # degree
    r:=(Length(line)-(m+1))/(m*n);# number of generators 
    dom:=[m+2..m*n+m+1]; out:=EmptyPlist(r);

    for i in [1..r] do
      out[i]:=EmptyPlist(n); 
      f:=line{dom+m*(i-1)*n};
      for j in [1..n] do 
        Add(out[i], Int(NormalizedWhitespace(f{[(j-1)*m+1..j*m]})));
      od;
      out[i]:=TransformationNC(out[i]);
    od;
    return out;
  end;

  #####

  file:=IO_FilteredFile([["gzip", ["-dcq"]]], arg[1], "r");

  if file=fail then 
    Error(arg[1], " is not a readable file,");
    return;
  fi;

  if Length(arg)>1 then 
    if IsPosInt(arg[2]) then 
      i:=0;
      repeat  
        i:=i+1; line:=IO_ReadLine(file);
      until i=arg[2] or line="";
      IO_Close(file); 
      if line="" then
        Error(arg[1], " only has ", i-1, " lines,"); 
        return;
      else
        return read_line(Chomp(line));
      fi;
    else
      IO_Close(file);
      Error("the second argument should be a positive integer,");
      return;
    fi;
  fi;

  line:=IO_ReadLines(file);
  IO_Close(file);
  return List(line, x-> read_line(Chomp(x)));
end);

# new for 0.5! - WriteCitrus - "for a string and trans. coll."
#############################################################################

# Usage: filename as a string and trans. coll. 

# Returns: nothing. 

InstallGlobalFunction(WriteCitrus, 
function(arg)
  local trans, gens, i, convert, output, n, m, str, s, f;
  
  if not Length(arg)=2 then 
    Error("Usage: trans. semigroup, trans. collection or list of same", 
    "and filename as string");
    return fail;
  fi;

  if IsExistingFile(arg[1]) and not IsWritableFile(arg[1]) then 
    Error(arg[1], " exists and is not a writable file");
    return fail;
  fi;

  if IsTransformationCollection(arg[2]) then 
    trans:=[arg[2]];
  elif IsTransformationCollection(arg[2][1]) then 
    trans:=arg[2];
  else
    Error("Usage: first arg must be trans. semi., trans. coll. or list of same");
    return fail;
  fi;

  gens:=EmptyPlist(Length(trans));
  for i in [1..Length(trans)] do 
    if IsTransformationSemigroup(trans[i]) then 
      if HasMinimalGeneratingSet(trans[i]) then
        gens[i]:=MinimalGeneratingSet(trans[i]);
      elif HasSmallGeneratingSet(trans[i]) then 
        gens[i]:=SmallGeneratingSet(trans[i]);
      else
        gens[i]:=Generators(trans[i]);
      fi;
    else
      gens:=trans;
    fi;
  od;
 
  #####

  convert:=function(f, m)
    local str, i;
    
    str:="";
    for i in f![1] do 
      i:=String(i);
      Append(str, Concatenation([ListWithIdenticalEntries(m-Length(i), ' ')],
      [i]));
    od;

    return Concatenation(str);
  end;

  #####

  output := OutputTextFile( arg[1], true );
  SetPrintFormattingStatus(output, false);
  
  for s in gens do 

    n:=String(DegreeOfTransformationCollection(s));
    m:=Length(n);
    str:=Concatenation(String(m), n);
  
    for f in s do
      Append(str, convert(f, m));
    od;

    AppendTo( output, str, "\n" );
  od;
  
  CloseStream(output);

  return;
end);

#EOF
