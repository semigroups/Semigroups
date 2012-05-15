#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains utilies for use with the Citrus package. 

# new for 0.5! - CitrusDir - for no arg.
#############################################################################

InstallGlobalFunction(CitrusDir, 
function()
  return PackageInfo("citrus")[1]!.InstallationPath;
end);

# new for 0.7 - CitrusIsNotCompiled - for no arg
#############################################################################

InstallGlobalFunction(CitrusIsNotCompiled, 
function(arg)
  Info(InfoWarning, 1, "Citrus is not compiled and consequently this",  
  " function is unavailable.");
  return fail;
end);

# mod for 0.7! - CitrusMakeDoc - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusMakeDoc, 
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("citrus")[1]!.
   InstallationPath, "/doc"), "citrus.xml", 
   ["utils.xml", "greens.xml", "inverse.xml", "orbits.xml", "properties.xml",
   "semigroups.xml", "transform.xml", "pperm.xml", "../PackageInfo.g"],
   "citrus", "MathJax");;
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

# mod for 0.7! - CitrusTestAll - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestAll, 
function()
  local dir_str, tst, dir, str, x;
  
  Print(
  "Reading all .tst files in the directory citrus/tst/...\n\n"); 
  dir_str:=Concatenation(PackageInfo("citrus")[1]!.InstallationPath,"/tst");
  tst:=DirectoryContents(dir_str);
  dir:=Directory(dir_str);
  for x in tst do
    str:=SplitString(x, ".");
    if Length(str)>=2 and str[2]="tst" then
      if not Citrus_C and str[1] in ["inverse", "pperm", "semigroups", 
         "testcompiled"]
        then 
        Print("not reading ", dir_str, "/", x, "\n(Citrus is not compiled)\n");
      else
        Print("reading ", dir_str,"/", x, " ...\n");
        Test(Filename(dir, x));
      fi;
      Print("\n");
    fi;
  od;  
  return;
end);

# new for 0.1! - CitrusTestInstall - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestInstall, 
function()
  Test(Filename(DirectoriesPackageLibrary("citrus","tst"),
   "testinstall.tst"));;
  if Citrus_C then 
    Test(Filename(DirectoriesPackageLibrary("citrus","tst"),
       "testcompiled.tst"));;
  fi;
  return;
end);

# new for 0.1! - CitrusTestManualExamples - "for no argument"
#############################################################################

InstallGlobalFunction(CitrusTestManualExamples,
function()
  local InfoLevelInfoWarning, InfoLevelInfoCitrus;
  
  if not Citrus_C then 
    Print("Citrus is not compiled and so this will produce many many errors.\n");
    return fail;
  fi;

  SizeScreen([80]); 
  InfoLevelInfoWarning:=InfoLevel(InfoWarning);
  InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);
  SetInfoLevel(InfoWarning, 0);
  SetInfoLevel(InfoCitrus, 0);

  TestManualExamples(Concatenation(PackageInfo("citrus")[1]!.
     InstallationPath, "/doc"), "citrus.xml", 
     ["utils.xml", "greens.xml", "orbits.xml", "properties.xml", "inverse.xml",
     "semigroups.xml", "transform.xml", "pperm.xml", "../PackageInfo.g"]);
  
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

  if not Citrus_C then 
    Print("Citrus is not compiled and so this will produce many many errors.");
    return fail;
  fi;
  
  ex:=ManualExamples("~/citrus/doc/", "citrus.xml",  [ "utils.xml",
  "greens.xml", "orbits.xml", "properties.xml", "pperm.xml", "inverse.xml",
  "semigroups.xml",  "transform.xml", "../PackageInfo.g" ], "Single" );;

  for i in [1..Length(ex)] do 
    Print("*** Example ", i, " ***\n");
    tst:=ReadTestExamplesString(ex[i]);
  od;
  if IsBoundGlobal("CitrusManualExamples") then 
    MakeReadWriteGlobal("CitrusManualExamples");
    UnbindGlobal("CitrusManualExamples");
  fi;

  BindGlobal("CitrusManualExamples", ex);
  Print("the manual examples are in the global variable",
  " CitrusManualExamples\n");

  return;
end);


# new for 0.1! - DClass - "for a trans. semi and trans. or Green's class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class or L-class or R-class.

InstallGlobalFunction(DClass, 
function(arg)

  if Length(arg)=2 and ((IsTransformationSemigroup(arg[1]) 
   and IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
   IsPartialPerm(arg[2]))) then 
    return GreensDClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensRClass(arg[1]) then 
    return DClassOfRClass(arg[1]);
  elif Length(arg)=1 and IsGreensLClass(arg[1]) then 
    return DClassOfLClass(arg[1]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return DClassOfHClass(arg[1]);
  fi;
  
  Error("Usage: (trans. semigp. and trans.), (partial perm. semigp. and",
  " partial perm) or H-class or L-class or R-class,");
  return;
end);

# new for 0.1! - DClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(DClassNC,
function(arg)

  if Length(arg)=2 and IsTransformationSemigroup(arg[1])
   and IsTransformation(arg[2]) then
    return GreensDClassOfElementNC(arg[1], arg[2]);
  elif Length(arg)=2 and (IsPartialPermSemigroup(arg[1]) and 
   IsInverseSemigroup(arg[1]) and IsPartialPerm(arg[2])) then 
    return GreensDClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("Usage: (trans. semigp. and trans.) or ", 
  "(partial perm. semigp. and partial perm)");
  return;
end);

# new for 0.1! - Degree - "for a transformation"
#############################################################################
# Notes: returns DegreeOfTransformation.

InstallOtherMethod(Degree, "for a transformation",
[IsTransformation], DegreeOfTransformation);

# new for 0.7! - Degree - "for a partial perm"
#############################################################################

InstallOtherMethod(Degree, "for a partial perm",
[IsPartialPerm], f-> f[6]);

# new for 0.1! - Degree - "for a transformation semigroup"
#############################################################################
# Notes: returns DegreeOfTransformationSemigroup.

InstallOtherMethod(Degree, "for a transformation semigroup",
[IsTransformationSemigroup], DegreeOfTransformationSemigroup);

# new for 0.7! - Degree - "for a partial perm semigroup"
#############################################################################

InstallOtherMethod(Degree, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> MaximumList(List(Generators(s), Degree)));

# new for 0.7! - Degree - "for a partial perm collection"
#############################################################################

InstallOtherMethod(Degree, "for a partial perm collection",
[IsPartialPermCollection], C-> MaximumList(List(C, Degree)));

# new for 0.1! - Generators - "for a semigroup or monoid"
############################################################################
# Notes: returns the monoid generators of a monoid, and the semigroup 
# generators of a semigroup. 

InstallOtherMethod(Generators, "for a semigroup or monoid",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

# new for 0.7! - Generators - "for an inverse semigroup"
############################################################################

InstallOtherMethod(Generators, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  if IsMonoid(s) then 
    return GeneratorsOfInverseMonoid(s);
  fi;
  return GeneratorsOfInverseSemigroup(s);
end);

# new for 0.1! - HClass - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(HClass, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) and
    IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
    IsPartialPerm(arg[2])) or (IsGreensClass(arg[1]) and
    (IsTransformation(arg[2]) or IsPartialPerm(arg[2]))) then 
    return GreensHClassOfElement(arg[1], arg[2]);
  fi;

  Error("usage: (trans. semigp. and trans.) or ",
  "(partial perm. semigp. and partial perm.) or ", 
  "(Green's class and (trans. or partial perm.))");
  return;
end);

# new for 0.1! - HClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(HClassNC, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) and
    IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
    IsPartialPerm(arg[2])) or (IsGreensClass(arg[1]) and
    (IsTransformation(arg[2]) or IsPartialPerm(arg[2]))) then
    return GreensHClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: (trans. semigp. and trans.) or ",
  "(partial perm. semigp. and partial perm.) or ", 
  "(Green's class and (trans. or partial perm.))");
  return;
end);

#III

# new for 0.7! - IteratorByIterator - "for an iterator and function"
#############################################################################

InstallGlobalFunction(IteratorByIterator,
function(old_iter, convert, filts)
  local iter, filt;
  iter:=IteratorByFunctions(rec(
    data:=old_iter,
    IsDoneIterator:=iter-> IsDoneIterator(iter!.data),
    NextIterator:=function(iter)
      local x;
      x:=NextIterator(iter!.data);
      if x=fail then 
        return fail;
      fi;
      return convert(x);
    end,
    ShallowCopy:=iter-> rec(data:=old_iter)));
  for filt in filts do 
    SetFilterObj(iter, filt);
  od;
  return iter;
end);

# mod for 0.5! - LClass - "for a trans. semi. and trans. or H-class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(LClass, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) and
   IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
   IsPartialPerm(arg[2])) or (IsGreensDClass(arg[1]) and
   (IsTransformation(arg[2]) or IsPartialPerm(arg[2]))) then  
   return GreensLClassOfElement(arg[1], arg[2]);     
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return LClassOfHClass(arg[1]);
  fi;
  Error("usage: (trans. semigp. and trans.) or ",
  "(partial perm. semigp. and partial perm.) or ",
  "(D-class and (trans. or partial perm.)) or ",
  "H-class"); 
  return;
end);

# new for 0.1! - LClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(LClassNC, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) and
   IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
   IsPartialPerm(arg[2])) or (IsGreensDClass(arg[1]) and
   (IsTransformation(arg[2]) or IsPartialPerm(arg[2]))) then
    return GreensLClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Error("usage: (trans. semigp. and trans.) or ",
  "(partial perm. semigp. and partial perm.) or ",
  "(D-class and (trans. or partial perm.))");  
  return;
end);

# new for 0.7! - ListByIterator - "for an iterator and pos int"
#############################################################################

InstallGlobalFunction(ListByIterator, 
function(iter, len)
  local out, i, x;

  out:=EmptyPlist(len);
  i:=0;

  for x in iter do  
    i:=i+1;
    out[i]:=x;
  od;
  
  return out;
end);


# new for 0.1! - Rank - "for a transformation"
#############################################################################
# Notes: returns RankOfTransformation. 

InstallOtherMethod(Rank, "for a transformation",
[IsTransformation], RankOfTransformation);

# new for 0.7 - Rank - "for a partial perm."
#############################################################################

InstallOtherMethod(Rank, "for a partial perm", 
[IsPartialPerm], f-> f[2]);

# new for 0.1! - RClass - "for a trans. semi. and trans. or H-class"
#############################################################################
# Usage: (trans. semigp. and trans.) or H-class.

InstallGlobalFunction(RClass, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) and
   IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
   IsPartialPerm(arg[2])) or (IsGreensDClass(arg[1]) and
   (IsTransformation(arg[2]) or IsPartialPerm(arg[2]))) then 
    return GreensRClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then 
    return RClassOfHClass(arg[1]);
  fi;
   Error("usage: (trans. semigp. and trans.) or ",
  "(partial perm. semigp. and partial perm.) or ",
  "(D-class and (trans. or partial perm.)) or ",
  "H-class"); 
  return;
end);

# new for 0.1! - RClassNC - "for a trans. semi. and trans."
#############################################################################
# Usage: trans. semigp. and trans.

InstallGlobalFunction(RClassNC, 
function(arg)

  if Length(arg)=2 and (IsTransformationSemigroup(arg[1]) and
   IsTransformation(arg[2])) or (IsPartialPermSemigroup(arg[1]) and
   IsPartialPerm(arg[2])) or (IsGreensDClass(arg[1]) and
   (IsTransformation(arg[2]) or IsPartialPerm(arg[2]))) then
    return GreensRClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Error("usage: (trans. semigp. and trans.) or ",
  "(partial perm. semigp. and partial perm.) or ",
  "(D-class and (trans. or partial perm.))");
  return;
end);

# new for 0.5! - ReadCitrus - "for a string and optional pos. int."
#############################################################################

InstallGlobalFunction(ReadCitrus, 
function(arg)
  local file, i, line;
 
  if not IsString(arg[1]) then 
    Error("the first argument must be a string,");
    return;
  else
    file:=SplitString(arg[1], ".");
    if file[Length(file)] = "gz" then 
      file:=IO_FilteredFile([["gzip", ["-dcq"]]], arg[1]);
    else  
      file:=IO_File(arg[1]);
    fi;
  fi;

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
        return ReadCitrusLine(Chomp(line));
      fi;
    else
      IO_Close(file);
      Error("the second argument should be a positive integer,");
      return;
    fi;
  fi;
  
  line:=IO_ReadLines(file);
  IO_Close(file);
  return List(line, x-> ReadCitrusLine(Chomp(x)));
end);

# mod for 0.7! - ReadCitrusLine - "for a string"
#############################################################################

InstallGlobalFunction(ReadCitrusLine, 
function(line)
  local m, n, r, dom, out, f, i, k, deg, rank, j;
  
  if not line[1]='p' then         # transformations
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
  elif line[1]='p' then # partial perms
    return ReadCitrusLinePP(line);
  fi;
end);

if IsBound(FullPartialPermNC) then 
  InstallGlobalFunction(ReadCitrusLinePP, 
  function(line)
    local r, i, k, out, m, deg, rank, f, j;
    
    r:=Length(line)-1; i:=2; k:=0; out:=[];

    while i<Length(line) do
      k:=k+1;
      m:=Int([line[i]]);                                      # blocksize
      deg:=Int(NormalizedWhitespace(line{[i+1..m+i]}));       # max domain
      rank:=Int(NormalizedWhitespace(line{[m+i+1..2*m+i]}));  # rank
      f:=line{[i+1..i+m*(deg+3*rank+6)]};
      out[k]:=EmptyPlist(deg+3*rank+6);
      for j in [1..deg+3*rank+6] do 
        Add(out[k], Int(NormalizedWhitespace(f{[(j-1)*m+1..j*m]})));
      od;
      out[k]:=FullPartialPermNC(out[k]);
      i:=i+m*(deg+3*rank+6)+1;
    od;
    return out;
  end);
else
  InstallGlobalFunction(ReadCitrusLinePP, ReturnFail);
fi;

# mod for 0.7! - WriteCitrus - "for a string and trans. coll."
#############################################################################

# Usage: filename as a string and trans. coll. 

# Returns: nothing. 

InstallGlobalFunction(WriteCitrus, 
function(arg)
  local trans, gens, convert, output, n, m, str, int, j, i, s, f;
  
  if not Length(arg)=2 then 
    Error("Usage: filename as string and trans, trans coll, partial perm or",
    " partial perm coll,");
    return;
  fi;

  if IsExistingFile(arg[1]) and not IsWritableFile(arg[1]) then 
    Error(arg[1], " exists and is not a writable file,");
    return;
  fi;

  if IsTransformationCollection(arg[2]) or IsPartialPermCollection(arg[2]) then 
    trans:=[arg[2]];
  elif IsTransformationCollection(arg[2][1]) or
   IsPartialPermCollection(arg[2][1]) then 
    trans:=arg[2];
  else
    Error("Usage: second arg must be trans or part perm semi, coll, or list",
    " of same,");
    return;
  fi;

  gens:=EmptyPlist(Length(trans));

  for i in [1..Length(trans)] do 
    if IsTransformationSemigroup(trans[i]) or
     IsPartialPermSemigroup(trans[i]) then 
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

  convert:=function(list, m)
    local str, i;
    
    str:="";
    for i in list do 
      i:=String(i);
      Append(str, Concatenation([ListWithIdenticalEntries(m-Length(i), ' ')],
      [i]));
    od;

    return Concatenation(str);
  end;

  #####

  output := OutputTextFile( arg[1], true );
  SetPrintFormattingStatus(output, false);
  if IsTransformationCollection(gens[1]) then 
    for s in gens do 
      n:=String(DegreeOfTransformationCollection(s));
      m:=Length(n);
      str:=Concatenation(String(m), n);
    
      for f in s do
        Append(str, convert(f![1], m));
      od;

      AppendTo( output, str, "\n" );
    od;
  elif IsPartialPermCollection(gens[1]) then 
    for s in gens do 
      str:="p";
      for f in s do 
        int:=InternalRepOfPartialPerm(f);
        j:=Length(String(int[6]));
        Append(str, Concatenation(String(j), convert(int, j)));
        if Length(int)<> 6+int[1]+3*int[2] then 
          Append(str, Concatenation([ListWithIdenticalEntries(j*int[2], ' ')]));
        fi;
      od;
      #Print(str, "\n");
      AppendTo(output, str, "\n");
    od;
  fi;

  CloseStream(output);

  return;
end);

InstallMethod(LambdaPt, "for a trans",
[IsTransformation], ImageSetOfTransformation);

if IsBound(RanSetPP) then 
  InstallMethod(LambdaPt, "for a partial perm",
    [IsPartialPerm], RanSetPP);
fi;

InstallMethod(RhoPt, "for a trans",
[IsTransformation], CanonicalTransSameKernel);

if IsBound(DomPP) then 
  InstallMethod(RhoPt, "for a partial perm",
   [IsPartialPerm], DomPP);
fi;

InstallOtherMethod(LambdaOrb, "for a D-class of a trans. semi",
[IsGreensDClass and IsGreensClassOfTransSemigp], ImageOrbit);

InstallOtherMethod(LambdaOrb, "for a D-class of a part perm semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], d-> d!.o);

InstallOtherMethod(RhoOrb, "for a D-class of a trans. semi",
[IsGreensDClass and IsGreensClassOfTransSemigp], KernelOrbit);

InstallOtherMethod(RhoOrb, "for a D-class of a part perm semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup], d-> d!.o);




#EOF
