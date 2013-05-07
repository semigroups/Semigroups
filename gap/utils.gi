#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains utilies for use with the Semigroups package. 

InstallGlobalFunction(SemigroupsDir, 
function()
  return PackageInfo("semigroups")[1]!.InstallationPath;
end);

#

InstallGlobalFunction(SemigroupsMakeDoc, 
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("semigroups")[1]!.
   InstallationPath, "/doc"), "citrus.xml", 
   ["utils.xml", "greens.xml", "inverse.xml", "orbits.xml", "properties.xml",
   "semigroups.xml", "transform.xml", "pperm.xml", "../PackageInfo.g"],
   "semigroups", "MathJax", "../../..");;
  return;
end);

#

InstallGlobalFunction(SemigroupsMathJaxDefault, 
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
Info(InfoSemigroups, 1, "don't forget to run SemigroupsMakeDoc()");
return;
end);

#

InstallGlobalFunction(SemigroupsMathJaxLocal, 
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
  Info(InfoSemigroups, 1, "don't forget to run SemigroupsMakeDoc()");
  return;
end);

#

InstallGlobalFunction(SemigroupsTestAll, 
function()
  local dir_str, tst, dir, str, x;
  
  Print(
  "Reading all .tst files in the directory semigroups/tst/...\n\n"); 
  dir_str:=Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,"/tst");
  tst:=DirectoryContents(dir_str);
  dir:=Directory(dir_str);
  for x in tst do
    str:=SplitString(x, ".");
    if Length(str)>=2 and str[2]="tst" then
      #if not Semigroups_C and str[1] in ["inverse", "pperm", "semigroups", 
      #   "testcompiled"]
      #  then 
      #  Print("not reading ", dir_str, "/", x, "\n(Semigroups is not compiled)\n");
      #else
        Print("reading ", dir_str,"/", x, " ...\n");
        Test(Filename(dir, x));
      #fi;
      Print("\n");
    fi;
  od;  
  return;
end);

#

InstallGlobalFunction(SemigroupsTestInstall, 
function()
  Test(Filename(DirectoriesPackageLibrary("semigroups","tst"),
   "testinstall.tst"));;
  #if Semigroups_C then 
    Test(Filename(DirectoriesPackageLibrary("semigroups","tst"),
       "testcompiled.tst"));;
  #fi;
  return;
end);

#

InstallGlobalFunction(SemigroupsTestManualExamples,
function()
  local InfoLevelInfoWarning, InfoLevelInfoSemigroups;
  
  #if not Semigroups_C then 
   # Print("Semigroups is not compiled and so this will produce many many errors.\n");
   # return fail;
  #fi;

  SizeScreen([80]); 
  InfoLevelInfoWarning:=InfoLevel(InfoWarning);
  InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);
  SetInfoLevel(InfoWarning, 0);
  SetInfoLevel(InfoSemigroups, 0);

  TestManualExamples(Concatenation(PackageInfo("semigroups")[1]!.
     InstallationPath, "/doc"), "semigroups.xml", 
     ["utils.xml", "greens.xml", "orbits.xml", "properties.xml", "inverse.xml",
     "semigroups.xml", "transform.xml", "pperm.xml", "../PackageInfo.g"]);
  
  SetInfoLevel(InfoWarning, InfoLevelInfoWarning);
  SetInfoLevel(InfoSemigroups, InfoLevelInfoSemigroups);
  Unbind(InfoLevelInfoSemigroups); Unbind(InfoLevelInfoWarning);
  return;
end);

#

InstallGlobalFunction(SemigroupsReadTestManualExamples, 
function()
  local ex, tst, i;

  #if not Semigroups_C then 
  #  Print("Semigroups is not compiled and so this will produce many many errors.");
  #  return fail;
  #fi;
  
  ex:=ManualExamples("~/semigroups/doc/", "semigroups.xml",  [ "utils.xml",
  "greens.xml", "orbits.xml", "properties.xml", "pperm.xml", "inverse.xml",
  "semigroups.xml",  "transform.xml", "../PackageInfo.g" ], "Single" );;

  for i in [1..Length(ex)] do 
    Print("*** Example ", i, " ***\n");
    tst:=ReadTestExamplesString(ex[i]);
  od;
  if IsBoundGlobal("SemigroupsManualExamples") then 
    MakeReadWriteGlobal("SemigroupsManualExamples");
    UnbindGlobal("SemigroupsManualExamples");
  fi;

  BindGlobal("SemigroupsManualExamples", ex);
  Print("the manual examples are in the global variable",
  " SemigroupsManualExamples\n");

  return;
end);

#

InstallGlobalFunction(ReadSemigroups, 
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
      if file=fail then 
        file:=IO_FilteredFile([["gzip", ["-dcq"]]], 
         Concatenation(arg[1], ".gz"));
      fi;
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
        return ReadSemigroupsLine(Chomp(line));
      fi;
    else
      IO_Close(file);
      Error("the second argument should be a positive integer,");
      return;
    fi;
  fi;
  
  line:=IO_ReadLines(file);
  IO_Close(file);
  return List(line, x-> ReadSemigroupsLine(Chomp(x)));
end);

#

InstallGlobalFunction(ReadSemigroupsLine, 
function(line)
  local i, k, out, m, deg, f, j;
  
  i:=2; k:=0; out:=[];

  while i<Length(line) do
    m:=Int([line[i]]);                                      # blocksize
    deg:=Int(NormalizedWhitespace(line{[i+1..m+i]}));       # max domain
    f:=line{[m+i+1..i+m*(deg+1)]};
    k:=k+1;
    out[k]:=EmptyPlist(deg);
    for j in [1..deg] do 
      Add(out[k], Int(NormalizedWhitespace(f{[(j-1)*m+1..j*m]})));
    od;
    i:=i+m*(deg+1)+1;
  od;
  
  if line[1]='t' then         # transformations
    Apply(out, TransformationNC); 
  elif line[1]='p' then # partial perms
    Apply(out, DensePartialPermNC);
  fi;
  return out;
end);

# Usage: filename as a string and trans. coll. 

# Returns: nothing. 

InstallGlobalFunction(WriteSemigroups, 
function(arg)
  local trans, gens, append, gzip, str, deg, nrdigits, out, i, s, f;
  
  if not (Length(arg)=3 or Length(arg)=2) then
    Error("Usage: filename as string and trans, trans coll, partial perm or",
    " partial perm coll, and a boolean,");
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

  if Length(arg)=3 and not IsBool(arg[3]) then 
    Error("usage: the third argument must be <true> or <false>,");
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

  append:=function(str, pt, m)
    local i, j;
    
    i:=String(pt);
    for j in [1..m-Length(i)] do 
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;

  #####

  #by default or by if arg[3]=true append the result to arg[1]
  
  gzip:=SplitString(arg[1], '.');
  gzip:=[JoinStringsWithSeparator(gzip{[1..Length(gzip)-1]}, "."),
   gzip[Length(gzip)]];
  if Length(arg)=2 or arg[3] then 
    if Length(gzip)>1 and gzip[2]="gz" then 
      str:=StringFile(gzip[1]);
    else 
      str:=StringFile(arg[1]);
    fi;
  else
    str:=StringFile(arg[1]);
  fi;
  
  if str=fail then 
    str:="";
  fi;
  
  if IsTransformationCollection(gens[1]) then 
    for s in gens do
      Append(str, "t");
      for f in s do
        deg:=String(DegreeOfTransformation(f));
        nrdigits:=Length(deg);
        Append(str, String(nrdigits));
        Append(str, deg);
        for i in [1..DegreeOfTransformation(f)] do 
          append(str, i^f, nrdigits);
        od;
      od;
      Append(str, "\n" );
    od;
  elif IsPartialPermCollection(gens[1]) then 
    for s in gens do 
      Append(str, "p");
      for f in s do 
        deg:=String(DegreeOfPartialPerm(f));
        nrdigits:=Length(String(Maximum(
         DegreeOfPartialPerm(f), CodegreeOfPartialPerm(f))));
        Append(str, String(nrdigits));
        append(str, deg, nrdigits);
        for i in [1..DegreeOfPartialPerm(f)] do 
          append(str, i^f, nrdigits);
        od;
      od;
      Append(str, "\n");
    od;
  fi;
  
  if Length(gzip)>1 and gzip[2]="gz" then 
    out:=FileString(gzip[1], str);
    Exec("gzip -fq9 ", gzip[1]);
    return out;
  fi;
  return FileString(arg[1], str);
end);

#EOF
