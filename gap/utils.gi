#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## this file contains utilies for use with the Semigroups package. 

# <path> to the folder containing the timings, <vers> the version number to
# check against.

BindGlobal("SemigroupsCompareTestTimings", 
function(path, vers)
  local dir, files, suffix, tstdir, tstfiles, split, tstfile, file;
  
  vers:=Concatenation("-", vers, ".");
  if not path[Length(path)]='/' then 
    Add(path, '/');
  fi;
  
  dir:=Directory(path);
  files:=DirectoryContents(dir);
  suffix:=Concatenation(vers, "tst.timings"); 

  tstdir:=DirectoriesPackageLibrary("semigroups", "tst" )[1];
  tstfiles:=DirectoryContents(tstdir);

  for file in files do
    split:=SplitString(file, '.');
    if split[Length(split)]="timings" then 
      if file{[Length(file)-Length(suffix)+1..Length(file)]}=suffix then 
        tstfile:=Concatenation(SplitString(split[1], '-')[1], ".tst");
        if not tstfile in tstfiles then 
          Print("can't find ", 
           Concatenation(SplitString(file[1], '-')[1], ".tst"), 
           "in semigroups/tst for comparison!\n");
        else
          Print("\n");
          Print(Test(Filename(tstdir, tstfile), rec(compareTimings:=file),
          "\n"));
        fi;
      fi;
    fi;
  od;
  return true;
end);

#

BindGlobal("SemigroupsCreateTestTimings", 
function(path, vers)
  local dir, files, file, out;
  
  vers:=Concatenation("-", vers, ".");
  if not path[Length(path)]='/' then 
    Add(path, '/');
  fi;
  
  dir:=DirectoriesPackageLibrary( "semigroups", "tst" )[1];
  files:=DirectoryContents(dir);
  
  for file in files do
    file:=SplitString(file, '.');
    if file[Length(file)]="tst" then 
      out:=Concatenation(path, JoinStringsWithSeparator(file, vers),
      ".timings");
      file:=JoinStringsWithSeparator(file, ".");
      Print("checking ", file ,"...\n");
      if not Test(Filename(dir, file)) then 
        Print(file, " returns differences in output, exiting...");
        return;
      fi;
      Print("writing ", file, " ...\n");
      Test(Filename(dir, file), rec(writeTimings:=out));
    fi;
  od;
  return true;
end);

#

BindGlobal("SemigroupsTestRec", rec());
MakeReadWriteGlobal("SemigroupsTestRec");

InstallGlobalFunction(SemigroupsStartTest, 
function()
  local record;

  record:=SemigroupsTestRec;  
  
  record.InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
  record.InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);;
  
  record.PartialPermDisplayLimit:=UserPreference("PartialPermDisplayLimit");;
  record.TransformationDisplayLimit
   :=UserPreference("TransformationDisplayLimit");;
  record.NotationForPartialPerms:=UserPreference("NotationForPartialPerms");;
  record.NotationForTransformations:=
   UserPreference("NotationForTransformations");;
 
  record.FreeInverseSemigroupElementDisplay := UserPreference("semigroups",
    "FreeInverseSemigroupElementDisplay");

  SetInfoLevel(InfoWarning, 0);;
  SetInfoLevel(InfoSemigroups, 0);
  
  SetUserPreference("PartialPermDisplayLimit", 100);;
  SetUserPreference("TransformationDisplayLimit", 100);;
  SetUserPreference("NotationForPartialPerms", "component");;
  SetUserPreference("NotationForTransformations", "input");;
  SetUserPreference("semigroups", "FreeInverseSemigroupElementDisplay", "minimal");
  return; 
end);

#

InstallGlobalFunction(SemigroupsStopTest, 
function()
  local record;

  record:=SemigroupsTestRec;  
  
  SetInfoLevel(InfoWarning, record.InfoLevelInfoWarning);;
  SetInfoLevel(InfoSemigroups, record.InfoLevelInfoSemigroups);
  
  SetUserPreference("PartialPermDisplayLimit", 
   record.PartialPermDisplayLimit);
  SetUserPreference("TransformationDisplayLimit", 
   record.TransformationDisplayLimit);
  SetUserPreference("NotationForPartialPerms", 
   record.NotationForPartialPerms);
  SetUserPreference("NotationForTransformations",
   record.NotationForTransformations);
  SetUserPreference("semigroups", "FreeInverseSemigroupElementDisplay",
   record.FreeInverseSemigroupElementDisplay);
  return;
end);

#

InstallGlobalFunction(SemigroupsDir, 
function()
  return PackageInfo("semigroups")[1]!.InstallationPath;
end);

#

InstallGlobalFunction(SemigroupsMakeDoc, 
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("semigroups")[1]!.
   InstallationPath, "/doc"), "main.xml", 
   ["utils.xml", "greens.xml", "orbits.xml", "properties.xml", "examples.xml",
    "semigroups.xml", "attributes-inverse.xml", "factor.xml", "freeinverse.xml",
    "attributes.xml", "display.xml", "../PackageInfo.g"],
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
      #  Print("not reading ", dir_str, "/", x, "\n(Semigroups is not
      #  compiled)\n");
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
  return;
end);

#

InstallGlobalFunction(SemigroupsManualExamples,
function()
return 
  ExtractExamples(DirectoriesPackageLibrary("semigroups","doc"), 
  "main.xml",  [ "utils.xml", "examples.xml", "factor.xml", "greens.xml",
  "orbits.xml", "properties.xml", "semigroups.xml",  "attributes-inverse.xml", 
  "freeinverse.xml", "attributes.xml", "display.xml",
  "../PackageInfo.g" ], "Single" );
end);

# if <arg> is some strings, then any example containing any of these strings is
# omitted from the test...

InstallGlobalFunction(SemigroupsTestManualExamples, 
function()
  local ex, omit, str;

  ex:=SemigroupsManualExamples(); 
  omit:=SemigroupsOmitFromTestManualExamples;
  if Length(omit)>0 then 
    Print("# not testing examples containing the strings");
    for str in omit do 
      ex:=Filtered(ex, x-> PositionSublist(x[1][1], str)=fail);
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n");
  fi;
  SemigroupsStartTest();
  RunExamples(ex);
  SemigroupsStopTest();
  return;
end);
#

InstallGlobalFunction(ReadGenerators, 
function(arg)
  local file, i, line;
 
  if not IsString(arg[1]) then 
    Error("the first argument must be a string,");
    return;
  else
    file:=SplitString(arg[1], ".");
    if file[Length(file)] = "gz" then 
      file:=IO_FilteredFile([["gzip", ["-dq"]]], arg[1]);
    else  
      file:=IO_File(arg[1]);
      if file=fail then 
        file:=IO_FilteredFile([["gzip", ["-dq"]]], 
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
        return ReadGeneratorsLine(Chomp(line));
      fi;
    else
      Error("the second argument should be a positive integer,");
      return;
    fi;
  fi;
  
  line:=IO_ReadLines(file);
  IO_Close(file);
  return List(line, x-> ReadGeneratorsLine(Chomp(x)));
end);

#

InstallGlobalFunction(ReadGeneratorsLine, 
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
  
  if line[1]='t' then   # transformations
    Apply(out, TransformationNC); 
  elif line[1]='p' then # partial perms
    Apply(out, DensePartialPermNC);
  fi;

  return out;
end);

# usage: filename as a string and trans. coll. 

# Returns: nothing. 

InstallGlobalFunction(WriteGenerators, 
function(arg)
  local trans, gens, append, gzip, mode, file, line, deg, nrdigits, i, writin, s, f;
  
  if not (Length(arg)=3 or Length(arg)=2) then
    Error("usage: filename as string and a transformation, transformation ",
    "collection, partial perm or partial perm collection, and a boolean,");
    return;
  fi;

  if IsExistingFile(arg[1]) then 
    if not IsWritableFile(arg[1]) then 
      Error(arg[1], " exists and is not a writable file,");
      return;
    fi;
  else
    PrintTo(arg[1], "");
  fi;

  if IsTransformationCollection(arg[2]) 
    or IsPartialPermCollection(arg[2]) then 
    trans:=[arg[2]];
  elif IsList(arg[2]) and IsBound(arg[2][1]) 
   and (IsTransformationCollection(arg[2][1]) 
    or IsPartialPermCollection(arg[2][1])) then 
    trans:=arg[2];
  else
    Error("usage: the 2nd argument must be transformation or partial perm ",
    "semigroup or collection, or a list of such semigroups or collections,");
    return;
  fi;

  if Length(arg)=3 and not IsBool(arg[3]) then 
    Error("usage: the 3rd argument must be <true> or <false>,");
    return;
  fi;

  gens:=EmptyPlist(Length(trans));

  for i in [1..Length(trans)] do 
    if IsTransformationSemigroup(trans[i]) 
      or IsPartialPermSemigroup(trans[i]) then 
      
      gens[i]:=GeneratorsOfSemigroup(trans[i]);
      # we could use a smaller generating set (i.e. GeneratorsOfMonoid,
      # GeneratorsOfInverseSemigroup etc) but we have no way of knowing which
      # generators we wrote, so better always use GeneratorsOfSemigroup
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

  gzip:=SplitString(arg[1], '.');
  gzip:=[JoinStringsWithSeparator(gzip{[1..Length(gzip)-1]}, "."),
   gzip[Length(gzip)]];
  
  #by default or if arg[3]=true append the result to arg[1]
  if Length(arg)=2 or arg[3] then 
    mode:="a";
  else
    mode:="w";
  fi;

  if Length(gzip)>1 and gzip[2]="gz" then
    file:=IO_FilteredFile([["gzip", ["-9q"]]], arg[1], mode);
  else 
    file:=IO_File(arg[1], mode);
  fi;
  
  if file=fail then 
    Error("something went wrong when trying to open the file for writing,");
    return;
  fi;

  if IsTransformationCollection(gens[1]) then 
    for s in gens do
      line:="t";
      for f in s do
        deg:=String(DegreeOfTransformation(f));
        nrdigits:=Length(deg);
        Append(line, String(nrdigits));
        Append(line, deg);
        for i in [1..DegreeOfTransformation(f)] do 
          append(line, i^f, nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    od;
  elif IsPartialPermCollection(gens[1]) then 
    for s in gens do 
      line:="p";
      for f in s do 
        deg:=String(DegreeOfPartialPerm(f));
        nrdigits:=Length(String(Maximum(
         DegreeOfPartialPerm(f), CodegreeOfPartialPerm(f))));
        Append(line, String(nrdigits));
        append(line, deg, nrdigits);
        for i in [1..DegreeOfPartialPerm(f)] do 
          append(line, i^f, nrdigits);
        od;
      od;
    od;
    IO_WriteLine(file, line);
  fi;
  
  return IO_Close(file);
end);

#

InstallMethod(ShortStringRep, "for a transformation",
[IsTransformation],
function(f)
  local append, line, deg, nrdigits, i;

  append:=function(str, pt, m)
    local i, j;
    i:=String(pt);
    for j in [1..m-Length(i)] do 
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;
  line:=""; 
  deg:=String(DegreeOfTransformation(f));
  nrdigits:=Length(deg);
  Append(line, String(nrdigits));
  Append(line, deg);
  for i in [1..DegreeOfTransformation(f)] do 
    append(line, i^f, nrdigits);
  od;
  return line;
end);

#EOF
