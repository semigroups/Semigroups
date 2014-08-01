#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## this file contains utilies for use with the Semigroups package. 

#

BindGlobal("SemigroupsDocXMLFiles",
  ["utils.xml", "greens.xml", "orbits.xml", "properties.xml", "examples.xml",
   "attributes-inverse.xml", "bipartition.xml", "blocks.xml", "attributes.xml",
   "semibipart.xml", "semitrans.xml", "semipperm.xml", "semigroups.xml", 
   "factor.xml", "freeinverse.xml", "display.xml", "normalizer.xml", 
   "maximal.xml", "reesmat-cong.xml", "ideals.xml", "isomorph.xml",
   "../PackageInfo.g"]);

# arg is the number of threads, defaults to 2...

BindGlobal("SemigroupsParallelTestAll",
function(arg)
  local n, dir_str, tst, dir, omit, files, filesplit, test, stringfile, farm,
  out, str, filename, file;
  
  if Length(arg)<>0 then 
    n:=arg[1];
  else 
    n:=2;
  fi;
  
  Print("Reading all .tst files in the directory semigroups/tst/...\n\n"); 
  dir_str:=Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,"/tst");
  tst:=DirectoryContents(dir_str);
  dir:=Directory(dir_str);

  omit:=SemigroupsOmitFromTestManualExamples;

  if Length(omit)>0 then 
    Print("not testing files containing the strings");
    for str in omit do 
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n\n");
  fi;
  files:=[]; 
  for filename in tst do
    if filename="testinstall-4.7.5.tst" 
     and not CompareVersionNumbers(GAPInfo.Version, "4.7.5") then 
      break;
    fi;

    filesplit:=SplitString(filename, ".");
    if Length(filesplit)>=2 and filesplit[Length(filesplit)]="tst" then
      test:=true;
      stringfile:=StringFile(Concatenation(dir_str, "/", filename));
      for str in omit do 
        if PositionSublist(stringfile, str)<>fail then 
          Print("not testing ", filename, ", it contains a test involving ", 
          str, ", which will not work . . .\n\n");
          test:=false;
          break;
        fi;
      od;
      if test then 
        Add(files, [Concatenation(dir_str, "/", filename)]);
      fi;
    fi;
  od;  

  farm:=ParWorkerFarmByFork(Test, rec(NumberJobs:=n));
  
  for file in files do 
    Submit(farm, file);
  od;

  while Length(farm!.outqueue)<Length(files) do 
    DoQueues(farm, false);
  od;

  out:=Pickup(farm);
  Kill(farm);
  return out;
end);
  

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
   InstallationPath, "/doc"), "main.xml", SemigroupsDocXMLFiles, "semigroups",
   "MathJax", "../../..");;
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
  local dir_str, tst, dir, omit, ex, filesplit, test, stringfile, str, filename;
  
  Print("Reading all .tst files in the directory semigroups/tst/...\n\n"); 
  dir_str:=Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,"/tst");
  tst:=DirectoryContents(dir_str);
  dir:=Directory(dir_str);

  omit:=SemigroupsOmitFromTestManualExamples;

  if Length(omit)>0 then 
    Print("not testing files containing the strings");
    for str in omit do 
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n\n");
  fi;
  
  for filename in tst do
    if filename="testinstall-4.7.5.tst" 
     and not CompareVersionNumbers(GAPInfo.Version, "4.7.5") then 
      Print("not testing ", filename, ", it requires GAP 4.7.5 or higher,\n\n");
      break;
    fi;

    filesplit:=SplitString(filename, ".");
    if Length(filesplit)>=2 and filesplit[Length(filesplit)]="tst" then
      test:=true;
      stringfile:=StringFile(Concatenation(dir_str, "/", filename));
      for str in omit do 
        if PositionSublist(stringfile, str)<>fail then 
          Print("not testing ", filename, ", it contains a test involving ", 
          str, ", which will not work . . .\n\n");
          test:=false;
          break;
        fi;
      od;
      if test then 
        Print("reading ", dir_str,"/", filename, " . . .\n");
        Test(Filename(dir, filename));
        Print("\n");
      fi;
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
  "main.xml",  SemigroupsDocXMLFiles, "Single" );
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

InstallGlobalFunction(IteratorFromGeneratorsFile, 
function(str)
  local file, record;
  
  file:=GeneratorsReadFile(str);
  
  if file=fail then 
    return fail;
  fi;
 
  record:=rec(file:=file, curr:=ReadGeneratorsLine(IO_ReadLine(file)));

  record.NextIterator:=function(iter)
    local next, line;
    next:=iter!.curr;
    line:=IO_ReadLine(iter!.file);
    if line<>"" then 
      iter!.curr:=ReadGeneratorsLine(line);
    else 
      iter!.curr:=line;
    fi;
    return next;
  end;
  
  record.IsDoneIterator:=function(iter)
    if iter!.curr="" then 
      if not iter!.file!.closed then 
        IO_Close(iter!.file);
      fi;
      return true;
    else
      return false;
    fi;
  end;

  record.ShallowCopy:=function(iter)
    local file;
    file:=GeneratorsReadFile(str);
    return rec(file:=file, curr:=ReadGeneratorsLine(IO_ReadLine(file)));
  end;

  InstallAtExit(function() if not file!.closed then IO_Close(file); fi; end);
  
  return IteratorByFunctions(record);
end);

#

InstallGlobalFunction(GeneratorsReadFile, 
function(str)
  local file;

  if not IsString(str) then 
    Error("usage: the argument must be a string,");
    return;
  fi;
  
  file:=SplitString(str, ".");
  if file[Length(file)] = "gz" then 
    file:=IO_FilteredFile([["gzip", ["-dq"]]], str, "r");
  elif file[Length(file)] = "xz" then 
    file:=IO_FilteredFile([["xz", ["-dq"]]], str, "r");
  else  
    file:=IO_File(str);
  fi;
  
  return file;
end);

#

InstallGlobalFunction(ReadGenerators, 
function(arg)
  local file, i, line;
 
  if IsString(arg[1]) then 
    file:=GeneratorsReadFile(arg[1]);
  elif IsFile(arg[1]) then 
    file:=arg[1];
  else
    Error("usage: the 1st argument must be a string or a file,");
    return;
  fi;
  
  if file=fail then 
    return fail;
  fi;
  
  if Length(arg)=2 then 
    if IsFile(arg[1]) then 
      Error("usage: the argument must be a file, or a string, or a string and a", 
      " positive integer,");
      return;
    fi;
    if IsPosInt(arg[2]) then 
      i:=0;
      repeat  
        i:=i+1; line:=IO_ReadLine(file);
      until i=arg[2] or line="";
      if IsString(arg[1]) then 
        IO_Close(file);
      fi;
      if line="" then
        Error(arg[1], " only has ", i-1, " lines,"); 
        return;
      else
        return ReadGeneratorsLine(Chomp(line));
      fi;
    else
      Error("usage: the 2nd argument must be a positive integer,");
      return;
    fi;
  elif Length(arg)>2 then 
    Error("usage: there should be at most 2 arguments,");
    return;
  fi;
  
  line:=IO_ReadLines(file);
  if IsString(arg[1]) then 
    IO_Close(file);
  fi;
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
  elif line[1]='b' then #bipartitions
    Apply(out, BipartitionByIntRepNC);
  fi;

  return out;
end);

#

InstallGlobalFunction(GeneratorsWriteFile, 
function(arg)
  local mode, file;

  if IsString(arg[1]) then 
    if IsExistingFile(arg[1]) then 
      if not IsWritableFile(arg[1]) then 
        Error(arg[1], " exists and is not a writable file,");
        return;
      fi;
    else
      if not (IsExistingFile(Concatenation(arg[1], ".gz")) or 
        IsExistingFile(Concatenation(arg[1], ".xz"))) then 
        Exec("touch ", arg[1]);
      fi;
    fi;
  else
    Error("usage: the 1st argument must be a string,");
    return;
  fi;

  if Length(arg)=2 and not (IsString(arg[2]) and (arg[2]="a" or arg[2]="w"))
   then 
    Error("usage: the 2nd argument must be \"a\" or \"w\",");
    return;
  fi;

  if Length(arg)>2 then 
    Error("usage: there must be at most 2 arguments,");
    return;
  fi;
  
  if Length(arg)=1 then 
    mode:="a";
  else
    mode:=arg[2];
  fi;

  file:=SplitString(arg[1], ".");
  if file[Length(file)] = "gz" then 
    file:=IO_FilteredFile([["gzip", ["-9q"]]], arg[1], mode);
  elif file[Length(file)] = "xz" then 
    file:=IO_FilteredFile([["xz", ["-9q"]]], arg[1], mode);
  else  
    file:=IO_File(arg[1], mode);
  fi;
  return file;
end);

#

InstallGlobalFunction(WriteGenerators, 
function(arg)
  local file, trans, gens, append, gzip, mode, line, deg, nrdigits, blocks, i, writin, s, f;
  
  if not (Length(arg)=3 or Length(arg)=2) then
    Error("usage: there should be 2 or 3 arguments,"); 
    return;
  fi;

  if IsString(arg[1]) then
    if IsBound(arg[3]) then 
      file:=GeneratorsWriteFile(arg[1], arg[3]);
    else 
      file:=GeneratorsWriteFile(arg[1]);
    fi;
  elif IsFile(arg[1]) then 
    file:=arg[1];
  else 
    Error("usage: the 1st argument must be a string or a file,");
    return;
  fi;

  if file=fail then 
    Error("couldn't open the file ", file, ",");
    return;
  fi;

  if IsTransformationCollection(arg[2]) 
    or IsPartialPermCollection(arg[2])
    or IsBipartitionCollection(arg[2]) then 
      trans:=[arg[2]];
  elif IsList(arg[2]) and IsBound(arg[2][1]) 
   and (IsTransformationCollection(arg[2][1]) 
    or IsPartialPermCollection(arg[2][1])
    or IsBipartitionCollection(arg[2][1])) then 
      trans:=arg[2];
  else
    Error("usage: the 2nd argument must be a transformation or partial perm\n",
    "semigroup or collection, or a list of such semigroups or collections,");
    return;
  fi;

  gens:=EmptyPlist(Length(trans));

  for i in [1..Length(trans)] do 
    if IsTransformationSemigroup(trans[i]) 
     or IsPartialPermSemigroup(trans[i]) 
     or IsBipartitionSemigroup(trans[i]) then 
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
      IO_WriteLine(file, line);
    od;
  elif IsBipartitionCollection(gens[1]) then 
    for s in gens do 
      line:="b";
      for f in s do 
        deg:=String(2*DegreeOfBipartition(f));
        nrdigits:=Length(deg);
        Append(line, String(nrdigits));
        Append(line, deg);
        blocks:=f!.blocks;
        for i in [1..Length(blocks)] do 
          append(line, blocks[i], nrdigits);
        od;
      od;
      IO_WriteLine(file, line);
    od;
  fi;
  
  if IsString(arg[1]) then  
    IO_Close(file);
  fi;
  return true;
end);

#

InstallGlobalFunction(WriteGeneratorsLine, 
function(f)
  local append, line, deg, strdeg, nrdigits, func, nr, i;
  
  append:=function(str, pt, m)
    local i, j;
    i:=String(pt);
    for j in [1..m-Length(i)] do 
      Append(str, " ");
    od;
    Append(str, i);
    return str;
  end;

  if IsTransformation(f) then 
    line:="t";
    deg:=DegreeOfTransformation(f);
    strdeg:=String(deg);
    nrdigits:=Length(strdeg);
    func:=POW;
    nr:=deg;
  elif IsPartialPerm(f) then 
    line:="p";
    deg:=DegreeOfPartialPerm(f);
    strdeg:=String(deg);
    nrdigits:=Length(String(Maximum(deg, CodegreeOfPartialPerm(f))));
    func:=POW;
    nr:=deg;
  elif IsBipartition(f) then 
    line:="b";
    deg:=DegreeOfBipartition(f);
    strdeg:=String(deg);
    nrdigits:=Length(strdeg);
    func:=function(i, f)
      return f!.blocks[i];
    end;
    nr:=2*deg;
  fi;
  
  Append(line, String(nrdigits));
  Append(line, strdeg);
  for i in [1..nr] do 
    append(line, func(i,f), nrdigits);
  od;
  return line;
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
