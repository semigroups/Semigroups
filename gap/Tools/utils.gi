#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## this file contains utilies for use with the Semigroups package.

#

BindGlobal("SemigroupsDocXMLFiles", ["../PackageInfo.g",
                                     "attributes-acting.xml",
                                     "attributes-inverse.xml",
                                     "attributes.xml",
                                     "bipartition.xml",
                                     "blocks.xml",
                                     "display.xml",
                                     "examples.xml",
                                     "factor.xml",
                                     "freeband.xml",
                                     "freeinverse.xml",
                                     "greens.xml",
                                     "ideals.xml",
                                     "isomorph.xml",
                                     "maximal.xml",
                                     "normalizer.xml",
                                     "orbits.xml",
                                     "pairs-cong.xml",
                                     "properties.xml",
                                     "reesmat-cong.xml",
                                     "semibipart.xml",
                                     "semigroups.xml",
                                     "semipperm.xml",
                                     "semitrans.xml",
                                     "simple-cong.xml",
                                     "univ-cong.xml",
                                     "utils.xml"]);

# arg is the number of threads, defaults to 2...

BindGlobal("SemigroupsParallelTestAll",
function(arg)
  local n, dir_str, tst, dir, omit, files, filesplit, test, stringfile, farm,
  out, str, filename, file;

  if Length(arg) <> 0 then
    n := arg[1];
  else
    n := 2;
  fi;

  Print("Reading all .tst files in the directory semigroups/tst/...\n\n");
  dir_str :=
   Concatenation(PackageInfo("semigroups")[1]!.InstallationPath, "/tst");
  tst := DirectoryContents(dir_str);
  dir := Directory(dir_str);

  omit := SemigroupsOmitFromTestManualExamples;

  if Length(omit) > 0 then
    Print("not testing files containing the strings");
    for str in omit do
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n\n");
  fi;
  files := [];
  for filename in tst do

    filesplit := SplitString(filename, ".");
    if Length(filesplit) >= 2 and filesplit[Length(filesplit)] = "tst" then
      test := true;
      stringfile := StringFile(Concatenation(dir_str, "/", filename));
      for str in omit do
        if PositionSublist(stringfile, str) <> fail then
          Print("not testing ", filename, ", it contains a test involving ",
                str, ", which will not work . . .\n\n");
          test := false;
          break;
        fi;
      od;
      if test then
        Add(files, [Concatenation(dir_str, "/", filename)]);
      fi;
    fi;
  od;

  farm := ParWorkerFarmByFork(Test, rec(NumberJobs := n));

  for file in files do
    Submit(farm, file);
  od;

  while Length(farm!.outqueue) < Length(files) do
    DoQueues(farm, false);
  od;

  out := Pickup(farm);
  Kill(farm);
  return out;
end);

#

BindGlobal("SemigroupsTestRec", rec());
MakeReadWriteGlobal("SemigroupsTestRec");

InstallGlobalFunction(SemigroupsStartTest,
function()
  local record;

  record := SemigroupsTestRec;

  # store current info levels
  record.InfoLevelInfoWarning := InfoLevel(InfoWarning);
  record.InfoLevelInfoSemigroups := InfoLevel(InfoSemigroups);
  record.InfoLevelInfoPackageLoading := InfoLevel(InfoPackageLoading);

  # store current user preferences
  record.PartialPermDisplayLimit := UserPreference("PartialPermDisplayLimit");
  record.TransformationDisplayLimit
   := UserPreference("TransformationDisplayLimit");
  record.NotationForPartialPerms := UserPreference("NotationForPartialPerms");
  record.NotationForTransformations :=
   UserPreference("NotationForTransformations");
  record.FreeInverseSemigroupElementDisplay :=
    UserPreference("semigroups", "FreeInverseSemigroupElementDisplay");
  
  # store current default options 
  record.SEMIGROUPS_DefaultOptionsRec :=
    ShallowCopy(SEMIGROUPS_DefaultOptionsRec);


  # set info levels
  SetInfoLevel(InfoWarning, 0);
  SetInfoLevel(InfoSemigroups, 0);
  SetInfoLevel(InfoPackageLoading, 0);

  # set user preferences
  SetUserPreference("PartialPermDisplayLimit", 100);
  SetUserPreference("TransformationDisplayLimit", 100);
  SetUserPreference("NotationForPartialPerms", "component");
  SetUserPreference("NotationForTransformations", "input");
  SetUserPreference("semigroups", "FreeInverseSemigroupElementDisplay",
                    "minimal");

  # set default options
  SEMIGROUPS_DefaultOptionsRec.report := false;

  # timing
  record.timeofday := IO_gettimeofday();

  record.STOP_TEST := STOP_TEST;

  UnbindGlobal("STOP_TEST");
  BindGlobal("STOP_TEST", SemigroupsStopTest);
  MakeReadWriteGlobal("STOP_TEST");

  return;
end);

#

InstallGlobalFunction(SemigroupsStopTest,
function(file)
  local timeofday, record, elapsed, str;

  record := SemigroupsTestRec;
  
  # restore info levels
  SetInfoLevel(InfoWarning, record.InfoLevelInfoWarning);
  SetInfoLevel(InfoSemigroups, record.InfoLevelInfoSemigroups);
  SetInfoLevel(InfoSemigroups, record.InfoLevelInfoPackageLoading);

  # restore user preferences
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
  
  # restore default options
  UnbindGlobal("SEMIGROUPS_DefaultOptionsRec");
  BindGlobal("SEMIGROUPS_DefaultOptionsRec",
             record.SEMIGROUPS_DefaultOptionsRec);
  MakeReadWriteGlobal("SEMIGROUPS_DefaultOptionsRec");

  # timing
  timeofday := IO_gettimeofday();

  elapsed := (timeofday.tv_sec - record.timeofday.tv_sec) * 1000
   + Int((timeofday.tv_usec - record.timeofday.tv_usec) / 1000);

  str := "elapsed time: ";
  Append(str, String(elapsed));
  Append(str, "ms\n");

  if not IsBound(GAPInfo.TestData.START_TIME)  then
      Error("Semigroups: SemigroupsStopTest:\n",
            "`STOP_TEST' command without `START_TEST' command for `", file,
            "'");
      return;
  fi;
  Print(GAPInfo.TestData.START_NAME, "\n");

  SetAssertionLevel(GAPInfo.TestData.AssertionLevel);
  Unbind(GAPInfo.TestData.AssertionLevel);
  Unbind(GAPInfo.TestData.START_TIME);
  Unbind(GAPInfo.TestData.START_NAME);
  Print(str);
  UnbindGlobal("STOP_TEST");
  BindGlobal("STOP_TEST", record.STOP_TEST);
  MakeReadWriteGlobal("STOP_TEST");
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
  MakeGAPDocDoc(Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                              "/doc"),
                "main.xml", SemigroupsDocXMLFiles, "semigroups", "MathJax",
                "../../..");
  return;
end);

#

InstallGlobalFunction(SemigroupsTestAll,
function()
  local dir_str, tst, dir, omit, filesplit, test, stringfile, str, filename;

  Print("Reading all .tst files in the directory semigroups/tst/...\n\n");
  dir_str :=
   Concatenation(PackageInfo("semigroups")[1]!.InstallationPath, "/tst");
  tst := DirectoryContents(dir_str);
  dir := Directory(dir_str);

  omit := SemigroupsOmitFromTestManualExamples;

  if Length(omit) > 0 then
    Print("not testing files containing the strings");
    for str in omit do
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n\n");
  fi;

  for filename in tst do

    filesplit := SplitString(filename, ".");
    if Length(filesplit) >= 2 and filesplit[Length(filesplit)] = "tst" then
      test := true;
      stringfile := StringFile(Concatenation(dir_str, "/", filename));
      for str in omit do
        if PositionSublist(stringfile, str) <> fail then
          Print("not testing ", filename, ", it contains a test involving ",
                str, ", which will not work . . .\n\n");
          test := false;
          break;
        fi;
      od;
      if test then
        Print("reading ", dir_str, "/", filename, " . . .\n");
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
   SEMIGROUPS_TEST("testinstall.tst");
end);

#

InstallGlobalFunction(SEMIGROUPS_TEST,
function(file)
  local generic;

  generic := SEMIGROUPS_DefaultOptionsRec.generic;

  Print("Testing with acting semigroup methods enabled . . .\n");
  SEMIGROUPS_DefaultOptionsRec.generic := false;
  Test(Filename(DirectoriesPackageLibrary("semigroups", "tst"),
                file));

  Print("Testing with acting semigroup methods disabled . . .\n");
  SEMIGROUPS_DefaultOptionsRec.generic := true;
  Test(Filename(DirectoriesPackageLibrary("semigroups", "tst"),
                file));

  SEMIGROUPS_DefaultOptionsRec.generic := generic;
  return;
end);

#

InstallGlobalFunction(SemigroupsManualExamples,
function()
  return ExtractExamples(DirectoriesPackageLibrary("semigroups", "doc"),
                         "main.xml", SemigroupsDocXMLFiles, "Single");
end);

# if <arg> is some strings, then any example containing any of these strings is
# omitted from the test...

InstallGlobalFunction(SemigroupsTestManualExamples,
function()
  local ex, omit, str;

  ex := SemigroupsManualExamples();
  omit := SemigroupsOmitFromTestManualExamples;
  if Length(omit) > 0 then
    Print("# not testing examples containing the strings");
    for str in omit do
      ex := Filtered(ex, x -> PositionSublist(x[1][1], str) = fail);
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n");
  fi;
  SemigroupsStartTest();
  RunExamples(ex);
  SemigroupsStopTest("");
  return;
end);
