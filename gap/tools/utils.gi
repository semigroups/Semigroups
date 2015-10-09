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

BindGlobal("SEMIGROUPS_DocXMLFiles", ["../PackageInfo.g",
                                      "attributes-acting.xml",
                                      "attributes-inverse.xml",
                                      "attributes.xml",
                                      "bipartition.xml",
                                      "blocks.xml",
                                      "boolmat.xml",
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
                                      "semiringmat.xml",
                                      "semibipart.xml",
                                      "semigroups.xml",
                                      "semipperm.xml",
                                      "semitrans.xml",
                                      "simple-cong.xml",
                                      "univ-cong.xml",
                                      "utils.xml"]);

InstallGlobalFunction(SemigroupsMakeDoc,
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                              "/doc"),
                "main.xml", SEMIGROUPS_DocXMLFiles, "semigroups", "MathJax",
                "../../..");
  return;
end);

BindGlobal("SEMIGROUPS_TestRec", rec());
MakeReadWriteGlobal("SEMIGROUPS_TestRec");

InstallGlobalFunction(SEMIGROUPS_StartTest,
function()
  local record;

  record := SEMIGROUPS_TestRec;

  # store current info levels
  record.InfoLevelInfoWarning := InfoLevel(InfoWarning);
  record.InfoLevelInfoSemigroups := InfoLevel(InfoSemigroups);
  record.InfoLevelInfoOrb := InfoLevel(InfoOrb);
  record.InfoLevelInfoGenSS := InfoLevel(InfoGenSS);
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
  SetInfoLevel(InfoOrb, 0);
  SetInfoLevel(InfoGenSS, 0);
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
  BindGlobal("STOP_TEST", SEMIGROUPS_StopTest);
  MakeReadWriteGlobal("STOP_TEST");

  return;
end);

#

InstallGlobalFunction(SEMIGROUPS_StopTest,
function(file)
  local timeofday, record, elapsed, str;

  record := SEMIGROUPS_TestRec;

  # restore info levels
  SetInfoLevel(InfoWarning, record.InfoLevelInfoWarning);
  SetInfoLevel(InfoSemigroups, record.InfoLevelInfoSemigroups);
  SetInfoLevel(InfoOrb, record.InfoLevelInfoOrb);
  SetInfoLevel(InfoOrb, record.InfoLevelInfoGenSS);
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
      ErrorMayQuit("Semigroups: SEMIGROUPS_StopTest:\n",
                   "`STOP_TEST' command without `START_TEST' command for `",
                   file,
                   "'");
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

# TODO redo this

InstallGlobalFunction(SEMIGROUPS_TestAll,
function()
  local dir_str, tst, dir, omit, filesplit, test, stringfile, str, filename;

  Print("Reading all .tst files in the directory semigroups/tst/...\n\n");
  dir_str :=
   Concatenation(PackageInfo("semigroups")[1]!.InstallationPath, "/tst");
  tst := DirectoryContents(dir_str);
  dir := Directory(dir_str);

  omit := SEMIGROUPS_OmitFromTests;

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

BindGlobal("SEMIGROUPS_PF",
function(pass)
  if pass then
    return "\033[1;32mPASSED!\033[0m\n";
  else
    return "\033[1;31mFAILED!\033[0m\n";
  fi;
end);

InstallGlobalFunction(SemigroupsTestStandard,
function(arg)
  local opts, file_ext, is_testable, tst_dir, contents, subdirs, str, farm,
        nr_tests, out, subdir, filename;

  if Length(arg) = 1 and IsRecord(arg[1]) then
    opts := arg[1];
    if not IsBound(opts.parallel) or not IsBool(opts.parallel) then
      opts.parallel := false;
    fi;
  else
    opts := rec(parallel := false);
  fi;

  Print("\n");

  file_ext := function(str)
    local split;
    split := SplitString(str, ".");
    if Length(split) > 1 then
      return split[Length(split)];
    else
      return "";
    fi;
  end;

  is_testable := function(dir, file)
    local stringfile, str;
    file := Concatenation(dir, "/", file);
    stringfile := StringFile(file);
    for str in SEMIGROUPS_OmitFromTests do
      if PositionSublist(stringfile, str) <> fail then
        Print("not testing ", file, ", it contains a test involving ",
              str, ", which will not work . . .\n\n");
        return false;
      fi;
    od;
    return true;
  end;

  if Length(SEMIGROUPS_OmitFromTests) > 0 then
    Print("not testing files containing the strings");
    for str in SEMIGROUPS_OmitFromTests do
      PRINT_STRINGIFY(", \"", str, "\"");
    od;
    PRINT_STRINGIFY(" . . .\n\n");
  fi;

  tst_dir  := Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                            "/tst/standard");
  contents := DirectoryContents(tst_dir);
  subdirs := [];

  for str in contents do
    # TODO remove: <<< and str = "attributes" >>> from below
    if str <> ".." and str <> "." and str = "attributes" then
      str := Concatenation(tst_dir, "/", str);
      if IsDirectoryPath(str) then
        Add(subdirs, str);
      fi;
    fi;
  od;

  SemigroupsTestInstall(rec(silent := false));
  if opts.parallel then
    farm := ParWorkerFarmByFork(SEMIGROUPS_Test,
                                rec(NumberJobs := 3));
    nr_tests := 0;
  else
    out := true;
  fi;

  for subdir in subdirs do
    contents := DirectoryContents(Directory(subdir));
    for filename in contents do
      if file_ext(filename) = "tst" and is_testable(subdir, filename) then
        if opts.parallel then
          nr_tests := nr_tests + 1;
          Submit(farm, [Filename(Directory(subdir), filename),
                        rec(silent := false)]);
        else
          if not SEMIGROUPS_Test(Filename(Directory(subdir), filename),
                                 rec(silent := false)) then
            out := false;
          fi;
        fi;
      fi;
    od;
  od;

  if opts.parallel then
    while Length(farm!.outqueue) < nr_tests do
      DoQueues(farm, false);
    od;

    out := Pickup(farm);
    Kill(farm);
  fi;

  return out;
end);

#

InstallGlobalFunction(SemigroupsTestInstall,
function(arg)
  local opts;

  if Length(arg) = 0 then
    opts := rec();
  else
    opts := arg[1];
  fi;
  #TODO check args
  return SEMIGROUPS_Test(Filename(DirectoriesPackageLibrary("semigroups",
                                                            "tst"),
                                  "testinstall.tst"),
                         opts);
end);

#

InstallGlobalFunction(SEMIGROUPS_Test,
function(arg)
  local file, opts, generic, split, print_file, width, enabled, disabled;

  if Length(arg) = 0 then
    ErrorMayQuit("Semigroups: SEMIGROUPS_Test: usage,\n",
                 "no arguments have been supplied,");
  fi;

  file := arg[1];

  if Length(arg) = 1 then
    opts := rec();
  else
    opts := arg[2];
  fi;

  # TODO process opts
  if not IsBound(opts.silent) then
    opts.silent := true;
  fi;

  generic := SEMIGROUPS_DefaultOptionsRec.generic;
  split := SplitString(file, "/");
  print_file := JoinStringsWithSeparator(split{
                                         [Length(split) - 2 .. Length(split)]},
                                         "/");

  width := SizeScreen()[1] - 3;
  if not opts.silent then
    Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n");
  fi;
  Print("Testing ", print_file,
        " [acting methods \033[44mENABLED\033[0m] . . .", "\n");
  if not opts.silent then
    Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  fi;

  SEMIGROUPS_DefaultOptionsRec.generic := false;
  enabled := Test(file);
  if not opts.silent then
    Print("\n", SEMIGROUPS_PF(enabled), "\n");
    Print(Concatenation(ListWithIdenticalEntries(width, "#")));
  fi;
  Print("\n");
  Print("Testing ", print_file,
        " [acting methods \033[44mDISABLED\033[0m] . . .", "\n");

  if not opts.silent then
    Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  fi;
  SEMIGROUPS_DefaultOptionsRec.generic := true;
  disabled := Test(file);
  if not opts.silent then
    Print("\n", SEMIGROUPS_PF(disabled));
  fi;
  Print("\n");

  SEMIGROUPS_DefaultOptionsRec.generic := generic;
  return enabled and disabled;
end);

#

InstallGlobalFunction(SEMIGROUPS_ManualExamples,
function()
  return ExtractExamples(DirectoriesPackageLibrary("semigroups", "doc"),
                         "main.xml", SEMIGROUPS_DocXMLFiles, "Single");
end);

BindGlobal("SEMIGROUPS_RunExamples",
function(exlists, excluded)
  local oldscr, l, sp, bad, s, start_time, test, end_time, elapsed, pex, new,
   inp, j, ex, i, attedStrin;

  oldscr := SizeScreen(  );
  SizeScreen( [ 72, oldscr[2] ] );
  for j  in [ 1 .. Length( exlists ) ]  do
    if j in excluded then
      Print( "\033[1;44m# Skipping list ", j, " . . .\033[0m\n" );
    else
      l := exlists[j];
      Print( "\033[1;100m# Running list ", j, " . . .\033[0m" );
      START_TEST( "" );
      for ex in l do
        sp := SplitString( ex[1], "\n", "" );
        bad := Filtered( [ 1 .. Length( sp ) ], function ( i )
          return Length( sp[i] ) > 72;
        end );
        s := InputTextString( ex[1] );

        start_time := IO_gettimeofday();
        test := Test( s, rec( ignoreComments := false,
                              width := 72,
                              EQ := EQ,
                              reportDiff := Ignore ) );
        end_time := IO_gettimeofday();
        CloseStream( s );
        elapsed := (end_time.tv_sec - start_time.tv_sec) * 1000
                   + Int((end_time.tv_usec - start_time.tv_usec) / 1000);
        pex := TEST.lastTestData;

        Print("\033[1;100m  elapsed time: ", String(elapsed), "ms\033[0m\n");

        if Length( bad ) > 0  then
          Print( "\033[1;31m# WARNING: Overlong lines ", bad, " in ",
          ex[2]{[ 1 .. 3 ]}, "\033[0m\n" );
        fi;

        if test = false  then
          for i  in [ 1 .. Length( pex[1] ) ]  do
            if EQ( pex[2][i], pex[4][i] ) <> true then
              Print( "\033[1;31m########> Diff in ", ex[2]{[ 1 .. 3 ]},
              "\n# Input is:\n" );
              PrintFormattedString( pex[1][i] );
              Print( "# Expected output:\n" );
              PrintFormattedString( pex[2][i] );
              Print( "# But found:\n" );
              PrintFormattedString( pex[4][i] );
              Print( "########\033[0m\n" );
            fi;
          od;
        fi;

        if test = false  then#FIXME is this needed?
          new := "";
          for i  in [ 1 .. Length( pex[1] ) ]  do
            inp := Concatenation( "gap> ",
               JoinStringsWithSeparator(
               SplitString( pex[1][i], "\n", "" ), "\n> " ), "\n" );
               Append( new, inp );
               Append( new, pex[4][i] );
          od;
          ex[2][4] := new;
        fi;
      od;
    fi;
  od;
  SizeScreen( oldscr );
  return;
end);
#

InstallGlobalFunction(SEMIGROUPS_TestManualExamples,
function(arg)
  local ex, omit, width, generic, exclude, str;

  ex := SEMIGROUPS_ManualExamples();
  if Length(arg) = 1 then
    if IsPosInt(arg[1]) and arg[1] <= Length(ex) then
      ex := [ex[arg[1]]];
    elif IsHomogeneousList(arg[1])
        and ForAll(arg[1], x -> IsPosInt(x) and x <= Length(ex)) then
      ex := ex{arg};
    else
      ErrorMayQuit("Semigroups: SEMIGROUPS_TestManualExamples: usage,\n",
                   "the argument must be a pos int or list of pos ints,");
    fi;
  elif Length(arg) > 1 then
    ErrorMayQuit("Semigroups: SEMIGROUPS_TestManualExamples: usage,\n",
                 "there should be 0 or 1 argument,");
  fi;

  omit := SEMIGROUPS_OmitFromTests;
  if Length(omit) > 0 then
    Print("# not testing examples containing the strings");
    for str in omit do
      ex := Filtered(ex, x -> PositionSublist(x[1][1], str) = fail);
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n");
  fi;

  width := SizeScreen()[1] - 3;
  generic := SEMIGROUPS_DefaultOptionsRec.generic;

  SEMIGROUPS_DefaultOptionsRec.generic := false;
  Print("\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n");
  Print("Testing manual examples [acting methods ",
        "\033[1;44mENABLED\033[0m] . . .\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  SEMIGROUPS_StartTest();
  SEMIGROUPS_RunExamples(ex, []);
  SEMIGROUPS_StopTest("");

  # TODO add extreme/standard tests for those examples below where it makes
  # sense.
  exclude := [48, 58, 61, 66, 87, 88, 89, 91, 93, 94, 96, 97, 100, 101, 102,
              103, 108, 109, 110, 113, 114, 115, 119, 126, 127, 133, 137, 139,
              141, 194, 195, 196];
  # 103 takes ages, 114 should be in an extreme test

  SEMIGROUPS_DefaultOptionsRec.generic := true;
  GASMAN("collect");
  Print("\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n");
  Print("Testing manual examples [acting methods ",
        "\033[1;44mDISABLED\033[0m] . . .\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  SEMIGROUPS_StartTest();

  SEMIGROUPS_RunExamples(ex, exclude);
  SEMIGROUPS_StopTest("");
  #TODO make SEMIGROUPS_StopTest accept no args, or 1 arg

  SEMIGROUPS_DefaultOptionsRec.generic := generic;
  return;
end);


