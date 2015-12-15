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

InstallMethod(ChooseHashFunction, "for a blist and pos int",
[IsBlistRep, IsPosInt],
  function(x, hashlen)
  return rec(func := HASH_FUNC_FOR_BLIST,
             data := hashlen);
end);

#############################################################################
# 1. Put things in the record SEMIGROUPS.
#############################################################################

SEMIGROUPS.ColorizeString := function(arg)
  local string, color, i;

  string := "";
  for i in [1 .. Length(arg) - 1] do
    if not IsString(arg[i]) then
      Append(string, PrintString(arg[i]));
    else
      Append(string, arg[i]);
    fi;
  od;

  if not IsString(arg[Length(arg)]) then
    return string;
  fi;

  color := arg[Length(arg)];

  if color = "green" then
    return Concatenation("\033[1;32m", string, "\033[0m");
  elif color = "red" then
    return Concatenation("\033[1;31m", string, "\033[0m");
  elif color = "lightblue" then
    return Concatenation("\033[94m", string, "\033[0m");
  elif color = "magenta" then
    return Concatenation("\033[35m", string, "\033[0m");
  elif color = "back_blue" then
    return Concatenation("\033[1;44m", string, "\033[0m");
  elif color = "back_gray" then
    return Concatenation("\033[1;100m", string, "\033[0m");
  fi;
  return string;
end;

SEMIGROUPS.DocXMLFiles := ["../PackageInfo.g",
                           "attributes-acting.xml",
                           "attributes-inverse.xml",
                           "attributes.xml",
                           "bipartition.xml",
                           "blocks.xml",
                           "boolmat.xml",
                           "conginv.xml",
                           "congpairs.xml",
                           "congrees.xml",
                           "congrms.xml",
                           "congruences.xml",
                           "conguniv.xml",
                           "constructions.xml",
                           "display.xml",
                           "examples.xml",
                           "factor.xml",
                           "freeband.xml",
                           "freeinverse.xml",
                           "graph-inverse.xml",
                           "greens.xml",
                           "ideals.xml",
                           "isomorph.xml",
                           "maximal.xml",
                           "maxplusmat.xml",
                           "normalizer.xml",
                           "orbits.xml",
                           "pbr.xml",
                           "pfmat.xml",
                           "properties.xml",
                           "semibipart.xml",
                           "semigroups.xml",
                           "semipbr.xml",
                           "semipperm.xml",
                           "semiringmat.xml",
                           "semitrans.xml",
                           "utils.xml"];

SEMIGROUPS.TestRec := rec();

SEMIGROUPS.StartTest := function()
  local record;

  record := SEMIGROUPS.TestRec;

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
    ShallowCopy(SEMIGROUPS.DefaultOptionsRec);

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
  SEMIGROUPS.DefaultOptionsRec.report := false;

  # timing
  record.timeofday := IO_gettimeofday();

  record.STOP_TEST := STOP_TEST;

  UnbindGlobal("STOP_TEST");
  BindGlobal("STOP_TEST", SEMIGROUPS.StopTest);
  MakeReadWriteGlobal("STOP_TEST");

  return;
end;

SEMIGROUPS.StopTest := function(file)
  local timeofday, record, elapsed, str;

  record := SEMIGROUPS.TestRec;

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
  SEMIGROUPS.DefaultOptionsRec := record.SEMIGROUPS_DefaultOptionsRec;

  # timing
  timeofday := IO_gettimeofday();

  elapsed := (timeofday.tv_sec - record.timeofday.tv_sec) * 1000
   + Int((timeofday.tv_usec - record.timeofday.tv_usec) / 1000);

  str := "elapsed time: ";
  Append(str, String(elapsed));
  Append(str, "ms\n");

  if not IsBound(GAPInfo.TestData.START_TIME)  then
      ErrorMayQuit("Semigroups: SEMIGROUPS.StopTest:\n",
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
end;

SEMIGROUPS.Test := function(arg)
  local file, opts, generic, split, print_file, width, enabled, disabled;

  if Length(arg) = 0 then
    ErrorMayQuit("Semigroups: SEMIGROUPS.Test: usage,\n",
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

  generic := SEMIGROUPS.DefaultOptionsRec.generic;
  split := SplitString(file, "/");
  print_file := JoinStringsWithSeparator(split{
                                         [Length(split) - 2 .. Length(split)]},
                                         "/");

  width := SizeScreen()[1] - 3;
  if not opts.silent then
    Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n");
  fi;
  Print("Testing file: ", print_file, "\nwith acting methods ",
        SEMIGROUPS.ColorizeString("ENABLED", "lightblue"), " . . .\n");
  if not opts.silent then
    Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  fi;

  SEMIGROUPS.DefaultOptionsRec.generic := false;
  enabled := Test(file);
  if not opts.silent then
    Print("\n");
    if enabled then
      Print(SEMIGROUPS.ColorizeString("PASSED!", "green"));
    else
      Print(SEMIGROUPS.ColorizeString("FAILED!", "red"));
    fi;
    Print("\n\n");
    Print(Concatenation(ListWithIdenticalEntries(width, "#")));
  fi;
  Print("\n");
  Print("Testing file: ", print_file, "\nwith acting methods ",
        SEMIGROUPS.ColorizeString("DISABLED", "magenta"), " . . .\n");

  if not opts.silent then
    Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  fi;
  SEMIGROUPS.DefaultOptionsRec.generic := true;
  disabled := Test(file);
  Print("\n");
  if not opts.silent then
    if disabled then
      Print(SEMIGROUPS.ColorizeString("PASSED!", "green"));
    else
      Print(SEMIGROUPS.ColorizeString("FAILED!", "red"));
    fi;
    Print("\n\n");
  fi;

  SEMIGROUPS.DefaultOptionsRec.generic := generic;
  return enabled and disabled;
end;

SEMIGROUPS.ManualExamples := function()
  return ExtractExamples(DirectoriesPackageLibrary("semigroups", "doc"),
                         "main.xml", SEMIGROUPS.DocXMLFiles, "Single");
end;

SEMIGROUPS.RunExamples := function(exlists, excluded)
  local oldscr, l, sp, bad, s, start_time, test, end_time, elapsed, pex, j, ex,
        i;

  oldscr := SizeScreen();
  SizeScreen([72, oldscr[2]]);
  for j in [1 .. Length(exlists)] do
    if j in excluded then
      Print(SEMIGROUPS.ColorizeString("# Skipping list ", j, " . . .\n",
                                      "back_blue"));
    else
      l := exlists[j];
      Print(SEMIGROUPS.ColorizeString("# Running list ", j, " . . .",
                                      "back_gray"));
      START_TEST("");
      for ex in l do
        sp := SplitString(ex[1], "\n", "");
        bad := Filtered([1 .. Length(sp)], function(i)
                                             return Length(sp[i]) > 72;
                                           end);
        s := InputTextString(ex[1]);

        start_time := IO_gettimeofday();
        test := Test(s, rec(ignoreComments := false,
                            width := 72,
                            EQ := EQ,
                            reportDiff := Ignore));
        end_time := IO_gettimeofday();
        CloseStream(s);
        elapsed := (end_time.tv_sec - start_time.tv_sec) * 1000
                   + Int((end_time.tv_usec - start_time.tv_usec) / 1000);
        pex := TEST.lastTestData;

        Print(SEMIGROUPS.ColorizeString(" elapsed time: ", elapsed,
                                        "back_gray"));
        Print("\n");

        if Length(bad) > 0  then
          Print(SEMIGROUPS.ColorizeString("# WARNING: Overlong lines ", bad,
                                         " in ", ex[2]{[1 .. 3]}, "red"));
          Print("\n");
        fi;

        if test = false  then
          for i in [1 .. Length(pex[1])]  do
            if EQ(pex[2][i], pex[4][i]) <> true then
              Print("\033[1;31m########> Diff in ", ex[2]{[1 .. 3]},
                    "\n# Input is:\n");
              PrintFormattedString(pex[1][i]);
              Print("# Expected output:\n");
              PrintFormattedString(pex[2][i]);
              Print("# But found:\n");
              PrintFormattedString(pex[4][i]);
              Print("########\033[0m\n");
            fi;
          od;
        fi;
      od;
    fi;
  od;
  SizeScreen(oldscr);
  return;
end;

SEMIGROUPS.TestManualExamples := function(arg)
  local ex, omit, width, generic, exclude, str;

  # TODO add extreme/standard tests for those examples below where it makes
  # sense.
  exclude := [48, 58, 61, 66, 87, 88, 89, 91, 93, 94, 96, 97, 100, 101, 102,
              103, 108, 109, 110, 113, 114, 115, 119, 126, 127, 133, 137, 139,
              141, 194, 195, 196];
  # 103 takes ages, 114 should be in an extreme test

  ex := SEMIGROUPS.ManualExamples();
  if Length(arg) = 1 then
    if IsPosInt(arg[1]) and arg[1] <= Length(ex) then
      ex := [ex[arg[1]]];
    elif IsHomogeneousList(arg[1])
        and ForAll(arg[1], x -> IsPosInt(x) and x <= Length(ex)) then
      ex := ex{arg};
    else
      ErrorMayQuit("Semigroups: SEMIGROUPS.TestManualExamples: usage,\n",
                   "the argument must be a pos int or list of pos ints,");
    fi;
  elif Length(arg) > 1 then
    ErrorMayQuit("Semigroups: SEMIGROUPS.TestManualExamples: usage,\n",
                 "there should be 0 or 1 argument,");
  fi;

  omit := SEMIGROUPS.OmitFromTests;
  if Length(omit) > 0 then
    Print("# not testing examples containing the strings");
    for str in omit do
      ex := Filtered(ex, x -> PositionSublist(x[1][1], str) = fail);
      Print(", \"", str, "\"");
    od;
    Print(" . . .\n");
  fi;

  width := SizeScreen()[1] - 3;
  generic := SEMIGROUPS.DefaultOptionsRec.generic;

  SEMIGROUPS.DefaultOptionsRec.generic := false;
  Print("\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n");
  Print("Testing manual examples with acting methods ",
        SEMIGROUPS.ColorizeString("ENABLED", "lightblue"), " . . .\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  SEMIGROUPS.StartTest();
  SEMIGROUPS.RunExamples(ex, []);
  SEMIGROUPS.StopTest("");

  SEMIGROUPS.DefaultOptionsRec.generic := true;
  GASMAN("collect");
  Print("\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n");
  Print("NOT Testing file manual examples with acting methods ",
        SEMIGROUPS.ColorizeString("DISABLED", "magenta"), " . . .\n");
  Print(Concatenation(ListWithIdenticalEntries(width, "#")), "\n\n");
  # SEMIGROUPS.StartTest();

  # SEMIGROUPS.RunExamples(ex, exclude);
  # SEMIGROUPS.StopTest("");
  #TODO make SEMIGROUPS.StopTest accept no args, or 1 arg

  SEMIGROUPS.DefaultOptionsRec.generic := generic;
  return;
end;

#############################################################################
# 2. User/global functions
#############################################################################

InstallGlobalFunction(SemigroupsMakeDoc,
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                              "/doc"),
                "main.xml", SEMIGROUPS.DocXMLFiles, "semigroups", "MathJax",
                "../../..");
  return;
end);

InstallGlobalFunction(SemigroupsTestStandard,
function(arg)
  local opts, file_ext, is_testable, dir, contents, farm, nr_tests, out,
        start_time, pass, end_time, elapsed, str, filename;

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
    for str in SEMIGROUPS.OmitFromTests do
      if PositionSublist(stringfile, str) <> fail then
        Print("not testing ", file, ", it contains a test involving ",
              str, ", which will not work . . .\n\n");
        return false;
      fi;
    od;
    return true;
  end;

  if Length(SEMIGROUPS.OmitFromTests) > 0 then
    Print("not testing files containing the strings");
    for str in SEMIGROUPS.OmitFromTests do
      PRINT_STRINGIFY(", \"", str, "\"");
    od;
    PRINT_STRINGIFY(" . . .\n\n");
  fi;

  dir  := Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                                    "/tst/standard");
  contents := DirectoryContents(dir);

  SemigroupsTestInstall(rec(silent := false));

  if opts.parallel then
    farm := ParWorkerFarmByFork(SEMIGROUPS.Test,
                                rec(NumberJobs := 3));
    nr_tests := 0;
  else
    out := true;
  fi;

  elapsed := 0;

  for filename in contents do
    if file_ext(filename) = "tst" and is_testable(dir, filename) then
      if opts.parallel then
        nr_tests := nr_tests + 1;
        Submit(farm, [Filename(Directory(dir), filename),
                      rec(silent := false)]);
      else
        start_time := IO_gettimeofday();
        pass := SEMIGROUPS.Test(Filename(Directory(dir), filename),
                                rec(silent := false));

        end_time := IO_gettimeofday();
        elapsed := elapsed + (end_time.tv_sec - start_time.tv_sec) * 1000
                   + Int((end_time.tv_usec - start_time.tv_usec) / 1000);
        if not pass then
          out := false;
        fi;
      fi;
    fi;
  od;

  if opts.parallel then
    while Length(farm!.outqueue) < nr_tests do
      DoQueues(farm, false);
    od;

    out := Pickup(farm);
    Kill(farm);
  fi;

  Print("TOTAL elapsed time: ", String(elapsed), "ms\n");
  return out;
end);

InstallGlobalFunction(SemigroupsTestInstall,
function(arg)
  local opts;

  if Length(arg) = 0 then
    opts := rec();
  else
    opts := arg[1];
  fi;
  #TODO check args
  return SEMIGROUPS.Test(Filename(DirectoriesPackageLibrary("semigroups",
                                                            "tst"),
                                  "testinstall.tst"),
                         opts);
end);
