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

# No attempt has been made to get good test coverage for this file, since it
# will hopefully be binned in the near future.

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
    return Concatenation("\033[32m", string, "\033[0m");
  elif color = "red" then
    return Concatenation("\033[31m", string, "\033[0m");
  elif color = "lightblue" then
    return Concatenation("\033[94m", string, "\033[0m");
  elif color = "magenta" then
    return Concatenation("\033[35m", string, "\033[0m");
  elif color = "back_blue" then
    return Concatenation("\033[44m", string, "\033[0m");
  elif color = "back_gray" then
    return Concatenation("\033[100m", string, "\033[0m");
  fi;
  return string;
end;

SEMIGROUPS.DocXMLFiles := ["../PackageInfo.g",
                           "attr.xml",
                           "attract.xml",
                           "attrinv.xml",
                           "bipart.xml",
                           "blocks.xml",
                           "boolmat.xml",
                           "cong.xml",
                           "conginv.xml",
                           "conglatt.xml",
                           "congpairs.xml",
                           "congrees.xml",
                           "congrms.xml",
                           "conguniv.xml",
                           "display.xml",
                           "elements.xml",
                           "factor.xml",
                           "ffmat.xml",
                           "freeband.xml",
                           "freeinverse.xml",
                           "fropin.xml",
                           "gree.xml",
                           "grpffmat.xml",
                           "ideals.xml",
                           "io.xml",
                           "isomorph.xml",
                           "isorms.xml",
                           "maximal.xml",
                           "maxplusmat.xml",
                           "normalizer.xml",
                           "orbits.xml",
                           "pbr.xml",
                           "properties.xml",
                           "semiact.xml",
                           "semibipart.xml",
                           "semiboolmat.xml",
                           "semicons.xml",
                           "semiex.xml",
                           "semiffmat.xml",
                           "semigraph.xml",
                           "semigroups.xml",
                           "semigrp.xml",
                           "semimaxplus.xml",
                           "semipbr.xml",
                           "semipperm.xml",
                           "semiringmat.xml",
                           "semitrans.xml",
                           "trans.xml",
                           "utils.xml"];

SEMIGROUPS.TestRec := rec(elapsed_last_test := 0);

SEMIGROUPS.TestRec.reportDiff := function(inp, expout, found, fnam, line, time)
  Print("\033[31m######## > Diff in:\n");
  if IsStream(fnam)  then
    Print("test stream, line ", line, "\n");
  else
    Print(fnam, ":", line, "\n");
  fi;
  Print("# Input is:\n", inp);
  Print("# Expected output:\n", expout);
  Print("# But found:\n", found);
  Print("########\033[0m\n");
  return;
end;

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
  return;
end;

SEMIGROUPS.StopTest := function()
  local record;

  record := SEMIGROUPS.TestRec;
  record.elapsed_last2_test := record.elapsed_last_test;
  record.elapsed_last_test := Runtime() - GAPInfo.TestData.START_TIME;

  # restore info levels
  SetInfoLevel(InfoWarning, record.InfoLevelInfoWarning);
  SetInfoLevel(InfoSemigroups, record.InfoLevelInfoSemigroups);
  SetInfoLevel(InfoOrb, record.InfoLevelInfoOrb);
  SetInfoLevel(InfoGenSS, record.InfoLevelInfoGenSS);
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
  return;
end;

SEMIGROUPS.Test := function(arg)
  local file, acting, split, pos, range, print_file, string_file, enabled,
  disabled;

  if Length(arg) = 1 then
    file := arg[1];
  else
    ErrorNoReturn("Semigroups: SEMIGROUPS.Test: usage,\n",
                  "there must be only 1 argument,");
  fi;

  if not IsString(file) then
    ErrorNoReturn("Semigroups: SEMIGROUPS.Test: usage,\n",
                  "the first arg must be a string,");
  fi;

  acting := SEMIGROUPS.DefaultOptionsRec.acting;

  split  := SplitString(file, "/");
  pos    := Position(split, "tst");
  if pos <> fail then
    range := [pos .. Length(split)];
  else
    range := [Length(split) - 2 .. Length(split)];
  fi;
  print_file  := JoinStringsWithSeparator(split{range}, "/");
  string_file := StringFile(file);

  if IsEmpty(string_file) then
    Print("File: ", print_file, " is empty!\n");
    return fail;
  elif Length(string_file) < 600 then
    Print("File: ", print_file, " probably contains no tests!\n");
  fi;

  Print("Testing file: ", print_file, " (acting := true)\n");
  SEMIGROUPS.DefaultOptionsRec.acting := true;
  enabled := Test(file, SEMIGROUPS.TestRec);

  Print("Testing file: ", print_file, " (acting := false)\n");
  SEMIGROUPS.DefaultOptionsRec.acting := false;
  disabled := Test(file, SEMIGROUPS.TestRec);

  SEMIGROUPS.DefaultOptionsRec.acting := acting;
  return enabled and disabled;
end;

SEMIGROUPS.ManualExamples := function()
  return ExtractExamples(DirectoriesPackageLibrary("semigroups", "doc"),
                         "main.xml", SEMIGROUPS.DocXMLFiles, "Single");
end;

SEMIGROUPS.RunExamples := function(exlists, excluded)
  local oldscr, passed, pad, l, sp, bad, s, start_time, test, end_time,
  elapsed, pex, j, ex, i;

  oldscr := SizeScreen();
  SizeScreen([72, oldscr[2]]);
  passed := true;
  pad := function(nr)
    nr := Length(String(Length(exlists))) - Length(String(nr)) + 1;
    return List([1 .. nr], x -> ' ');
  end;

  for j in [1 .. Length(exlists)] do
    if j in excluded then
      Print(SEMIGROUPS.ColorizeString("# Skipping example ", j, pad(j),
                                      " . . .\n", "back_blue"));
    else
      l := exlists[j];
      Print("# Running example ", j, pad(j), " . . .");
      START_TEST("");
      for ex in l do
        sp := SplitString(ex[1], "\n", "");
        bad := Filtered([1 .. Length(sp)], i -> Length(sp[i]) > 72);
        s := InputTextString(ex[1]);

        start_time := IO_gettimeofday();
        test := Test(s, rec(ignoreComments := false,
                            width := 72,
                            EQ := EQ,
                            reportDiff := Ignore,
                            showProgress := false));
        end_time := IO_gettimeofday();
        CloseStream(s);
        elapsed := (end_time.tv_sec - start_time.tv_sec) * 1000
                   + Int((end_time.tv_usec - start_time.tv_usec) / 1000);
        pex := TEST.lastTestData;

        Print(" msecs: ", elapsed, "\n");

        if Length(bad) > 0 then
          Print(SEMIGROUPS.ColorizeString("# WARNING: Overlong lines ", bad,
                                         " in ", ex[2]{[1 .. 3]}, "red"));
          Print("\n");
          passed := false;
        fi;

        if test = false then
          for i in [1 .. Length(pex[1])] do
            if EQ(pex[2][i], pex[4][i]) <> true then
              Print("\033[31m########> Diff in:\n",
                    "# ", ex[2][1], ":", ex[2][2],
                    "\n# Input is:\n");
              PrintFormattedString(pex[1][i]);
              Print("# Expected output:\n");
              PrintFormattedString(pex[2][i]);
              Print("# But found:\n");
              PrintFormattedString(pex[4][i]);
              Print("########\033[0m\n");
              passed := false;
            fi;
          od;
        fi;
      od;
    fi;
  od;
  SizeScreen(oldscr);
  return passed;
end;

SEMIGROUPS.TestManualExamples := function(arg)
  local ex, omit, width, acting, passed, str;

  ex := SEMIGROUPS.ManualExamples();
  if Length(arg) = 1 then
    if IsPosInt(arg[1]) and arg[1] <= Length(ex) then
      ex := [ex[arg[1]]];
    elif IsHomogeneousList(arg[1])
        and ForAll(arg[1], x -> IsPosInt(x) and x <= Length(ex)) then
      ex := ex{arg};
    else
      ErrorNoReturn("Semigroups: SEMIGROUPS.TestManualExamples: usage,\n",
                    "the argument must be a pos int or list of pos ints,");
    fi;
  elif Length(arg) > 1 then
    ErrorNoReturn("Semigroups: SEMIGROUPS.TestManualExamples: usage,\n",
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
  acting := SEMIGROUPS.DefaultOptionsRec.acting;

  SEMIGROUPS.DefaultOptionsRec.acting := true;
  SEMIGROUPS.StartTest();
  passed := SEMIGROUPS.RunExamples(ex, []);
  SEMIGROUPS.StopTest();

  SEMIGROUPS.DefaultOptionsRec.acting := acting;
  return passed;
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
function()
  local dir;
  dir  := Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                                    "/tst/standard");
  return SEMIGROUPS.RunTestsDir(dir);
end);

InstallGlobalFunction(SemigroupsTestExtreme,
function()
  local dir;
  dir  := Concatenation(PackageInfo("semigroups")[1]!.InstallationPath,
                                    "/tst/extreme");
  return SEMIGROUPS.RunTestsDir(dir);
end);

SEMIGROUPS.RunTestsDir := function(dir)
  local file_ext, is_testable, contents, failed, passed, elapsed, pass,
  elapsed_this_test, str, filename;

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

  contents := ["../testinstall.tst"];
  Append(contents, DirectoryContents(dir));

  failed  := [];
  passed  := [];
  elapsed := 0;

  for filename in contents do
    if file_ext(filename) = "tst" and is_testable(dir, filename) then
      pass := SEMIGROUPS.Test(Filename(Directory(dir), filename));
      elapsed_this_test := SEMIGROUPS.TestRec.elapsed_last_test
                           + SEMIGROUPS.TestRec.elapsed_last2_test;
      elapsed := elapsed + elapsed_this_test;
      if pass <> fail then # No test was carried out
        if not pass then
          Add(failed, [filename,
                       "FAILED",
                       Concatenation("msecs: ", String(elapsed_this_test))]);
        else
          Add(passed, [filename,
                       "PASSED",
                       Concatenation("msecs: ", String(elapsed_this_test))]);
        fi;
      fi;
    fi;
  od;

  Print("Total msecs: ", String(elapsed), "\n");
  GASMAN("collect");

  return [failed, passed];
end;

InstallGlobalFunction(SemigroupsTestInstall,
function()
  GASMAN("collect");
  return SEMIGROUPS.Test(Filename(DirectoriesPackageLibrary("semigroups",
                                                            "tst"),
                                  "testinstall.tst"));
end);

# The following is based on doc/ref/testconsistency.g

# Detect which ManSection should be used to document obj. Returns one of
# "Func", "Oper", "Meth", "Filt", "Prop", "Attr", "Var", "Fam", "InfoClass"
#
# See PRINT_OPERATION where some of the code below is borrowed

SEMIGROUPS.ManSectionType := function(op)
  local   class, flags, types, catok, repok, propok, seenprop, t;
  if IsInfoClass(op) then
    return "InfoClass";
  elif IsFamily(op) then
    return "Fam";
  elif not IsFunction(op) then
    return "Var";
  elif IsFunction(op) and not IsOperation(op) then
    return "Func";
  elif IsOperation(op) then
    class := "Oper";
    if IS_IDENTICAL_OBJ(op, IS_OBJECT) then
      class := "Filt";
    elif op in CONSTRUCTORS then
      class := "Constructor";
      # seem to never get one
    elif IsFilter(op) then
      class := "Filt";
      flags := TRUES_FLAGS(FLAGS_FILTER(op));
      types := INFO_FILTERS{flags};
      catok := true;
      repok := true;
      propok := true;
      seenprop := false;
      for t in types do
        if not t in FNUM_REPS then
          repok := false;
        fi;
        if not t in FNUM_CATS then
          catok := false;
        fi;
        if not t in FNUM_PROS and not t in FNUM_TPRS then
          propok := false;
        fi;
        if t in FNUM_PROS then
          seenprop := true;
        fi;
      od;
      if seenprop and propok then
        class := "Prop";
      elif catok then
        class := "Filt";
        # in PRINT_OPERATION - "Category";
      elif repok then
        class := "Filt";
        # in PRINT_OPERATION - "Representation";
      fi;
    elif Tester(op) <> false  then
      # op is an attribute
      class := "Attr";
    fi;
    return class;
  else
    return fail;
  fi;
end;

# Checks whether ManSections are using the right kind of elements

SEMIGROUPS.CheckManSectionTypes := function(doc, verbose...)
  local display_warnings, types, r, x, errcount, name, pos, obj, man, y, yint,
  referrcount, warncount, type, matches, match, matches2, stats,
  elt, t, s;

  if Length(verbose) = 0 then
    display_warnings := false;
  else
    display_warnings := verbose[1];
  fi;
  types := ["Func", "Oper", "Meth", "Filt", "Prop", "Attr", "Var", "Fam",
            "InfoClass"];
  r := ParseTreeXMLString(doc[1]);
  CheckAndCleanGapDocTree(r);
  x := XMLElements(r, types);
  errcount := 0;
  Print("****************************************************************\n");
  Print("*** Checking types in ManSections \n");
  Print("****************************************************************\n");
  for elt in x do
    name := elt.attributes.Name;
    if not name in ["IsBound", "Unbind", "Info", "Assert", "TryNextMethod",
                    "QUIT", "-infinity"] then
      if EvalString(Concatenation("IsBound(", name, ")")) <> true then
        pos := OriginalPositionDocument(doc[2], elt.start);
        Print(pos[1], ":", pos[2], " : ", name, " is unbound \n");
        errcount := errcount + 1;
      else
        obj := EvalString(name);
        man := SEMIGROUPS.ManSectionType(obj);
        # we allow to use "Meth" for "Oper" but probably should issue a warning
        # if there is no at least one "Oper" for any "Meth"
        if (man <> elt.name) and
            not (man in ["Attr", "Prop", "Oper"] and elt.name = "Meth") then
          pos := OriginalPositionDocument(doc[2], elt.start);
          Print(pos[1], ":", pos[2], " : ", name, " uses ", elt.name,
                " instead of ", man, "\n");
          errcount := errcount + 1;
        fi;
      fi;
    fi;
  od;

  Print("****************************************************************\n");
  Print("*** Checking types in cross-references \n");
  Print("****************************************************************\n");
  y := XMLElements(r, ["Ref"]);
  Print("Found ", Length(y), " Ref elements ");
  yint := Filtered(y, elt -> not IsBound(elt.attributes.BookName)
                             or (IsBound(elt.attributes.BookName)
                                 and elt.attributes.BookName = "ref"));
  Print("including ", Length(yint), " within the Reference manual\n");
  y := Filtered(yint, elt -> ForAny(types, t -> IsBound(elt.attributes.(t))));

  referrcount := 0;
  warncount := 0;
  for elt in y do
    type := First(types, t -> IsBound(elt.attributes.(t)));
    if type <> fail then
      matches := Filtered(x, t -> t.attributes.Name = elt.attributes.(type));
      if Length(matches) = 0 then
        pos := OriginalPositionDocument(doc[2], elt.start);
        Print(pos[1], ":", pos[2], " : no match for ", type, " := ",
              elt.attributes.(type), "\n");
        referrcount := referrcount + 1;
        continue;
      elif Length(matches) = 1 then
        match := matches[1];
      elif IsBound(elt.attributes.Label) then
        matches := Filtered(matches, t -> IsBound(t.attributes.Label));
        matches := Filtered(matches, t -> t.attributes.Label =
                                          elt.attributes.Label);
        if Length(matches) > 1 then
          ErrorNoReturn("Semigroups: SEMIGROUPS.CheckManSectionTypes:\n",
                        "Multiple labels - this should not happen!");
        fi;
        match := matches[1];
      else
        matches2 := Filtered(matches, t -> not IsBound(t.attributes.Label));
        if Length(matches2) = 0 then
          pos := OriginalPositionDocument(doc[2], elt.start);
          Print(pos[1], ":", pos[2],
                " : no match (wrong type or missing label?) for ", type, " := ",
                elt.attributes.(type), "\n");
          Print("  Suggestions: \n");
          matches := Filtered(matches, t -> IsBound(t.attributes.Label));
          for t in matches do
            Print("Use ", t.name, " with Label := \"", t.attributes.Label,
                  "\" (for Arg := \"", t.attributes.Arg, "\")\n");
          od;

          referrcount := referrcount + 1;
          continue;
        elif Length(matches2) > 1 then
          ErrorNoReturn("Semigroups: SEMIGROUPS.CheckManSectionTypes:\n",
                        "Multiple labels - this should not happen!");
        else
          match := matches[1];
        fi;
      fi;
      if match.name <> type then
        pos := OriginalPositionDocument(doc[2], elt.start);
        if display_warnings then
          Print(pos[1], ":", pos[2], " : Ref to ", elt.attributes.(type),
                " uses ", type, " instead of ", match.name, "\n");
        fi;
        warncount := warncount + 1;
      fi;
    fi;
  od;

  Print("****************************************************************\n");
  stats := Collected(List(x, elt -> elt.name));
  Print("Selected ", Length(x), " ManSections of the following types:\n");
  for s in stats do
    Print(s[1], " - ", s[2], "\n");
  od;
  Print("Found ", errcount, " errors in ManSection types \n");

  Print("Selected ", Length(y), " Ref elements referring to ManSections \n");
  Print("Found ", referrcount, " errors and ", warncount,
        " warnings in Ref elements \n");

  if display_warnings then
    Print("To suppress warnings, use SEMIGROUPS.CheckManSectionTypes",
          "(doc,false) or with one argument\n");
  else
    Print("To show warnings, use SEMIGROUPS.CheckManSectionTypes(doc,true);",
          "\n");
  fi;
  Print("****************************************************************\n");
  return errcount = 0;
end;

SEMIGROUPS.CheckDocCoverage := function(doc)
  local r, x, with, without, mansect, pos, y;
  r := ParseTreeXMLString(doc[1]);
  CheckAndCleanGapDocTree(r);
  x := XMLElements(r, ["ManSection"]);
  with := 0;
  without := 0;
  Print("****************************************************************\n");
  Print("*** Looking for ManSections having no examples \n");
  Print("****************************************************************\n");
  for mansect in x do
    pos := OriginalPositionDocument(doc[2], mansect.start);
    y := XMLElements(mansect, ["Example"]);
    if Length(y) = 0 then
      if IsBound(mansect.content[1].attributes) and
          IsBound(mansect.content[1].attributes.Name) then
        Print(pos[1], ":", pos[2], " : ", mansect.content[1].attributes.Name);
      elif IsBound(mansect.content[2].attributes) and
          IsBound(mansect.content[2].attributes.Name) then
        Print(pos[1], ":", pos[2], " : ", mansect.content[2].attributes.Name);
      else
        Print(pos[1], ":", pos[2], " : ",
              mansect.content[1].content[1].content);
      fi;
      without := without + 1;
      Print("\n");
    else
      with := with + 1;
    fi;
  od;
  Print("****************************************************************\n");
  Print("*** Doc coverage report \n");
  Print("****************************************************************\n");
  Print(Length(x), " mansections \n");
  Print(with, " with examples \n");
  Print(without, " without examples \n");
end;

SEMIGROUPS.CheckManualConsistency := function()
  local doc;

  doc := ComposedXMLString(Concatenation(SEMIGROUPS.PackageDir, "/doc"),
                           "main.xml",
                           SEMIGROUPS.DocXMLFiles,
                           true);
  SEMIGROUPS.CheckDocCoverage(doc);
  return SEMIGROUPS.CheckManSectionTypes(doc, true);
end;

SEMIGROUPS.DocumentedPackageVariables := function()
  local doc, r, x, out, mansect, record;
  doc := ComposedXMLString(Concatenation(SEMIGROUPS.PackageDir, "/doc"),
                           "main.xml",
                           SEMIGROUPS.DocXMLFiles,
                           true);
  r := ParseTreeXMLString(doc[1]);
  CheckAndCleanGapDocTree(r);
  x := XMLElements(r, ["ManSection"]);
  out := [];
  for mansect in x do
    for record in mansect.content do
      if IsBound(record.attributes) and IsBound(record.attributes.Name) then
        AddSet(out, record.attributes.Name);
      fi;
    od;
  od;
  return out;
end;

# info := PackageVariablesInfo("semigroups", "3.0.0");
# which must be run before loading Semigroups
SEMIGROUPS.UndocumentedPackageVariables := function(info)
  local out, suppressions, obsoletes, documented, name, part, entry;

  out := [];
  suppressions := ["*", ".", "/", "<", "=", "[]", "^", "in", "{}",
                   "HTAdd_TreeHash_C", "HTValue_TreeHash_C"];

  obsoletes := ["RandomTransformationSemigroup", "RandomTransformationMonoid",
                "RandomPartialPermSemigroup", "RandomPartialPermMonoid",
                "RandomMatrixSemigroup", "RandomMatrixMonoid", "DotDClasses",
                "DotDClasses", "PartialTransformationSemigroup",
                "AsPartialPermSemigroup", "AsTransformationSemigroup",
                "AsBipartitionSemigroup", "AsBlockBijectionSemigroup",
                "AsMatrixSemigroup", "IsomorphismBipartitionSemigroup",
                "IsomorphismBlockBijectionSemigroup",
                "IsomorphismMatrixSemigroup",
                "FactorisableDualSymmetricInverseSemigroup",
                "SingularFactorisableDualSymmetricInverseSemigroup",
                "IsSynchronizingTransformationCollection"];
  documented := SEMIGROUPS.DocumentedPackageVariables();

  for part in info do
    if (part[1]{[1 .. 3]} = "new" or part[1]{[1 .. 9]} = "other new") and not
        part[1] = "new methods" then
      for entry in part[2] do
        name := entry[1][1];
        if not name in suppressions
            and not name in obsoletes
            and not name in documented
            and not ForAll(name, x -> IsUpperAlphaChar(x) or x = '_')
            and not (Length(name) >= 3 and name{[1 .. 3]} = "Has")
            and not (Length(name) >= 3 and name{[1 .. 3]} = "Set")
            and not (Length(name) >= 11 and name{[1 .. 11]} = "SEMIGROUPS_")
            and not (Length(name) >= 2
                     and name{[Length(name) - 1, Length(name)]} = "NC") then
          AddSet(out, [name, JoinStringsWithSeparator(entry[2], ":")]);
        fi;
      od;
    fi;
  od;
  return out;
end;
