############################################################################
##
#W  congruences/congpairs.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using a pair enumeration and union-find method.
##
#############################################################################
##
## A congruence here is defined by a semigroup and a list of generating pairs.
## Most of the work is done by SEMIGROUPS_Enumerate, a hidden function which
## begins to multiply known pairs in the congruence by the semigroup's
## generators, checking its results periodically against a supplied "lookfunc"
## which checks whether some condition has been fulfilled.
##
## Any function which requires information about a congruence may call
## SEMIGROUPS_Enumerate with a lookfunc to allow it to terminate as soon as the
## necessary information is found, without doing extra work.  Information found
## so far is then stored in a "congruence data" object, and work may be resumed
## in subsequent calls to SEMIGROUPS_Enumerate.
##
## If all the pairs of the congruence have been found, the congruence data
## object is discarded, and a lookup table is stored, giving complete
## information about the congruence classes.  If a lookup table is available,
## it is always used instead of SEMIGROUPS_Enumerate, which will always return
## fail from then on.
##
## Most methods in this file apply to (two-sided) congruences, as well as left
## congruences and right congruences.  The _InstallMethodsForCongruences
## function is called three times when Semigroups is loaded, installing slightly
## different methods for left, right, and two-sided congruences.  Of course a
## left congruence may turn out also to be a right congruence, and so on, but
## the properties HasGeneratingPairsOf(Left/Right)MagmaCongruence allow us to
## determine which type of relation we are treating it as.
##
## See J.M. Howie's "Fundamentals of Semigroup Theory" Section 1.5, and see
## Michael Torpey's MSc thesis "Computing with Semigroup Congruences" Chapter 2
## (www-circa.mcs.st-and.ac.uk/~mct25/files/mt5099-report.pdf) for more details.
##
#############################################################################

# This function creates the congruence data object for cong.  It should only
# be called once.

SEMIGROUPS.SetupCongData := function(cong)
  # This function creates the congruence data object for cong.  It should only
  # be called once.
  local S, elms, genpairs, gendata, pairs, hashlen, ht, treehashsize, data,
        pairstoapply, pos, found, ufdata;

  S := Range(cong);
  elms := SEMIGROUP_AS_LIST(GenericSemigroupData(S));

  # Is this a left, right, or 2-sided congruence?
  if HasGeneratingPairsOfMagmaCongruence(cong) then
    genpairs := GeneratingPairsOfSemigroupCongruence(cong);
  elif HasGeneratingPairsOfLeftMagmaCongruence(cong) then
    genpairs := GeneratingPairsOfLeftSemigroupCongruence(cong);
  elif HasGeneratingPairsOfRightMagmaCongruence(cong) then
    genpairs := GeneratingPairsOfRightSemigroupCongruence(cong);
  fi;

  gendata := GenericSemigroupData(S);
  pairs   := List(genpairs, x -> [Position(gendata, x[1]),
                                  Position(gendata, x[2])]);
  hashlen := SEMIGROUPS.OptionsRec(S).hashlen.L;

  ht := HTCreate([1, 1], rec(forflatplainlists := true,
                             treehashsize := hashlen));

  data := rec(cong         := cong,
              pairstoapply := pairs,
              pos          := 0,
              ht           := ht,
              elms         := elms,
              found        := false,
              ufdata       := UF_NEW(Size(S)));

  cong!.data := Objectify(NewType(FamilyObj(cong),
                                  SEMIGROUPS_IsSemigroupCongruenceData),
                          data);
end;

# Enumerates pairs in the congruence in batches until lookfunc is satisfied

InstallMethod(SEMIGROUPS_Enumerate,
"for semigroup congruence data and a function",
[SEMIGROUPS_IsSemigroupCongruenceData, IsFunction],
function(data, lookfunc)
  local cong, ufdata, pairstoapply, ht, S, left, right, genstoapply, i, nr, x,
        check_period, j, y, next, newtable, ii;

  # Restore our place from the congruence data record
  cong         := data!.cong;
  ufdata       := data!.ufdata;
  pairstoapply := data!.pairstoapply;
  ht           := data!.ht;
  S            := Range(cong);

  # Find the necessary Cayley graphs
  if HasGeneratingPairsOfLeftMagmaCongruence(cong) then
    left := LeftCayleyGraphSemigroup(S);
  elif HasGeneratingPairsOfRightMagmaCongruence(cong) then
    right := RightCayleyGraphSemigroup(S);
  elif HasGeneratingPairsOfMagmaCongruence(cong) then
    left := LeftCayleyGraphSemigroup(S);
    right := RightCayleyGraphSemigroup(S);
  fi;

  genstoapply := [1 .. Size(GeneratorsOfSemigroup(S))];
  i     := data!.pos;
  nr    := Size(pairstoapply);

  if i = 0 then
    # Add the generating pairs themselves
    for x in pairstoapply do
      if x[1] <> x[2] and HTValue(ht, x) = fail then
        HTAdd(ht, x, true);
        UF_UNION(ufdata, x);
      fi;
    od;
  fi;

  # Have we already found what we were looking for?
  if lookfunc(data) then
    data!.found := true;
    return data;
  fi;

  # Define how often we check the lookfunc
  check_period := 200;

  # Main loop: find new pairs and keep multiplying them
  # We may need left-multiples, right-multiples, or both
  if HasGeneratingPairsOfMagmaCongruence(cong) then
    while i < nr do
      i := i + 1;
      x := pairstoapply[i];
      # Add the pair's left-multiples
      for j in genstoapply do
        y := [left[x[1]][j], left[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          UF_UNION(ufdata, y);
        fi;
      od;
      # Add the pair's right-multiples
      for j in genstoapply do
        y := [right[x[1]][j], right[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          UF_UNION(ufdata, y);
        fi;
      od;
      # Check lookfunc periodically
      if i mod check_period = 0 and lookfunc(data) then
        # Save our place
        data!.pos := i;
        data!.found := true;
        return data;
      fi;
    od;
  elif HasGeneratingPairsOfLeftMagmaCongruence(cong) then
    # Main loop: find new pairs and keep multiplying them
    while i < nr do
      i := i + 1;
      x := pairstoapply[i];
      # Add the pair's left-multiples
      for j in genstoapply do
        y := [left[x[1]][j], left[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          UF_UNION(ufdata, y);
        fi;
      od;
      # Check lookfunc periodically
      if i mod check_period = 0 and lookfunc(data) then
        # Save our place
        data!.pos := i;
        data!.found := true;
        return data;
      fi;
    od;
  elif HasGeneratingPairsOfRightMagmaCongruence(cong) then
    while i < nr do
      i := i + 1;
      x := pairstoapply[i];
      # Add the pair's right-multiples
      for j in genstoapply do
        y := [right[x[1]][j], right[x[2]][j]];
        if y[1] <> y[2] and HTValue(ht, y) = fail then
          HTAdd(ht, y, true);
          nr := nr + 1;
          pairstoapply[nr] := y;
          UF_UNION(ufdata, y);
        fi;
      od;
      # Check lookfunc periodically
      if i mod check_period = 0 and lookfunc(data) then
        # Save our place
        data!.pos := i;
        data!.found := true;
        return data;
      fi;
    od;
  fi;

  # All pairs have been found: save a lookup table
  next := 1;
  newtable := EmptyPlist(UF_SIZE(ufdata));
  for i in [1 .. UF_SIZE(ufdata)] do
    ii := UF_FIND(ufdata, i);
    if ii = i then
      newtable[i] := next;
      next := next + 1;
    else
      newtable[i] := newtable[ii];
    fi;
  od;
  SetAsLookupTable(cong, newtable);

  # Also store the completed union-find table
  cong!.ufdata := ufdata;

  # No need for congruence data object any more
  Unbind(cong!.data);

  # Return the data object with important final data
  data!.found := lookfunc(data);
  data!.lookup := newtable;
  return data;
end);

#

InstallMethod(IsRightSemigroupCongruence,
"for a left semigroup congruence with known generating pairs",
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(congl)
  local pairs, cong2;
  # Is this left congruence right-compatible?
  # First, create the 2-sided congruence generated by these pairs.
  pairs := GeneratingPairsOfLeftSemigroupCongruence(congl);
  cong2 := SemigroupCongruence(Range(congl), pairs);

  # congl is right-compatible iff these describe the same relation
  if congl = cong2 then
    SetGeneratingPairsOfMagmaCongruence(congl, pairs);
    SetIsSemigroupCongruence(congl, true);
    return true;
  else
    SetIsSemigroupCongruence(congl, false);
    return false;
  fi;
end);

#

InstallMethod(IsLeftSemigroupCongruence,
"for a right semigroup congruence with known generating pairs",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(congr)
  local pairs, cong2;
  # Is this right congruence left-compatible?
  # First, create the 2-sided congruence generated by these pairs.
  pairs := GeneratingPairsOfRightSemigroupCongruence(congr);
  cong2 := SemigroupCongruence(Range(congr), pairs);

  # congr is left-compatible iff these describe the same relation
  if congr = cong2 then
    SetGeneratingPairsOfMagmaCongruence(congr, pairs);
    SetIsSemigroupCongruence(congr, true);
    return true;
  else
    SetIsSemigroupCongruence(congr, false);
    return false;
  fi;
end);

#

InstallMethod(IsSemigroupCongruence,
"for a left semigroup congruence with known generating pairs",
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(cong)
  return IsRightSemigroupCongruence(cong);
end);

#

InstallMethod(IsSemigroupCongruence,
"for a right semigroup congruence with known generating pairs",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong)
  return IsLeftSemigroupCongruence(cong);
end);

#

BindGlobal("_GenericCongruenceEquality",
function(c1, c2)
  # This function tests equality of left, right, or 2-sided congruences
  return Range(c1) = Range(c2) and AsLookupTable(c1) = AsLookupTable(c2);
end);

# _GenericCongruenceEquality tests equality for any combination of left, right
# and 2-sided congruences, so it is installed for the six combinations below.
# If the arguments are the same type of congruence, a different method is used

InstallMethod(\=,
Concatenation("for a left semigroup congruence with known generating pairs ",
              "and a right semigroup congruence with known generating pairs"),
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence,
 IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
_GenericCongruenceEquality);

InstallMethod(\=,
Concatenation("for a right semigroup congruence with known generating pairs ",
              "and a left semigroup congruence with known generating pairs"),
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence,
 IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
_GenericCongruenceEquality);

InstallMethod(\=,
Concatenation("for a left semigroup congruence with known generating pairs ",
              "and a semigroup congruence with known generating pairs"),
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
_GenericCongruenceEquality);

InstallMethod(\=,
Concatenation("for a right semigroup congruence with known generating pairs ",
              "and a semigroup congruence with known generating pairs"),
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
_GenericCongruenceEquality);

InstallMethod(\=,
Concatenation("for a semigroup congruence with known generating pairs ",
              "and a left semigroup congruence with known generating pairs"),
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
_GenericCongruenceEquality);

InstallMethod(\=,
Concatenation("for a semigroup congruence with known generating pairs ",
              "and a right semigroup congruence with known generating pairs"),
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
_GenericCongruenceEquality);

MakeReadWriteGlobal("_GenericCongruenceEquality");
UnbindGlobal("_GenericCongruenceEquality");

#

################################################################################
# We now have some methods which apply to left congruences, right congruences
# and 2-sided congruences.  These functions behave only slightly differently for
# these three types of object, so they are installed by the function
# _InstallMethodsForCongruences, which takes a record describing the type of
# object the filters apply to (left, right, or 2-sided).
#
# See below for the loop where this function is invoked. It is required to do
# this in a function so that the values _record,
# _GeneratingPairsOfXSemigroupCongruence, etc are available (as local
# variables in the function) when the methods installed in this function are
# actually called. If we don't use a function here, the values in _record etc
# are unbound by the time the methods are called.
################################################################################

BindGlobal("_InstallMethodsForCongruences",
function(_record)
  local _GeneratingPairsOfXSemigroupCongruence,
        _HasGeneratingPairsOfXSemigroupCongruence,
        _XSemigroupCongruence,
        _IsXSemigroupCongruence,
        _IsXCongruenceClass;

  _GeneratingPairsOfXSemigroupCongruence :=
    EvalString(Concatenation("GeneratingPairsOf",
                             _record.type_string,
                             "MagmaCongruence"));
  _HasGeneratingPairsOfXSemigroupCongruence :=
    EvalString(Concatenation("HasGeneratingPairsOf",
                             _record.type_string,
                             "MagmaCongruence"));
  _XSemigroupCongruence :=
    EvalString(Concatenation(_record.type_string,
                             "SemigroupCongruence"));
  _IsXSemigroupCongruence :=
    EvalString(Concatenation("Is",
                             _record.type_string,
                             "SemigroupCongruence"));
  _IsXCongruenceClass :=
    EvalString(Concatenation("Is",
                             _record.type_string,
                             "CongruenceClass"));

  #

  InstallMethod(\in,
  Concatenation("for dense list and ", _record.info_string,
                "semigroup congruence with known generating pairs"),
  [IsDenseList, _IsXSemigroupCongruence and
   _HasGeneratingPairsOfXSemigroupCongruence],
  function(pair, cong)
    local S, p1, p2, table, lookfunc;

    # Input checks
    S := Range(cong);
    if not IsFinite(S) then
      TryNextMethod();
    fi;
    if Size(pair) <> 2 then
      ErrorNoReturn("Semigroups: \\in (for a congruence): usage,\n",
                    "the first arg <pair> must be a list of length 2,");
    fi;
    if not (pair[1] in S and pair[2] in S) then
      ErrorNoReturn("Semigroups: \\in (for a congruence): usage,\n",
                    "elements of the first arg <pair> must be\n",
                    "in the range of the second arg <cong>,");
    fi;

    p1 := Position(GenericSemigroupData(S), pair[1]);
    p2 := Position(GenericSemigroupData(S), pair[2]);

    # Use lookup table if available
    if HasAsLookupTable(cong) then
      table := AsLookupTable(cong);
      return table[p1] = table[p2];
    else
      # Otherwise, begin calculating the lookup table and look for this pair
      lookfunc := function(data)
        return UF_FIND(data!.ufdata, p1)
               = UF_FIND(data!.ufdata, p2);
      end;
      return SEMIGROUPS_Enumerate(cong, lookfunc)!.found;
    fi;
  end);

  #

  InstallMethod(AsLookupTable,
  Concatenation("for a ", _record.info_string,
                "semigroup congruence with known generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong)
    local data;
    if not IsFinite(Range(cong)) then
      TryNextMethod();
    fi;
    # Enumerate the congruence until all pairs are found
    data := SEMIGROUPS_Enumerate(cong, ReturnFalse);
    # Return the resultant lookup table
    return data!.lookup;
  end);

  #

  InstallMethod(SEMIGROUPS_Enumerate,
  Concatenation("for a ", _record.info_string, "semigroup congruence",
                " with known generating pairs and a function"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   IsFunction],
  function(cong, lookfunc)
    # If we have a lookup table, then we have complete information
    # and there is nothing left to enumerate
    if HasAsLookupTable(cong) then
      return fail;
    fi;
    # If the congruence data does not exist, then we need to set it up
    if not IsBound(cong!.data) then
      SEMIGROUPS.SetupCongData(cong);
    fi;
    return SEMIGROUPS_Enumerate(cong!.data, lookfunc);
  end);

  #

  InstallMethod(EquivalenceClasses,
  Concatenation("for a ", _record.info_string,
                "semigroup congruence with known generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong)
    local classes, next, tab, elms, i;
    if not IsFinite(Range(cong)) then
      TryNextMethod();
    fi;
    classes := [];
    next := 1;
    tab := AsLookupTable(cong);
    elms := SEMIGROUP_AS_LIST(GenericSemigroupData(Range(cong)));
    for i in [1 .. Size(tab)] do
      if tab[i] = next then
        classes[next] := EquivalenceClassOfElementNC(cong, elms[i]);
        next := next + 1;
      fi;
    od;
    return classes;
  end);

  #

  InstallMethod(NonTrivialEquivalenceClasses,
  Concatenation("for a ", _record.info_string,
                "semigroup congruence with known generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong)
    local classes;
    if not IsFinite(Range(cong)) then
      TryNextMethod();
    fi;
    classes := EquivalenceClasses(cong);
    return Filtered(classes, c -> Size(c) > 1);
  end);

  #

  InstallMethod(\=,
  Concatenation("for ", _record.info_string,
                "semigroup congruences with known generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   _IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong1, cong2)
    if not (IsFinite(Range(cong1)) and IsFinite(Range(cong2))) then
      TryNextMethod();
    fi;
    return Range(cong1) = Range(cong2)
           and ForAll(_GeneratingPairsOfXSemigroupCongruence(cong1),
                      pair -> pair in cong2)
           and ForAll(_GeneratingPairsOfXSemigroupCongruence(cong2),
                      pair -> pair in cong1);
  end);

  #

  InstallMethod(ImagesElm,
  Concatenation("for a ", _record.info_string, "semigroup congruence",
                " with known generating pairs and an associative element"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   IsAssociativeElement],
  function(cong, elm)
    local lookup, gendata, classNo, elms;
    if not IsFinite(Range(cong)) then
      TryNextMethod();
    fi;
    lookup := AsLookupTable(cong);
    gendata := GenericSemigroupData(Range(cong));
    classNo := lookup[Position(gendata, elm)];
    elms := SEMIGROUP_AS_LIST(gendata);
    return elms{Positions(lookup, classNo)};
  end);

  #

  InstallMethod(NrEquivalenceClasses,
  Concatenation("for a ", _record.info_string,
                "semigroup congruence with generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong)
    local S;
    S := Range(cong);
    if not IsFinite(S) then
      TryNextMethod();
    fi;
    return Maximum(AsLookupTable(cong));
  end);

  #

  InstallMethod(ViewObj,
  Concatenation("for a ", _record.info_string,
                "semigroup congruence with generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong)
    Print("<", _record.info_string, "semigroup congruence over ");
    ViewObj(Range(cong));
    Print(" with ", Size(_GeneratingPairsOfXSemigroupCongruence(cong)),
          " generating pairs>");
  end);

  #

  InstallMethod(EvalString(
  Concatenation("Join", _record.type_string, "SemigroupCongruences")),
  Concatenation("for two ", _record.info_string,
                "semigroup congruences with generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   _IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(c1, c2)
    local pairs, cong, ufdata, uf2, i, ii, next, newtable;
    if Range(c1) <> Range(c2) then
      ErrorNoReturn("Semigroups: Join", _record.type_string,
                    "SemigroupCongruences: usage,\n",
                    "congruences must be defined over the same semigroup,");
    fi;
    pairs := Concatenation(
               ShallowCopy(_GeneratingPairsOfXSemigroupCongruence(c1)),
               ShallowCopy(_GeneratingPairsOfXSemigroupCongruence(c2)));
    cong := _XSemigroupCongruence(Range(c1), pairs);

    # Join the lookup tables
    if HasAsLookupTable(c1) and HasAsLookupTable(c2) then
      # First join the union-find tables
      ufdata := UF_COPY(c1!.ufdata);
      uf2 := c2!.ufdata;
      for i in [1 .. UF_SIZE(uf2)] do
        ii := UF_FIND(uf2, i);
        if ii <> i then
          UF_UNION(ufdata, [i, ii]);
        fi;
      od;
      cong!.ufdata := ufdata;

      # Now normalise this as a lookup table
      next := 1;
      newtable := EmptyPlist(UF_SIZE(ufdata));
      for i in [1 .. UF_SIZE(ufdata)] do
        ii := UF_FIND(ufdata, i);
        if ii = i then
          newtable[i] := next;
          next := next + 1;
        else
          newtable[i] := newtable[ii];
        fi;
      od;
      SetAsLookupTable(cong, newtable);
    fi;

    return cong;
  end);

  #

  #TODO: A method for MeetXSemigroupCongruences

  #

  InstallMethod(IsSubrelation,
  Concatenation("for two ", _record.info_string,
                "semigroup congruences with generating pairs"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   _IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence],
  function(cong1, cong2)
    # Tests whether cong1 contains all the pairs in cong2
    if Range(cong1) <> Range(cong2) then
      ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                    "congruences must be defined over the same semigroup,");
    fi;
    return ForAll(_GeneratingPairsOfXSemigroupCongruence(cong2),
                  pair -> pair in cong1);
  end);

  #

  ###########################################################################
  # LatticeOfXCongruences
  ###########################################################################
  InstallMethod(EvalString(
  Concatenation("LatticeOf", _record.type_string, "Congruences")),
  "for a semigroup",
  [IsSemigroup],
  S -> SEMIGROUPS.LatticeOfXCongruences(S, _record.type_string, rec()));

  ###########################################################################
  # XCongruencesOfSemigroup
  ###########################################################################
  InstallMethod(EvalString(
  Concatenation(_record.type_string, "CongruencesOfSemigroup")),
  "for a semigroup",
  [IsSemigroup],
  function(S)
    local lattice_func;
    if not IsFinite(S) then
      TryNextMethod();
    fi;
    # Find the lattice of congruences, and retrieve
    # the list of congruences from inside it
    lattice_func := EvalString(Concatenation("LatticeOf",
                                             _record.type_string,
                                             "Congruences"));
    return lattice_func(S)![2];
  end);

  ###########################################################################
  # MinimalXCongruencesOfSemigroup
  ###########################################################################
  InstallMethod(EvalString(
  Concatenation("Minimal", _record.type_string, "CongruencesOfSemigroup")),
  "for a semigroup",
  [IsSemigroup],
  function(S)
    local lattice;
    if not IsFinite(S) then
      TryNextMethod();
    fi;
    # Find the lattice of congruences, and retrieve
    # the list of congruences from inside it
    lattice := SEMIGROUPS.LatticeOfXCongruences(S, _record.type_string,
                                                rec(minimal := true));
    return lattice![2];
  end);

  ###########################################################################
  # methods for classes
  ###########################################################################

  InstallMethod(EquivalenceClassOfElement,
  Concatenation("for a ", _record.info_string, "semigroup congruence",
                " with generating pairs and an associative element"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   IsAssociativeElement],
  function(cong, elm)
    if not elm in Range(cong) then
      ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                    "the second arg <elm> must be in the ",
                    "semigroup of the first arg <cong>,");
    fi;
    return EquivalenceClassOfElementNC(cong, elm);
  end);

  #

  InstallMethod(EquivalenceClassOfElementNC,
  Concatenation("for a ", _record.info_string, "semigroup congruence",
                " with generating pairs and an associative element"),
  [_IsXSemigroupCongruence and _HasGeneratingPairsOfXSemigroupCongruence,
   IsAssociativeElement],
  function(cong, elm)
    local fam, class;
    fam := FamilyObj(Range(cong));
    class := Objectify(NewType(fam, _IsXCongruenceClass
                               and IsEquivalenceClassDefaultRep),
                       rec(rep := elm));
    SetParentAttr(class, Range(cong));
    SetEquivalenceClassRelation(class, cong);
    SetRepresentative(class, elm);
    if HasIsFinite(Range(cong)) and IsFinite(Range(cong)) then
      SetIsFinite(class, true);
    fi;
    return class;
  end);

  #

  InstallMethod(Enumerator,
  Concatenation("for a ", _record.info_string, "congruence class"),
  [_IsXCongruenceClass],
  function(class)
    local cong, S, enum;

    cong := EquivalenceClassRelation(class);
    S := Range(cong);

    if not IsFinite(S) then
      TryNextMethod();
    fi;

    # cong has been enumerated: return a list
    if HasAsLookupTable(cong) then
      return Enumerator(AsList(class));
    fi;

    # cong has not yet been enumerated: make functions
    enum := rec();

    enum.ElementNumber := function(enum, pos)
      local lookfunc, result, table, classno, i;
      if pos <= enum!.len then
        return enum!.elms[enum!.list[pos]];
      fi;
      lookfunc := function(data)
        local classno, i;
        classno := UF_FIND(data!.ufdata, enum!.rep);
        for i in [1 .. UF_SIZE(data!.ufdata)] do
          if (not enum!.found[i]) and UF_FIND(data!.ufdata, i) = classno then
            enum!.found[i] := true;
            enum!.len := enum!.len + 1;
            enum!.list[enum!.len] := i;
          fi;
        od;
        return enum!.len >= pos;
      end;
      result := SEMIGROUPS_Enumerate(enum!.cong, lookfunc);
      if result = fail then
        # cong has AsLookupTable
        table := AsLookupTable(enum!.cong);
        classno := table[enum!.rep];
        for i in [1 .. Size(Range(enum!.cong))] do
          if table[i] = classno and not enum!.found[i] then
            enum!.found[i] := true;
            enum!.len := enum!.len + 1;
            enum!.list[enum!.len] := i;
          fi;
        od;
        SetSize(class, enum!.len);
        SetAsList(class, enum!.list);
      fi;
      if pos <= enum!.len then
        return enum!.elms[enum!.list[pos]];
      else
        return fail;
      fi;
    end;

    enum.NumberElement := function(enum, elm)
      local x, lookfunc, result, table, classno, i;
      x := Position(enum!.gendata, elm);
      lookfunc := function(data)
        return UF_FIND(data!.ufdata, x)
               = UF_FIND(data!.ufdata, enum!.rep);
      end;
      result := SEMIGROUPS_Enumerate(enum!.cong, lookfunc);
      if result = fail then
        # cong has AsLookupTable
        table := AsLookupTable(enum!.cong);
        classno := table[enum!.rep];
        for i in [1 .. Size(Range(enum!.cong))] do
          if table[i] = classno and not enum!.found[i] then
            enum!.found[i] := true;
            enum!.len := enum!.len + 1;
            enum!.list[enum!.len] := i;
            if i = x then
              result := enum!.len;
            fi;
          fi;
        od;
        SetSize(class, enum!.len);
        SetAsList(class, enum!.list);
        if result = fail then
          return Position(enum!.list, x);
        else
          return result;
        fi;
      elif result!.found then
        # elm is in the class
        if enum!.found[x] then
          # elm already has a position
          return Position(enum!.list, x);
        else
          # put elm in the next available position
          enum!.found[x] := true;
          enum!.len := enum!.len + 1;
          enum!.list[enum!.len] := x;
          return enum!.len;
        fi;
      else
        # elm is not in the class
        return fail;
      fi;
    end;

    enum := EnumeratorByFunctions(class, enum);
    enum!.cong := EquivalenceClassRelation(UnderlyingCollection(enum));
    enum!.gendata := GenericSemigroupData(Range(enum!.cong));
    enum!.elms := SEMIGROUP_AS_LIST(enum!.gendata);
    enum!.rep := Position(enum!.gendata,
                          Representative(UnderlyingCollection(enum)));
    enum!.list := [enum!.rep];
    enum!.found := BlistList([1 .. Size(Range(enum!.cong))], [enum!.rep]);
    enum!.len := 1;

    return enum;
  end);

  #

  InstallMethod(\in,
  Concatenation("for an associative element and a ",
                _record.info_string, "congruence class"),
  [IsAssociativeElement, _IsXCongruenceClass],
  function(elm, class)
    if not IsFinite(Parent(class)) then
      TryNextMethod();
    fi;
    return [elm, Representative(class)] in EquivalenceClassRelation(class);
  end);

  #

  InstallMethod(Size,
  Concatenation("for a ", _record.info_string, "congruence class"),
  [_IsXCongruenceClass],
  function(class)
    local p, tab;
    if not IsFinite(Parent(class)) then
      TryNextMethod();
    fi;
    p := Position(GenericSemigroupData(Parent(class)), Representative(class));
    tab := AsLookupTable(EquivalenceClassRelation(class));
    return Number(tab, n -> n = tab[p]);
  end);

  #

  InstallMethod(\=,
  Concatenation("for two ", _record.info_string, "congruence classes"),
  [_IsXCongruenceClass, _IsXCongruenceClass],
  function(class1, class2)
    return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
      and [Representative(class1), Representative(class2)]
          in EquivalenceClassRelation(class1);
  end);

  #

  InstallMethod(AsList,
  Concatenation("for a ", _record.info_string, "congruence class"),
  [_IsXCongruenceClass],
  function(class)
    return ImagesElm(EquivalenceClassRelation(class), Representative(class));
  end);

  #

end);
# End of _InstallMethodsForCongruences function

for _record in [rec(type_string := "",
                    info_string := ""),
                rec(type_string := "Left",
                    info_string := "left "),
                rec(type_string := "Right",
                    info_string := "right ")] do
  _InstallMethodsForCongruences(_record);
od;

Unbind(_record);
MakeReadWriteGlobal("_InstallMethodsForCongruences");
UnbindGlobal("_InstallMethodsForCongruences");

#

# Multiplication for congruence classes: only makes sense for 2-sided
InstallMethod(\*,
"for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
    ErrorNoReturn("Semigroups: \*: usage,\n",
                  "the args must be classes of the same congruence,");
  fi;
  return CongruenceClassOfElement(EquivalenceClassRelation(class1),
                                  Representative(class1) *
                                  Representative(class2));
end);

###########################################################################
# IsSubrelation methods between 1-sided and 2-sided congruences
###########################################################################
InstallMethod(IsSubrelation,
"for semigroup congruence and left semigroup congruence",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(cong, lcong)
  # Tests whether cong contains all the pairs in lcong
  if Range(cong) <> Range(lcong) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return ForAll(GeneratingPairsOfLeftSemigroupCongruence(lcong),
                pair -> pair in cong);
end);

#

InstallMethod(IsSubrelation,
"for semigroup congruence and right semigroup congruence",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong, rcong)
  # Tests whether cong contains all the pairs in rcong
  if Range(cong) <> Range(rcong) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return ForAll(GeneratingPairsOfRightSemigroupCongruence(rcong),
                pair -> pair in cong);
end);

###########################################################################
# Some individual methods for congruences
###########################################################################

InstallMethod(PrintObj,
"for a left semigroup congruence with known generating pairs",
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(cong)
  Print("LeftSemigroupCongruence( ");
  PrintObj(Range(cong));
  Print(", ");
  Print(GeneratingPairsOfLeftSemigroupCongruence(cong));
  Print(" )");
end);

InstallMethod(PrintObj,
"for a right semigroup congruence with known generating pairs",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong)
  Print("RightSemigroupCongruence( ");
  PrintObj(Range(cong));
  Print(", ");
  Print(GeneratingPairsOfRightSemigroupCongruence(cong));
  Print(" )");
end);

InstallMethod(PrintObj,
"for a semigroup congruence with known generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  Print("SemigroupCongruence( ");
  PrintObj(Range(cong));
  Print(", ");
  Print(GeneratingPairsOfSemigroupCongruence(cong));
  Print(" )");
end);

#

###############################################################################
# LatticeOfXCongruences function
###############################################################################
# This abstract function takes a semigroup 'S', a string 'type_string' and a
# record 'record'.
# type_string should be in ["Left", "Right", ""] and describes the sort of
# relations we want to find (respectively: left congruences, right congruences,
# two-sided congruences), referred to as 'x congs' below.
# record may contain any of the following components, which should be set to
# 'true' to have the stated effect:
#   * minimal - Return only minimal x-congs
#   * 1gen - Return only x-congs with a single generating pair
#   * poor - Return only x-congs with no 2-sided congruences as subrelations
###############################################################################
SEMIGROUPS.LatticeOfXCongruences := function(S, type_string, record)
  local _XSemigroupCongruence, elms, pairs, congs1, nrcongs, children, parents,
        pair, badcong, newchildren, newparents, newcong, i, c, p, congs,
        join_func, length, found, start, j, k, set_func, lattice;

  _XSemigroupCongruence := EvalString(Concatenation(type_string,
                                                    "SemigroupCongruence"));
  elms := SEMIGROUP_AS_LIST(GenericSemigroupData(S));

  # Get all non-reflexive pairs in SxS
  pairs := Combinations(elms, 2);

  # Get all the unique 1-generated congs
  Info(InfoSemigroups, 1, "Getting all 1-generated congs...");
  congs1 := [];     # List of all congs found so far
  nrcongs := 0;     # Number of congs found so far
  children := [];   # List of lists of children
  parents := [];    # List of lists of parents
  for pair in pairs do
    badcong := false;
    newchildren := []; # Children of newcong
    newparents := [];  # Parents of newcong
    newcong := _XSemigroupCongruence(S, pair);
    for i in [1 .. Length(congs1)] do
      if IsSubrelation(congs1[i], newcong) then
        if IsSubrelation(newcong, congs1[i]) then
          # This is not a new cong - drop it!
          badcong := true;
          break;
        else
          Add(newparents, i);
        fi;
      elif IsSubrelation(newcong, congs1[i]) then
        Add(newchildren, i);
      fi;
    od;
    if not badcong then
      nrcongs := nrcongs + 1;
      congs1[nrcongs] := newcong;
      children[nrcongs] := newchildren;
      parents[nrcongs] := newparents;
      for c in newchildren do
        Add(parents[c], nrcongs);
      od;
      for p in newparents do
        Add(children[p], nrcongs);
      od;
    fi;
  od;
  congs := ShallowCopy(congs1);

  # We now have all 1-generated congs, which must include all the minimal
  # congs.  We can return if necessary.
  if IsBound(record.minimal) and record.minimal = true then
    # Find all the minimal congs (those with no children)
    congs := congs{Positions(children, [])};
    # Note: we don't include the trivial cong
    # Set the MinimalXCongruencesOfSemigroup attribute
    set_func := EvalString(Concatenation("SetMinimal",
                                         type_string,
                                         "CongruencesOfSemigroup"));
    set_func(S, congs);
    # Minimal congs cannot contain each other
    children := ListWithIdenticalEntries(Length(congs), []);
    lattice := Objectify(NewType(FamilyObj(children),
                                 SEMIGROUPS_IsCongruenceLattice),
                         [children, congs]);
    return lattice;
  elif IsBound(record.1gen) and record.1gen = true then
    # Add the trivial cong at the start
    children := Concatenation([[]], children + 1);
    for i in [2 .. nrcongs + 1] do
      Add(children[i], 1, 1);
    od;
    Add(congs, _XSemigroupCongruence(S, []), 1);
    # Return the lattice, but don't set any attributes
    lattice := Objectify(NewType(FamilyObj(children),
                                 SEMIGROUPS_IsCongruenceLattice),
                         [children, congs]);
    return lattice;
  fi;

  # Take all their joins
  Info(InfoSemigroups, 1, "Taking joins...");
  join_func := EvalString(Concatenation("Join",
                                        type_string,
                                        "SemigroupCongruences"));
  length := 0;
  found := true;
  while found do
    # There are new congs to try joining
    start := length + 1;     # New congs start here
    found := false;          # Have we found any more congs on this sweep?
    length := Length(congs); # Remember starting position for next sweep
    for i in [start .. Length(congs)] do # for each new cong
      for j in [1 .. Length(congs1)] do  # for each 1-generated cong
        newcong := join_func(congs[i], congs1[j]);
        badcong := false;  # Is newcong the same as another cong?
        newchildren := []; # Children of newcong
        newparents := [];  # Parents of newcong
        for k in [1 .. Length(congs)] do
          if IsSubrelation(congs[k], newcong) then
            if IsSubrelation(newcong, congs[k]) then
              # This is the same as an old cong - discard it!
              badcong := true;
              break;
            else
              Add(newparents, k);
            fi;
          elif IsSubrelation(newcong, congs[k]) then
            Add(newchildren, k);
          fi;
        od;
        if not badcong then
          nrcongs := nrcongs + 1;
          congs[nrcongs] := newcong;
          children[nrcongs] := newchildren;
          parents[nrcongs] := newparents;
          for c in newchildren do
            Add(parents[c], nrcongs);
          od;
          for p in newparents do
            Add(children[p], nrcongs);
          od;
          found := true;
        fi;
      od;
    od;
  od;

  # Add the trivial cong at the start
  children := Concatenation([[]], children + 1);
  for i in [2 .. nrcongs + 1] do
    Add(children[i], 1, 1);
  od;
  Add(congs, _XSemigroupCongruence(S, []), 1);

  # We have a list of all the congs
  set_func := EvalString(Concatenation("Set",
                                       type_string,
                                       "CongruencesOfSemigroup"));
  set_func(S, congs);

  # Objectify the result
  lattice := Objectify(NewType(FamilyObj(children),
                               SEMIGROUPS_IsCongruenceLattice),
                       [children, congs]);
  return lattice;
end;

#

InstallMethod(DotString,
"for a congruence lattice",
[SEMIGROUPS_IsCongruenceLattice],
function(latt)
  # Call the below function, with info turned off
  return DotString(latt, rec(info := false));
end);

#

InstallMethod(DotString,
"for a congruence lattice and a record",
[SEMIGROUPS_IsCongruenceLattice, IsRecord],
function(latt, opts)
  local congs, S, symbols, i, nr, rel, str, j, k;
  # If the user wants info, then change the node labels
  if opts.info = true then
    # The congruences are stored inside the lattice object
    congs := latt![2];
    S := Range(congs[1]);
    symbols := EmptyPlist(Length(latt));
    for i in [1 .. Length(latt)] do
      nr := NrEquivalenceClasses(congs[i]);
      if nr = 1 then
        symbols[i] := "U";
      elif nr = Size(S) then
        symbols[i] := "T";
      elif IsReesCongruence(congs[i]) then
        symbols[i] := Concatenation("R", String(i));
      else
        symbols[i] := String(i);
      fi;
    od;
  else
    symbols := List([1 .. Length(latt)], String);
  fi;

  rel := List([1 .. Length(latt)], x -> Filtered(latt[x], y -> x <> y));
  str := "";

  if Length(rel) < 40 then
    Append(str, "//dot\ngraph graphname {\n     node [shape=circle]\n");
  else
    Append(str, "//dot\ngraph graphname {\n     node [shape=point]\n");
  fi;

  for i in [1 .. Length(rel)] do
    j := Difference(rel[i], Union(rel{rel[i]}));
    i := symbols[i];
    for k in j do
      k := symbols[k];
      Append(str, Concatenation(i, " -- ", k, "\n"));
    od;
  od;

  Append(str, " }");

  return str;
end);
