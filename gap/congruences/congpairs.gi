############################################################################
##
##  congruences/congpairs.gi
##  Copyright (C) 2015-17                                Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any semigroup congruence with generating
## pairs.  These act as a wrapper to the congpairs.cc file, which in turn uses
## the congruence methods in libsemigroups.
##
#############################################################################

# TODO: A method for MeetXSemigroupCongruences

#############################################################################
# Internal attributes
#############################################################################

# The following are stored as attributes to avoid recalculation

InstallMethod(EquivalenceRelationLookup,
"for an enumerable semigroup congruence",
[IsEnumerableSemigroupCongruence],
function(cong)
  # This will use the coset numbers in the TC table
  CONG_PAIRS_LOOKUP_PART(cong);
  return cong!.__fin_cong_lookup;
end);

InstallMethod(EquivalenceRelationPartition,
"for a semigroup congruence by generating pairs rep",
[IsCongruenceByGeneratingPairsRep],
function(cong)
  local S, gens, words_part;
  S := Range(cong);
  gens := GeneratorsOfSemigroup(S);
  words_part := CONG_PAIRS_NONTRIVIAL_CLASSES(cong);
  return List(words_part, c -> List(c, w -> Product(w, i -> gens[i])));
end);

InstallMethod(CongruenceByGeneratingPairsPartition,
"for an enumerable semigroup congruence",
[IsEnumerableSemigroupCongruence],
function(cong)
  CONG_PAIRS_LOOKUP_PART(cong);
  return cong!.__fin_cong_partition;
end);

InstallMethod(CongruenceByGeneratingPairsPartition,
"for an fp semigroup congruence",
[IsFpSemigroupCongruence],
function(cong)
  local enum, nrclasses, part, i, word;
  enum := Enumerator(Range(cong));
  nrclasses := NrEquivalenceClasses(cong);
  part := EmptyPlist(nrclasses);
  for i in [1 .. nrclasses] do
    part[i] := [];
  od;
  for i in [1 .. Size(enum)] do
    word := SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(enum[i]));
    Add(part[CONG_PAIRS_ELM_COSET_ID(cong, word)], i);
  od;
  return part;
end);

InstallMethod(CongruenceClassByGeneratingPairsCosetId,
"for a congruence class by gen pairs rep",
[IsCongruenceClassByGeneratingPairsRep],
c -> CONG_PAIRS_ELM_COSET_ID(EquivalenceClassRelation(c), c!.rep));

InstallMethod(CongruenceClassByGeneratingPairsType,
"for a congruence by gen pairs rep",
[IsCongruenceByGeneratingPairsRep],
function(cong)
  local side, cat;

  if cong!.type = "right" then
    side := IsRightCongruenceClass;
  elif cong!.type = "left" then
    side := IsLeftCongruenceClass;
  else
    Assert(1, cong!.type = "twosided");
    side := IsCongruenceClass;
  fi;

  if IsFpSemigroupCongruence(cong) then
    cat := IsFpSemigroupCongruenceClass;
  else
    Assert(1, IsEnumerableSemigroupCongruence(cong));
    cat := IsEnumerableSemigroupCongruenceClass;
  fi;

  return NewType(FamilyObj(Range(cong)), side and cat);
end);

#############################################################################
# Internal functions
#############################################################################

SEMIGROUPS.JoinCongruences := function(constructor, c1, c2)
  local pairs, cong;

  if Range(c1) <> Range(c2) then
    ErrorNoReturn("Semigroups: SEMIGROUPS.JoinCongruences: usage,\n",
                  "the congruences must be defined over the same semigroup,");
  fi;

  pairs := Concatenation(ShallowCopy(c1!.genpairs), ShallowCopy(c2!.genpairs));
  cong := constructor(Range(c1), pairs);

  # TODO redo this!
  # Join the lookup tables
  # if HasAsLookupTable(c1) and HasAsLookupTable(c2) then
  #   # First join the union-find tables
  #   ufdata := UF_COPY(c1!.ufdata);
  #   uf2 := c2!.ufdata;
  #   for i in [1 .. UF_SIZE(uf2)] do
  #     ii := UF_FIND(uf2, i);
  #     if ii <> i then
  #       UF_UNION(ufdata, [i, ii]);
  #     fi;
  #   od;
  #   cong!.ufdata := ufdata;

  #   # Now normalise this as a lookup table
  #   next := 1;
  #   newtable := EmptyPlist(UF_SIZE(ufdata));
  #   for i in [1 .. UF_SIZE(ufdata)] do
  #     ii := UF_FIND(ufdata, i);
  #     if ii = i then
  #       newtable[i] := next;
  #       next := next + 1;
  #     else
  #       newtable[i] := newtable[ii];
  #     fi;
  #   od;
  #   SetAsLookupTable(cong, newtable);
  # fi;
  #  TODO if one or the other does not have the lookup could do TC on
  #  which ever is smaller using the pairs of the other.
  return cong;
end;

#############################################################################
# Congruences
#############################################################################

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list of generating pairs",
[IsSemigroup, IsList], RankFilter(IsList and IsEmpty),
function(S, genpairs)
  local filt, pair, fam, cong;

  # Check the argument, and choose the correct filter
  if IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S)) then
    filt := IsFpSemigroupCongruence;
  elif IsEnumerableSemigroupRep(S) then
    filt := IsEnumerableSemigroupCongruence;
  else
    TryNextMethod();
  fi;

  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of length 2,");
    elif not pair[1] in S or not pair[2] in S then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of elements of <S>,");
    fi;
  od;

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, filt and IsSemigroupCongruence),
                    rec(genpairs := Immutable(genpairs),
                        report   := SEMIGROUPS.OptionsRec(S).report,
                        type     := "twosided",
                        range    := S));
  SetSource(cong, S);
  SetRange(cong, S);
  SetGeneratingPairsOfMagmaCongruence(cong, cong!.genpairs);

  if filt = IsFpSemigroupCongruence then
    cong!.fp_nrgens := Length(GeneratorsOfSemigroup(S));
    cong!.fp_rels :=
      List(RelationsOfFpSemigroup(S),
           rel -> [SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[1])),
                   SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[2]))]);
    cong!.fp_extra :=
      List(genpairs,
           pair -> [SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(pair[1])),
                    SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(pair[2]))]);
  fi;

  return cong;
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list of generating pairs",
[IsSemigroup, IsList], RankFilter(IsList and IsEmpty),
function(S, genpairs)
  local filt, pair, fam, cong;

  # Check the argument, and choose the correct filter
  if IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S)) then
    filt := IsFpSemigroupCongruence;
  elif IsEnumerableSemigroupRep(S) then
    filt := IsEnumerableSemigroupCongruence;
  else
    TryNextMethod();
  fi;

  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: LeftSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of length 2,");
    elif not pair[1] in S or not pair[2] in S then
      ErrorNoReturn("Semigroups: LeftSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of elements of <S>,");
    fi;
  od;

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, filt and IsLeftSemigroupCongruence),
                    rec(genpairs := Immutable(genpairs),
                        report   := SEMIGROUPS.OptionsRec(S).report,
                        type     := "left",
                        range    := S));
  SetSource(cong, S);
  SetRange(cong, S);
  SetGeneratingPairsOfLeftMagmaCongruence(cong, cong!.genpairs);

  if filt = IsFpSemigroupCongruence then
    cong!.fp_nrgens := Length(GeneratorsOfSemigroup(S));
    cong!.fp_rels :=
      List(RelationsOfFpSemigroup(S),
           rel -> [SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[1])),
                   SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[2]))]);
    cong!.fp_extra :=
      List(genpairs,
           pair -> [SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(pair[1])),
                    SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(pair[2]))]);
  fi;

  return cong;
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list of generating pairs",
[IsSemigroup, IsList], RankFilter(IsList and IsEmpty),
function(S, genpairs)
  local filt, pair, fam, cong;

  # Check the argument, and choose the correct filter
  if IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S)) then
    filt := IsFpSemigroupCongruence;
  elif IsEnumerableSemigroupRep(S) then
    filt := IsEnumerableSemigroupCongruence;
  else
    TryNextMethod();
  fi;

  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: RightSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of length 2,");
    elif not pair[1] in S or not pair[2] in S then
      ErrorNoReturn("Semigroups: RightSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of elements of <S>,");
    fi;
  od;

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, filt and IsRightSemigroupCongruence),
                    rec(genpairs := Immutable(genpairs),
                        report   := SEMIGROUPS.OptionsRec(S).report,
                        type     := "right",
                        range    := S));
  SetSource(cong, S);
  SetRange(cong, S);
  SetGeneratingPairsOfRightMagmaCongruence(cong, cong!.genpairs);

  if filt = IsFpSemigroupCongruence then
    cong!.fp_nrgens := Length(GeneratorsOfSemigroup(S));
    cong!.fp_rels :=
      List(RelationsOfFpSemigroup(S),
           rel -> [SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[1])),
                   SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[2]))]);
    cong!.fp_extra :=
      List(genpairs,
           pair -> [SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(pair[1])),
                    SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(pair[2]))]);
  fi;

  return cong;
end);

#############################################################################
# Properties of congruences
#############################################################################

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

InstallMethod(IsSemigroupCongruence,
"for a left semigroup congruence with known generating pairs",
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(cong)
  return IsRightSemigroupCongruence(cong);
end);

InstallMethod(IsSemigroupCongruence,
"for a right semigroup congruence with known generating pairs",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong)
  return IsLeftSemigroupCongruence(cong);
end);

#############################################################################
# Attributes of congruences
#############################################################################

InstallMethod(NrEquivalenceClasses,
"for a semigroup congruence by generating pairs rep",
[IsCongruenceByGeneratingPairsRep], CONG_PAIRS_NR_CLASSES);

InstallMethod(EquivalenceClasses,
"for an enumerable semigroup congruence",
[IsEnumerableSemigroupCongruence],
function(cong)
  local part, enum, reps, classes, next, i;

  part := CongruenceByGeneratingPairsPartition(cong);
  enum := EnumeratorCanonical(Range(cong));
  reps := List(part, x -> enum[x[1]]);

  classes := EmptyPlist(Length(reps));
  next := 1;

  for i in [1 .. Length(reps)] do
    classes[next] := EquivalenceClassOfElementNC(cong, reps[i]);
    SetCongruenceClassByGeneratingPairsCosetId(classes[next], i);
    SetSize(classes[next], Length(part[i]));

    next := next + 1;
  od;

  return classes;
end);

InstallMethod(EquivalenceClasses,
"for an fp semigroup congruence",
[IsFpSemigroupCongruence],
function(cong)
  local enum, nrclasses, classes, class_added, count, i, word, id;
  enum := Enumerator(Range(cong));
  nrclasses := NrEquivalenceClasses(cong);

  # Add each class when we find a representative for it
  classes := EmptyPlist(nrclasses);
  class_added := BlistList([1 .. nrclasses], []);
  count := 0;
  i := 0;
  repeat
    i := i + 1;
    word := SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(enum[i]));
    id := CONG_PAIRS_ELM_COSET_ID(cong, word);
    if not class_added[id] then
      classes[id] := EquivalenceClassOfElementNC(cong, enum[i]);
      SetCongruenceClassByGeneratingPairsCosetId(classes[id], id);
      class_added[id] := true;
      count := count + 1;
    fi;
  until count = nrclasses;

  return classes;
end);

InstallMethod(NonTrivialEquivalenceClasses,
"for a semigroup congruence by generating pairs rep",
[IsCongruenceByGeneratingPairsRep],
function(cong)
  local part, nr_classes, classes, i;
  part := EquivalenceRelationPartition(cong);
  nr_classes := Length(part);
  classes := EmptyPlist(nr_classes);
  for i in [1 .. nr_classes] do
    classes[i] := EquivalenceClassOfElementNC(cong, part[i][1]);
    SetAsList(classes[i], part[i]);
  od;
  return classes;
end);

#############################################################################
# Operations for congruences
#############################################################################

InstallMethod(ImagesElm,
"for an enumerable semigroup congruence and a multiplicative element",
[IsEnumerableSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  local lookup, enum, id, part;
  lookup := EquivalenceRelationCanonicalLookup(cong);
  enum   := EnumeratorCanonical(Range(cong));
  id     := lookup[Position(enum, elm)];
  part   := CongruenceByGeneratingPairsPartition(cong);
  return List(part[id], i -> enum[i]);
end);

InstallMethod(ImagesElm,
"for an fp semigroup congruence and a multiplicative element",
[IsFpSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  local part, pos;
  part := EquivalenceRelationPartition(cong);
  pos := PositionProperty(part, l -> [elm, l[1]] in cong);
  if pos = fail then
    return [elm];  # singleton
  fi;
  return part[pos];  # non-singleton
end);

InstallMethod(JoinSemigroupCongruences,
"for 2-sided semigroup congruences by generating pairs rep",
[IsCongruenceByGeneratingPairsRep and IsSemigroupCongruence,
 IsCongruenceByGeneratingPairsRep and IsSemigroupCongruence],
function(c1, c2)
  if c1 = c2 then
    return c1;
  fi;
  return SEMIGROUPS.JoinCongruences(SemigroupCongruence, c1, c2);
end);

InstallMethod(JoinLeftSemigroupCongruences,
"for left semigroup congruences by generating pairs rep",
[IsCongruenceByGeneratingPairsRep and IsLeftSemigroupCongruence,
 IsCongruenceByGeneratingPairsRep and IsLeftSemigroupCongruence],
function(c1, c2)
  if c1 = c2 then
    return c1;
  fi;
  return SEMIGROUPS.JoinCongruences(LeftSemigroupCongruence, c1, c2);
end);

InstallMethod(JoinRightSemigroupCongruences,
"for right semigroup congruences by generating pairs rep",
[IsCongruenceByGeneratingPairsRep and IsRightSemigroupCongruence,
 IsCongruenceByGeneratingPairsRep and IsRightSemigroupCongruence],
function(c1, c2)
  if c1 = c2 then
    return c1;
  fi;
  return SEMIGROUPS.JoinCongruences(RightSemigroupCongruence, c1, c2);
end);

#############################################################################
# Operators/comparisons of congruences
#############################################################################

InstallMethod(IsSubrelation,
"for two congruence by generating pairs reps",
[IsCongruenceByGeneratingPairsRep,
 IsCongruenceByGeneratingPairsRep],
function(cong1, cong2)
  # Only valid for certain combinations of types
  if not (cong1!.type = cong2!.type or cong1!.type = "twosided") then
    TryNextMethod();
  fi;

  # Check semigroup
  if Range(cong1) <> Range(cong2) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;

  # Test whether cong1 contains all the pairs in cong2
  return ForAll(cong2!.genpairs, pair -> pair in cong1);
end);

InstallMethod(CongruenceTestMembershipNC,
"for enumerable semigroup congruence and two elements",
[IsEnumerableSemigroupCongruence,
 IsMultiplicativeElement, IsMultiplicativeElement],
function(cong, elm1, elm2)
  return CONG_PAIRS_IN(cong, elm1, elm2);
end);

InstallMethod(CongruenceTestMembershipNC,
"for fp semigroup congruence and two elements",
[IsFpSemigroupCongruence, IsMultiplicativeElement, IsMultiplicativeElement],
function(cong, elm1, elm2)
  local word1, word2;
  word1 := SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(elm1));
  word2 := SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(elm2));
  return CONG_PAIRS_IN(cong, word1, word2);
end);

InstallMethod(\=,
"for two congruence by generating pairs reps",
[IsCongruenceByGeneratingPairsRep,
 IsCongruenceByGeneratingPairsRep],
function(c1, c2)
  if c1!.type = c2!.type then
    return Range(c1) = Range(c2)
           and ForAll(c1!.genpairs, pair -> pair in c2)
           and ForAll(c2!.genpairs, pair -> pair in c1);
  fi;
  TryNextMethod();
end);

#############################################################################
# Printing and viewing of congruences
#############################################################################

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

#############################################################################
#############################################################################
# Congruence classes
#############################################################################
#############################################################################

InstallMethod(EquivalenceClassOfElement,
"for congruence by gen pairs rep and multiplicative element",
[IsCongruenceByGeneratingPairsRep, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "the second arg <elm> must be in the ",
                  "semigroup of the first arg <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for enumerable semigroup congruence and multiplicative element",
[IsEnumerableSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  local class;

  class := Objectify(CongruenceClassByGeneratingPairsType(cong),
                     rec(rep := elm, cong := cong));

  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  SetIsFinite(class, true);

  return class;
end);

InstallMethod(EquivalenceClassOfElementNC,
"for fp semigroup congruence and multiplicative element",
[IsFpSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  local rep, class;

  # Store rep in an easy way to process
  rep := SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(elm));
  class := Objectify(CongruenceClassByGeneratingPairsType(cong),
                     rec(rep := rep, cong := cong));

  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);

  return class;
end);

InstallMethod(\in,
"for a multiplicative element and a congruence class by gen pairs rep",
[IsMultiplicativeElement, IsCongruenceClassByGeneratingPairsRep],
function(elm, class)
  return [elm, Representative(class)] in EquivalenceClassRelation(class);
end);

InstallMethod(\=,
"for two congruence classes by gen pairs rep", IsIdenticalObj,
[IsCongruenceClassByGeneratingPairsRep,
 IsCongruenceClassByGeneratingPairsRep],
function(class1, class2)
  local cong;
  cong := EquivalenceClassRelation(class1);
  if cong <> EquivalenceClassRelation(class2) then
    return false;
  fi;
  return [Representative(class1), Representative(class2)] in cong;
end);

InstallMethod(\<,
"for two fp semigroup congruence classes",
[IsFpSemigroupCongruenceClass, IsFpSemigroupCongruenceClass],
function(class1, class2)
  local cong;
  cong := EquivalenceClassRelation(class1);
  if cong <> EquivalenceClassRelation(class2) then
    return false;
  fi;
  return CONG_PAIRS_LESS_THAN(cong, class1!.rep, class2!.rep);
end);

InstallMethod(AsList,
"for a congruence class by generating pairs rep",
[IsCongruenceClassByGeneratingPairsRep],
function(class)
  return ImagesElm(EquivalenceClassRelation(class), Representative(class));
end);

InstallMethod(Enumerator, "for a congruence class by gen pairs rep",
[IsCongruenceClassByGeneratingPairsRep], AsList);

InstallMethod(Size,
"for an enumerable semigroup congruence class",
[IsEnumerableSemigroupCongruenceClass],
function(class)
  local cong, part, id;
  cong := EquivalenceClassRelation(class);
  part := CongruenceByGeneratingPairsPartition(cong);
  id   := CongruenceClassByGeneratingPairsCosetId(class);
  return Size(part[id]);
end);

InstallMethod(Size,
"for an fp semigroup congruence class",
[IsFpSemigroupCongruenceClass],
function(class)
  return Size(AsList(class));
end);
