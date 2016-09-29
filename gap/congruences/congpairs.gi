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
## generating pairs.
##
#############################################################################

#TODO: A method for MeetXSemigroupCongruences

#############################################################################
# Internal attributes
#############################################################################

# The following are stored as attributes to avoid recalculation

InstallMethod(EquivalenceRelationLookup,
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep],
function(cong)
  # This will use the coset numbers in the TC table
  CONG_PAIRS_LOOKUP_PART(cong);
  return cong!.__fin_cong_lookup;
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep],
function(cong)
  local lookup, max, dictionary, next, out, i, new_nr;
  lookup := EquivalenceRelationLookup(cong);
  max := NrEquivalenceClasses(cong);
  # We assume lookup uses the numbers 1 to max, but in an unknown order
  dictionary := ListWithIdenticalEntries(max, 0);
  next := 1;
  out := EmptyPlist(max);
  for i in [1 .. Length(lookup)] do
    new_nr := dictionary[lookup[i]];
    if new_nr = 0 then
      dictionary[lookup[i]] := next;
      new_nr := next;
      next := next + 1;
    fi;
    out[i] := new_nr;
  od;
  return out;
end);

InstallMethod(FiniteCongruenceByGeneratingPairsPartition,
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep], 
function(cong) 
  CONG_PAIRS_LOOKUP_PART(cong);
  return cong!.__fin_cong_partition;
end);

InstallMethod(FiniteCongruenceClassByGeneratingPairsCosetId, 
"for a finite congruence class by gen pairs rep",
[IsFiniteCongruenceClassByGeneratingPairsRep],
CONG_PAIRS_CLASS_COSET_ID);

InstallMethod(FiniteCongruenceClassByGeneratingPairsType, 
"for a finite congruence by gen pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep],
function(cong)
  local IsXCongruenceClass;

  if cong!.type = "right" then 
    IsXCongruenceClass := IsRightCongruenceClass;
  elif cong!.type = "left" then  
    IsXCongruenceClass := IsLeftCongruenceClass;
  else
    Assert(1, cong!.type = "twosided");
    IsXCongruenceClass := IsCongruenceClass;
  fi;
   
  return NewType(FamilyObj(Range(cong)), IsXCongruenceClass
                 and IsFiniteCongruenceClassByGeneratingPairsRep);
end);

#############################################################################
# Internal functions
#############################################################################

SEMIGROUPS.JoinCongruences := function(constructor, c1, c2)
  local pairs, cong, ufdata, uf2, i, ii, next, newtable;

  if Range(c1) <> Range(c2) then
    ErrorNoReturn("Semigroups: SEMIGROUPS.JoinCongruences: usage,\n",
                  "the congruences must be defined over the same semigroup,");
  fi;

  pairs := Concatenation(ShallowCopy(c1!.genpairs), ShallowCopy(c2!.genpairs));
  cong := constructor(Range(c1), pairs);

  # TODO redo this!
  # Join the lookup tables
  #if HasAsLookupTable(c1) and HasAsLookupTable(c2) then
  #  # First join the union-find tables
  #  ufdata := UF_COPY(c1!.ufdata);
  #  uf2 := c2!.ufdata;
  #  for i in [1 .. UF_SIZE(uf2)] do
  #    ii := UF_FIND(uf2, i);
  #    if ii <> i then
  #      UF_UNION(ufdata, [i, ii]);
  #    fi;
  #  od;
  #  cong!.ufdata := ufdata;

  #  # Now normalise this as a lookup table
  #  next := 1;
  #  newtable := EmptyPlist(UF_SIZE(ufdata));
  #  for i in [1 .. UF_SIZE(ufdata)] do
  #    ii := UF_FIND(ufdata, i);
  #    if ii = i then
  #      newtable[i] := next;
  #      next := next + 1;
  #    else
  #      newtable[i] := newtable[ii];
  #    fi;
  #  od;
  #  SetAsLookupTable(cong, newtable);
  #fi; 
  # TODO if one or the other does not have the lookup could do TC on
  # which ever is smaller using the pairs of the other.
  return cong;
end;

SEMIGROUPS.CongByGenPairs := function(S, genpairs, type)
  local pair, filter, set_pairs, fam, cong, report, range;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: SEMIGROUPS.CongByGenPairs: usage,\n",
                    "<pairs> must all be lists of length 2,");
    elif not pair[1] in S or not pair[2] in S then
      ErrorNoReturn("Semigroups: SEMIGROUPS.CongByGenPairs: usage,\n",
                    "<pairs> must all be lists of elements of <S>,");
    fi;
  od;

  if type = "left" then
    filter := IsLeftSemigroupCongruence;
    set_pairs := SetGeneratingPairsOfLeftMagmaCongruence;
  elif type = "right" then
    filter := IsRightSemigroupCongruence;
    set_pairs := SetGeneratingPairsOfRightMagmaCongruence;
  elif type = "twosided" then
    filter := IsSemigroupCongruence;
    set_pairs := SetGeneratingPairsOfMagmaCongruence;
  else
    ErrorNoReturn("Semigroups: SEMIGROUPS.CongByGenPairs: usage,\n",
                  "<type> must be \"left\", \"right\", or \"twosided\",");
  fi;

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  # Create the default type for the elements.
  cong := Objectify(NewType(fam, 
                            IsFiniteCongruenceByGeneratingPairsRep and filter),
                    rec(genpairs := Immutable(genpairs),
                        report   := SEMIGROUPS.OptionsRec(S).report,
                        type     := type,
                        range    := S));
  SetSource(cong, S);
  SetRange(cong, S);
  set_pairs(cong, Immutable(genpairs));

  return cong;
end;

#############################################################################
# Congruences
#############################################################################

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list of generating pairs", IsElmsColls,
[IsSemigroup, IsList], #FIXME change to IsEnumerableSemigroupRep when it exists
function(S, genpairs)
  return SEMIGROUPS.CongByGenPairs(S, genpairs, "twosided");
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list of generating pairs", IsElmsColls,
[IsSemigroup, IsList], #FIXME change to IsEnumerableSemigroupRep when it exists
function(S, genpairs)
  return SEMIGROUPS.CongByGenPairs(S, genpairs, "left");
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list of generating pairs", IsElmsColls,
[IsSemigroup, IsList], #FIXME change to IsEnumerableSemigroupRep when it exists
function(S, genpairs)
  return SEMIGROUPS.CongByGenPairs(S, genpairs, "right");
end);

# Empty list constructors to override library function
InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a semigroup and an empty list",
[IsSemigroup, IsList and IsEmpty], 1,
#FIXME change to IsEnumerableSemigroupRep when it exists
function(S, genpairs)
  return SEMIGROUPS.CongByGenPairs(S, genpairs, "twosided");
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for a semigroup and an empty list",
[IsSemigroup, IsList and IsEmpty], 1,
function(S, genpairs)
  return SEMIGROUPS.CongByGenPairs(S, genpairs, "left");
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for a semigroup and an empty list",
[IsSemigroup, IsList and IsEmpty], 1,
function(S, genpairs)
  return SEMIGROUPS.CongByGenPairs(S, genpairs, "right");
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
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep], CONG_PAIRS_NR_CLASSES);

InstallMethod(EquivalenceClasses,
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep],
function(cong)
  local part, enum, reps, classes, next, i;

  part := FiniteCongruenceByGeneratingPairsPartition(cong);
  enum := EnumeratorCanonical(Range(cong));
  reps := List(part, x -> enum[x[1]]);

  classes := EmptyPlist(Length(reps)); 
  next := 1;

  for i in [1 .. Length(reps)] do  
    classes[next] := EquivalenceClassOfElementNC(cong, reps[i]);
    SetFiniteCongruenceClassByGeneratingPairsCosetId(classes[next], i);
    SetSize(classes[next], Length(part[i]));

    next := next + 1;
  od;

  return classes;
end);

InstallMethod(NonTrivialEquivalenceClasses,
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep],
function(cong)
  return Filtered(EquivalenceClasses(cong), c -> Size(c) > 1);
end);

#############################################################################
# Operations for congruences
#############################################################################

InstallMethod(ImagesElm,
"for a finite semigroup congruence by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep,
 IsMultiplicativeElement],
function(cong, elm)
  local lookup, enum, id, part;

  lookup := EquivalenceRelationLookup(cong);
  enum   := EnumeratorCanonical(Range(cong));
  id     := lookup[Position(Range(cong), elm)];
  part   := FiniteCongruenceByGeneratingPairsPartition(cong);

  return List(part[id], i -> enum[i]);
end);

InstallMethod(JoinSemigroupCongruences,
"for finite (2-sided) semigroup congruences by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep and IsSemigroupCongruence,
 IsFiniteCongruenceByGeneratingPairsRep and IsSemigroupCongruence],
function(c1, c2)
  if c1 = c2 then
    return c1;
  fi;
  return SEMIGROUPS.JoinCongruences(SemigroupCongruence, c1, c2);
end);

InstallMethod(JoinLeftSemigroupCongruences,
"for finite (left) semigroup congruences by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep and IsLeftSemigroupCongruence,
 IsFiniteCongruenceByGeneratingPairsRep and IsLeftSemigroupCongruence],
function(c1, c2)
  if c1 = c2 then
    return c1;
  fi;
  return SEMIGROUPS.JoinCongruences(LeftSemigroupCongruence, c1, c2);
end);

InstallMethod(JoinRightSemigroupCongruences,
"for finite (right) semigroup congruences by generating pairs rep",
[IsFiniteCongruenceByGeneratingPairsRep and IsRightSemigroupCongruence,
 IsFiniteCongruenceByGeneratingPairsRep and IsRightSemigroupCongruence],
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
"for two finite congruence by generating pairs reps",
[IsFiniteCongruenceByGeneratingPairsRep,
 IsFiniteCongruenceByGeneratingPairsRep],
function(cong1, cong2)
  local pair, fact_pair;
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
  return ForAll(cong2!.genpairs, pair -> CONG_PAIRS_IN(cong1, pair));
end);

InstallMethod(\in,
"for dense list and finite semigroup congruence by generating pairs rep",
[IsDenseList, IsFiniteCongruenceByGeneratingPairsRep],
function(pair, cong)
  local S;
  S := Range(cong);
  if Size(pair) <> 2 then
    ErrorNoReturn("Semigroups: \\in (for a congruence): usage,\n",
                  "the first arg <pair> must be a list of length 2,");
  elif not (pair[1] in S and pair[2] in S) then
    ErrorNoReturn("Semigroups: \\in (for a congruence): usage,\n",
                  "elements of the first arg <pair> must be\n",
                  "in the range of the second arg <cong>,");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then 
    return true;
  fi;
  return CONG_PAIRS_IN(cong, pair);
end);

InstallMethod(\=, 
"for finite congruence by generating pairs rep and congruence with gen pairs",
[IsFiniteCongruenceByGeneratingPairsRep,
 IsFiniteCongruenceByGeneratingPairsRep],
function(c1, c2)
  if c1!.type = c2!.type then
    return Range(c1) = Range(c2)
           and ForAll(c1!.genpairs, pair -> CONG_PAIRS_IN(c2, pair))
           and ForAll(c2!.genpairs, pair -> CONG_PAIRS_IN(c1, pair));
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
"for finite congruence by gen pairs rep and multiplicative element",
[IsFiniteCongruenceByGeneratingPairsRep, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "the second arg <elm> must be in the ",
                  "semigroup of the first arg <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for finite congruence by gen pairs rep and multiplicative element",
[IsFiniteCongruenceByGeneratingPairsRep, IsMultiplicativeElement],
function(cong, elm)
  local class;
  
  class := Objectify(FiniteCongruenceClassByGeneratingPairsType(cong),
                     rec(rep := elm, cong := cong));

  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  SetIsFinite(class, true);

  return class;
end);

InstallMethod(\in,
"for a multiplicative element and a finite congruence class by gen pairs rep",
[IsMultiplicativeElement, IsFiniteCongruenceClassByGeneratingPairsRep],
function(elm, class)
  return [elm, Representative(class)] in EquivalenceClassRelation(class);
end);

InstallMethod(\=,
"for two finite congruence classes by gen pairs rep", IsIdenticalObj,
[IsFiniteCongruenceClassByGeneratingPairsRep,
 IsFiniteCongruenceClassByGeneratingPairsRep],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
    and FiniteCongruenceClassByGeneratingPairsCosetId(class1)
        = FiniteCongruenceClassByGeneratingPairsCosetId(class2);
end);

InstallMethod(AsList,
"for a finite congruence class by gen pairs rep",
[IsFiniteCongruenceClassByGeneratingPairsRep],
function(class)
  local cong, part, id, enum;
  
  cong := EquivalenceClassRelation(class);
  part := FiniteCongruenceByGeneratingPairsPartition(cong);
  id   := FiniteCongruenceClassByGeneratingPairsCosetId(class);
  enum := EnumeratorCanonical(Range(cong));
  return List(part[id], i -> enum[i]);
end);

InstallMethod(Enumerator, "for a finite congruence class by gen pairs rep",
[IsFiniteCongruenceClassByGeneratingPairsRep], AsList);

InstallMethod(Size,
"for a finite congruence class by gen pairs rep",
[IsFiniteCongruenceClassByGeneratingPairsRep],
function(class)
  local cong, part, id;
  if HasAsList(class) then 
    # TODO: This is currently unreachable
    return Size(AsList(class));
  fi;
  cong := EquivalenceClassRelation(class);
  part := FiniteCongruenceByGeneratingPairsPartition(cong);
  id   := FiniteCongruenceClassByGeneratingPairsCosetId(class);
  return Size(part[id]);
end);
