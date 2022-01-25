###########################################################################
##
##  main/froidure-pin.gi
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for accessing the kernel level version of the
# Froidure-Pin algorithm for enumerating arbitrary semigroups.

#  For some details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

InstallMethod(CanComputeFroidurePin, "for a semigroup",
[IsSemigroup], S -> CanComputeGapFroidurePin(S) or
                    CanComputeCppFroidurePin(S));

InstallMethod(HasFroidurePin, "for a semigroup",
[IsSemigroup], S -> HasGapFroidurePin(S) or
                    HasCppFroidurePin(S));

InstallTrueMethod(CanComputeFroidurePin, CanComputeGapFroidurePin);

for x in [IsFpSemigroup and IsFinite,
          IsFpMonoid and IsFinite,
          IsMatrixOverFiniteFieldSemigroup,
          IsGraphInverseSemigroup and IsFinite,
          IsMcAlisterTripleSubsemigroup,
          IsSemigroup and IsFreeBandElementCollection,
          IsPermGroup,
          IsFreeInverseSemigroupCategory] do
  InstallTrueMethod(CanComputeGapFroidurePin, x);
od;

InstallMethod(CanComputeGapFroidurePin, "for a semigroup",
[IsSemigroup], ReturnFalse);

InstallTrueMethod(CanComputeGapFroidurePin,
IsSemigroup and HasMultiplicationTable and HasGeneratorsOfSemigroup);

InstallImmediateMethod(CanComputeGapFroidurePin,
IsReesZeroMatrixSubsemigroup and HasRowsOfReesZeroMatrixSemigroup
    and HasColumnsOfReesZeroMatrixSemigroup, 0,
function(R)
  return IsPermGroup(UnderlyingSemigroup(R))
    or CanComputeFroidurePin(UnderlyingSemigroup(R));
end);

InstallImmediateMethod(CanComputeGapFroidurePin,
IsReesZeroMatrixSubsemigroup and HasGeneratorsOfSemigroup, 0,
function(R)
  return CanComputeFroidurePin(ParentAttr(R));
end);

InstallImmediateMethod(CanComputeGapFroidurePin,
IsReesMatrixSubsemigroup and HasRowsOfReesMatrixSemigroup
    and HasColumnsOfReesMatrixSemigroup, 0,
function(R)
  return IsPermGroup(UnderlyingSemigroup(R))
    or CanComputeFroidurePin(UnderlyingSemigroup(R));
end);

InstallImmediateMethod(CanComputeGapFroidurePin,
IsReesMatrixSubsemigroup and HasGeneratorsOfSemigroup, 0,
function(R)
  return CanComputeFroidurePin(ParentAttr(R));
end);

InstallImmediateMethod(CanComputeGapFroidurePin,
IsQuotientSemigroup and HasQuotientSemigroupPreimage, 0,
S -> CanComputeFroidurePin(QuotientSemigroupPreimage(S)));

InstallTrueMethod(CanComputeGapFroidurePin,
IsSemigroupIdeal and IsReesMatrixSubsemigroup);

InstallTrueMethod(CanComputeGapFroidurePin,
IsSemigroupIdeal and IsReesZeroMatrixSubsemigroup);

InstallImmediateMethod(CanComputeGapFroidurePin,
IsDualSemigroupRep and HasGeneratorsOfSemigroup, 0,
function(S)
  local W;
  # Get the whole family!
  W := DualSemigroupOfFamily(ElementsFamily(FamilyObj(S)));
  return CanComputeFroidurePin(W);
end);

InstallMethod(GapFroidurePin, "for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  local data, hashlen, nrgens, nr, val, i;

  data := rec(elts := [],
              final := [],
              first := [],
              found := false,
              genslookup := [],
              left := [],
              len := 1,
              lenindex := [],
              nrrules := 0,
              parent := S,
              prefix := [],
              reduced := [[]],
              right := [],
              rules := [],
              stopper := false,
              suffix := [],
              words := []);

  hashlen         := SEMIGROUPS.OptionsRec(S).hashlen;

  data.gens := ShallowCopy(GeneratorsOfSemigroup(S));
  nrgens    := Length(data.gens);
  data.ht   := HTCreate(data.gens[1], rec(treehashsize := hashlen));
  nr        := 0;
  data.one  := false;
  data.pos  := 1;
  data.lenindex[1] := 1;
  data.genstoapply := [1 .. nrgens];

  # add the generators
  for i in data.genstoapply do
    val := HTValue(data.ht, data.gens[i]);
    if val = fail then  # new generator
      nr := nr + 1;
      HTAdd(data.ht, data.gens[i], nr);
      data.elts[nr] := data.gens[i];
      data.words[nr] := [i];
      data.first[nr] := i;
      data.final[nr] := i;
      data.prefix[nr] := 0;
      data.suffix[nr] := 0;
      data.left[nr] := EmptyPlist(nrgens);
      data.right[nr] := EmptyPlist(nrgens);
      data.genslookup[i] := nr;
      data.reduced[nr] := List([1 .. nrgens], ReturnFalse);

      if data.one = false and ForAll(data.gens,
                                     y -> data.gens[i] * y = y
                                        and y * data.gens[i] = y) then
        data.one := nr;
      fi;
    else  # duplicate generator
      data.genslookup[i] := val;
      data.nrrules := data.nrrules + 1;
      data.rules[data.nrrules] := [[i], [val]];
    fi;
  od;

  data.nr := nr;
  return data;
end);

#############################################################################
# 1. Internal methods
#############################################################################

# This is a fallback method in case we don't know any better way to check this

InstallMethod(IsFinite,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  return Size(S) < infinity;
end);

InstallMethod(AsSet,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  if not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a semigroup) must be finite,");
  fi;
  return SortedList(RUN_FROIDURE_PIN(GapFroidurePin(S), -1).elts);
end);

InstallMethod(AsList,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
AsListCanonical);

InstallMethod(AsListCanonical,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  if not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a semigroup) must be finite,");
  fi;
  return RUN_FROIDURE_PIN(GapFroidurePin(S), -1).elts;
end);

# For ideals and other generatorless semigroups

InstallMethod(AsListCanonical, "for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  GeneratorsOfSemigroup(S);
  return AsListCanonical(S);
end);

InstallMethod(Enumerator,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  if (IsReesMatrixSemigroup(S) or IsReesZeroMatrixSemigroup(S))
      and HasIsWholeFamily(S) and IsWholeFamily(S) then
    TryNextMethod();
  fi;
  return EnumeratorCanonical(S);
end);

InstallMethod(EnumeratorCanonical,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  local enum;

  if HasAsListCanonical(S) then
    return AsListCanonical(S);
  fi;

  enum := rec();

  enum.NumberElement := function(enum, x)
    return PositionCanonical(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    local fp;
    fp := GapFroidurePin(S);
    if not (IsBound(fp.elts) and nr < Length(fp.elts) and IsBound(fp.elts[nr]))
        then
      fp := RUN_FROIDURE_PIN(fp, nr);
    fi;

    if nr <= Length(fp.elts) and IsBound(fp.elts[nr]) then
      return fp.elts[nr];
    else
      return fail;
    fi;
  end;

  enum.Length := enum -> Size(S);

  enum.AsList := function(enum)
    return AsListCanonical(S);
  end;

  enum.Membership := function(x, enum)
    return PositionCanonical(S, x) <> fail;
  end;

  enum.IsBound\[\] := function(enum, nr)
    return nr <= Length(enum);
  end;

  enum := EnumeratorByFunctions(S, enum);
  SetIsSemigroupEnumerator(enum, true);
  return enum;
end);

InstallMethod(IteratorCanonical,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
S -> IteratorList(EnumeratorCanonical(S)));

InstallMethod(Iterator,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
S -> IteratorList(Enumerator(S)));

InstallMethod(IteratorSorted,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
S -> IteratorList(EnumeratorSorted(S)));

InstallMethod(Iterator, "for semigroup enumerator sorted",
[IsSemigroupEnumerator and IsSSortedList],
enum -> IteratorSorted(UnderlyingCollection(enum)));

# different method for ideals

InstallMethod(Size,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  if HasIsFinite(S) and not IsFinite(S) then
    return infinity;
  fi;
  return Length(RUN_FROIDURE_PIN(GapFroidurePin(S), -1).elts);
end);

# different method for ideals

InstallMethod(\in,
"for mult. elt. and a semigroup with CanComputeGapFroidurePin + generators",
[IsMultiplicativeElement,
 IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(x, S)
  return PositionCanonical(S, x) <> fail;
end);

# different method for ideals

InstallMethod(Idempotents,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  return EnumeratorCanonical(S){IdempotentsSubset(S, [1 .. Size(S)])};
end);

InstallMethod(PositionCanonical,
"for a semigroup with CanComputeGapFroidurePin, generators, and mult. elt",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  local fp, ht, nr, val, limit, pos;

  if FamilyObj(x) <> ElementsFamily(FamilyObj(S)) then
    return fail;
  fi;

  fp := GapFroidurePin(S);
  ht := fp.ht;
  nr := fp.nr;
  repeat
    val := HTValue(ht, x);
    if val <> fail then
      return val;
    fi;
    limit := nr + 1;
    fp := RUN_FROIDURE_PIN(fp, limit);
    pos := fp.pos;
    nr := fp.nr;
  until pos > nr;

  return HTValue(ht, x);
end);

# Position exists so that we can call it on objects with an uninitialised data
# structure, without first having to initialise the data structure to realise
# that <x> is not in it.

# This returns the current position of x, if it is already known to belong to
# S.

InstallMethod(Position,
"for a semigroup with CanComputeGapFroidurePin, mult. element, zero cyc",
[IsSemigroup and CanComputeGapFroidurePin, IsMultiplicativeElement, IsZeroCyc],
PositionOp);

InstallMethod(PositionOp,
"for a semigroup with CanComputeGapFroidurePin, multi. element, zero cyc",
[IsSemigroup and CanComputeGapFroidurePin, IsMultiplicativeElement, IsZeroCyc],
function(S, x, n)
  if FamilyObj(x) <> ElementsFamily(FamilyObj(S)) then
    return fail;
  fi;
  return HTValue(GapFroidurePin(S).ht, x);
end);

InstallMethod(PositionSortedOp,
"for a semigroup with CanComputeGapFroidurePin, generators and mult. elt.",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  if FamilyObj(x) <> ElementsFamily(FamilyObj(S)) then
    return fail;
  elif not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a semigroup) must be finite,");
  fi;
  return Position(AsSet(S), x);
end);

InstallMethod(IsEnumerated, "for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  local fp;
  if HasGapFroidurePin(S) then
    fp := GapFroidurePin(S);
    return fp.pos > fp.nr;
  fi;
  return false;
end);

# the main algorithm

InstallMethod(Enumerate,
"for a semigroup with CanComputeGapFroidurePin and known generators and pos int",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup, IsInt],
function(S, limit)
  RUN_FROIDURE_PIN(GapFroidurePin(S), limit);
  return S;
end);

InstallMethod(Enumerate,
"for a semigroup with CanComputeGapFroidurePin and known generators",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup],
function(S)
  return Enumerate(S, -1);
end);

# same method for ideals

InstallMethod(RightCayleyGraphSemigroup,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin], 3,
function(S)
  return RUN_FROIDURE_PIN(GapFroidurePin(S), -1).right;
end);

InstallMethod(RightCayleyDigraph,
"for a semigroup with CanComputeGapFroidurePin rep",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  local D;
  if not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a semigroup) must be finite,");
  fi;

  D := DigraphNC(MakeImmutable(RightCayleyGraphSemigroup(S)));
  SetFilterObj(D, IsCayleyDigraph);
  SetSemigroupOfCayleyDigraph(D, S);
  SetGeneratorsOfCayleyDigraph(D, GeneratorsOfSemigroup(S));
  return D;
end);

# same method for ideals

InstallMethod(LeftCayleyGraphSemigroup,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin], 3,
function(S)
  return RUN_FROIDURE_PIN(GapFroidurePin(S), -1).left;
end);

InstallMethod(LeftCayleyDigraph,
"for a semigroup with CanComputeGapFroidurePin rep",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  local D;
  if not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a semigroup) must be finite,");
  fi;
  D := DigraphNC(MakeImmutable(LeftCayleyGraphSemigroup(S)));
  SetFilterObj(D, IsCayleyDigraph);
  SetSemigroupOfCayleyDigraph(D, S);
  SetGeneratorsOfCayleyDigraph(D, GeneratorsOfSemigroup(S));
  return D;
end);

InstallMethod(NrIdempotents, "for a semigroup with CanComputeGapFroidurePin rep",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  else
    return Length(Idempotents(S));
  fi;
end);

InstallMethod(Factorization,
"for a semigroup with CanComputeGapFroidurePin and a positive integer",
[IsSemigroup and CanComputeGapFroidurePin, IsPosInt],
function(S, i)
  local words;

  if i > Size(S) then
    ErrorNoReturn("the 2nd argument (a positive integer) is greater ",
                  "than the size of the 1st argument (a semigroup)");
  fi;

  words := RUN_FROIDURE_PIN(GapFroidurePin(S), -1).words;
  return ShallowCopy(words[i]);
end);

InstallMethod(MinimalFactorization,
"for a semigroup with CanComputeGapFroidurePin and a multiplicative element",
[IsSemigroup and CanComputeGapFroidurePin, IsMultiplicativeElement],
function(S, x)
  local words;

  if not x in S then
    ErrorNoReturn("the 2nd argument (a mult. elt.) is not an element ",
                  "of the 1st argument (a semigroup)");
  fi;

  words := RUN_FROIDURE_PIN(GapFroidurePin(S), -1).words;
  return ShallowCopy(words[PositionCanonical(S, x)]);
end);

InstallMethod(MinimalFactorization,
"for a semigroup with CanComputeGapFroidurePin and a pos. int.",
[IsSemigroup and CanComputeGapFroidurePin, IsPosInt],
function(S, i)
  local words;

  if i > Size(S) then
    ErrorNoReturn("expected a value in [1, ", Size(S), "] for ",
                  "the 2nd argument, but found ", i);
  fi;

  words := RUN_FROIDURE_PIN(GapFroidurePin(S), -1).words;
  return ShallowCopy(words[i]);
end);

InstallMethod(RulesOfSemigroup,
"for a semigroup with CanComputeGapFroidurePin",
[IsSemigroup and CanComputeGapFroidurePin],
function(S)
  return RUN_FROIDURE_PIN(GapFroidurePin(S), -1).rules;
end);

InstallMethod(IdempotentsSubset,
"for a semigroup with CanComputeGapFroidurePin + known generators, hom. list",
[IsSemigroup and CanComputeGapFroidurePin and HasGeneratorsOfSemigroup,
 IsHomogeneousList],
function(S, list)
  local fp, left, final, prefix, elts, out, i, j, pos;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  fp := RUN_FROIDURE_PIN(GapFroidurePin(S), -1);
  left   := fp.left;
  final := fp.final;
  prefix := fp.prefix;
  elts := fp.elts;

  out := [];
  for pos in list do
    i := pos;
    j := pos;
    repeat
      j := left[j][final[i]];
      i := prefix[i];
    until i = 0;
    if j = pos then
      Add(out, pos);
    fi;
  od;
  return out;
end);
