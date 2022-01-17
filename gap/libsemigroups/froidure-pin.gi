###########################################################################
##
##  froidure-pin.gi
##  Copyright (C) 2020                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
###########################################################################
##

# We have the following loop instead of IsMatrixOverSemiringSemigroup because
# semigroups of matrices over a finite field below to
# IsMatrixOverSemiringSemigroup, but cannot compute CppFroidurePin

InstallTrueMethod(CanComputeFroidurePin, CanComputeCppFroidurePin);

for x in [IsTransformationSemigroup,
          IsBooleanMatSemigroup,
          IsIntegerMatrixSemigroup,
          IsMaxPlusMatrixSemigroup,
          IsMinPlusMatrixSemigroup,
          IsTropicalMinPlusMatrixSemigroup,
          IsTropicalMaxPlusMatrixSemigroup,
          IsNTPMatrixSemigroup,
          IsProjectiveMaxPlusMatrixSemigroup,
          IsBipartitionSemigroup,
          IsPBRSemigroup,
          IsTransformationSemigroup,
          IsPartialPermSemigroup] do
  InstallTrueMethod(CanComputeCppFroidurePin,
                    x and HasGeneratorsOfSemigroup);
  InstallTrueMethod(CanComputeCppFroidurePin,
                    x and IsSemigroupIdeal);
od;

InstallMethod(CanComputeCppFroidurePin, "for a semigroup", [IsSemigroup],
ReturnFalse);

###########################################################################
## Function for getting the correct record from the `libsemigroups` record.
###########################################################################

DeclareOperation("FroidurePinMemFnRec", [IsSemigroup]);

InstallMethod(FroidurePinMemFnRec, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  if DegreeOfTransformationSemigroup(S) <= 16
      and IsBound(LIBSEMIGROUPS_HPCOMBI_ENABLED) then
    return libsemigroups.FroidurePinTransf16;
  elif DegreeOfTransformationSemigroup(S) <= 65536 then
    return libsemigroups.FroidurePinTransfUInt2;
  elif DegreeOfTransformationSemigroup(S) <= 18446744073709551616 then
    return libsemigroups.FroidurePinTransfUInt4;
  else
    Error("transformation degree is too high!");
  fi;
end);

InstallMethod(FroidurePinMemFnRec, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(S)
  local N;
  N := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S));
  if N <= 16 and IsBound(LIBSEMIGROUPS_HPCOMBI_ENABLED) then
    return libsemigroups.FroidurePinPPerm16;
  elif N <= 65536 then
    return libsemigroups.FroidurePinPPermUInt2;
  elif N <= 18446744073709551616 then
    return libsemigroups.FroidurePinPPermUInt4;
  else
    Error("partial perm degree is too high!");
  fi;
end);

InstallMethod(FroidurePinMemFnRec, "for a boolean matrix semigroup",
[IsBooleanMatSemigroup],
function(S)
  local N;
  N := DimensionOfMatrixOverSemiring(Representative(S));
  if N <= 8 then
    return libsemigroups.FroidurePinBMat8;
  else
    return libsemigroups.FroidurePinBMat;
  fi;
end);

InstallMethod(FroidurePinMemFnRec, "for a bipartition semigroup",
[IsBipartitionSemigroup], S -> libsemigroups.FroidurePinBipart);

InstallMethod(FroidurePinMemFnRec, "for an integer matrix semigroup",
[IsIntegerMatrixSemigroup], S -> libsemigroups.FroidurePinIntMat);

InstallMethod(FroidurePinMemFnRec, "for an max-plus matrix semigroup",
[IsMaxPlusMatrixSemigroup], S -> libsemigroups.FroidurePinMaxPlusMat);

InstallMethod(FroidurePinMemFnRec, "for an min-plus matrix semigroup",
[IsMinPlusMatrixSemigroup], S -> libsemigroups.FroidurePinMinPlusMat);

InstallMethod(FroidurePinMemFnRec, "for a tropical max-plus matrix semigroup",
[IsTropicalMaxPlusMatrixSemigroup],
S -> libsemigroups.FroidurePinMaxPlusTruncMat);

InstallMethod(FroidurePinMemFnRec, "for a tropical min-plus matrix semigroup",
[IsTropicalMinPlusMatrixSemigroup],
S -> libsemigroups.FroidurePinMinPlusTruncMat);

InstallMethod(FroidurePinMemFnRec, "for an ntp matrix semigroup",
[IsNTPMatrixSemigroup], S -> libsemigroups.FroidurePinNTPMat);

InstallMethod(FroidurePinMemFnRec,
"for a projective max-plus matrix semigroup",
[IsProjectiveMaxPlusMatrixSemigroup],
S -> libsemigroups.FroidurePinProjMaxPlusMat);

InstallMethod(FroidurePinMemFnRec, "for a pbr semigroup",
[IsPBRSemigroup], S -> libsemigroups.FroidurePinPBR);

BindGlobal("_GetElement",
function(coll, x)
  Assert(1, IsMultiplicativeElementCollection(coll));
  Assert(1, IsMultiplicativeElement(x));
  if not (IsPartialPermCollection(coll) or IsTransformationCollection(coll)) then
    return x;
  elif IsPartialPermCollection(coll) then
    return [x, Maximum(DegreeOfPartialPermCollection(coll),
                       CodegreeOfPartialPermCollection(coll))];
  else
    return [x, DegreeOfTransformationCollection(coll)];
  fi;
end);

###########################################################################
## Constructor - constructs a libsemigroups CppFroidurePin object, and adds the
## generators of the semigroup to that.
###########################################################################

InstallMethod(HasCppFroidurePin, "for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  return IsBound(S!.CppFroidurePin)
      and IsValidGapbind14Object(S!.CppFroidurePin);
end);

InstallMethod(HasCppFroidurePin, "for a semigroup", [IsSemigroup], ReturnFalse);

InstallGlobalFunction(CppFroidurePin,
function(S)
  local record, T, add_generator, coll, x;
  Assert(1, IsSemigroup(S));
  Assert(1, CanComputeCppFroidurePin(S));
  if HasCppFroidurePin(S) then
    return S!.CppFroidurePin;
  fi;
  Unbind(S!.CppFroidurePin);
  record := FroidurePinMemFnRec(S);
  T  := record.make([]);
  add_generator := record.add_generator;
  coll := GeneratorsOfSemigroup(S);
  for x in coll do
    add_generator(T, _GetElement(coll, x));
  od;
  S!.CppFroidurePin := T;
  return T;
end);

###########################################################################
## Size/IsFinite
###########################################################################

InstallMethod(Size, "for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  if not IsFinite(S) then
    return infinity;
  fi;
  return FroidurePinMemFnRec(S).size(CppFroidurePin(S));
end);

InstallMethod(IsFinite, "for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  return FroidurePinMemFnRec(S).size(CppFroidurePin(S)) < infinity;
end);

###########################################################################
## AsSet/AsList etc
###########################################################################

InstallMethod(AsSet, "for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local result, sorted_at, T, i;
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  elif IsPartialPermSemigroup(S) then
    # Special case required because < for libsemigroups PartialPerms and < for
    # GAP partial perms are different.
    return AsSet(AsList(S));
  fi;
  result := EmptyPlist(Size(S));
  sorted_at := FroidurePinMemFnRec(S).sorted_at;
  T := CppFroidurePin(S);
  for i in [1 .. Size(S)] do
    result[i] := sorted_at(T, i - 1);
  od;
  SetIsSSortedList(result, true);
  return result;
end);

InstallMethod(AsListCanonical,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local result, at, T, i;
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  result := EmptyPlist(Size(S));
  at := FroidurePinMemFnRec(S).at;
  T := CppFroidurePin(S);
  for i in [1 .. Size(S)] do
    result[i] := at(T, i - 1);
  od;
  return result;
end);

InstallMethod(AsList,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin], AsListCanonical);

###########################################################################
## Position etc
###########################################################################

InstallMethod(PositionCanonical,
"for a semigroup with CanComputeCppFroidurePin and mult. element",
[IsSemigroup and CanComputeCppFroidurePin, IsMultiplicativeElement],
function(S, x)
  local pos;
  if IsPartialPermSemigroup(S) then
    if DegreeOfPartialPermSemigroup(S) < DegreeOfPartialPerm(x)
        or CodegreeOfPartialPermSemigroup(S) < CodegreeOfPartialPerm(x) then
      return fail;
    fi;
  elif IsTransformationSemigroup(S) then
    if DegreeOfTransformationSemigroup(S) < DegreeOfTransformation(x) then
      return fail;
    fi;
  fi;

  pos := FroidurePinMemFnRec(S).position(CppFroidurePin(S), _GetElement(S, x));
  if pos < 0 then
    return fail;
  else
    return pos + 1;
  fi;
end);

InstallMethod(Position,
"for a semigroup with CanComputeCppFroidurePin, mult. element, and zero",
[IsSemigroup and CanComputeCppFroidurePin, IsMultiplicativeElement, IsZeroCyc],
PositionOp);

InstallMethod(PositionOp,
"for a semigroup with CanComputeCppFroidurePin, mult. element, and zero",
[IsSemigroup and CanComputeCppFroidurePin, IsMultiplicativeElement, IsZeroCyc],
function(S, x, n)
  local pos;
  if IsPartialPermSemigroup(S) then
    if DegreeOfPartialPermSemigroup(S) < DegreeOfPartialPerm(x)
        or CodegreeOfPartialPermSemigroup(S) < CodegreeOfPartialPerm(x) then
      return fail;
    fi;
  elif IsTransformationSemigroup(S) then
    if DegreeOfTransformationSemigroup(S) < DegreeOfTransformation(x) then
      return fail;
    fi;
  fi;

  pos := FroidurePinMemFnRec(S).current_position(CppFroidurePin(S),
                                                 _GetElement(S, x));
  if pos < 0 then
    return fail;
  else
    return pos + 1;
  fi;
end);

InstallMethod(PositionSortedOp,
"for a semigroup with CanComputeCppFroidurePin and mult. element",
[IsSemigroup and CanComputeCppFroidurePin, IsMultiplicativeElement],
function(S, x)
  local pos;
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  elif IsPartialPermSemigroup(S) then
    if DegreeOfPartialPermSemigroup(S) < DegreeOfPartialPerm(x)
        or CodegreeOfPartialPermSemigroup(S) < CodegreeOfPartialPerm(x) then
      return fail;
    fi;
  elif IsTransformationSemigroup(S) then
    if DegreeOfTransformationSemigroup(S) < DegreeOfTransformation(x) then
      return fail;
    fi;
  fi;
  pos := FroidurePinMemFnRec(S).sorted_position(CppFroidurePin(S),
                                                _GetElement(S, x));
  if pos < 0 then
    return fail;
  else
    return pos + 1;
  fi;
end);

###########################################################################
## Membership
###########################################################################

InstallMethod(\in,
"for mult. element and a semigroup with CanComputeCppFroidurePin",
[IsMultiplicativeElement, IsSemigroup and CanComputeCppFroidurePin],
{x, S} -> PositionCanonical(S, x) <> fail);

###########################################################################
## NrIdempotents
###########################################################################

InstallMethod(NrIdempotents,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  return FroidurePinMemFnRec(S).number_of_idempotents(CppFroidurePin(S));
end);

InstallMethod(Idempotents,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  return FroidurePinMemFnRec(S).idempotents(CppFroidurePin(S));
end);

###########################################################################
## MinimalFactorization
###########################################################################

InstallMethod(MinimalFactorization,
"for a semigroup with CanComputeCppFroidurePin and pos. int",
[IsSemigroup and CanComputeCppFroidurePin, IsPosInt],
function(S, i)
  return FroidurePinMemFnRec(S).factorisation(CppFroidurePin(S), i - 1) + 1;
end);

InstallMethod(MinimalFactorization,
"for a semigroup with CanComputeCppFroidurePin and mult. element",
[IsSemigroup and CanComputeCppFroidurePin, IsMultiplicativeElement],
function(S, x)
  if not x in S then
    Error("the 2nd argument (a mult. element) is not an element ",
          "of the first argument (a semigroup)");
  fi;
  return MinimalFactorization(S, PositionCanonical(S, x));
end);

InstallMethod(Factorization,
"for a semigroup with CanComputeCppFroidurePin and pos. int",
[IsSemigroup and CanComputeCppFroidurePin, IsPosInt],
MinimalFactorization);

InstallMethod(Factorization,
"for a semigroup with CanComputeCppFroidurePin and mult. element",
[IsSemigroup and CanComputeCppFroidurePin, IsMultiplicativeElement],
MinimalFactorization);

###########################################################################
## Enumerate
###########################################################################

InstallMethod(Enumerate,
"for a semigroup with CanComputeCppFroidurePin and pos int",
[IsSemigroup and CanComputeCppFroidurePin, IsInt],
function(S, n)
  FroidurePinMemFnRec(S).enumerate(CppFroidurePin(S), n);
  return S;
end);

InstallMethod(Enumerate,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  FroidurePinMemFnRec(S).enumerate(CppFroidurePin(S), -1);
  return S;
end);

InstallMethod(IsEnumerated,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  return FroidurePinMemFnRec(S).finished(CppFroidurePin(S));
end);

###########################################################################
## Cayley graphs etc
###########################################################################

InstallMethod(LeftCayleyGraphSemigroup,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  return FroidurePinMemFnRec(S).left_cayley_graph(CppFroidurePin(S)) + 1;
end);

InstallMethod(LeftCayleyDigraph,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local D;
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  D := DigraphNC(LeftCayleyGraphSemigroup(S));
  SetFilterObj(D, IsCayleyDigraph);
  SetSemigroupOfCayleyDigraph(D, S);
  SetGeneratorsOfCayleyDigraph(D, GeneratorsOfSemigroup(S));
  return D;
end);

InstallMethod(RightCayleyGraphSemigroup,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  return FroidurePinMemFnRec(S).right_cayley_graph(CppFroidurePin(S)) + 1;
end);

InstallMethod(RightCayleyDigraph,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local D;
  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;
  D := DigraphNC(RightCayleyGraphSemigroup(S));
  SetFilterObj(D, IsCayleyDigraph);
  SetSemigroupOfCayleyDigraph(D, S);
  SetGeneratorsOfCayleyDigraph(D, GeneratorsOfSemigroup(S));
  return D;
end);

########################################################################
## Enumerators
########################################################################

InstallMethod(EnumeratorSorted,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local sorted_at, T, enum;

  if not IsFinite(S) then
    Error("the 1st argument (a semigroup) is not finite");
  fi;

  sorted_at := FroidurePinMemFnRec(S).sorted_at;
  T := CppFroidurePin(S);

  enum := rec();

  enum.NumberElement := function(enum, x)
    return PositionSortedOp(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    return sorted_at(T, nr - 1);
  end;

  enum.Length := enum -> Size(S);

  enum.Membership := function(x, enum)
    return PositionCanonical(S, x) <> fail;
  end;

  enum.IsBound\[\] := function(enum, nr)
    return nr <= Size(S);
  end;

  enum := EnumeratorByFunctions(S, enum);
  SetIsSemigroupEnumerator(enum, true);
  SetIsSSortedList(enum, true);

  return enum;
end);

InstallMethod(Enumerator,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin], 6, EnumeratorCanonical);

InstallMethod(EnumeratorCanonical,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local at, T, enum;

  if HasAsListCanonical(S) then
    return AsListCanonical(S);
  fi;

  at := FroidurePinMemFnRec(S).at;
  T := CppFroidurePin(S);

  enum := rec();

  enum.NumberElement := function(enum, x)
    return PositionCanonical(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    if nr > Length(enum) then
      return fail;
    else
      return at(T, nr - 1);
    fi;
  end;

  enum.Length := function(enum)
    if not IsFinite(S) then
      return infinity;
    else
      return Size(S);
    fi;
  end;

  enum.Membership := function(x, enum)
    return PositionCanonical(S, x) <> fail;
  end;

  enum.IsBound\[\] := function(enum, nr)
    return nr <= Size(S);
  end;

  enum := EnumeratorByFunctions(S, enum);
  SetIsSemigroupEnumerator(enum, true);

  return enum;
end);

# The next method is necessary since it does not necessarily involve
# enumerating the entire semigroup in the case that the semigroup is partially
# enumerated and <list> only contains indices that are within the so far
# enumerated range. The default methods in the library do, because they require
# the length of the enumerator to check that the list of positions is valid.

InstallMethod(ELMS_LIST, "for a semigroup enumerator and a list",
[IsSemigroupEnumerator, IsList],
function(enum, list)
  local S, result, at, T, i;
  S := UnderlyingCollection(enum);
  if not CanComputeCppFroidurePin(S) then
    TryNextMethod();
  fi;
  result := EmptyPlist(Length(list));
  at := FroidurePinMemFnRec(S).at;
  T := CppFroidurePin(S);
  for i in list do
    Add(result, at(T, i - 1));
  od;
  return result;
end);

########################################################################
## Iterators
########################################################################

InstallMethod(IteratorCanonical,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
S -> IteratorList(EnumeratorCanonical(S)));

InstallMethod(Iterator,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin], IteratorCanonical);

InstallMethod(IteratorSorted,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
S -> IteratorList(EnumeratorSorted(S)));

########################################################################
## MultiplicationTable
########################################################################

InstallMethod(MultiplicationTable,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  local N, result, pos_to_pos_sorted, fast_product, T, next, k, i, j;
  if not IsFinite(S) then
    Error("the first argument (a semigroup) must be finite,");
  fi;
  N      := Size(S);
  result := List([1 .. N], x -> EmptyPlist(N));
  pos_to_pos_sorted := FroidurePinMemFnRec(S).position_to_sorted_position;
  fast_product := FroidurePinMemFnRec(S).fast_product;
  T := CppFroidurePin(S);
  for i in [0 .. N - 1] do
    next := result[pos_to_pos_sorted(T, i) + 1];
    for j in [0 .. N - 1] do
      k := pos_to_pos_sorted(T, j) + 1;
      next[k] := pos_to_pos_sorted(T, fast_product(T, i, j)) + 1;
    od;
  od;
  return result;
end);

########################################################################
## ClosureSemigroupOrMonoidNC
########################################################################

# TODO(later) require a ClosureSemigroupDestructive that uses closure directly,
# not copy then closure.

InstallMethod(ClosureSemigroupOrMonoidNC,
"for fn, CanComputeCppFroidurePin, finite mult. elt list, and record",
[IsFunction,
 IsSemigroup and CanComputeCppFroidurePin,
 IsMultiplicativeElementCollection and IsFinite and IsList,
 IsRecord],
function(Constructor, S, coll, opts)
  local n, R, M, N, CppT, add_generator, generator, T, x, i;

  # opts must be copied and processed before calling this function
  # coll must be copied before calling this function
  if ForAll(coll, x -> x in S) then
    # To avoid copying unless necessary!
    return S;
  fi;

  coll := Shuffle(coll);
  if IsGeneratorsOfActingSemigroup(coll) then
    n := ActionDegree(coll);
    Sort(coll, function(x, y)
                 return ActionRank(x, n) > ActionRank(y, n);
               end);
  elif Length(coll) < 120 then
    Sort(coll, IsGreensDGreaterThanFunc(Semigroup(coll)));
  fi;

  R := FroidurePinMemFnRec(S);

  # Perform the closure
  if IsPartialPermSemigroup(S) or IsTransformationSemigroup(S) then
    if IsPartialPermSemigroup(S) then
      M := Maximum(DegreeOfPartialPermCollection(coll),
                   CodegreeOfPartialPermCollection(coll));
      N := Maximum(DegreeOfPartialPermSemigroup(S),
                   CodegreeOfPartialPermSemigroup(S));
    else
      M := DegreeOfTransformationCollection(coll);
      N := DegreeOfTransformationSemigroup(S);
    fi;
    if M > N then
      # Can't use closure, TODO use copy_closure
      # FIXME if M goes larger than the type of R can support this will end
      # badly
      CppT  := R.make([]);
      add_generator := R.add_generator;
      for x in GeneratorsOfSemigroup(S) do
        add_generator(CppT, [x, M]);
      od;
      R.closure(CppT, List(coll, x -> [x, M]));
    else
      CppT := R.copy([CppFroidurePin(S)]);
      R.closure(CppT, List(coll, x -> [x, N]));
    fi;
  else
    CppT := R.copy([CppFroidurePin(S)]);
    R.closure(CppT, coll);
  fi;

  generator := R.generator;

  # Recover the new generating set from the closure
  coll := EmptyPlist(R.number_of_generators(CppT));
  for i in [1 .. R.number_of_generators(CppT)] do
    Add(coll, generator(CppT, i - 1));
  od;
  # Construct new semigroup so as to reset all attributes
  T := Constructor(coll, opts);
  T!.CppFroidurePin := CppT;
  return T;
end);

########################################################################
## New in v4
########################################################################

InstallMethod(RulesOfSemigroup,
"for a semigroup with CanComputeCppFroidurePin",
[IsSemigroup and CanComputeCppFroidurePin],
function(S)
  Enumerate(S);
  return FroidurePinMemFnRec(S).rules(CppFroidurePin(S)) + 1;
end);

InstallMethod(IdempotentsSubset,
"for a semigroup with CanComputeCppFroidurePin and hom. list",
[IsSemigroup and CanComputeCppFroidurePin, IsHomogeneousList],
function(S, list)
  local is_idempotent, T;
  is_idempotent := FroidurePinMemFnRec(S).is_idempotent;
  T := CppFroidurePin(S);
  return Filtered(list, x -> is_idempotent(T, x - 1));
end);
