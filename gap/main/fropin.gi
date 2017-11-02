###########################################################################
##
#W  fropin.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
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

InstallTrueMethod(IsEnumerableSemigroupRep,
IsSemigroup and IsGeneratorsOfEnumerableSemigroup);

# This is optional, but it is useful in several places, for example, to be able
# to use MinimalFactorization with a perm group.
InstallTrueMethod(IsEnumerableSemigroupRep, IsGroup and IsFinite);

# This should be removed ultimately, but is included now because there are too
# few methods for fp semigroup and monoids at present.
InstallTrueMethod(IsEnumerableSemigroupRep, IsFpSemigroup and IsFinite);
InstallTrueMethod(IsEnumerableSemigroupRep, IsFpMonoid and IsFinite);

InstallTrueMethod(IsEnumerableSemigroupRep,
IsReesMatrixSubsemigroup and IsGeneratorsOfEnumerableSemigroup);

InstallTrueMethod(IsEnumerableSemigroupRep,
IsReesZeroMatrixSubsemigroup and IsGeneratorsOfEnumerableSemigroup);

InstallTrueMethod(IsEnumerableSemigroupRep,
                  IsQuotientSemigroup and IsGeneratorsOfEnumerableSemigroup);

# Methods for IsGeneratorsOfEnumerableSemigroup
InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsGeneratorsOfActingSemigroup);

InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsBipartitionCollection);
InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsTransformationCollection);
InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsPartialPermCollection);
InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsMatrixOverFiniteFieldCollection);

InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsPBRCollection);

InstallTrueMethod(IsGeneratorsOfEnumerableSemigroup,
                  IsGraphInverseSubsemigroup and IsFinite);

InstallMethod(IsGeneratorsOfEnumerableSemigroup,
"for a matrix over semiring collection", [IsMatrixOverSemiringCollection],
IsGeneratorsOfSemigroup);

# The HasRows and HasColumns is currently essential due to some problems in the
# Rees(Zero)MatrixSemigroup code.

InstallImmediateMethod(IsGeneratorsOfEnumerableSemigroup,
IsReesZeroMatrixSubsemigroup and HasRows and HasColumns, 0,
function(R)
  return IsGeneratorsOfEnumerableSemigroup([Representative(R)]);
end);

InstallMethod(IsGeneratorsOfEnumerableSemigroup,
"for a Rees 0-matrix semigroup element collection",
[IsReesZeroMatrixSemigroupElementCollection],
function(coll)
  local R;
  R := ReesMatrixSemigroupOfFamily(FamilyObj(Representative(coll)));
  return IsPermGroup(UnderlyingSemigroup(R))
    or IsEnumerableSemigroupRep(UnderlyingSemigroup(R));
end);

# The HasRows and HasColumns is currently essential due to some problems in the
# Rees(Zero)MatrixSemigroup code.

InstallImmediateMethod(IsGeneratorsOfEnumerableSemigroup,
IsReesMatrixSubsemigroup and HasRows and HasColumns, 0,
function(R)
  return IsGeneratorsOfEnumerableSemigroup([Representative(R)]);
end);

InstallMethod(IsGeneratorsOfEnumerableSemigroup,
"for a Rees matrix semigroup element collection",
[IsReesMatrixSemigroupElementCollection],
function(coll)
  local R;
  R := ReesMatrixSemigroupOfFamily(FamilyObj(Representative(coll)));
  return IsPermGroup(UnderlyingSemigroup(R))
    or IsEnumerableSemigroupRep(UnderlyingSemigroup(R));
end);

InstallImmediateMethod(IsGeneratorsOfEnumerableSemigroup,
IsQuotientSemigroup and HasQuotientSemigroupPreimage, 0,
function(S)
  return IsGeneratorsOfEnumerableSemigroup(QuotientSemigroupPreimage(S));
end);

# The value 4 in the next method could be 5, but then the Iterator method for
# FreeBand(4) is very slow because it involves running the Froidure-Pin
# algorithm on FreeBand(4) to determine the data structure for
# GreensDRelation(FreeBand(4)) because the D-classes of a free band are used in
# the Iterator method, and IsEnumerableSemigroupRep's enumerate the semigroup
# fully in the method for GreensDRelation, but the fully enumerated semigroup
# is not required for the Iterator method (it does something completely
# different), and so this is a waste of effort. Basically the only reason to
# include free bands in IsEnumerableSemigroupRep is that they do not have
# enough methods installed, and so we just use this for now.

# FIXME Remove this in the future.

InstallMethod(IsGeneratorsOfEnumerableSemigroup,
"for a free band element collection",
[IsFreeBandElementCollection],
function(coll)
  return Length(ContentOfFreeBandElementCollection(coll)) < 4;
end);

InstallMethod(IsGeneratorsOfEnumerableSemigroup,
"for a multiplicative element collection",
[IsMultiplicativeElementCollection], ReturnFalse);

# This function is used to initialise the data record for an enumerable
# semigroup which does not have a C++ implementation.

BindGlobal("FROPIN",
function(S)
  local data, hashlen, nrgens, nr, val, i;
  if (not IsEnumerableSemigroupRep(S))
      or Length(GeneratorsOfSemigroup(S)) = 0 then
    ErrorNoReturn("Semigroups: FROPIN: usage,\n",
                  "the argument must be a semigroup with at least 1 ",
                  "generator,");
  elif IsBound(S!.__en_semi_fropin) then
    return S!.__en_semi_fropin;
  fi;

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

  data.report     := SEMIGROUPS.OptionsRec(S).report;
  data.batch_size := SEMIGROUPS.OptionsRec(S).batch_size;
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
    if val = fail then # new generator
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
    else # duplicate generator
      data.genslookup[i] := val;
      data.nrrules := data.nrrules + 1;
      data.rules[data.nrrules] := [[i], [val]];
    fi;
  od;

  data.nr := nr;
  S!.__en_semi_fropin := data;
  return data;
end);

#############################################################################
# 1. Internal methods
#############################################################################

# This is a fallback method in case we don't know any better way to check this

InstallMethod(IsFinite, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  return EN_SEMI_SIZE(S) < infinity;
end);

InstallMethod(AsSet, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: AsSet: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  fi;
  return EN_SEMI_AS_SET(S);
end);

InstallMethod(EnumeratorSorted,
"for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  local enum;

  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: EnumeratorSorted: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  elif Length(GeneratorsOfSemigroup(S)) = 0
      or not (IsTransformationSemigroup(S)
              or IsPartialPermSemigroup(S)
              or IsBipartitionSemigroup(S)
              or IsBooleanMatSemigroup(S)
              or IsPBRSemigroup(S)
              or IsMatrixOverSemiringSemigroup(S)) then
     # This method only works for semigroups to which the libsemigroups
     # code applies
    TryNextMethod();
  fi;

  enum := rec();

  enum.NumberElement := function(enum, x)
    return EN_SEMI_POSITION_SORTED(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    return EN_SEMI_ELEMENT_NUMBER_SORTED(S, nr);
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

InstallMethod(IteratorSorted,
"for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup], 8,
# to beat the method for transformation semigroups, FIXME
function(S)
  local iter;
  if HasAsSSortedList(S) then
    return IteratorList(AsSSortedList(S));
  fi;

  iter        := rec();
  iter.pos    := 0;
  iter.parent := S;

  iter.NextIterator   := EN_SEMI_NEXT_ITERATOR_SORTED;
  iter.IsDoneIterator := EN_SEMI_IS_DONE_ITERATOR;

  iter.ShallowCopy := function(iter)
    return rec(pos := 0, parent := iter!.parent);
  end;

  return IteratorByFunctions(iter);
end);

InstallMethod(AsList, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup], AsListCanonical);

InstallMethod(AsListCanonical,
"for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: AsListCanonical: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  fi;
  return EN_SEMI_AS_LIST(S);
end);

# For ideals and other generatorless enumerable semigroups

InstallMethod(AsListCanonical, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  GeneratorsOfSemigroup(S);
  return AsListCanonical(S);
end);

InstallMethod(Enumerator, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup], 2,
EnumeratorCanonical);

InstallMethod(EnumeratorCanonical,
"for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup], 2,
# to beat the method for a Rees matrix semigroup, FIXME!!
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
    return EN_SEMI_ELEMENT_NUMBER(S, nr);
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.Length := function(enum)
    if not IsFinite(S) then
      return infinity;
    else
      return EN_SEMI_SIZE(S);
    fi;
  end;

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

# The next method is necessary since it does not necessarily involve
# enumerating the entire semigroup in the case that the semigroup is partially
# enumerated and <list> only contains indices that are within the so far
# enumerated range. The default methods in the library do, because they require
# the length of the enumerator to check that the list of positions is valid.

InstallMethod(ELMS_LIST, "for a semigroup enumerator and a list",
[IsSemigroupEnumerator, IsList],
function(enum, list)
  return EN_SEMI_ELMS_LIST(UnderlyingCollection(enum), list);
end);

InstallMethod(Iterator, "for semigroup enumerator sorted",
[IsSemigroupEnumerator and IsSSortedList],
function(enum)
  return IteratorSorted(UnderlyingCollection(enum));
end);

InstallMethod(Iterator, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
2, # to beat the method for a Rees matrix semigroup, FIXME!!
IteratorCanonical);

InstallMethod(IteratorCanonical,
"for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  local iter;

  if HasAsListCanonical(S) then
    return IteratorList(AsListCanonical(S));
  fi;

  iter        := rec();
  iter.pos    := 0;
  iter.parent := S;

  iter.NextIterator   := EN_SEMI_NEXT_ITERATOR;
  iter.IsDoneIterator := EN_SEMI_IS_DONE_ITERATOR;

  iter.ShallowCopy := function(iter)
    return rec(pos := 0, parent := S);
  end;

  return IteratorByFunctions(iter);
end);

InstallMethod(Iterator, "for semigroup enumerator",
[IsSemigroupEnumerator],
function(enum)
  return Iterator(UnderlyingCollection(enum));
end);

# different method for ideals

InstallMethod(Size, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  if not IsFinite(S) then
    return infinity;
  fi;
  return EN_SEMI_SIZE(S);
end);

# different method for ideals

InstallMethod(\in,
"for multiplicative element and an enumerable semigroup with known generators",
[IsMultiplicativeElement,
 IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(x, S)
  return PositionCanonical(S, x) <> fail;
end);

# different method for ideals

InstallMethod(Idempotents, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  return EN_SEMI_IDEMPOTENTS(S);
end);

InstallMethod(PositionCanonical,
"for an enumerable semigroup with known generators and multiplicative element",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  if FamilyObj(x) <> ElementsFamily(FamilyObj(S))
      or (IsTransformation(x)
          and DegreeOfTransformation(x) > DegreeOfTransformationSemigroup(S))
      or (IsPartialPerm(x)
          and DegreeOfPartialPerm(x) > DegreeOfPartialPermSemigroup(S)) then
    return fail;
  fi;

  return EN_SEMI_POSITION(S, x);
end);

# Position exists so that we can call it on objects with an uninitialised data
# structure, without first having to initialise the data structure to realise
# that <x> is not in it.

# This returns the current position of x, if it is already known to belong to
# S.

InstallMethod(Position,
"for an enumerable semigroup, mult. element, zero cyc",
[IsEnumerableSemigroupRep, IsMultiplicativeElement, IsZeroCyc],
function(S, x, n)
  return PositionOp(S, x, n);
end);

InstallMethod(PositionOp,
"for an enumerable semigroup, multi. element, zero cyc",
[IsEnumerableSemigroupRep, IsMultiplicativeElement, IsZeroCyc],
function(S, x, n)

  if FamilyObj(x) <> ElementsFamily(FamilyObj(S))
      or (IsTransformation(x)
          and DegreeOfTransformation(x) > DegreeOfTransformationSemigroup(S))
      or (IsPartialPerm(x)
          and DegreeOfPartialPerm(x) > DegreeOfPartialPermSemigroup(S)) then
    return fail;
  fi;

  return EN_SEMI_CURRENT_POSITION(S, x);
end);

InstallMethod(PositionSortedOp,
"for an enumerable semigroup with known generators and multiplicative element",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup,
 IsMultiplicativeElement],
function(S, x)
  if FamilyObj(x) <> ElementsFamily(FamilyObj(S))
      or (IsTransformation(x)
          and DegreeOfTransformation(x) > DegreeOfTransformationSemigroup(S))
      or (IsPartialPerm(x)
          and DegreeOfPartialPerm(x) > DegreeOfPartialPermSemigroup(S)) then
    return fail;
  elif not IsFinite(S) then
    ErrorNoReturn("Semigroups: PositionSortedOp: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  fi;

  return EN_SEMI_POSITION_SORTED(S, x);
end);

InstallMethod(IsFullyEnumerated, "for an enumerable semigroup",
[IsEnumerableSemigroupRep], EN_SEMI_IS_DONE);

InstallMethod(Display, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)

  Print("<");
  if EN_SEMI_IS_DONE(S) then
    Print("fully ");
  else
    Print("partially ");
  fi;

  Print("enumerated semigroup with ", EN_SEMI_CURRENT_SIZE(S));
  Print(" elements, ", EN_SEMI_CURRENT_NR_RULES(S), " rules, ");
  Print("max word length ", EN_SEMI_CURRENT_MAX_WORD_LENGTH(S), ">");
  return;
end);

# the main algorithm

InstallMethod(Enumerate,
"for an enumerable semigroup with known generators and pos int",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup, IsInt],
EN_SEMI_ENUMERATE);

InstallMethod(Enumerate, "for an enumerable semigroup with known generators",
[IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup],
function(S)
  return Enumerate(S, -1);
end);

# same method for ideals

InstallMethod(RightCayleyGraphSemigroup, "for an enumerable semigroup rep",
[IsEnumerableSemigroupRep], 3,
function(S)
  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: RightCayleyGraphSemigroup: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  fi;
  return EN_SEMI_RIGHT_CAYLEY_GRAPH(S);
end);

# same method for ideals

InstallMethod(LeftCayleyGraphSemigroup,
"for an enumerable semigroup rep",
[IsEnumerableSemigroupRep], 3,
function(S)
  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: LeftCayleyGraphSemigroup: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  fi;
  return EN_SEMI_LEFT_CAYLEY_GRAPH(S);
end);

InstallMethod(MultiplicationTable, "for an enumerable semigroup",
[IsEnumerableSemigroupRep],
function(S)
  local tab;
  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: MultiplicationTable: usage,\n",
                  "the first argument (a semigroup) must be finite,");
  fi;
  tab := EN_SEMI_CAYLEY_TABLE(S);
  if tab <> fail then
    return tab;
  fi;
  TryNextMethod();
end);

InstallMethod(NrIdempotents, "for an enumerable semigroup rep",
[IsEnumerableSemigroupRep],
function(S)
  if not IsFinite(S) then
    TryNextMethod();
  elif HasIdempotents(S) then
    return Length(Idempotents(S));
  fi;

  return EN_SEMI_NR_IDEMPOTENTS(S);
end);

InstallMethod(MinimalFactorization,
"for an enumerable semigroup and a multiplicative element",
[IsEnumerableSemigroupRep, IsMultiplicativeElement],
function(S, x)
  if not x in S then
    ErrorNoReturn("Semigroups: MinimalFactorization:\n",
                  "the second argument <x> is not an element ",
                  "of the first argument <S>,");
  fi;
  return EN_SEMI_FACTORIZATION(S, PositionCanonical(S, x));
end);
