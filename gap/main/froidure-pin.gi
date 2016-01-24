###########################################################################
##
#W  semigroups-generic.gi
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

#############################################################################
# 1. Internal methods
#############################################################################

InstallMethod(Iterator, "for semigroup enumerator sorted",
[IsSemigroupEnumerator and IsSSortedList],
function(enum)
  return IteratorSorted(UnderlyingCollection(enum));
end);

InstallMethod(AsSet, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  if (not IsActingSemigroup(S)) and SEMIGROUPS.IsCCSemigroup(S) then
    return SEMIGROUP_AS_SET(GenericSemigroupData(S));
  fi;
  TryNextMethod();
end);

InstallMethod(EnumeratorSorted, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local enum;

  if HasAsSSortedList(S) then
    return AsSSortedList(S);
  elif not SEMIGROUPS.IsCCSemigroup(S)
    or Length(GeneratorsOfSemigroup(S)) = 0 then
    TryNextMethod();
  fi;

  enum := rec();

  enum.NumberElement := function(enum, x)
    return PositionSorted(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    return SEMIGROUP_ELEMENT_NUMBER_SORTED(GenericSemigroupData(S), nr);
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.Length := enum -> SEMIGROUP_SIZE(GenericSemigroupData(S));

  enum.Membership := function(enum, x)
    return Position(S, x) <> fail;
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.IsBound\[\] := function(enum, nr)
    return nr <= SEMIGROUP_SIZE(GenericSemigroupData(S));
  end;

  enum := EnumeratorByFunctions(S, enum);
  SetIsSemigroupEnumerator(enum, true);
  SetIsSSortedList(enum, true);
  return enum;
end);

InstallMethod(IteratorSorted, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 8,
# to beat the generic method for transformation semigroups, FIXME
function(S)
  local iter;

  if HasAsSSortedList(S) then
    return IteratorList(AsSSortedList(S));
  fi;

  iter      := rec();
  iter.pos  := 0;
  iter.data := GenericSemigroupData(S);

  iter.NextIterator := SEMIGROUP_NEXT_ITERATOR_SORTED;
  if SEMIGROUPS.IsCCSemigroup(S) then
    iter.IsDoneIterator := SEMIGROUP_IS_DONE_ITERATOR_CC;
  else
    iter.IsDoneIterator := SEMIGROUP_IS_DONE_ITERATOR;
  fi;

  iter.ShallowCopy := function(iter)
    return rec(pos := 0, data := iter!.data);
  end;

  return IteratorByFunctions(iter);
end);

InstallMethod(AsList, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return SEMIGROUP_AS_LIST(GenericSemigroupData(S));
end);

InstallMethod(Iterator, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 2,
# to beat the generic method for a Rees matrix semigroup, FIXME!!
function(S)
  local iter;

  if HasAsList(S) then
    return IteratorList(AsList(S));
  fi;

  iter      := rec();
  iter.pos  := 0;
  iter.data := GenericSemigroupData(S);

  iter.NextIterator := SEMIGROUP_NEXT_ITERATOR;
  if SEMIGROUPS.IsCCSemigroup(S) then
    iter.IsDoneIterator := SEMIGROUP_IS_DONE_ITERATOR_CC;
  else
    iter.IsDoneIterator := SEMIGROUP_IS_DONE_ITERATOR;
  fi;

  iter.ShallowCopy := function(iter)
    return rec(pos := 0, data := iter!.data);
  end;

  return IteratorByFunctions(iter);
end);

InstallMethod(Iterator, "for semigroup enumerator",
[IsSemigroupEnumerator],
function(enum)
  return Iterator(UnderlyingCollection(enum));
end);

InstallMethod(Enumerator, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 2,
# to beat the generic method for a Rees matrix semigroup, FIXME!!
function(S)
  local enum;

  if HasAsList(S) then
    return IteratorList(AsList(S));
  elif Length(GeneratorsOfSemigroup(S)) = 0 then
    TryNextMethod();
  fi;

  enum := rec();

  enum.NumberElement := function(enum, x)
    return Position(GenericSemigroupData(S), x);
  end;

  enum.ElementNumber := function(enum, nr)
    return SEMIGROUP_ELEMENT_NUMBER(GenericSemigroupData(S), nr);
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.Length := enum -> SEMIGROUP_SIZE(GenericSemigroupData(S));

  enum.AsList := function(enum)
    return SEMIGROUP_AS_LIST(GenericSemigroupData(S));
  end;

  enum.Membership := function(enum, x)
    return Position(S, x) <> fail;
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.IsBound\[\] := function(enum, nr)
    return nr <= SEMIGROUP_SIZE(GenericSemigroupData(S));
  end;

  enum := EnumeratorByFunctions(S, enum);
  SetIsSemigroupEnumerator(enum, true);
  return enum;
end);

# different method for ideals

InstallMethod(Size, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
S -> SEMIGROUP_SIZE(GenericSemigroupData(S)));

# different method for ideals

InstallMethod(\in,
"for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsSemigroup and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(GenericSemigroupData(S), x) <> fail;
end);

# different method for ideals

InstallMethod(Idempotents, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local data, elts, idempotents, nr, i;

  data := Enumerate(GenericSemigroupData(S));

  if not IsBound(data!.idempotents) then
    elts := SEMIGROUP_AS_LIST(data);
    idempotents := EmptyPlist(Length(elts));
    nr := 0;

    for i in [1 .. Length(elts)] do
      if elts[i] * elts[i] = elts[i] then
        nr := nr + 1;
        idempotents[nr] := i;
      fi;
    od;

    data!.idempotents := idempotents;
    ShrinkAllocationPlist(idempotents);
  fi;

  return SEMIGROUP_AS_LIST(data){data!.idempotents};
end);

#

InstallMethod(Position,
"for generic semigroup data, an associative element, zero cyc",
[IsGenericSemigroupData, IsAssociativeElement, IsZeroCyc],
function(data, x, n)
  if FamilyObj(x) <> ElementsFamily(FamilyObj(data)) then
    return fail;
  fi;

  if (IsTransformation(x)
      and DegreeOfTransformation(x) >
      DegreeOfTransformationCollection(data!.gens))
      or (IsPartialPerm(x)
          and DegreeOfPartialPerm(x) >
          DegreeOfPartialPermCollection(data!.gens)) then
    return fail;
  fi;

  return SEMIGROUP_POSITION(data, x);
end);

InstallMethod(PositionSortedOp,
"for a semigroup with generators, an associative element",
[IsSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(S, x)
  local gens;

  if FamilyObj(x) <> ElementsFamily(FamilyObj(S)) then
    return fail;
  fi;

  gens := GeneratorsOfSemigroup(S);

  if (IsTransformation(x)
      and DegreeOfTransformation(x) >
      DegreeOfTransformationCollection(gens))
      or (IsPartialPerm(x)
          and DegreeOfPartialPerm(x) >
          DegreeOfPartialPermCollection(gens)) then
    return fail;
  fi;
  if SEMIGROUPS.IsCCSemigroup(S) then
    return SEMIGROUP_POSITION_SORTED(GenericSemigroupData(S), x);
  fi;
  return Position(AsSet(S), x);
end);

#

InstallMethod(Length, "for generic semigroup data", [IsGenericSemigroupData],
SEMIGROUP_CURRENT_SIZE);

# FIXME remove this?

InstallMethod(ELM_LIST, "for generic semigroup data, and pos int",
[IsGenericSemigroupData, IsPosInt],
function(data, nr)
  return data!.elts[nr];
end);

#

InstallMethod(ViewObj, [IsGenericSemigroupData],
function(data)
  Print("<");

  if SEMIGROUP_IS_DONE(data) then
    Print("closed ");
  else
    Print("open ");
  fi;

  Print("semigroup data with ", SEMIGROUP_CURRENT_SIZE(data), " elements, ");
  Print(SEMIGROUP_CURRENT_NR_RULES(data), " relations, ");
  Print("max word length ", SEMIGROUP_CURRENT_MAX_WORD_LENGTH(data), ">");
  return;
end);

#

InstallMethod(PrintObj, [IsGenericSemigroupData],
2, # to beat the method for an enumerator!
function(data)
  local recnames, com, i, nam;

  recnames := ["degree", "elts", "final", "first", "found", "gens",
               "genslookup", "genstoapply", "ht", "left", "len", "lenindex",
               "nr", "nrrules", "one", "pos", "prefix", "reduced", "right",
               "rules", "stopper", "suffix", "words", "leftscc", "rightscc",
               "leftrightscc", "hclasses", "idempotents", "Interface_CC"];

  Print("\>\>rec(\n\>\>");
  com := false;
  i := 1;
  for nam in Set(recnames) do
    if IsBound(data!.(nam)) then
      if com then
          Print("\<\<,\n\>\>");
      else
          com := true;
      fi;
      SET_PRINT_OBJ_INDEX(i);
      i := i + 1;
      Print(nam, "\< := \>");
      if nam = "ht" then
        ViewObj(data!.(nam));
      else
        PrintObj(data!.(nam));
      fi;
    fi;
  od;
  Print(" \<\<\<\<)");

  return;
end);

# same method for ideals

InstallMethod(GenericSemigroupData, "for a semigroup",
[IsSemigroup],
function(S)
  local data, hashlen, nrgens, nr, val, i;

  if Length(GeneratorsOfSemigroup(S)) = 0 then
    TryNextMethod();
  fi;

  if SEMIGROUPS.IsCCSemigroup(S) then

    data             := rec();
    data.gens        := ShallowCopy(GeneratorsOfSemigroup(S));
    data.nr          := 0;
    data.pos         := 0;
    data.degree      := SEMIGROUPS.DegreeOfSemigroup(S);
    data.report      := SEMIGROUPS.OptionsRec(S).report;
    data.batch_size  := SEMIGROUPS.OptionsRec(S).batch_size;
    data.genstoapply := [1 .. Length(GeneratorsOfSemigroup(S))];

    return Objectify(NewType(FamilyObj(S), IsGenericSemigroupData and IsMutable
                                           and IsAttributeStoringRep), data);
  elif ChooseHashFunction(Representative(S), 1) = fail then 
    TryNextMethod();
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
              prefix := [],
              reduced := [[]],
              right := [],
              rules := [],
              stopper := false,
              suffix := [],
              words := []);

  data.batch_size := SEMIGROUPS.OptionsRec(S).batch_size;
  hashlen := SEMIGROUPS.OptionsRec(S).hashlen.L;

  data.gens := ShallowCopy(GeneratorsOfSemigroup(S));
  nrgens := Length(data.gens);
  data.ht := HTCreate(data.gens[1], rec(treehashsize := hashlen));
  nr := 0;
  data.one := false;
  data.pos := 1;
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

  return Objectify(NewType(FamilyObj(S), IsGenericSemigroupData
                                         and IsMutable
                                         and IsAttributeStoringRep), data);
end);

# the main algorithm

InstallMethod(Enumerate, "for generic semigroup data",
[IsGenericSemigroupData],
data -> Enumerate(data, infinity, ReturnFalse));

#

InstallMethod(Enumerate, "for generic semigroup data and cyclotomic",
[IsGenericSemigroupData, IsCyclotomic],
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# <lookfunc> has arguments <data=S!.semigroupe> and an index <j> in
# <[1..Length(data!.elts)]>.

InstallMethod(Enumerate, "for generic semigroup data, cyclotomic, function",
[IsGenericSemigroupData, IsCyclotomic, IsFunction],
function(data, limit, lookfunc)
  data := SEMIGROUP_ENUMERATE(data, limit, lookfunc, lookfunc <> ReturnFalse);
  return data;
end);
