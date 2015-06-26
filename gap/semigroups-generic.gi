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

InstallMethod(SEMIGROUPS_IsCCSemigroup, "for a semigroup",
[IsSemigroup], 
function(S) 
  return IsTransformationSemigroup(S)
           or IsPartialPermSemigroup(S) 
           or IsBipartitionSemigroup(S)
           or IsBooleanMatSemigroup(S) 
           or IsPartitionedBinaryRelationSemigroup(S) 
           or IsMatrixOverSemiringSemigroup(S);
end);

InstallGlobalFunction(SEMIGROUPS_DegreeOfSemigroup,
function(arg) 
  local S, coll;

  S := arg[1];
  if Length(arg) = 1 then 
    coll := [Representative(S)];
  elif Length(arg) = 2 then 
    coll := arg[2];
  else
    Error();
    return;
  fi;

  if IsTransformationSemigroup(S) then
    return Maximum(DegreeOfTransformationSemigroup(S),
                   DegreeOfTransformationCollection(coll));
  elif IsPartialPermSemigroup(S) then
    return Maximum(DegreeOfPartialPermSemigroup(S), 
                   DegreeOfPartialPermCollection(coll), 
                   CodegreeOfPartialPermCollection(coll));
  elif IsMatrixOverSemiringSemigroup(S) then 
    return DimensionOfMatrixOverSemiring(Representative(S)) ^ 2;
  elif IsBipartitionSemigroup(S) then 
    return 2 * DegreeOfBipartitionSemigroup(S);
  elif IsPartitionedBinaryRelationSemigroup(S) then 
    return 2 * DegreeOfPartitionedBinaryRelationSemigroup(S);
  else
    Error();
    return;
  fi;
end);

# different method for ideals

InstallMethod(Enumerator, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 2, 
# to beat the generic method for a Rees matrix semigroup, FIXME!!
function(S)
  local enum;

  enum := rec();

  enum.NumberElement := function(enum, elt)
    return Position(GenericSemigroupData(S), elt);
  end;

  enum.ElementNumber := function(enum, nr)
    local data;
    data := GenericSemigroupData(S);
    if not IsBound(data!.elts) or not IsBound(data!.elts[nr]) then
      ELEMENTS_SEMIGROUP(data, nr);
    fi;
    return data!.elts[nr];
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.Length := enum -> SIZE_SEMIGROUP(GenericSemigroupData(S));

  enum.AsList := enum -> ELEMENTS_SEMIGROUP(GenericSemigroupData(S), infinity);

  enum.Membership := function(enum, elt)
    return Position(GenericSemigroupData(S), elt) <> fail;
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.IsBound\[\] := function(enum, nr)
    return nr <= SIZE_SEMIGROUP(GenericSemigroupData(S));
  end;

  return EnumeratorByFunctions(S, enum);
end);

# different method for ideals

InstallMethod(Size, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
S -> SIZE_SEMIGROUP(GenericSemigroupData(S)));

# different method for ideals

InstallMethod(\in,
"for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsSemigroup and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(GenericSemigroupData(S), x) <> fail;
end);

# different method for ideals
# TODO: use the same technique as used by Semigroupe

InstallMethod(Idempotents, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local data, elts, idempotents, nr, i;

  data := Enumerate(GenericSemigroupData(S));

  if not IsBound(data!.idempotents) then
    elts := ELEMENTS_SEMIGROUP(data, infinity);
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

  return ELEMENTS_SEMIGROUP(data, infinity){data!.idempotents};
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

  return POSITION_SEMIGROUP(data, x);
end);

#

InstallMethod(Length, "for generic semigroup data", [IsGenericSemigroupData],
LENGTH_SEMIGROUP);

#

InstallMethod(ELM_LIST, "for generic semigroup data, and pos int",
[IsGenericSemigroupData, IsPosInt],
function(data, nr)
  return data!.elts[nr];
end);

#

InstallMethod(ViewObj, [IsGenericSemigroupData],
function(data)
  Print("<");

  if IsClosedData(data) or IS_CLOSED_SEMIGROUP(data) then
    SetFilterObj(data, IsClosedData);
    Print("closed ");
  else
    Print("open ");
  fi;

  Print("semigroup data with ", LENGTH_SEMIGROUP(data), " elements, ");
  Print(NR_RULES_SEMIGROUP(data), " relations, ");
  Print("max word length ", MAX_WORD_LEN_SEMIGROUP(data), ">");
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

  if SEMIGROUPS_IsCCSemigroup(S) then 
    data := rec();
    data.gens := ShallowCopy(GeneratorsOfSemigroup(S));
    data.nr := 0;
    data.pos := 0;
    # the degree is the length of the std::vector required to hold the object
    data.degree := SEMIGROUPS_DegreeOfSemigroup(S);
    data.report := SEMIGROUPS_OptionsRec(S).report;
    data.batch_size := SEMIGROUPS_OptionsRec(S).batch_size;
    data.genstoapply := [1 .. Length(GeneratorsOfSemigroup(S))];
    return Objectify(NewType(FamilyObj(S), IsGenericSemigroupData and IsMutable
                                           and IsAttributeStoringRep), data);
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

  data.batch_size := SEMIGROUPS_OptionsRec(S).batch_size;
  hashlen := SEMIGROUPS_OptionsRec(S).hashlen.L;

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
  data := ENUMERATE_SEMIGROUP(data, limit, lookfunc,
                              lookfunc <> ReturnFalse);

  if IS_CLOSED_SEMIGROUP(data) then
    SetFilterObj(data, IsClosedData);
  fi;

  return data;
end);
