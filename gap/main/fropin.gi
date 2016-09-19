###########################################################################
##
#W  froidure-pin.gi
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

  enum.Membership := function(x, enum)
    return Position(GenericSemigroupData(S), x) <> fail;
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
"for a multiplicative element and finite semigroup with generators",
[IsMultiplicativeElement, IsSemigroup and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(GenericSemigroupData(S), x) <> fail;
end);

# this has different meaning than Position in the data

InstallMethod(\in,
"for a multiplicative element and generic semigroup data",
[IsMultiplicativeElement, IsGenericSemigroupData],
function(x, data)
  return SEMIGROUP_POSITION_CURRENT(data, x) <> fail;
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

InstallMethod(Position,
"for generic semigroup data, a multiplicative element, zero cyc",
[IsGenericSemigroupData, IsMultiplicativeElement, IsZeroCyc],
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
"for a semigroup with generators, and object",
[IsSemigroup and HasGeneratorsOfSemigroup, IsMultiplicativeElement],
function(S, x)
  local gens;

  if not (IsAssociativeElement(x) or IsMatrixOverSemiring(x))
      or FamilyObj(x) <> ElementsFamily(FamilyObj(S)) then
    return fail;
  fi;

  gens := GeneratorsOfSemigroup(S);

  if (IsTransformation(x)
      and DegreeOfTransformation(x) >
      DegreeOfTransformationCollection(gens))
      or
      (IsPartialPerm(x)
       and DegreeOfPartialPerm(x) >
       DegreeOfPartialPermCollection(gens)) then
    return fail;
  fi;
  if SEMIGROUPS.IsCCSemigroup(S) then
    return SEMIGROUP_POSITION_SORTED(GenericSemigroupData(S), x);
  fi;
  return Position(AsSet(S), x);
end);

InstallMethod(Length, "for generic semigroup data", [IsGenericSemigroupData],
SEMIGROUP_CURRENT_SIZE);

# FIXME remove this?

InstallMethod(ELM_LIST, "for generic semigroup data, and pos int",
[IsGenericSemigroupData, IsPosInt],
function(data, nr)
  return SEMIGROUP_ELEMENT_NUMBER(data, nr);
end);

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
    data.genstoapply := [1 .. Length(GeneratorsOfSemigroup(S))];
    data.batch_size  := SEMIGROUPS.OptionsRec(S).batch_size;
    data.gens        := ShallowCopy(GeneratorsOfSemigroup(S));
    data.degree      := SEMIGROUPS.DegreeOfSemigroup(S);
    data.nr_threads  := SEMIGROUPS.OptionsRec(S).nr_threads;
    data.report      := SEMIGROUPS.OptionsRec(S).report;

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

  data.report     := SEMIGROUPS.OptionsRec(S).report;
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

InstallMethod(Enumerate, "for generic semigroup data and cyclotomic",
[IsGenericSemigroupData, IsCyclotomic],
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# <lookfunc> has arguments <data=S!.semigroupe> and an index <j> in
# <[1..Length(data!.elts)]>.

if IsBound(SEMIGROUP_ENUMERATE) then
  # <lookfunc> has arguments <data=S!.semigroupe> and an index <j> in
  # <[1..Length(data!.elts)]>.
  InstallMethod(Enumerate, "for generic semigroup data, cyclotomic, function",
  [IsGenericSemigroupData, IsCyclotomic, IsFunction],
  function(data, limit, lookfunc)
    data := SEMIGROUP_ENUMERATE(data, limit, lookfunc, lookfunc <> ReturnFalse);
    return data;
  end);
else
  InstallMethod(Enumerate, "for generic semigroup data, cyclotomic, function",
  [IsGenericSemigroupData, IsCyclotomic, IsFunction],
  function(data, limit, lookfunc)
    local looking, found, i, nr, len, one, stopper, nrrules, elts, gens,
    nrgens, genstoapply, genslookup, lenindex, first, final, prefix, suffix,
    words, right, left, reduced, ht, rules, htadd, htvalue, stop, lentoapply,
    b, s, r, new, newword, val, p, j, k;

    if lookfunc <> ReturnFalse then
      looking := true;
      # only applied to new elements, not old ones!!!
      data!.found := false;
      # in case we previously looked for something and found it
    else
      looking := false;
    fi;

    found := false;

    i := data!.pos;
    # current position we are about to apply gens to .. .
    nr := data!.nr;
    # number of elements found so far .. .

    if i > nr then
      SetFilterObj(data, IsClosedData);
      return data;
    fi;

    len := data!.len;
    # current word length
    one := data!.one;
    # < elts[one] > is the mult. neutral element
    stopper := data!.stopper;
    # stop when we have applied generators to elts[stopper]
    nrrules := data!.nrrules;
    # Length(rules)

    elts := data!.elts;
    # the so far enumerated elements
    gens := data!.gens;
    # the generators
    nrgens := Length(gens);
    genstoapply := data!.genstoapply;
    # list of indices of generators to apply in inner loop
    genslookup := data!.genslookup;
    # genslookup[i] = Position(elts, gens[i])
    # this is not always < i + 1 > !
    lenindex := data!.lenindex;
    # lenindex[len] = position in < words > and < elts > of
    # first element of length < len >
    first := data!.first;
    # elts[i] = gens[first[i]] * elts[suffix[i]], first letter
    final := data!.final;
    # elts[i] = elts[prefix[i]] * gens[final[i]]
    prefix := data!.prefix;
    # see final, 0 if prefix is empty i.e. elts[i] is a gen
    suffix := data!.suffix;
    # see first, 0 if suffix is empty i.e. elts[i] is a gen
    words := data!.words;
    # words[i] is a word in the gens equal to elts[i]

    right := data!.right;
    # elts[right[i][j]] = elts[i] * gens[j], right Cayley graph
    left := data!.left;
    # elts[left[i][j]] = gens[j] * elts[i], left Cayley graph
    reduced := data!.reduced;
    # words[right[i][j]] is reduced if reduced[i][j] = true
    ht := data!.ht;
    # HTValue(ht, x) = Position(elts, x)
    rules := data!.rules;
    # the relations

    if IsBoundGlobal("ORBC") then
      htadd := HTAdd_TreeHash_C;
      htvalue := HTValue_TreeHash_C;
    else
      htadd := HTAdd;
      htvalue := HTValue;
    fi;

    stop := false;

    while i <= nr and not stop do
      lentoapply := [1 .. len];
      while i <= nr and Length(words[i]) = len and not stop do
        b := first[i];
        s := suffix[i];
        # elts[i] = gens[b] * elts[s]
        for j in genstoapply do # consider < elts[i] * gens[j] >
          if s <> 0 and not reduced[s][j] then
            # < elts[s] * gens[j] > is not reduced
            r := right[s][j];
            # elts[r] = elts[s] * gens[j]
            if prefix[r] <> 0 then
              right[i][j] := right[left[prefix[r]][b]][final[r]];
              # elts[i] * gens[j] = gens[b] * elts[prefix[r]] * gens[final[r]];
              # reduced[i][j] = ([words[i], j] = words[right[i][j]])
              reduced[i][j] := false;
            elif r = one then
              # < elts[r] > is the identity
              right[i][j] := genslookup[b];
              reduced[i][j] := true;
              # < elts[i] * gens[j] = b > and it is reduced
            else # prefix[r] = 0, i.e. elts[r] is one of the generators
              right[i][j] := right[genslookup[b]][final[r]];
              # elts[i] * gens[j] = gens[b] * gens[final[r]];
              # reduced[i][j] = ([words[i], j] = words[right[i][j]])
              reduced[i][j] := false;
            fi;
          else # < elts[s] * gens[j] > is reduced
            new := elts[i] * gens[j];
            # < newword >= < elts[i] * gens[j] >
            newword := words[i]{lentoapply};
            # better than ShallowCopy
            newword[len + 1] := j;
            # using Concatenate here is very bad!
            val := htvalue(ht, new);

            if val <> fail then
              nrrules := nrrules + 1;
              rules[nrrules] := [newword, words[val]];
              right[i][j] := val;
              # < newword > and < words[val] > represent the same element (but
              # are not equal) and so < newword > is not reduced

            else # < new > is a new element!
              nr := nr + 1;
              htadd(ht, new, nr);

              if one = false
                  and ForAll(gens, y -> new * y = y and y * new = y) then
                one := nr;
              fi;

              if s <> 0 then
                suffix[nr] := right[s][j];
              else
                suffix[nr] := genslookup[j];
              fi;

              elts[nr] := new;
              words[nr] := newword;
              first[nr] := b;
              final[nr] := j;
              prefix[nr] := i;
              right[nr] := EmptyPlist(nrgens);
              left[nr] := EmptyPlist(nrgens);
              reduced[nr] := BlistList(genstoapply, []);

              right[i][j] := nr;
              reduced[i][j] := true;

              if looking and (not found) and lookfunc(data, nr) then
                found := true;
                stop := true;
                data!.found := nr;
              else
                stop := nr >= limit;
              fi;
            fi;
          fi;
        od;
        # finished applying gens to < elts[i] >
        stop := (stop or i = stopper);
        i := i + 1;
      od;
      # finished words of length < len > or < looking and found >
      if i > nr or Length(words[i]) <> len then
        # process words of length < len > into < left >
        if len > 1 then
          for j in [lenindex[len] .. i - 1] do
            # loop over all words of length < len - 1 >
            p := prefix[j];
            b := final[j];
            for k in genstoapply do
              left[j][k] := right[left[p][k]][b];
              # gens[k] * elts[j] = (gens[k] * elts[p]) * gens[b]
            od;
          od;
        elif len = 1 then
          for j in [lenindex[len] .. i - 1] do
            # loop over all words of length < 1 >
            b := final[j];
            for k in genstoapply do
              left[j][k] := right[genslookup[k]][b];
              # gens[k] * elts[j] = elts[k] * gens[b]
            od;
          od;
        fi;
        len := len + 1;
        lenindex[len] := i;
      fi;
    od;

    data!.nr := nr;
    data!.nrrules := nrrules;
    data!.one := one;
    data!.pos := i;
    data!.len := len;

    if i > nr then
      SetFilterObj(data, IsClosedData);
      # Unbind some of the unnecessary components here!
    fi;

    return data;
  end);
fi;
