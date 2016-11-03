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

# This function is used to initialise the data record for an enumerable
# semigroup which does not have a C++ implementation.

BindGlobal("INIT_FROPIN",
function(S)
  local data, hashlen, nrgens, nr, val, i;

  if (not IsSemigroup(S)) or Length(GeneratorsOfSemigroup(S)) = 0 then
    ErrorNoReturn("Semigroups: INIT_FROPIN: usage,\n",
                  "the argument must be a semigroup with at least 1 ",
                  "generator");
  elif IsBound(S!.__en_semi_frp_data) then
    return S!.__en_semi_frp_data;
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
  hashlen         := SEMIGROUPS.OptionsRec(S).hashlen.L;

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
  S!.__en_semi_frp_data := data;
  return data;
end);

InstallMethod(IsEnumerableSemigroup, "for a semigroup", [IsSemigroup],
ReturnFalse);

#############################################################################
# 1. Internal methods
#############################################################################

InstallMethod(AsSet, "for a generic semigroup with generators",
[IsEnumerableSemigroup and HasGeneratorsOfSemigroup], EN_SEMI_AS_SET);

InstallMethod(EnumeratorSorted, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local enum;

  if HasAsSSortedList(S) then
    return AsSSortedList(S);
  elif Length(GeneratorsOfSemigroup(S)) = 0
      or not (IsTransformationSemigroup(S)
              or IsPartialPermSemigroup(S)
              or IsBipartitionSemigroup(S)
              or IsBooleanMatSemigroup(S)
              or IsPBRSemigroup(S)
              or IsMatrixOverSemiringSemigroup(S)) then
    TryNextMethod();
  fi;

  enum := rec();

  enum.NumberElement := function(enum, x)
    return EN_SEMI_POSITION_SORTED(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    return EN_SEMI_ELEMENT_NUMBER_SORTED(S, nr);
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.Length := enum -> EN_SEMI_SIZE(S);

  enum.Membership := function(enum, x)
    return Position(S, x) <> fail;
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.IsBound\[\] := function(enum, nr)
    return nr <= EN_SEMI_SIZE(S);
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

InstallMethod(AsList, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], AsListCanonical);

InstallMethod(AsListCanonical, "for a semigroup", [IsSemigroup],
EN_SEMI_AS_LIST);

InstallMethod(Enumerator, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 2,
function(S)
  if HasAsList(S) then
    return AsList(S);
  elif Length(GeneratorsOfSemigroup(S)) = 0 then
    TryNextMethod();
  fi;
  return EnumeratorCanonical(S);
end);

InstallMethod(EnumeratorCanonical, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 2,
# to beat the generic method for a Rees matrix semigroup, FIXME!!
function(S)
  local enum;

  if HasAsListCanonical(S) then
    return AsListCanonical(S);
  elif Length(GeneratorsOfSemigroup(S)) = 0 then
    TryNextMethod();
  fi;

  enum := rec();

  enum.NumberElement := function(enum, x)
    return Position(S, x);
  end;

  enum.ElementNumber := function(enum, nr)
    return EN_SEMI_ELEMENT_NUMBER(S, nr);
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.Length := enum -> EN_SEMI_SIZE(S);

  enum.AsList := function(enum)
    return AsListCanonical(S);
  end;

  enum.Membership := function(x, enum)
    return Position(S, x) <> fail;
  end;

  # FIXME this should be Size(S) hack around RZMS
  enum.IsBound\[\] := function(enum, nr)
    return nr <= EN_SEMI_SIZE(S);
  end;

  enum := EnumeratorByFunctions(S, enum);
  SetIsSemigroupEnumerator(enum, true);
  return enum;
end);

InstallMethod(ELMS_LIST, "for an enumerator of a semigroup",
[IsSemigroupEnumerator, IsList],
function(enum, list)
  local out, y, x;

  out := [];
  for x in list do
    y := enum[x];
    if y <> fail then
      Add(out, y);
    else
      ErrorNoReturn("Semigroups: ELMS_LIST: List Elements, <list>[", x,
                    "] must have an assigned value,");
    fi;
  od;
  return out;
end);

InstallMethod(Iterator, "for semigroup enumerator sorted",
[IsSemigroupEnumerator and IsSSortedList],
function(enum)
  return IteratorSorted(UnderlyingCollection(enum));
end);

InstallMethod(Iterator, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
2, # to beat the generic method for a Rees matrix semigroup, FIXME!!
IteratorCanonical);

InstallMethod(IteratorCanonical, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
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

InstallMethod(Size, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], EN_SEMI_SIZE);

# different method for ideals

InstallMethod(\in,
"for a multiplicative element and finite semigroup with generators",
[IsMultiplicativeElement, IsSemigroup and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(S, x) <> fail;
end);

# different method for ideals

InstallMethod(Idempotents, "for a generic semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local elts, idempotents, nr, i;

  Enumerate(S);
  # FIXME there should be a C method for this
  if not IsBound(S!.idempotents) then
    elts := AsList(S);
    idempotents := [];
    nr := 0;

    for i in [1 .. Length(elts)] do
      if elts[i] * elts[i] = elts[i] then
        nr := nr + 1;
        idempotents[nr] := i;
      fi;
    od;

    S!.idempotents := idempotents; # TODO this should be in the fropin data
  fi;

  return AsList(S){S!.idempotents};
end);

InstallMethod(Position, "for enumerable semigroup and multiplicative element",
[IsSemigroup, IsMultiplicativeElement],
function(S, x)
  return PositionOp(S, x, 0);
end);

InstallMethod(Position,
"for enumerable semigroup, multiplicative element, and zero cyc",
[IsSemigroup, IsMultiplicativeElement, IsZeroCyc],
function(S, x, n)
  return PositionOp(S, x, n);
end);

InstallMethod(PositionOp,
"for enumerable semigroup, multiplicative element, and zero cyc",
[IsSemigroup, IsMultiplicativeElement, IsZeroCyc],
function(S, x, n)
  if FamilyObj(x) <> ElementsFamily(FamilyObj(S)) then
    return fail;
  fi;

  if (IsTransformation(x)
      and DegreeOfTransformation(x) > DegreeOfTransformationSemigroup(S))
      or (IsPartialPerm(x)
          and DegreeOfPartialPerm(x) > DegreeOfPartialPermSemigroup(S)) then
    return fail;
  fi;

  return EN_SEMI_POSITION(S, x);
end);

InstallMethod(PositionSortedOp,
"for a semigroup and object",
[IsSemigroup, IsMultiplicativeElement],
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
  return EN_SEMI_POSITION_SORTED(S, x);
end);

InstallMethod(Display, [IsSemigroup],
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

InstallMethod(Enumerate, "for enumerable semigroup and pos int",
[IsSemigroup, IsPosInt], EN_SEMI_ENUMERATE);

InstallMethod(Enumerate, "for enumerable semigroup",
[IsSemigroup],
function(S)
  return Enumerate(S, 1152921504606846975);
end);

if not IsBound(EN_SEMI_ENUMERATE) then
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
