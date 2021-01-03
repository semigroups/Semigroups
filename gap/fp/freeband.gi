###############################################################################
##
##  freeband.gi
##  Copyright (C) 2013-15                                  Julius Jonusas
##
##  Licensing information can be foundin the README file of this package.
##
###############################################################################

# TODO: this is not really finished.

# TODO
# InstallMethod(FreeBandOfFreeBandElement,

InstallMethod(ContentOfFreeBandElement, "for a free band element",
[IsFreeBandElement],
function(w)
  return ListBlist([1 .. Length(w!.cont)], w!.cont);
end);

InstallMethod(ContentOfFreeBandElementCollection,
"for a free band element collection",
[IsFreeBandElementCollection],
function(coll)
  local n, content, w;

  n := Length(coll[1]!.cont);
  content := BlistList([1 .. n], []);

  for w in coll do
    UniteBlist(content, w!.cont);
    if SizeBlist(content) = n then
      break;
    fi;
  od;

  return ListBlist([1 .. n], content);
end);

###############################################################################
## Internal
###############################################################################

SEMIGROUPS.FreeBandElmToWord := function(elem)
  local tuple, out, first, pre_tuple, last, su_tuple, word1, word2;

  tuple := elem!.tuple;

  if elem!.word <> fail then
    return elem!.word;
  elif tuple![2] = 0 then  # tuple corresponds to one the generators
    out := [tuple![1]];
  else
    first := tuple![1];
    pre_tuple := tuple![2];  # tuple correspoding to the prefix
    last := tuple![3];
    su_tuple := tuple![4];  # tuple corresponding to the sufix

    # if first = last we only need a single letter
    if first = last then
      out := Concatenation(SEMIGROUPS.FreeBandElmToWord(pre_tuple), [first],
                           SEMIGROUPS.FreeBandElmToWord(su_tuple));
      # otherwise we need distinct letters for both first and last
    else
      word1 := Concatenation(SEMIGROUPS.FreeBandElmToWord(pre_tuple), [first]);
      word2 := Concatenation([last], SEMIGROUPS.FreeBandElmToWord(su_tuple));
      if word1 = word2 then
        out := word1;
      else
        out := Concatenation(word1, word2);
      fi;
    fi;
  fi;
  elem!.word := out;
  return out;
end;

SEMIGROUPS.HashFunctionForFreeBandElements := function(x, data)
  return ORB_HashFunctionForPlainFlatList(SEMIGROUPS.FreeBandElmToWord(x),
                                          data);
end;

InstallMethod(IsGeneratorsOfInverseSemigroup, "for a free band element coll",
[IsFreeBandElementCollection], ReturnFalse);

InstallTrueMethod(IsFinite, IsFreeBandSubsemigroup);

InstallGlobalFunction(FreeBand,
function(arg)
  local names, F, type, ngens, gens, filts, S, m;

  # Get and check the argument list, and construct names if necessary.
  if Length(arg) = 1 and IsInt(arg[1]) and 0 < arg[1] then
    names := List([1 .. arg[1]],
                  i -> Concatenation("x", String(i)));
  elif Length(arg) = 2 and IsInt(arg[1]) and 0 < arg[1]
      and IsString(arg[2]) then
    names := List([1 .. arg[1]],
                  i -> Concatenation(arg[2], String(i)));
  elif 1 <= Length(arg) and ForAll(arg, IsString) then
    names := arg;
  elif Length(arg) = 1 and IsList(arg[1])
      and ForAll(arg[1], IsString) then
    names := arg[1];
  else
    ErrorNoReturn("Semigroups: FreeBand: usage,\n",
                  "FreeBand(<name1>,<name2>..) or FreeBand(<rank> [, name]),");
  fi;

  MakeImmutable(names);

  F := NewFamily("FreeBandElementsFamily", IsFreeBandElement,
                                           CanEasilySortElements);

  type := NewType(F, IsFreeBandElement and IsPositionalObjectRep);

  ngens := Length(names);
  gens := EmptyPlist(ngens);

  for m in [1 .. ngens] do
    gens[m] := Objectify(type, rec(tuple := [m, 0, m, 0],
                                   cont := BlistList([1 .. ngens], [m]),
                                   word := [m]));
  od;

  StoreInfoFreeMagma(F, names, IsFreeBandElement);

  filts := IsFreeBandCategory and IsAttributeStoringRep and IsWholeFamily and
           IsFreeBand;

  if IsGeneratorsOfEnumerableSemigroup(gens) then
    filts := filts and IsEnumerableSemigroupRep;
  fi;

  S := Objectify(NewType(FamilyObj(gens), filts),
                 rec(opts := SEMIGROUPS.DefaultOptionsRec));

  SetGeneratorsOfMagma(S, gens);

  FamilyObj(S)!.semigroup := S;
  F!.semigroup := S;

  return S;
end);

InstallMethod(IsFreeBand, "for a semigroup",
[IsSemigroup],
function(S)
  local used, occurred, gens, max_d, g;

  if not IsBand(S) then
    return false;
  fi;

  if IsFreeBandSubsemigroup(S) then
    gens := Generators(S);
    used := BlistList([1 .. Length(gens[1]!.cont)], []);
    occurred := BlistList([1 .. Length(gens[1]!.cont)], []);
    for g in gens do
      used := IntersectionBlist(used, g!.cont);
      if g!.tuple[2] = 0 then
        occurred[g!.tuple[1]] := true;
      fi;
    od;
    if used = occurred then
      return true;
    fi;
  fi;

  if not IsActingSemigroup(S) then
    S := AsSemigroup(IsTransformationSemigroup, S);
  fi;

  max_d := MaximalDClasses(S);
  return Size(S) = Size(FreeBand(Length(max_d)));
end);

InstallMethod(Iterator, "for a Greens D-class of a free band",
[IsFreeBandElementCollection and IsGreensDClass],
function(dclass)
  local NextIterator_FreeBandDClass, NewIterator_FreeBandDClass,
  ShallowCopyLocal, record, s, content, rep,
  NextIterator_FreeBandDClassWithPrint;

  s := Parent(dclass);
  rep := Representative(dclass);
  content := rep!.cont;

  NextIterator_FreeBandDClass := function(iter)
    local output, i, content, tempcont, tuple;

    if iter!.element <> fail then
      # output := StructuralCopy(iter!.element); doesn't work
      output := rec(tuple := StructuralCopy(iter!.element!.tuple),
                    content := ShallowCopy(iter!.element!.content),
                    word := ShallowCopy(iter!.element!.word));
    else
      return fail;
    fi;

    content := iter!.content;
    tuple := iter!.element!.tuple;
    iter!.element!.word := fail;

    if tuple[2] = 0 then
      iter!.element := fail;
    elif iter!.iter1!.element <> fail then
      # Prefix word is not done yet
      tuple[2] := NextIterator_FreeBandDClass(iter!.iter1);
      iter!.element!.tuple := tuple;
    elif Position(content, true, tuple[1]) <> fail then
      # Update the first component
      i := Position(content, true, tuple[1]);
      tuple[1] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter1 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      tuple[2] := NextIterator_FreeBandDClass(iter!.iter1);
      iter!.element!.tuple := tuple;
    elif iter!.iter2!.element <> fail then
      # Suffix word is not done yet
      tuple[4] := NextIterator_FreeBandDClass(iter!.iter2);
      iter!.element!.tuple := tuple;
      # Restart the prefix
      i := Position(content, true);
      tuple[1] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter1 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      tuple[2] := NextIterator_FreeBandDClass(iter!.iter1);
      iter!.element!.tuple := tuple;
    elif Position(content, true, tuple[3]) <> fail then
      # Update the third component
      i := Position(content, true, tuple[3]);
      tuple[3] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter2 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      tuple[4] := NextIterator_FreeBandDClass(iter!.iter2);
      # Restart the prefix
      i := Position(content, true);
      tuple[1] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter1 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      tuple[2] := NextIterator_FreeBandDClass(iter!.iter1);
      iter!.element!.tuple := tuple;
    else
      iter!.element := fail;
    fi;
    return output;
  end;

  #

  NextIterator_FreeBandDClassWithPrint := function(iter)
    local next_value;

    next_value := NextIterator_FreeBandDClass(iter);

    if next_value = fail then
      return fail;
    else
      return Product(List(SEMIGROUPS.FreeBandElmToWord(next_value),
                     x -> GeneratorsOfSemigroup(iter!.semigroup)[x]));
    fi;
  end;

  #

  NewIterator_FreeBandDClass := function(s, content)
    local record, first, tempcont, elem;

    first := Position(content, true);
    elem := Objectify(TypeObj(s.1), rec(tuple := [],
                                        content := content,
                                        word := fail));

    # If the content is of size one
    if Position(content, true, first) = fail then
      elem!.tuple := [first, 0, first, 0];
      elem!.word := [first];
      record := rec(element := elem,
                    iter1 := fail,
                    iter2 := fail,
                    semigroup := s,
                    content := content) ;
    else
      tempcont := ShallowCopy(content);
      tempcont[first] := false;
      record := rec(element := elem,
                    iter1 := NewIterator_FreeBandDClass(s, tempcont),
                    iter2 := NewIterator_FreeBandDClass(s, tempcont),
                    semigroup := s,
                    content := content);
      record!.element!.tuple := [first,
                                 NextIterator_FreeBandDClass(record!.iter1),
                                 first,
                                 NextIterator_FreeBandDClass(record!.iter2)];
    fi;
    return record;
  end;

  #

  ShallowCopyLocal := record ->
    rec(last_called_by_is_done := record!.last_called_by_is_done,
        next_value := record!.next_value,
        IsDoneIterator := record!.IsDoneIterator,
        NextIterator := record!.NextIterator);

  record := NewIterator_FreeBandDClass(s, content);
  record!.NextIterator := NextIterator_FreeBandDClassWithPrint;
  record!.ShallowCopy := ShallowCopyLocal;
  return IteratorByNextIterator(record);
end);

# A free band iterator has component: content, dclass_iter.

InstallMethod(Iterator, "for a free band",
[IsFreeBandCategory],
function(s)
  local NextIterator_FreeBand, ShallowCopyLocal, record ;

  NextIterator_FreeBand := function(iter)
    local next_dclass_value, content, i, rep, dclass;

    next_dclass_value := NextIterator(iter!.dclass_iter);
    content := iter!.content;

    if next_dclass_value <> fail then
      # The current content is not done yet
      return next_dclass_value;
    elif ForAll(content, x -> x) then
      # Last content finished
      return fail;
    else
      # Change content
      for i in [1 .. Length(content)] do
        if content[i] then
          content[i] := false;
        else
          content[i] := true;
          break;
        fi;
      od;
      # Create the corresponding D-class, without actualy enumerating it.
      i := Position(content, true);
      rep := SEMIGROUPS.UniversalFakeOne;
      while i <> fail do
        rep := rep * GeneratorsOfSemigroup(s)[i];
        i := Position(content, true, i);
      od;
      dclass := GreensDClassOfElement(s, rep);
      iter!.dclass_iter := Iterator(dclass);
      return NextIterator(iter!.dclass_iter);
    fi;
  end;

  ShallowCopyLocal := record ->
    rec(last_called_by_is_done := record!.last_called_by_is_done,
        next_value := record!.next_value,
        IsDoneIterator := record!.IsDoneIterator,
        NextIterator := record!.NextIterator);

  record := rec(content :=
                BlistList([1 .. Length(GeneratorsOfSemigroup(s))], [1]),
                dclass_iter := Iterator(GreensDClassOfElement(s, s.1)));
  record!.NextIterator := NextIterator_FreeBand;
  record!.ShallowCopy := ShallowCopyLocal;
  return IteratorByNextIterator(record);
end);

# The method below does not apply to S when it is in IsEnumerableSemigroupRep
# since the rank of IsEnumerableSemigroupRep is highter than that of
# IsFreeBandCategory.

InstallMethod(GreensDClassOfElement, "for a free band and element",
[IsFreeBandCategory, IsFreeBandElement],
function(S, x)
  local type, D;
  # FIXME in the future when free bands are not in IsEnumerableSemigroupRep,
  # remove the next two lines
  if IsEnumerableSemigroupRep(S) then
    TryNextMethod();
  elif not x in S then
    ErrorNoReturn("Semigroups: GreensDClassOfElement: usage,\n",
                  "the element does not belong to the semigroup,");
  fi;

  type := NewType(FamilyObj(S), IsEquivalenceClass and
                  IsEquivalenceClassDefaultRep and IsGreensDClass);
  D := Objectify(type, rec());
  SetParent(D, S);
  SetRepresentative(D, x);
  SetEquivalenceClassRelation(D, GreensDRelation(S));

  return D;
end);

InstallMethod(ViewString, "for a free band element",
[IsFreeBandElement], PrintString);

InstallMethod(PrintString, "for a free band element",
[IsFreeBandElement],
function(elem)
  return Concatenation(List(SEMIGROUPS.FreeBandElmToWord(elem),
                            x -> FamilyObj(elem)!.names[x]));
end);

InstallMethod(ViewObj, "for a free band",
[IsFreeBandCategory],
function(S)
  if GAPInfo.ViewLength * 10 < Length(GeneratorsOfMagma(S)) then
    Print("<free band with ", Length(GeneratorsOfSemigroup(S)),
          " generators>");
  else
    Print("<free band on the generators ",
          GeneratorsOfSemigroup(S), ">");
  fi;
end);

# TODO Is there a more efficient way to compare elements? JJ

InstallMethod(\=, "for elements of a free band",
IsIdenticalObj, [IsFreeBandElement, IsFreeBandElement],
function(x, y)
  local i;

  if x!.cont <> y!.cont then
    return false;
  fi;
  for i in [1 .. 4] do
    if x!.tuple![i] <> y!.tuple![i] then
      return false;
    fi;
  od;
  return true;
end);

# TODO Is it possible to find a non recursive way to compare elements? JJ

InstallMethod(\<, "for elements of a free band",
IsIdenticalObj, [IsFreeBandElement, IsFreeBandElement],
function(x, y)
  local i, tuple1, tuple2;

  tuple1 := x!.tuple;
  tuple2 := y!.tuple;

  i := 1;
  while i <= 3 and tuple1![i] = tuple2![i] do
    i := i + 1;
  od;

  if tuple2[i] = 0  then
    return false;
  elif tuple1[i] = 0 then
    return true;
  fi;
  return tuple1[i] < tuple2[i];
end);

InstallMethod(\*, "for elements of a free band", IsIdenticalObj,
[IsFreeBandElement, IsFreeBandElement],
function(x, y)
  local type, out, diff, new_first, new_prefix, new_last, new_sufix, copy;

  type := TypeObj(x);
  # if the content of two elements is the same we only need the prefix of the
  # first and the sufix of the second one
  # cont = blist
  if IsSubsetBlist(x!.cont, y!.cont) then
    out := [x!.tuple[1], x!.tuple[2]];
  else
    diff := DifferenceBlist(y!.cont, x!.cont);
    # new_first is the last letter to occur first in the product
    new_first := y!.tuple[1];
    new_prefix := y!.tuple[2];
    while true do
      if diff[new_first] and new_prefix = 0 then
        copy := ShallowCopy(x);  # are shallow copies necessary?
        out := [new_first, copy];
        break;
      elif diff[new_first] then
        copy := ShallowCopy(x * new_prefix);
        out := [new_first, copy];
        break;
      else
        new_first := new_prefix!.tuple[1];
        new_prefix := new_prefix!.tuple[2];
      fi;
    od;
  fi;

  if IsSubsetBlist(y!.cont, x!.cont) then
    out{[3, 4]} := [y!.tuple[3], y!.tuple[4]];
  else
    diff := DifferenceBlist(x!.cont, y!.cont);
    # new_last is the first letter to occur for the last time in the product
    new_last := x!.tuple[3];
    new_sufix := x!.tuple[4];
    repeat
      if diff[new_last] and new_sufix = 0 then
        copy := ShallowCopy(y);
        out{[3 .. 4]} := [new_last, copy];
        break;
      elif diff[new_last] then
        copy := ShallowCopy(new_sufix * y);
        out{[3, 4]} := [new_last, copy];
        break;
      else
        new_last := new_sufix!.tuple[3];
        new_sufix := new_sufix!.tuple[4];
      fi;
    until IsBound(out[3]);
  fi;

  return Objectify(type, rec(tuple := out,
                             cont := UnionBlist(x!.cont, y!.cont),
                             word := fail));
end);

InstallMethod(Size, "for a free band",
[IsFreeBandCategory and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local c, output, k, i, n;

  n := Length(FamilyObj(S.1)!.names);

  c := [];
  for k in [1 .. n] do
    c[k] := 1;
    for i in [1 .. k - 1] do
      c[k] := c[k] * (k - i + 1) ^ (2 ^ i);
    od;
  od;

  output := 0;
  for k in [1 .. n] do
    output := output + Binomial(n, k) * c[k];
  od;

  return output;
end);

InstallMethod(ChooseHashFunction, "for a free band element and int",
[IsFreeBandElement, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForFreeBandElements,
             data := hashlen);
end);

SEMIGROUPS.PrefixTupleOfFreeBandElement := function(word, n)
  local lookup, distinct, L, i;
  # word is a list of pos ints. n is the content size.
  # Returns a list: first element is largest prefix of word which has content
  # size n - 1.
  # Second element is nth character to make its first appearance.

  # Start with some argument checks and easy outputs (shouldn't be used).
  if not (IsList(word) and not IsEmpty(word)) then
    ErrorNoReturn("expected a non-empty list of pos ints as first argument");
  fi;
  if not IsPosInt(n) then
    ErrorNoReturn("expected a positive integer as second argument");
  fi;
  if n > Length(word) then
    ErrorNoReturn("second argument (content size) must be smaller than length ",
                  "of first argument (word)");
  fi;
  if n = 1 then
    return [[], word[1]];
  fi;

  # If the arguments are nontrivial, start scanning the word.
  lookup          := [];
  lookup[word[1]] := 1;
  distinct        := 1;
  L               := Length(word);
  for i in [2 .. L] do
    if not IsBound(lookup[word[i]]) then
      distinct := distinct + 1;
      if distinct = n then
        # in this case the number of distinct letters has hit the max
        return [word{[1 .. i - 1]}, word[i]];
      fi;
      lookup[word[i]] := distinct;
    fi;
  od;

  # if distinct never reached n, then n was larger than content of word.
  ErrorNoReturn("second argument (content size) must be at most equal to the ",
                "number of distinct entries in the first argument");
end;

SEMIGROUPS.ShortCanonicalFormOfFreeBandElement := function(word)
  local L, lookup, distinct, l1, l2, preftup, sufftup, pref, suff,
  n1, n2, n, char, i;
  # Returns shortest representation of the free band equivalence class of
  # the input.
  if not IsList(word) then
    ErrorNoReturn("Expected a list of pos ints as input");
  fi;
  for char in word do
    if not IsPosInt(char) then
      ErrorNoReturn("Expected a list of pos ints as input");
    fi;
  od;

  # Check if word is short, in which case output is easy.
  L := Length(word);
  if L = 0 then
    return [];
  elif L = 1 then
    return word;
  fi;

  # If word is longer: first we need size of the alph.
  lookup          := [];
  lookup[word[1]] := 1;
  distinct        := 1;
  for i in [2 .. L] do
    if not IsBound(lookup[word[i]]) then
      distinct        := distinct + 1;
      lookup[word[i]] := distinct;
    fi;
  od;

  # If the  size of the alph is 1 or 2, output is easy.
  if distinct = 1 then
    # only one letter. Delete copies.
    return [word[1]];
  fi;
  if distinct = 2 then
    # then only the first and last letters matter.
    if word[1] <> word[L] then
      return [word[1], word[L]];
    fi;
    # otherwise the first and last are same. Need to know middle.
    l1 := word[1];
    for char in word do
      if char <> l1 then
        l2 := char;
        return [l1, l2, l1];
      fi;
    od;
  fi;

  # If we've made it this far then the content is at least of size 3.
  # Need to find prefix and suffix. Note suffix will be reversed in the output.
  preftup := SEMIGROUPS.PrefixTupleOfFreeBandElement(word, distinct);
  sufftup := SEMIGROUPS.PrefixTupleOfFreeBandElement(Reversed(word), distinct);

  # Turn the two tuples into the two halves of the output.
  pref := SEMIGROUPS.ShortCanonicalFormOfFreeBandElement(preftup[1]);
  Add(pref, preftup[2]);

  suff := SEMIGROUPS.ShortCanonicalFormOfFreeBandElement(sufftup[1]);
  Add(suff, sufftup[2]);
  suff := Reversed(suff);

  # Now see if any cancellations are possible between suffixes of pref and
  # prefixes of suff. Give priority to longest possible.
  n1 := Length(pref);
  n2 := Length(suff);
  n  := Minimum(n1, n2);
  for i in [0 .. n - 1] do
    if pref{[n1 - n + i + 1 .. n1]} = suff{[1 .. n - i]} then
      # a cancellation is possible. Cancel and return.
      return Concatenation(pref, suff{[n - i + 1 .. n2]});
    fi;
  od;

  # If we've made it this far then no cancellations are possible.
  return Concatenation(pref, suff);
end;

InstallMethod(ToddCoxeterBand, "for a pos int and list of lists of words",
[IsPosInt, IsList],
function(N, R)
  local new_coset, tauf, canon, push_relation, process_coincidences,
  A, k, active_cosets, table, coincidences, words, n, word,
  pair, char, coset, i, tau;

  new_coset := function(coset, char)
    local new_word, target, cosetm, charm, pword;
    # intelligently creates a new coset ONLY if (non-forced) tau(coset, char)
    # is not defined.
    if table[coset][char] = 0 then
      new_word := canon(Concatenation(words[coset], [char]));
      target   := tau(1, new_word);

      if target = 0 then
        # in this case following new_word from empty word does not lead us the
        # full way and we need to define more cosets.
        # Need to follow word again to see how far we got before undefined.
        cosetm := 1;
        pword  := [];  # partial word, add letters to it each time
        for charm in new_word do
          # extend partial word
          Add(pword, charm);
          if table[cosetm][charm] = 0 then
            # edge is undefined, define a new one.
            table[cosetm][charm] := k;
            active_cosets[k]     := true;
            Add(table, ListWithIdenticalEntries(Length(A), 0));
            Add(words, ShallowCopy(pword));
            # we need to re-canonicalise pword at the moment because canon does
            # not always output the shortlex-least word.
            # TODO remove this comment soon
            k := k + 1;
          fi;
          cosetm := table[cosetm][charm];
        od;

        # now we've defined all the intermediate words, get the original
        # request to point in the right place.
        table[coset][char] := k - 1;  # k had been incremented 1 too many

      else
        # in this case following new_word led us somewhere and we should
        # point there
        table[coset][char] := target;
      fi;
    fi;
  end;

  tauf := function(coset, word)
    local char;
    # forced tau. This creates new cosets as necessary.
    if Length(word) = 0 then
      return coset;
    fi;
    for char in word do
      if table[coset][char] = 0 then
        # if the product is undefined, define it, and start coset back up
        # at the newly defined value (k-1).
        new_coset(coset, char);
      fi;
      coset := table[coset][char];
    od;
    return coset;
  end;

  tau := function(coset, word)
    local char;
    # non-forced tau, checks whether you can get the whole way.
    # for use in new-coset.
    if Length(word) = 0 then
      return coset;
    fi;
    for char in word do
      if table[coset][char] = 0 then
        return 0;
      fi;
      coset := table[coset][char];
    od;
    return coset;
  end;

  canon := function(word)
    # expresses a word in free band-canonical form.
    # NOTE: it is essential to the validity of this algorithm that the canonical
    # form returned is shortlex minimal. Otherwise new_coset doesn't work
    # properly.
    return SEMIGROUPS.ShortCanonicalFormOfFreeBandElement(word);
  end;

  push_relation := function(coset, u, v)
    local ut, vt;
    ut := tauf(coset, u);
    vt := tauf(coset, v);
    if ut <> vt then
      Add(coincidences, [ut, vt]);
    fi;
  end;

  process_coincidences := function()
    # changed to depth-first.
    local i, j, char, coset, pair, current, counter;
    if Length(coincidences) = 0 then
      return;
    fi;
    while Length(coincidences) <> 0 do
      # current := Length(coincidences);
      current := 1;
      i       := Minimum(coincidences[current]);
      j       := Maximum(coincidences[current]);
      if i = j then
      fi;
      counter := 0;
      if i <> j then
        for char in A do
          if table[j][char] <> 0 then
            if table[i][char] = 0 then
              table[i][char] := table[j][char];
            elif table[i][char] <> 0 then
              counter := counter + 1;
              Add(coincidences, [table[i][char], table[j][char]]);
            fi;
          fi;
        od;
        # for coset in ListBlist([1 .. k - 1], active_cosets) do
        for coset in [1 .. k - 1] do
          for char in A do
            if table[coset][char] = j then
              table[coset][char] := i;
            fi;
          od;
        od;
        for pair in coincidences do
          if pair[1] = j then
            pair[1] := i;
          fi;
          if pair[2] = j then
            pair[2] := i;
          fi;
        od;
        active_cosets[j] := false;
      fi;
      Remove(coincidences, current);
      # Unbind(parents[j]);
      # Unbind(edges[j]);
    od;
  end;

  A             := [1 .. N];
  # F             := FreeBand(N);               # obsolete since new canon func
  # G             := GeneratorsOfSemigroup(F);  # obsolete since new canon func
  k             := 2;
  active_cosets := [true];
  table         := [[]];
  coincidences  := [];
  words         := [[]];
  for char in A do
    table[1][char] := 0;
  od;
  n := 0;
  repeat

    n  := n + 1;

    # only do anything if the current coset is active
    if active_cosets[n] then

      # populate the current line of the table with new cosets if need be
      for char in A do
        new_coset(n, char);
      od;

      # push the coset n through every explicit relation
      for pair in R do
        push_relation(n, pair[1], pair[2]);
      od;

      # push the current coset through every known implicit relation
      for word in words do
        pair := [Concatenation(word, word), word];
        push_relation(n, pair[1], pair[2]);  # word is already canonical
      od;

      # push every previous coset through the current implicit relation
      word := words[n];
      pair := [Concatenation(word, word), word];
      for i in ListBlist([1 .. n - 1], active_cosets{[1 .. n - 1]}) do
        push_relation(i, pair[1], pair[2]);
      od;

    fi;

    process_coincidences();

  until n = k - 1;

  for pair in R do
    if Length(pair[1]) = 0 or Length(pair[2]) = 0 then
      # then one of these must be a monoid presentation.
      return Length(ListBlist([1 .. k - 1], active_cosets));
    fi;
  od;

  # if no relations have the empty word then this is not a monoid presentation.
  return Length(ListBlist([1 .. k - 1], active_cosets)) - 1;
end);
