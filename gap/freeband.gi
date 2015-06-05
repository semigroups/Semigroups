###############################################################################
##
#W  freeband.gi
#Y  Copyright (C) 2013-15                                  Julius Jonusas
##
##  Licensing information can be foundin the README file of this package.
##
###############################################################################

InstallTrueMethod(IsFinite, IsFreeBandSubsemigroup);

#

InstallGlobalFunction(SEMIGROUPS_FreeBandElmToWord,
function(elem)
  local tuple, out, first, pre_tuple, last, su_tuple, word1, word2;

  tuple := elem!.tuple;

  if elem!.word <> fail then
    return elem!.word;
  elif tuple = [] then
    out := [];
  elif tuple![2] = 0 then # tuple corresponds to one the generators
    out := [tuple![1]];
  else
    first := tuple![1];
    pre_tuple := tuple![2]; # tuple correspoding to the prefix
    last := tuple![3];
    su_tuple := tuple![4];  # tuple corresponding to the sufix

    # if first = last we only need a single letter
    if first = last then
      out := Concatenation(SEMIGROUPS_FreeBandElmToWord(pre_tuple), [first],
                           SEMIGROUPS_FreeBandElmToWord(su_tuple));
      # otherwise we need distinct letters for both first and last
    else
      word1 := Concatenation(SEMIGROUPS_FreeBandElmToWord(pre_tuple), [first]);
      word2 := Concatenation([last], SEMIGROUPS_FreeBandElmToWord(su_tuple));
      if word1 = word2 then
        out := word1;
      else
        out := Concatenation(word1, word2);
      fi;
    fi;
  fi;
  elem!.word := out;
  return out;
end);

InstallGlobalFunction(FreeBand,
function(arg)
  local names, F, type, gens, S, m, ngens;

  # Get and check the argument list, and construct names if necessary.
  if Length(arg) = 1 and IsInt(arg[1]) and 0 < arg[1] then
    names := List([1 .. arg[1]],
                  i -> Concatenation("x", String(i)));
  elif Length(arg) = 2 and IsInt(arg[1]) and 0 < arg[1] then
    names := List([1 .. arg[1]],
                  i -> Concatenation(arg[2], String(i)));
  elif 1 <= Length(arg) and ForAll(arg, IsString) then
    names := arg;
  elif Length(arg) = 1 and IsList(arg[1])
      and ForAll(arg[1], IsString) then
    names := arg[1];
  else
    Error("Semigroups: FreeBand: usage,\n",
          "FreeBand(<name1>,<name2>..) or FreeBand(<rank> [, name]),");
    return;
  fi;

  MakeImmutable(names);

  F := NewFamily("FreeBandElementsFamily", IsFreeBandElement,
                                           CanEasilySortElements);

  type := NewType(F, IsFreeBandElement and IsPositionalObjectRep);

  ngens := Length(names);
  gens := EmptyPlist(ngens);

  for m in  [1 .. ngens] do
    gens[m] := Objectify(type, rec(tuple := [m, 0, m, 0],
                                   cont := BlistList([1 .. ngens], [m]),
                                   word := [m]));
  od;

  StoreInfoFreeMagma(F, names, IsFreeBandElement);

  S := Objectify(NewType(FamilyObj(gens),
                         IsFreeBandCategory and IsSemigroup and
                         IsAttributeStoringRep),
                 rec());
  SetGeneratorsOfMagma(S, gens);
  SetIsFreeBand(S, true);
  FamilyObj(S)!.semigroup := S;
  F!.semigroup := S;
  SetIsWholeFamily(S, true);
  return S;
end);

InstallMethod(IsFreeBand, "for a semigroup",
[IsSemigroup],
function(s)
  local used, occured, gens, max_d, g;

  if not IsBand(s) then
    return false;
  fi;

  if IsFreeBandSubsemigroup(s) then
    gens := Generators(s);
    used := BlistList([1 .. Length(gens[1]!.cont)], []);
    occured := BlistList([1 .. Length(gens[1]!.cont)], []);
    for g in gens do
      used := IntersectionBlist(used, g!.cont);
      if g!.tuple[2] = 0 then
        occured[g!.tuple[1]] := true;
      fi;
    od;
    if used = occured then
      return true;
    fi;
  fi;

  if not IsActingSemigroup(s) then
    s := AsTransformationSemigroup(s);
  fi;

  max_d := MaximalDClasses(s);
  return Size(s) = Size(FreeBand(Length(max_d)));
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
      return Product(List(SEMIGROUPS_FreeBandElmToWord(next_value),
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

# A free band iteratror has component: content, dclass_iter.

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
      rep := UniversalFakeOne;
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

#

InstallMethod(GreensDClassOfElement, "for a free band an element",
[IsFreeBandCategory, IsFreeBandElement],
function(s, x)
  local type, d;

  if not x in s then
    Error("Semigroups: GreensDClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;

  type := NewType(FamilyObj(s), IsEquivalenceClass and
                  IsEquivalenceClassDefaultRep and IsGreensDClass);
  d := Objectify(type, rec());
  SetParent(d, s);
  SetRepresentative(d, x);
  # SetEquivalenceClassRelation(d, GreensDRelation(s));
  # TODO Add a new method for GreensDRelations. JJ
  return d;
end);

#

InstallMethod(ViewString, "for a free band element",
[IsFreeBandElement], PrintString);

#

InstallMethod(PrintString, "for a free band element",
[IsFreeBandElement],
function(elem)
  return Concatenation(List(SEMIGROUPS_FreeBandElmToWord(elem),
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
  else
    for i in [1 .. 4] do
      if x!.tuple![i] <> y!.tuple![i] then
        return false;
      fi;
    od;
  fi;
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
  else
    return tuple1[i] < tuple2[i];
  fi;

  return tuple1![i] < tuple2![i];
end);

#

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
        copy := ShallowCopy(x); # are shallow copies necessary?
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

#

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

#

InstallGlobalFunction(SEMIGROUPS_HashFunctionForFreeBandElements,
function(x, data)
  return ORB_HashFunctionForPlainFlatList(SEMIGROUPS_FreeBandElmToWord(x),
                                          data);
end);

#

InstallMethod(ChooseHashFunction, "for a free band element and int",
[IsFreeBandElement, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionForFreeBandElements,
             data := hashlen);
end);
