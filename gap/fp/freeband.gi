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

SEMIGROUPS.FreeBandElementByGraphInsertNode := function(x, tuple)
  local u, i;

  if tuple in x!.lookup then
    return x!.lookup[tuple];
  fi;

  x!.cont[tuple[1]] := true;
  x!.cont[tuple[3]] := true;

  for i in [2, 4] do
    if tuple[i] <> 0 then
      x!.indeg[tuple[i]] := x!.indeg[tuple[i]] + 1;
    fi;
  od;

  if Length(x!.graveyard) <> 0 then
    u := Remove(x!.graveyard);
    for i in [2, 4] do
      if x!.graph[u][i] <> 0 then
        x!.indeg[x!.graph[u][i]] := x!.indeg[x!.graph[u][i]] - 1;
        if x!.indeg[x!.graph[u][i]] = 0 then
          Add(x!.graveyard, x!.graph[u][i]);
          Unbind(x!.lookup[x!.graph[u][i]]);
        fi;
      fi;
    od;
    x!.graph[u] := tuple;
  else
    u := Length(x!.graph) + 1;
    Add(x!.graph, tuple);
    Add(x!.indeg, 0);
  fi;

  x!.lookup[tuple] := u;

  if x!.root = tuple[2] or x!.root = tuple[4] then
    x!.root := u;
  fi;

  return u;
end;

SEMIGROUPS.FreeBandElementByGraphRightMultiplyByLetter := function(x, a)
  local u, v, w, add_spine;

  add_spine := function(v)
    local u, que;
    que := [];
    while x!.graph[v][1] <> 0 do
      Add(que, v);
      v := x!.graph[v][4];
    od;
    v := SEMIGROUPS.FreeBandElementByGraphInsertNode(x, [a, v, a, v]);
    while Length(que) > 0 do
      u := Remove(que);
      v := SEMIGROUPS.FreeBandElementByGraphInsertNode(x,
           [a, u, x!.graph[u][3], v]);
    od;
    return v;
  end;

  if not x!.cont[a] then
    x!.root := add_spine(x!.root);
  else
    u := x!.root;
    while x!.graph[u][3] <> a do
      u := x!.graph[u][4];
    od;
    w := x!.graph[u][4];
    v := x!.graph[w][4];
    if v <> 0 then
      x!.indeg[w] := x!.indeg[w] - 1;
      if x!.indeg[w] = 0 then
        Add(x!.graveyard, w);
        Unbind(x!.lookup[x!.graph[w]]);
      fi;
      Unbind(x!.lookup[x!.graph[u]]);
      x!.graph[u][3] := x!.graph[w][3];
      x!.graph[u][4] := add_spine(v);
      x!.indeg[x!.graph[u][4]] := x!.indeg[x!.graph[u][4]] + 1;
      x!.lookup[x!.graph[u]] := u;
    fi;
  fi;
end;

SEMIGROUPS.FreeBandElementByGraphRemoveDeadVertices := function(x)
  local l, r, i, j, n, u, v, is_dead, new_pos;
  i := 1;
  n := Length(x!.graveyard);
  while i <= n do
    u := x!.graveyard[i];
    for j in [2, 4] do
      v := x!.graph[u][j];
      x!.indeg[v] := x!.indeg[v] - 1;
      if x!.indeg[v] = 0 then
        Add(x!.graveyard, v);
        Unbind(x!.lookup[x!.graph[v]]);
        n := n + 1;
      fi;
    od;
    i := i + 1;
  od;
  is_dead := BlistList([1 .. Length(x!.graph)], x!.graveyard);
  new_pos := ListWithIdenticalEntries(Length(x!.graph), 0);
  i := 1;
  for u in [1 .. Length(x!.graph)] do
    if not is_dead[u] then
      new_pos[u] := i;
      i := i + 1;
    fi;
  od;
  x!.root := new_pos[x!.root];
  x!.lookup := HashMap();
  for u in [1 .. Length(x!.graph)] do
    if not is_dead[u] then
      l := 0;
      if x!.graph[u][2] <> 0 then
        l := new_pos[x!.graph[u][2]];
      fi;
      r := 0;
      if x!.graph[u][4] <> 0 then
        r := new_pos[x!.graph[u][4]];
      fi;
      x!.graph[new_pos[u]] := [x!.graph[u][1], l, x!.graph[u][3], r];
      x!.indeg[new_pos[u]] := x!.indeg[u];
      x!.lookup[x!.graph[new_pos[u]]] := new_pos[u];
    fi;
  od;
  x!.graph := x!.graph{[1 .. i - 1]};
  x!.indeg := x!.indeg{[1 .. i - 1]};
  x!.graveyard := [];
  return x!.root;
end;

SEMIGROUPS.FreeBandElementByGraphDotString := function(x)
  local S, i;
  S := "//dot\ndigraph {\nedge [colorscheme=set19]\n";
  S := Concatenation(S, "node [shape=circle style=filled colorscheme=set19]\n");
  x!.graveyard := Set(x!.graveyard);
  for i in [1 .. Length(x!.graph)] do
    if i in x!.graveyard then
      S := Concatenation(S, String(i), " [fillcolor=white]\n");
    else
      S := Concatenation(S, String(i), "\n");
    fi;
  od;
  for i in [1 .. Length(x!.graph)] do
    if x!.graph[i][2] <> 0 then
      S := Concatenation(S, String(i), "->",
                         String(x!.graph[i][2]),
                         " [arrowhead=empty color=",
                         String(x!.graph[i][1]),
                         "]\n");
    fi;
    if x!.graph[i][4] <> 0 then
      S := Concatenation(S, String(i), "->",
                         String(x!.graph[i][4]),
                         " [arrowhead=normal color=",
                         String(x!.graph[i][3]),
                         "]\n");
    fi;
  od;
  S := Concatenation(S, "}");
  return S;
end;

SEMIGROUPS.FreeBandElementByGraph := function(S)
  local F, type, x, s;
  F    := NewFamily("FreeBandElementsByGraphFamily", IsFreeBandElementByGraph);
  type := NewType(F, IsFreeBandElementByGraph and IsPositionalObjectRep);
  x    := Objectify(type, rec(root := 1,
                              graph := [[0, 0, 0, 0]],
                              cont := BlistList([1 .. Maximum(S)], []),
                              graveyard := [],
                              indeg := [0],
                              lookup := HashMap()));
  x!.lookup[[0, 0, 0, 0]] := 1;
  for s in S do
    SEMIGROUPS.FreeBandElementByGraphRightMultiplyByLetter(x, s);
  od;
  # SEMIGROUPS.FreeBandElementByGraphRemoveDeadVertices(x);
  return x;
end;

SEMIGROUPS.FreeBandElementByGraphIsEqual := function(x, y)
  local que, u, v, seen_x, seen_y, i;
  if x!.cont <> y!.cont then
    return false;
  fi;
  que := [[x!.root, y!.root]];
  seen_x := BlistList([1 .. Length(x!.graph)], []);
  seen_x[x!.root] := true;
  seen_y := BlistList([1 .. Length(y!.graph)], []);
  seen_y[y!.root] := true;
  while Length(que) <> 0 do
    u := Remove(que);
    v := u[2];
    u := u[1];
    if x!.graph[u][1] <> y!.graph[v][1] or x!.graph[u][3] <> y!.graph[v][3] then
      return false;
    fi;
    for i in [2, 4] do
      if (x!.graph[u][i] = 0 and y!.graph[v][i] <> 0) or
          (x!.graph[u][i] <> 0 and y!.graph[v][i] = 0) then
        return false;
      fi;
      if x!.graph[u][i] <> 0 then
        if (seen_x[x!.graph[u][i]] and not seen_y[y!.graph[v][i]]) or
            (not seen_x[x!.graph[u][i]] and seen_y[y!.graph[v][i]]) then
          return false;
        fi;
        if not seen_x[x!.graph[u][i]] then
          Add(que, [x!.graph[u][i], y!.graph[v][i]]);
          seen_x[x!.graph[u][i]] := true;
          seen_y[y!.graph[v][i]] := true;
        fi;
      fi;
    od;
  od;
  return true;
end;

SEMIGROUPS.FreeBandElementByGraphCanonicalWord := function(x)
  local S, recursive_step;

  recursive_step := function(u, l, r)
    local m;
    if x!.graph[u][1] = 0 then
      return;
    fi;
    m := QuoInt(r + l, 2);
    S[m] := x!.graph[u][1];
    S[m + 1] := x!.graph[u][3];
    recursive_step(x!.graph[u][2], l, m - 1);
    recursive_step(x!.graph[u][4], m + 2, r);
  end;

  S := ListWithIdenticalEntries(2 ^ (SizeBlist(x!.cont) + 1) - 2, 0);
  recursive_step(x!.root, 1, Length(S));
  return S;
end;
