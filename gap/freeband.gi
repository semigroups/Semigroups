################################################################################
##
#W  freeband.gi
#Y  Copyright (C) 2013-14                                  
##
##  Licensing information can be foundin the README file of this package.
##
################################################################################


################################################################################
##
##  FreeBand( <rank> )
##

InstallGlobalFunction(FreeBand,
function(arg)
  local names, F, type, gens, S, m;

  F := NewFamily( "FreeBandElementsFamily", IsFreeBandElement, 
       CanEasilySortElements);

  type := NewType(F, IsFreeBandElement and IsPositionalObjectRep);

  names := List( [ 1 .. arg[1] ], i -> Concatenation("x", String(i)));
  MakeImmutable( names );

  gens:=EmptyPlist( Length(names) );
  for m in  [1 .. Length(names)] do
    gens[m] := Objectify(type, [m, 0, m, 0]);
  od;
  StoreInfoFreeMagma( F, names, IsFreeBandElement );
  S := Semigroup(gens);
  SetIsFreeBand(S, true);

  FamilyObj(S)!.semigroup := S;
  F!.semigroup := S;

  SetIsWholeFamily( S, true);

  return S;
end );

################################################################################
##
## WordToTuple & TupleToWord
##

WordToTuple := function(word)
  local first, last, i; 

  if word = [] then
    return 0;
  else  
    first := []; # the list of first occurrences
    last := [];  # the list of last occurrences
    for i in [1 .. Length(word)] do
      last[word[i]] := i; 
      if not IsBound(first[word[i]]) then
        first[word[i]] := i;
      fi;
    od;
  fi;
  return [ word[Maximum(first)], WordToTuple(word{[1..Maximum(first)-1]}),
         word[Minimum(last)],WordToTuple(word{[Minimum(last)+1.. Length(word)]}) ];
end;


TupleToWord := function(tuple)
  if tuple = [] then
    return [];
  elif tuple![2] = 0 then
    return [tuple![1]];
  elif tuple![1] = tuple![3] then
    return Concatenation(TupleToWord(tuple![2]), [tuple![1]], TupleToWord(tuple![4]));
  else 
    return Concatenation(TupleToWord(tuple![2]), [tuple![1], tuple![3]],
                         TupleToWord(tuple![4]) );
  fi;
end;

################################################################################
##
## Iterator
##

BindGlobal("NextIterator_FreeBand",
function(iter)
  local NextIteratorWithContent, NewIterator, PrintIterator, ones, content, i, old_iter;
  
  NewIterator := function(C)
    local it, el, i, iscontdone, C1;

    el := [];
    iscontdone := false;
    if Sum(C) = 1 then
      for i in [1 .. Length(C)] do
        if C[i] = 1 then break; fi;
      od;
      el := [i,0,i,0];
      iscontdone := true;
    else
      for i in [1 .. Length(C)] do
        if C[i] = 1 then 
          el[1] := i; el[3] := i;
          break;
        fi;
      od;
      C1 := ShallowCopy(C);
      C1[i] := 0;
      el[2] := NewIterator(C1); el[4] := NewIterator(C1);
    fi;

    it := rec(
      isdone := false,
      content := C,
      contdone := iscontdone,
      nrgen := iter!.nrgen,
      semigroup := iter!.semigroup,
      element := el );
    return it;
  end;


  NextIteratorWithContent := function(it)
## The content C is given as a list with C[i] = 1 if i is in C and 0 otherwise. 
    local i, j, C1, C2, C;
## suppose if you enter it!.content = false

    C := it!.content;
    if not it!.element[2]!.contdone then 
      C1 := ShallowCopy(C); C1[it!.element[1]] := 0;
      it!.element[2] := NextIteratorWithContent(C1, it!.element[2]);
    elif not it!.element[4]!.contdone then 
      C2 := ShallowCopy(C); C2[it!.element[3]] := 0;
      it!.element[4] := NextIteratorWithContent(C2, it!.element[4]);
    else 
      for i in [it!.element[1] + 1 .. it!.nrgen] do
        if C[i] = 1  then
          break;
        fi;
      od;
      if IsBound(i) and C[i] = 1 then
        it!.element[1] := i;
        C1 := ShallowCopy(C); C1[it!.element[1]] := 0; 
        C2 := ShallowCopy(C); C2[it!.element[3]] := 0;
        it!.element[2] := NewIterator(C1); it!.element[4] := NewIterator(C2);
      else
        for i in [it!.element[3] + 1 .. it!.nrgen] do
          if C[i] = 1  then
            break;
          fi;
        od;
        j := Position(C, 1);
        if IsBound(i) and C[i] = 1 then
          it!.element[1] := j;
          it!.element[3] := i; 
          C1 := ShallowCopy(C); C1[it!.element[1]] := 0; 
          C2 := ShallowCopy(C); C2[it!.element[3]] := 0;
          it!.element[2] := NewIterator(C1); it!.element[4] := NewIterator(C2);
        else
          it!.contdone := true; 
        fi;
      fi;
    fi;
    
    if ForAll(it!.content, x-> x=1) and it!.contdone = true then
      it!.isdone := true;
    fi;

    return it!.element;
  end; 
 
  content := iter!.content;
  ones := [];
  for i in [1 .. iter!.nrgen] do
    ones[i] :=1;
  od;
  
  if iter!.contdone and content = ones then
    return fail;
  elif iter!.contdone and not content = ones then
    for i in [1 .. iter!.nrgen] do
      if content[i] = 0 then
        content[i] := 1; break;
      else
        content[i] := 0;
      fi;
    od;
    old_iter := NewIterator(content);
    iter!.isdone := old_iter!.isdone;
    iter!.content := old_iter!.content;
    iter!.contdone := old_iter!.contdone;
    iter!.nrgen := old_iter!.nrgen;
    iter!.semigroup := old_iter!.semigroup;
    iter!.element := old_iter!.element;
  else
    NextIteratorWithContent(iter);  
  fi;

  PrintIterator := function(it)
    local tuple;
  
    tuple := ShallowCopy(it!.element);

    if tuple[2] = 0 then
      return [tuple[1]];
    else
      return Concatenation(PrintIterator(tuple[2]), [tuple[1]], [tuple[3]],
             PrintIterator(tuple[4]));
    fi;
  end;
 

   return Product( List( PrintIterator(iter),
                          x -> Generators(iter!.semigroup)[x] ) );
end );

BindGlobal("ShallowCopy_FreeBand", iter -> rec(
                isdone := iter!.isdone,
                content := iter!.content,
                contdone := iter!.contdone,
                nrgen := iter!.nrgen,
                semigroup := iter!.semigroup,
                element := ShallowCopy(iter!.element) ) );

BindGlobal("IsDoneIterator_FreeBand", iter -> iter!.isdone );

InstallMethod( Iterator, "for a free band",
  [IsFreeBand], S -> IteratorByFunctions( rec(

  IsDoneIterator := IsDoneIterator_FreeBand,
  NextIterator   := NextIterator_FreeBand,
  ShallowCopy    := ShallowCopy_FreeBand,

  semigroup      := S,
  nrgen          := Length(Generators(S)),
  content        := [1..Length(Generators(S))]*0 + [1],
  contdone       := true,
  isdone         := (Length(Generators(S)) = 1), 
  element        := [1, 0, 1, 0]) ) );

###########################################################################
##
## ViewObj
##

InstallMethod(PrintObj, "for a free band element",
[IsFreeBandElement], ViewObj);


InstallMethod(ViewObj, "for a free band element",
[IsFreeBandElement],
function(tuple)

  Print( Concatenation( List( TupleToWord(tuple),
                          x -> FamilyObj(tuple)!.names[x] ) ) );
  return;
end);

InstallMethod(ViewObj,
"for a free band containing the whole family",
[IsFreeBand],
function( S )
  if GAPInfo.ViewLength * 10 < Length( GeneratorsOfMagma( S ) ) then
       Print( "<free band with ", Length( GeneratorsOfSemigroup( S ) ),
                  " generators>" );
  else
    Print( "<free band on the generators ",
           GeneratorsOfSemigroup( S ), ">" );
  fi;
end );

#############################################################################
##
## Equality
##

InstallMethod(\=, "for elements of a free band",
IsIdenticalObj,
[IsFreeBandElement, IsFreeBandElement],
function(tuple1, tuple2)
  local isequal,i;

  isequal := true;
  for i in [1 .. 4] do
    if tuple1![i] <> tuple2![i] then
      isequal := false;
      break;
    fi;
  od;
  return isequal;
end );

#############################################################################
##
## Inequality
##

InstallMethod(\<, "for elements of a free band",
IsIdenticalObj,
[IsFreeBandElement, IsFreeBandElement],
function(tuple1, tuple2)
  local list1, list2, i;

  list1 := []; list2 := [];

  for i in [1 .. 4] do
    list1[i] := tuple1![i];
    list2[i] := tuple2![i];
  od;
  return list1 < list2;
end );

##########################################################################
##
## Multiplication
##

InstallMethod(\*, "for elements of a free band", IsIdenticalObj,
[IsFreeBandElement, IsFreeBandElement],
function(tuple1, tuple2)
  
  return Objectify( TypeObj(tuple1), WordToTuple( Concatenation( TupleToWord(tuple1),
                    TupleToWord(tuple2)) ) );

end );


################################################################################
##
## Size 
##

InstallMethod(Size, "for a free band", [IsFreeBand],
function(S)
  local c, output, k, i, n;
  
  n := Length(FamilyObj(S.1)!.names);
 
  c := [];
  for k in [ 1 .. n ] do
    c[k] := 1;
    for i in [1 .. k - 1] do
      c[k] := c[k] * (k - i + 1)^(2^i);
    od;
  od;
  
  output := 0;
  for k in [ 1 .. n ] do
    output := output + Binomial( n, k ) * c[k];
  od;

  return output;
end );
