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
  local NextIteratorWithContent, NewIterator, PrintIterator, ones, content, i,
        tempiter, output;
  
  NewIterator := function(cont)
# Create a new iterator (technicaly not an iterator) for a specified content.
    local element, iscontdone, i, iter1, iter2, tempcont, record; 

    element := [];
    iscontdone := false;
    if Sum(cont) = 1 then
      i := Position(cont, 1);
      element := [i,0,i,0];            iscontdone := true;
      iter1 := fail;                   iter2 := fail;
    else
      i := Position(cont, 1);
      element[1] := i;                 element[3] := i;
      tempcont := ShallowCopy(cont);   tempcont[i] := 0;
      iter1 := NewIterator(tempcont);  iter2 := NewIterator(tempcont);
      element[2] := iter1!.element;    element[4] := iter2!.element;
    fi;

    record := rec(
      isdone := false,                 content := cont,
      contdone := iscontdone,          nrgen := iter!.nrgen,
      semigroup := iter!.semigroup,    element := element,
      iter1 := iter1,                  iter2 := iter2
      );
    return record;
  end;


  NextIteratorWithContent := function(iter)
# Iterate for a fixed content.
    local cont, iter1, iter2, i, tempcont, output;

    cont := iter!.content;
    iter1 := iter!.iter1;
    iter2 := iter!.iter2;
    output := ShallowCopy(iter!.element);

    if Sum(cont) =  1 then 
      # iter1 = fail is only possible if content is of size 1
      iter!.element := fail;
    elif iter!.contdone then
      return fail;
    elif not iter1!.contdone then 
      iter!.element[2] := NextIteratorWithContent(iter1);
    else
      i := Position(cont, 1, iter!.element[1]); 
      if i <> fail then
      # If we fall into this case increase the first component of the quadruple
        iter!.element[1] := i;
        tempcont := ShallowCopy(cont);
        tempcont[i] := 0;
        iter1 := NewIterator(tempcont);
        iter!.element[2] := NextIteratorWithContent(iter1);
      elif not iter2!.contdone then
      # Being in this or following case implies that the prefix is maximal 
        iter!.element[4] := NextIteratorWithContent(iter2);
      else
        i := Position(cont, 1, iter!.element[3]);
        if i <> fail then
          iter!.element[3] := i;
          tempcont := ShallowCopy(cont);
          tempcont[i] := 0;
          iter2 := NewIterator(tempcont);
          iter!.element[4] := NextIteratorWithContent(iter2);
          i := Position(cont, 1);
       # Restart prefix
          iter!.element[1] := i;
          tempcont := ShallowCopy(cont);
          tempcont[i] := 0;
          iter1 := NewIterator(tempcont);
          iter!.element[2] := NextIteratorWithContent(iter1);
        else
          iter!.contdone := true; 
        fi;
      fi;
    fi;
   
    # Move this out of the function
    if ForAll(iter!.content, x-> x=1) and iter!.contdone = true then
      iter!.isdone := true;
    fi;
    return output;
  end; 

# A function to convert a iter!.element list into a free band word 
  PrintIterator := function(list)
    local output;

    if list[2] = 0 then
      # Implies that list = [i, 0, i, 0];
      return GeneratorsOfSemigroup(iter!.semigroup)[list[1]];
    else
      return PrintIterator(list[2])*
             GeneratorsOfSemigroup(iter!.semigroup)[list[1]]*
             GeneratorsOfSemigroup(iter!.semigroup)[list[3]]*
             PrintIterator(list[4]);
    fi;
  end;

# If the iterator is done iterating a specific content (D-class) then change
# the content.
  content := iter!.content;
  ones := [1 .. iter!.nrgen]*0+1;
  output := ShallowCopy(iter!.element);

  if iter!.contdone and content = ones then
    return fail;
  elif iter!.contdone then
  # If we are in this case, then content <> ones and we change the content
    for i in [1 .. iter!.nrgen] do
      if content[i] = 0 then
        content[i] := 1; break;
      else
        content[i] := 0;
      fi;
    od;
# NewIterator is a local function and it doesn't create a proper iterator
    tempiter := NewIterator(content);
    iter!.isdone := tempiter!.isdone;
    iter!.content := tempiter!.content;
    iter!.contdone := tempiter!.contdone;
    iter!.nrgen := tempiter!.nrgen;
    iter!.semigroup := tempiter!.semigroup;
    iter!.element := tempiter!.element;
    iter!.iter1 := tempiter!.iter1;
    iter!.iter2 := tempiter!.iter2;
  else
  # Otherwise we can get next iterator for the current content
    NextIteratorWithContent(iter);  
  fi;

 return PrintIterator(output);
end );

BindGlobal("ShallowCopy_FreeBand", iter -> rec(
                isdone := iter!.isdone,
                content := iter!.content,
                contdone := iter!.contdone,
                nrgen := iter!.nrgen,
                semigroup := iter!.semigroup,
                iter1 := iter!.iter1,
                iter2 := iter!.iter2,
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
  iter1          := fail,
  iter2          := fail,
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
