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

#

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

InstallMethod(Iterator, "for a Greens D-class of a free band",
[IsFreeBandElementCollection and IsGreensDClass],
function(dclass)
  local NextIterator_FreeBandDClass, NewIterator_FreeBandDClass, ShallowCopyLocal,
  record, s, content, rep, NextIterator_FreeBandDClassWithPrint;
  
  s := Parent(dclass);
  rep := Representative(dclass); 
  content :=BlistList( [1 .. Length(GeneratorsOfSemigroup(s))],
            Set(TupleToWord([rep![1], rep![2], rep![3], rep![4]]))); 
   
  NextIterator_FreeBandDClass := function(iter)
    local output, i, content, tempcont;
  
    if iter!.element <> fail then
      output := StructuralCopy(iter!.element);
    else
      return fail;
    fi;
   
    content := iter!.content;
  
    if iter!.element[2] = 0 then
      iter!.element := fail;
    elif iter!.iter1!.element <> fail then
  # Prefix word is not done yet
      iter!.element[2] := NextIterator_FreeBandDClass(iter!.iter1); 
    elif Position(content, true, iter!.element[1]) <> fail then
  # Update the first component
      i := Position(content, true, iter!.element[1]);
      iter!.element[1] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter1 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      iter!.element[2] := NextIterator_FreeBandDClass(iter!.iter1);
    elif iter!.iter2!.element <> fail then
  # Sufix word is not done yet
      iter!.element[4] := NextIterator_FreeBandDClass(iter!.iter2); 
  # Restart the prefix
      i := Position(content, true);
      iter!.element[1] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter1 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      iter!.element[2] := NextIterator_FreeBandDClass(iter!.iter1);
    elif Position(content, true, iter!.element[3]) <> fail then # Increase the third comp
  # Update the third component
      i := Position(content, true, iter!.element[3]);
      iter!.element[3] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter2 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      iter!.element[4] := NextIterator_FreeBandDClass(iter!.iter2);
  # Restart the prefix 
      i := Position(content, true);
      iter!.element[1] := i;
      tempcont := ShallowCopy(content);
      tempcont[i] := false;
      iter!.iter1 := NewIterator_FreeBandDClass(iter!.semigroup, tempcont);
      iter!.element[2] := NextIterator_FreeBandDClass(iter!.iter1);
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
      return Product(List(TupleToWord(next_value),
                     x -> GeneratorsOfSemigroup(iter!.semigroup)[x]));
    fi;
  end;

#
  
  NewIterator_FreeBandDClass := function(s, content)
    local record, first, tempcont;
   
    first := Position(content, true);  
  
  # If the content is of size one
    if Position(content, true, first) = fail then
      record := rec( element := [first, 0, first, 0],
                     iter1 := fail,
                     iter2 := fail,
                     semigroup := s,
                     content := content) ;
    else
      tempcont := ShallowCopy(content);
      tempcont[first] := false;
      record := rec( iter1 := NewIterator_FreeBandDClass(s, tempcont),
                     iter2 := NewIterator_FreeBandDClass(s, tempcont),
                     semigroup := s,
                     content := content) ;
      record!.element := [first, NextIterator_FreeBandDClass(record!.iter1),
                          first, NextIterator_FreeBandDClass(record!.iter2)]; 
    fi;
    return record;
  end;
    
  #
  
    ShallowCopyLocal := record -> rec(
      lasr_called_by_is_done := record!.last_called_by_is_done,
      next_value := record!.next_value,
      IsDoneIterator := record!.IsDoneIterator,
      NextIterator := record!.IsDoneIterator );
  
  record := NewIterator_FreeBandDClass(s, content); 
  record!.NextIterator := NextIterator_FreeBandDClassWithPrint;
  record!.ShallowCopy := ShallowCopyLocal;
  return IteratorByNextIterator(record);
end);

#

#BindGlobal("NextIterator_FreeBand",
#function(iter)
#  local 
#
## If the iterator is done iterating a specific content (D-class) then change
## the content.
#  content := iter!.content;
#  ones := [1 .. iter!.nrgen]*0+1;
#  output := StructuralCopy(iter!.element);
#
#  if iter!.contdone and content = ones then
#    iter!.element := fail;
#  elif iter!.contdone then
#  # If we are in this case, then content <> ones and we change the content
#    for i in [1 .. iter!.nrgen] do
#      if content[i] = 0 then
#        content[i] := 1; break;
#      else
#        content[i] := 0;
#      fi;
#    od;
#   # NewIterator is a local function and it doesn't create a proper iterator
#    tempiter := NewIterator(content);
#    iter!.isdone := tempiter!.isdone;
#    iter!.content := tempiter!.content;
#    iter!.contdone := tempiter!.contdone;
#    iter!.nrgen := tempiter!.nrgen;
#    iter!.semigroup := tempiter!.semigroup;
#    iter!.element := tempiter!.element;
#    iter!.iter1 := tempiter!.iter1;
#    iter!.iter2 := tempiter!.iter2;
#    NextIteratorWithContent(iter!.iter1);
#    NextIteratorWithContent(iter!.iter2);
#  else
#  # Otherwise we can get next iterator for the current content
#    NextIteratorWithContent(iter);  
#  fi;
#  
# return PrintIterator(output);
#end );
#
#BindGlobal("ShallowCopy_FreeBand", iter -> rec(
#                isdone := iter!.isdone,
#                content := iter!.content,
#                contdone := iter!.contdone,
#                nrgen := iter!.nrgen,
#                semigroup := iter!.semigroup,
#                iter1 := iter!.iter1,
#                iter2 := iter!.iter2,
#                element := StructuralCopy(iter!.element) ) );
#
#BindGlobal("IsDoneIterator_FreeBand", iter -> iter!.isdone );
#
#InstallMethod( Iterator, "for a free band",
#  [IsFreeBand], S -> IteratorByFunctions( rec(
#
#  IsDoneIterator := IsDoneIterator_FreeBand,
#  NextIterator   := NextIterator_FreeBand,
#  ShallowCopy    := ShallowCopy_FreeBand,
#
#  semigroup      := S,
#  nrgen          := Length(Generators(S)),
#  content        := [1..Length(Generators(S))]*0 + [1],
#  contdone       := true,
#  isdone         := (Length(Generators(S)) = 1), 
#  iter1          := fail,
#  iter2          := fail,
#  element        := [1, 0, 1, 0]) ) );
#
############################################################################
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
