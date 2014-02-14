################################################################################
##
#W  freeband.gi
#Y  Copyright (C) 2013-14                                  
##
##  Licensing information can be foundin the README file of this package.
##
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
##
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
