################################################################################
##
#W  freeband.gi
#Y  Copyright (C) 2013-14                                         Julius Jonusas     
##
##  Licensing information can be foundin the README file of this package.
##
################################################################################
##
##  FreeBand( <rank> [, names] )
##  FreeBand( name1, name2, ... ) 
##  FreeBand( names ) 
##

InstallGlobalFunction(FreeBand,
function(arg)
  local names, F, type, gens, S, m;

 # Get and check the argument list, and construct names if necessary.
  if Length( arg ) = 1 and IsInt( arg[1] ) and 0 < arg[1] then
    names:= List( [ 1 .. arg[1] ],
                  i -> Concatenation( "x", String(i) ) );
  elif Length( arg ) = 2 and IsInt( arg[1] ) and 0 < arg[1] then
    names:= List( [ 1 .. arg[1] ],
                  i -> Concatenation( arg[2], String(i) ) );
  elif 1 <= Length( arg ) and ForAll( arg, IsString ) then
    names:= arg;
  elif Length( arg ) = 1 and IsList( arg[1] )
                          and ForAll( arg[1], IsString ) then
    names:= arg[1];
  else
    Error("usage: FreeBand(<name1>,<name2>..) or FreeBand(<rank> [, name])");
  fi;
  MakeImmutable( names );

  F := NewFamily( "FreeBandElementsFamily", IsFreeBandElement, 
       CanEasilySortElements);

  type := NewType(F, IsFreeBandElement and IsPositionalObjectRep);

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
  local word1, word2, out;
  
  #if IsBound(tuple![5]) then 
  #  return tuple![5];
  #fi;

  if tuple = [] then
    out:=[];
  elif tuple![2] = 0 then
    out:=[tuple![1]];
  elif tuple![1] = tuple![3] then
    out:=Concatenation(TupleToWord(tuple![2]), [tuple![1]], TupleToWord(tuple![4]));
  else
    word1 := Concatenation(TupleToWord(tuple![2]), [tuple![1]]);
    word2 := Concatenation([tuple![3]],TupleToWord(tuple![4]));
    if word1 = word2 then
      out:=word1; 
    else
      out:=Concatenation(word1, word2);
    fi;
  fi;
  tuple![5]:=out;
  return out;
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
    elif Position(content, true, iter!.element[3]) <> fail then
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
    NextIterator := record!.NextIterator );

  record := NewIterator_FreeBandDClass(s, content); 
  record!.NextIterator := NextIterator_FreeBandDClassWithPrint;
  record!.ShallowCopy := ShallowCopyLocal;
  return IteratorByNextIterator(record);
end);

# A free band iteratror has component: content, dclass_iter.

InstallMethod(Iterator, "for a free band",
[IsFreeBand],
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

  ShallowCopyLocal := record -> rec(
    lasr_called_by_is_done := record!.last_called_by_is_done,
    next_value := record!.next_value,
    IsDoneIterator := record!.IsDoneIterator,
    NextIterator := record!.NextIterator );

  record := rec( content := BlistList([1 .. Length(GeneratorsOfSemigroup(s))], [1]),
                 dclass_iter := Iterator(GreensDClassOfElement(s, s.1)));  
  record!.NextIterator := NextIterator_FreeBand;
  record!.ShallowCopy := ShallowCopyLocal;
  return IteratorByNextIterator(record);
end);
############################################################################
##
## GreensDClassOfElement
##

InstallMethod(GreensDClassOfElement, "for a free band an element",
[IsFreeBand, IsFreeBandElement],
function(s, x)
  local type, d;

  if not x in s then
    Error("the element does not belong to the semigroup.");
    return;
  fi;

  type := NewType( FamilyObj( s ), IsEquivalenceClass and
                   IsEquivalenceClassDefaultRep and IsGreensDClass);
  d := Objectify( type, rec());
  SetParent(d, s);
  SetRepresentative(d, x);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  return d;
end);

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
  local i;

  for i in [1 .. 4] do
    if tuple1![i] <> tuple2![i] then
      return false;  
    fi;
  od;
  return true;
end );

#############################################################################
##
## Inequality
##

InstallMethod(\<, "for elements of a free band",
IsIdenticalObj,
[IsFreeBandElement, IsFreeBandElement],
function(tuple1, tuple2)
  local i;
  i:=1;
  while i<=3 and tuple1![i]=tuple2![i] do  
    i:=i+1;
  od;

  return tuple1![i]<tuple2![i];
end);

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

InstallMethod(Size, "for a free band", 
[IsFreeBand and IsFinite and HasGeneratorsOfSemigroup], 
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

#

#InstallGlobalFunction(ORB_HashFunctionForBlocks,
#function(blocks, data)
#  return ORB_HashFunctionForPlainFlatList(blocks!.blocks, data);
#end);

#

#InstallMethod(ChooseHashFunction, "for blocks",
#[IsBlocks, IsInt],
#function(t,hashlen)
#  return rec(func := ORB_HashFunctionForBlocks, data:=hashlen);
#end );

