###########################################################################
##
#W  ideals-generic.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains method specific to generic ideals of semigroups. 

# We use the result of enumerating the GenericSemigroupData of the
# supersemigroup of an ideal to calculate elements, size, test membership, find
# idempotents, etc. We get a generating set and use that otherwise. 

# enumerate the ideal until <enum[limit]> is bound or <lookfunc(enum, nr)> is
# <true>

BindGlobal("SEMIGROUPS_EnumerateIdeal",
function(enum, limit, lookfunc)
  local nr, looking, found, i, lookup, indices, data, left, right, genstoapply, j, len, lookfunc2, l, k;
  
  nr:=enum!.nr;

  if limit<nr then # we already know descendants of enum[i]
    return enum;
  fi;

  if lookfunc<>ReturnFalse then 
    looking:=true;              # only applied to new elements, not old ones!!!
    enum!.found:=fail;          # in case we previously looked for something and found it
    found:=false;
  else
    looking:=false;
  fi;

  i:=enum!.pos;             # the next position to which we apply generators...
  lookup:=enum!.lookup;
  indices:=enum!.indices;

  data:=GenericSemigroupData(SupersemigroupOfIdeal(UnderlyingCollection(enum)));
  left := data!.left;
  right := data!.right;
  genstoapply := data!.genstoapply;
  
  while nr <= limit and i <= nr and not (looking and found) do

    j:=indices[i]; # position in <data> corresponding to <enum[i]>
    # enumerate <data> until we've seen all the left and right descendants of
    # <data!.elts[j]>...
    if not IsBound(left[j][1]) then 
      # enumerate <data> until the left and right descendants of the <j>th element
      # are known. The left descendants of the <j>th element are installed after
      # every word of length equal to the <j>th element has had its right
      # descendants installed. 
      len:=Length(data!.words[j]);
      if not IsBound(data!.lenindex[len+1]) then # no words longer than <len> are known.  
        lookfunc2:=function(data, nr)            # so we look for one...
          return Length(data!.words[nr])=len+1;
        end;
        Enumerate(data, infinity, lookfunc2);
        if Length(data!.words[data!.nr])=len+1 then 
          data!.lenindex[len+1]:=data!.nr; #JDM maybe a bad idea
        fi;
        # at the end of this either there is a word in <data> of length <len+1>
        # or <data> is closed.
      fi;
      if not IsClosedData(data) then 
        data!.stopper:=data!.lenindex[len+1];
        Enumerate(data);
        # enumerate <data> until the right descendants of the first word of
        # length longer than <enum[i]> are known, so that the left descendants
        # of <enum[i]> are known. 
        data!.stopper:=false;
      fi;
    fi;
    # by here we know <left[indices[i]]> and <right[indices[i]]>, i.e. all the
    # descendants of <enum[i]=data!.elts[indices[i]]> are known.
   
    # install the descendants of <enum[i]> in the enumerator...
    for k in genstoapply do 
      l:=right[j][k];
      if not IsBound(lookup[l]) then 
        nr:=nr+1;
        indices[nr]:=l;
        lookup[l]:=nr;
        if looking and not found then 
          if lookfunc(enum, nr) then 
            found:=true;
            enum!.found:=nr;
          fi;
        fi;
      fi;
      l:=left[j][k];
      if not IsBound(lookup[l]) then 
        nr:=nr+1;
        indices[nr]:=l;
        lookup[l]:=nr;
        if looking and not found then 
          if lookfunc(enum, nr) then 
            found:=true;
            enum!.found:=nr;
          fi;
        fi;
      fi;
    od;
    i:=i+1;
  od;

  enum!.nr:=nr;
  enum!.pos:=i;
  
  return enum;
end);

#

InstallMethod(Enumerator, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 1, # to beat the library method
function(I)
  local record, data, gens, i, pos;

  data:=GenericSemigroupData(SupersemigroupOfIdeal(I));

  record:=rec(
    pos:=1,       # the first position in <indices> whose descendants might not
                  # have been installed
    indices:=[],  # the indices of elements in <I> in <S>
    nr:=0,        # the length of <indices>
    lookup:=[] ); # if <data!.elts[i]> is an element of <I>, then
                  # <lookup[i]=Position(Enumerator(I), data!.elts[i])

  # add the generators to <record>

  gens:=GeneratorsOfSemigroupIdeal(I);
  for i in [1..Length(gens)] do 
    pos:=Position(data, gens[i]); # this should not be fail
    if not IsBound(record.lookup[pos]) then 
      record.nr:=record.nr+1;
      record.lookup[pos]:=record.nr;
      record.indices[record.nr]:=pos;
    fi;
  od;

  record.NumberElement:=function(enum, elt)
    local pos, lookfunc;
    pos:=Position(data, elt);
    
    if pos=fail then 
      return fail;
    elif IsBound(enum!.lookup[pos]) then 
      return enum!.lookup[pos];
    fi;
    lookfunc:=function(enum, i)
      return enum!.indices[i]=pos;
    end;
    return SEMIGROUPS_EnumerateIdeal(enum, infinity, lookfunc)!.found;
    # enumerate until lookup[pos] is bound...
    
  end;

  record.ElementNumber:=function(enum, nr)
    if not IsBound(enum!.indices[nr]) then 
      SEMIGROUPS_EnumerateIdeal(enum, nr, ReturnFalse);
    fi;
    return data!.elts[enum!.indices[nr]];
  end;

  record.IsBound\[\]:=function(enum, nr)
    return IsBound(enum!.indices[nr]);
  end;
  
  record.Length:=enum -> SEMIGROUPS_EnumerateIdeal(enum, infinity, ReturnFalse)!.nr;
  
  return EnumeratorByFunctions(I, record);
end);

#

InstallMethod(Size, "for a semigroup ideal with generators", 
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 
function(I)
  return Length(Enumerator(I));
end);

#

InstallMethod(\in, "for an associative element and semigroup ideal with generators",
[IsAssociativeElement, IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(x, I)
  return Position(Enumerator(I), x)<>fail;
end);

#JDM: this should be better, more like the method in ideals-acting.gi

InstallMethod(GeneratorsOfSemigroup, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local U, enum, x;
  
  U:=Semigroup(GeneratorsOfSemigroupIdeal(I));
  enum:=Enumerator(I);

  for x in enum do
    if not x in U then
      U:=Semigroup(U, x);
    fi;
  od;

  return GeneratorsOfSemigroup(U);
end);

#

InstallMethod(Idempotents, "for a semigroup ideal with generators", 
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local enum, elts, indices, idempotents, nr, i;

  elts:=GenericSemigroupData(SupersemigroupOfIdeal(I))!.elts;
  enum:=Enumerator(I);
  if not IsBound(enum!.idempotents) then 
    SEMIGROUPS_EnumerateIdeal(enum, infinity, ReturnFalse);
    indices:=enum!.indices;
    idempotents:=EmptyPlist(Length(indices));
    nr:=0;

    for i in indices do 
      if elts[i]*elts[i]=elts[i] then 
        nr:=nr+1;
        idempotents[nr]:=i;
      fi;
    od;
    
    enum!.idempotents:=idempotents;
    ShrinkAllocationPlist(idempotents);
  fi;

  return elts{enum!.idempotents};
end);

#

