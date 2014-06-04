###########################################################################
##
#W  ideals-generic.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# We use the result of enumerating the SEEData of the supersemigroup of an ideal
# to calculate elements, size, test membership, find idempotents, etc. We get a
# generating set and use that otherwise. 


# this function installs all the descendants of <enum[i]> in <enum> if <enum>
# has at least <i> elements, and otherwise returns <fail>.

BindGlobal("SEMIGROUPS_EnumerateIdeal",
function(enum, limit, lookfunc)
  local pos, data, indices, j, len, lookfunc, lookup, nr, genstoapply, l, k;
  
  pos:=enum!.pos;

  if IsBound( then # we already know descendants of enum[i]
    return enum;
  fi;
  
  data:=SEEData(Supersemigroup(UnderlyingCollection(enum)));
  indices:=enum!.indices;

  i:=indices[limit]; # position in <data> corresponding to <enum[i]>
  
  # enumerate data until we've seen all the descendants
  # of <i>...
  if not IsBound(left[i][1]) then 
    if i>data!.nr then # enumerate <data> until the <i>th element is known...
      Enumerate(data, i);
    fi;
    if i>data!.nr then #there is no <i>th element
      return fail;  
    fi;
    
    # enumerate <data> until the left and right descendants of the <i>th element
    # are known. The left descendants of the <i>th element are installed after
    # every word of length equal to the <i>th element has had its right
    # descendants installed. 
    if not IsBound(left[i][1]) then 
      len:=Length(data!.words[i]);
      if not IsBound(data!.lenindex[len+1]) then # no words longer than <len> are known.  
        lookfunc:=function(data, nr)             # so we look for one...
          return Length(data!.words[nr])=len+1;
        end;
        Enumerate(data, infinity, lookfunc);
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
  fi;
  # by here we know what left[indices[j]] is known for all j=pos..i.
 
  # install the descendants of <enum[i]> in the enumerator...
  lookup:=enum!.lookup;
  nr:=enum!.nr;
  genstoapply:=data!.genstoapply;

  for j in [pos..i] do
    j:=indices[j];
    for k in genstoapply do 
      l:=right[j][k];
      if not IsBound(lookup[l]) then 
        nr:=nr+1;
        indices[nr]:=l;
        lookup[l]:=nr;
      fi;
      l:=left[j][k];
      if not IsBound(lookup[l]) then 
        nr:=nr+1;
        indices[nr]:=l;
        lookup[l]:=nr;
      fi;
    od;
  od;

  enum!.nr:=nr;
  enum!.pos:=i+1;
  return enum;
end);

InstallMethod(Enumerator, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local record;

  S:=SupersemigroupOfIdeal(I);
  data:=SEEData(S);

  record:=rec(
    pos:=0,       # the first position in <indices> whose descendants might not
                  # have been installed
    indices:=[],  # the indices of elements in <I> in <S>
    nr:=0,        # the length of <indices>
    lookup:=[] ); # if <data!.elts[i]> is an element of <I>, then
                  # <lookup[i]=Position(Enumerator(I), data!.elts[i])

  record.NumberElement:=function(enum, elt)
    nr:=Position(data, elt);
    
    if nr=fail then 
      return fail;
    elif IsBound(enum!.lookup[nr]) then 
      return enum!.lookup[nr];
    fi;


    SEMIGROUPS_NumberElementIdeal(enum, ); # until lookup[pos] is bound...
  end;

  record.ElementNumber:=function(enum, nr)
    if not IsBound(enum!.indices[nr]) then 
      SEMIGROUPS_EnumerateIdeal(enum, nr);
    fi;
    return data!.elts[enum!.indices[nr]];
  end;

      
    

    


end);



InstallMethod(GeneratorsOfSemigroup, "for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  
  U:=Semigroup(GeneratorsOfSemigroupIdeal(I));
  elts:=Enumerator(I);
  i:=0;
  n:=Length(gens);

  for x in elts do
    if not x in U then
      U:=Semigroup(U, x);
    fi;
  od;

  SetSEEData(S, SEEData(U));
  return GeneratorsOfSemigroup(U);
end);


