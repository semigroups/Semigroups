
# code for generic semigroups, ties in the Froidure-Pin Semigroupe algorithm to
# GAP methods

InstallMethod(Size, "for a finite semigroup with generators", 
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(Enumerate(S, infinity, ReturnFalse).elts)-1;
end);

#

InstallMethod(RightCayleyGraphSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(S);
  return data.right{[2..Length(data.right)]}-1; # first point is a dummy
end);

#

InstallMethod(LeftCayleyGraphSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(S);
  return data.left{[2..Length(data.left)]}-1; # first point is a dummy
end);

#

InstallMethod(\in, "for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(S, x)<>fail;
end);

#

InstallMethod(Factorization,
"for a finite semigroup with generators and an associative element",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(S, x)
  local pos;
  pos:=Position(S, x); 
  if pos=fail then 
    return fail;
  fi;
  return SemigroupData(S).words[pos];
end);

# JDM: probably have a global function or attribute RightCayleyGraphSCC or
# similar.

InstallMethod(NrRClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(S);
  if not IsBound(data.rightscc) then 
    data.rightscc:=GABOW_SCC(Enumerate(S).right);
  fi;
    
  return data.rightscc.count-1;
end);

#

InstallMethod(NrLClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(S);
  if not IsBound(data.leftscc) then 
    if not IsBound(data.rightscc) then 
      data.rightscc:=GABOW_SCC(Enumerate(S).right);
    fi;

    data.leftscc:=GABOW_SCC_DCLASSES(data.left, data.rightscc);
  fi;
    
  return data.leftscc.count-1;
end);

#

InstallMethod(NrDClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(S);
  if not IsBound(data.leftscc) then 
    if not IsBound(data.rightscc) then 
      data.rightscc:=GABOW_SCC(Enumerate(S).right);
    fi;

    data.leftscc:=GABOW_SCC_DCLASSES(data.left, data.rightscc);
  fi;
    
  return data.leftscc.nrdclasses;
end);

#

InstallMethod(MinimalIdeal, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(S);
  if not IsBound(data.rightscc) then 
    data.rightscc:=GABOW_SCC(Enumerate(S).right);
  fi;
  return SemigroupIdeal(S, data.elts[data.rightscc.min]);
end);

# JDM: this could have a c method...

InstallMethod(NrHClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, nr, rid, rnr, count, sorted, lid, lnr, hid, hlookup, now, hindex, j, cur, i;
  
  data:=Enumerate(S);
  if not IsBound(data.rightscc) then 
    data.rightscc:=GABOW_SCC(Enumerate(S).right);
  fi;
  if not IsBound(data.leftscc) then 
    data.leftscc:=GABOW_SCC_DCLASSES(data.left, data.rightscc);
  fi;
  nr:=Length(data.elts);
  rid:=data.rightscc.id;      # lookup for index of R-class containing <elts[i]>
  rnr:=data.rightscc.count-1; # number of R-classes
  count:=[1..rnr+1]*0;        
  count[1]:=2;

  # count the number of elements in each R-class
  for i in [2..nr] do 
    count[rid[i]+1]:=count[rid[i]+1]+1;
  od;
  # count[id[i]] is the next available position to contain an element of R-class
  # with index id[i]. 
  for i in [2..rnr] do 
    count[i]:=count[i-1]+count[i];
  od;
  
  # List(sorted, i-> id[i])= id sorted, without the last element...
  sorted:=EmptyPlist(nr);
  for i in [2..nr] do 
    sorted[count[rid[i]]]:=i;
    count[rid[i]]:=count[rid[i]]+1;
  od;

  lid:=data.leftscc.id; # lookup for L-class indices
  hid:=[1..nr]*0;       # lookup for H-class indices
  now:=0;               # current R-class 
  hindex:=0;            # current H-class index
  hlookup:=[1..data.leftscc.count-1]*0; 
  # H-class corresponding to L-class in the current R-class <now>

  # browse the L-class table...
  for i in [2..nr] do
    j:=sorted[i];
    if rid[j]>now then # new R-class
      now:=rid[j]; 
      cur:=hindex; # H-class indices for elements of R-class <now> start at <cur+1>
    fi;
    if hlookup[lid[j]]<=cur then # then we have a new H-class, otherwise, this is an 
      hindex:=hindex+1;          # existing H-class in the current R-class. 
      hlookup[lid[j]]:=hindex;
    fi;
    hid[j]:=hlookup[lid[j]];
  od;
  data.hid:=hid;

  return hindex;
end);

# JDM: can probably do better than this by considering Green's classes.

InstallMethod(Idempotents, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, elts, idempotents, nr, i;

  data:=Enumerate(S);

  if not IsBound(data.idempotents) then 

    elts:=data.elts;
    idempotents:=EmptyPlist(Length(elts)-1);
    nr:=0;

    for i in [2..Length(elts)] do 
      if elts[i]*elts[i]=elts[i] then 
        nr:=nr+1;
        idempotents[nr]:=i;
      fi;
    od;
    
    data.idempotents:=idempotents;
  fi;

  return data.elts{data.idempotents};
end);


    




