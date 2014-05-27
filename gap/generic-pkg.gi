###########################################################################
##
#W  generic-pkg.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Code for generic semigroups (i.e. ones which know \*), ties in the
# Froidure-Pin Semigroupe algorithm to GAP methods. This file only contains
# methods for operations (etc) that are *only* available in the Semigroups
# package.

# JDM: probably have a global function or attribute RightCayleyGraphSCC or
# similar.

InstallMethod(NrRClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.rightscc) then 
    data!.rightscc:=GABOW_SCC(data!.right);
  fi;
    
  return data!.rightscc.count;
end);

#

InstallMethod(NrLClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.leftscc) then 
    if not IsBound(data!.rightscc) then 
      data!.rightscc:=GABOW_SCC(data!.right);
    fi;

    data!.leftscc:=GABOW_SCC_DCLASSES(data!.left, data!.rightscc);
  fi;
    
  return data!.leftscc.count;
end);

#

InstallMethod(NrDClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.leftscc) then 
    if not IsBound(data!.rightscc) then 
      data!.rightscc:=GABOW_SCC(data!.right);
    fi;

    data!.leftscc:=GABOW_SCC_DCLASSES(data!.left, data!.rightscc);
  fi;
    
  return data!.leftscc.nrdclasses;
end);

#

InstallMethod(MinimalIdeal, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.rightscc) then 
    data!.rightscc:=GABOW_SCC(data!.right);
  fi;
  return SemigroupIdeal(S, data!.elts[data!.rightscc.min]);
end);

# JDM: this could have a c method...

InstallMethod(NrHClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, nr, rid, rnr, count, sorted, lid, lnr, hid, hlookup, now, hindex, j, cur, i;
  
  data:=Enumerate(PinData(S));
  
  if not IsBound(data!.rightscc) then 
    data!.rightscc:=GABOW_SCC(data!.right);
  fi;
  if not IsBound(data!.leftscc) then 
    data!.leftscc:=GABOW_SCC_DCLASSES(data!.left, data!.rightscc);
  fi;

  nr:=Length(data!.elts);
  rid:=data!.rightscc.id;      # lookup for index of R-class containing <elts[i]>
  rnr:=data!.rightscc.count;   # number of R-classes
  count:=[1..rnr+1]*0;   
  count[1]:=1;

  # count the number of elements in each R-class
  for i in [1..nr] do 
    count[rid[i]+1]:=count[rid[i]+1]+1;
  od;
  # count[rid[i]] is the next available position to contain an element of R-class
  # with index rid[i]. 
  for i in [2..rnr] do 
    count[i]:=count[i-1]+count[i];
  od;
  
  # List(sorted, i-> rid[i])= rid sorted, without the last element...
  sorted:=EmptyPlist(nr);
  for i in [1..nr] do 
    sorted[count[rid[i]]]:=i;
    count[rid[i]]:=count[rid[i]]+1;
  od;

  lid:=data!.leftscc.id; # lookup for L-class indices
  hid:=[1..nr]*0;       # lookup for H-class indices
  now:=0;               # current R-class 
  hindex:=0;            # current H-class index
  hlookup:=[1..data!.leftscc.count]*0; 
  # H-class corresponding to L-class in the current R-class <now>

  # browse the L-class table...
  for i in [1..nr] do
    j:=sorted[i];
    if rid[j]>now then # new R-class
      now:=rid[j]; 
      cur:=hindex; 
      # H-class indices for elements of R-class <now> start at <cur+1>
    fi;
    if hlookup[lid[j]]<=cur then 
      # then we have a new H-class, otherwise, this is an 
      hindex:=hindex+1;          # existing H-class in the current R-class. 
      hlookup[lid[j]]:=hindex;
    fi;
    hid[j]:=hlookup[lid[j]];
  od;
  data!.hid:=hid;

  return hindex;
end);

