############################################################################# 
## 
#W  options.g 
#Y  Copyright (C) 2013-14                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

BindGlobal("SEMIGROUPS_OptionsRec", 
  rec(  small:=false,
        hashlen:=rec(S:=251, M:=6257, L:=25013),
        regular:=false,
        exhaustive:=false  ));

MakeReadWriteGlobal("SEMIGROUPS_OptionsRec");

BindGlobal("SEMIGROUPS_ProcessOptionsRec", function(record)
  local n, x;
  
  for x in RecNames(SEMIGROUPS_OptionsRec) do 
    if not IsBound(record.(x)) then 
      record.(x):=SEMIGROUPS_OptionsRec.(x);
    fi;
  od;

  if IsBound(record.hashlen) and IsPosInt(record.hashlen) then 
    n:=record.hashlen;
    record.hashlen:=rec(S:=NextPrimeInt(Int(n/100)),
     M:=NextPrimeInt(Int(n/4)), L:=NextPrimeInt(n));         
  fi;

  return record;
end);

DeclareAttribute("SemigroupOptions", IsSemigroup);

InstallMethod(SemigroupOptions, "for a semigroup", [IsSemigroup], 
function(S)
  return SEMIGROUPS_OptionsRec;
end);
