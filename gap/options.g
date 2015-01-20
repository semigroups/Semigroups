#############################################################################
##
#W  options.g
#Y  Copyright (C) 2013-15                                 James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

BindGlobal("SemigroupsOptionsRec",
  rec(  small := false,
        hashlen := rec(S := 251, M := 6257, L := 25013),
        regular := false,
        acting := true
      ));

MakeReadWriteGlobal("SemigroupsOptionsRec");

BindGlobal("SemigroupOptions", function(record)
  local n, x;

  for x in RecNames(SemigroupsOptionsRec) do
    if not IsBound(record.(x)) then
      record.(x) := SemigroupsOptionsRec.(x);
    fi;
  od;

  if IsBound(record.hashlen) and IsPosInt(record.hashlen) then
    n := record.hashlen;
    record.hashlen := rec(S := NextPrimeInt(Int(n / 100)),
     M := NextPrimeInt(Int(n / 4)), L := NextPrimeInt(n));
  fi;

  return record;
end);
