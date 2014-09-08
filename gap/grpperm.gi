




InstallGlobalFunction(SEMIGROUPS_ElementConjugateStabChain,
function(S, rep, conj, largest)
  local pnt, min, val, compare, gen, i;
    
    if Length( S.generators ) = 0  then
        return rep;
    fi;
    pnt := S.orbit[1]^conj;
    min := 0;

    if largest then 
      val := 0;
      compare := function(x, y) return x > y; end;
    else
      val :=  LargestMovedPoint(S.generators) + 1;
      compare := LT;
    fi;
    
    for i  in S.orbit  do
        if compare((i ^ conj) ^ rep, val)  then
            min := i;
            val := (i ^ conj) ^ rep;
        fi;
    od;
    while pnt <> min  do
        gen := S.transversal[min] ^ conj;
        rep := LeftQuotient( gen, rep );
        min := min ^ gen;
    od;
    return SEMIGROUPS_ElementConjugateStabChain(S.stabilizer, rep, conj, largest);
end);

InstallGlobalFunction(LargestElementConjugateStabChain,
function(S, rep, conj)
  return SEMIGROUPS_ElementConjugateStabChain(S, rep, conj, true);
end);

InstallGlobalFunction(SmallestElementConjugateStabChain,
function(S, rep, conj)
  return SEMIGROUPS_ElementConjugateStabChain(S, rep, conj, false);
end);

