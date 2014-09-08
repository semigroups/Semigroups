




InstallGlobalFunction(SEMIGROUPS_ElementConjugateStabChain,
function(S, rep, conj, val, compare)
    local  min, pnt, i, gen;
    if Length( S.generators ) = 0  then
        return rep;
    fi;
    pnt := S.orbit[1]^conj;
    min := 0;
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
    return SEMIGROUPS_ElementConjugateStabChain( S.stabilizer, rep, conj, val,
      compare );
end);

InstallGlobalFunction(LargestElementConjugateStabChain,
function(S, rep, conj)
  return SEMIGROUPS_ElementConjugateStabChain(S, rep, conj,
    LargestMovedPoint(S.generators)+1, function(x,y) return x>y; end);
end);

InstallGlobalFunction(SmallestElementConjugateStabChain,
function(S, rep, conj)
  return SEMIGROUPS_ElementConjugateStabChain(S, rep, conj, 0, LT);
end);

