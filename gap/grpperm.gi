


InstallGlobalFunction(IteratorSortedConjugateStabChain, 
function(S, conj)
   record := rec();
   record.rep := ();
   record.NextIterator := 

  
end);

#

InstallGlobalFunction(LargestElementConjugateStabChain,
function(S, rep, conj)
  local pnt, max, val, gen, i;
    
    if Length( S.generators ) = 0  then
      return rep;
    fi;
    
    pnt := S.orbit[1]^conj;
    max := 0;
    val := 0;
    
    for i in S.orbit  do
      if (i ^ conj) ^ rep > val  then
        max := i;
        val := (i ^ conj) ^ rep;
      fi;
    od;

    while pnt <> max  do
      gen := S.transversal[max] ^ conj;
      rep := LeftQuotient( gen, rep );
      max := max ^ gen;
    od;
    
    return LargestElementConjugateStabChain( S.stabilizer, rep, conj );
end);

#



