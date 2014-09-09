

MySift:=function(S, g, factor)
  local bpt, img;

  while IsBound( S.stabilizer ) and g <> S.identity do
    bpt := S.orbit[ 1 ];
    img := bpt ^ g;
    if IsBound( S.transversal[ img ] )  then
      while img <> bpt do
        Add(factor, S.transversal[ img ]);
        g := g * S.transversal[ img ];
        img := bpt ^ g;
      od;
      S := S.stabilizer;
    else
      return factor;
    fi;
  od;
  return List(Reversed(factor), x-> x^-1);
end;

tmp:=function(S, rep, indices, level)
  local pnt, max, val, gen, i;

   if Length( S.generators ) = 0  then
      return rep;
    fi;
    
    pnt := S.orbit[1];
    max := indices[level];
    val := indices[level] ^ rep;
    
    while pnt <> max  do
      gen := S.transversal[max];
      rep := LeftQuotient( gen, rep );
      max := max ^ gen;
    od;
    
    return tmp( S.stabilizer, rep, indices, level+1);
end;

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



