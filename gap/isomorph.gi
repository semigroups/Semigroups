#############################################################################
##
#W  isomorph.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# returns the lex-least multiplication table of the semigroup <S>

if not IsGrapeLoaded then 
  
  InstallMethod(SmallestMultiplicationTable, "for a semigroup",
  [IsSemigroup],
  function(S)
    Info(InfoWarning, 1, GrapeIsNotLoadedString);
    return fail;
  end);  

else 
  
  InstallMethod(SmallestMultiplicationTable, "for a semigroup",
  [IsSemigroup],
  function(S)
    local LitNum, NumLit, diag2lits, tbl2lits, lits_to_tbl, onLiterals, n, mtS,
    diagS, phi, diaglitsS, minS, permS, tbllitsS, stabS;
     
    LitNum:=function(ln, n)
      return [QuoInt(ln-1,n^2)+1,QuoInt((ln-1) mod n^2,n)+1,(ln-1) mod n+1];
    end;

    NumLit:=function(lit,n)
      # lit = [ row, col, val ]
      return (lit[1]-1)*n^2 + (lit[2]-1)*n + lit[3];
    end;

    diag2lits:=function(diag,n)
      return List( [1..n], i -> NumLit( [i,i,diag[i]], n ) );
    end;

    tbl2lits:=function(table,n)
      local i,j,literals, val;
      literals:=[];
      for i in [1..n] do
        for j in [1..n] do
          val:= table[i][j];
          Add(literals, NumLit([i,j,val],n));
        od;
      od;

      return literals;
    end;
    
    lits_to_tbl:=function(lits, n)
      local table, i, j;
      table:=[];
      for i in [0..n-1] do 
        table[i+1]:=[];
        for j in [1..n] do 
          table[i+1][j]:=LitNum(lits[i*n+j], n)[3];
        od;
      od;
      return table;
    end;

    onLiterals:=n->function(ln,pi)
      local lit,imlit;
      lit:=LitNum(ln,n);
      imlit:=OnTuples(lit,pi);
      return NumLit(imlit,n);
    end; 

    # for not too big semigroups...
    n:=Size(S);
    mtS:=MultiplicationTable(S);
    diagS:=DiagonalOfMat(mtS);
    phi:=ActionHomomorphism(SymmetricGroup(n), [1..n^3], onLiterals(n));

    # get minimal representative of diagonal
    diaglitsS:=diag2lits(diagS, n);
    minS:=SmallestImageSet(Image(phi), diaglitsS);
    permS:=RepresentativeAction(Image(phi), diaglitsS, minS, OnSets);
    diagS:=List(minS, x->LitNum(x,n)[3]);

    # work with stabiliser of new diagonal on changed table
    tbllitsS:=OnSets( tbl2lits( mtS, n ), permS);
    stabS:=Stabilizer(Image(phi), minS, OnSets);

    return lits_to_tbl(SmallestImageSet(stabS, tbllitsS), n);
  end);

fi;

#

if not (IsGrapeLoaded and IsGrapeCompiled) then 
  InstallMethod(IsIsomorphicSemigroup, "for semigroups with generators",
  [IsSemigroup and HasGeneratorsOfSemigroup, IsSemigroup and
  HasGeneratorsOfSemigroup],  
  function(S, T)
    Info(InfoWarning, 1, GrapeIsNotCompiledString);
    return fail;
  end);  
else 
  InstallMethod(IsIsomorphicSemigroup, "for semigroups with generators",
  [IsSemigroup and HasGeneratorsOfSemigroup, IsSemigroup and
  HasGeneratorsOfSemigroup],  
  function(S, T)
    local pS, pT, iso;

    if Size(S)<>Size(T) or NrRClasses(S)<>NrRClasses(T) or 
      NrDClasses(S)<>NrDClasses(T) or NrLClasses(S)<>NrLClasses(T) or 
      NrHClasses(S)<>NrHClasses(T) or NrIdempotents(S)<>NrIdempotents(T) 
     then 
      return false;
    elif Size(S)=1 then 
      return true;
    elif Size(S)<32 or (HasSmallestMultiplicationTable(S) 
     and HasSmallestMultiplicationTable(T)) then 
      return SmallestMultiplicationTable(S)=SmallestMultiplicationTable(T);
    fi;
    
    # compare the partial orders of the D-classes

    pS:=Graph(Group(()), [1..NrDClasses(S)], OnPoints,
     function(i,j)
       return i in DirectedGraphTransitiveClosure(PartialOrderOfDClasses(S)[j]);
     end, true);

    pT:=Graph(Group(()), [1..NrDClasses(T)], OnPoints,
     function(i,j)
       return i in DirectedGraphTransitiveClosure(PartialOrderOfDClasses(T)[j]);
     end, true);
    
    iso:=GraphIsomorphism(pS, pT);
    
    if iso=fail then 
      return false;
    fi;
    Error("not yet implemented,");

  end);
fi;

#EOF
