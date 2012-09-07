#############################################################################
###
##W  setup.gi
##Y  Copyright (C) 2011-12                                James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

###############################################################################
# Setup - install the basic things required for specific acting semigroups    #
###############################################################################

InstallMethod(ActionDegree, "for a transformation",
[IsTransformation], x-> x[1]);

InstallMethod(ActionDegree, "for a bipartition",
[IsBipartition], x-> x[1]/2);

InstallMethod(ActionDegree, "for a partial perm",
[IsPartialPerm], x-> x[2]);

InstallMethod(ActionRank, "for a transformation",
[IsTransformation], x-> x[2]);

InstallMethod(ActionRank, "for a bipartition",
[IsBipartition], x-> x[2]);

InstallMethod(ActionRank, "for a partial perm",
[IsPartialPerm], x-> x[2]);

# new for 1.0! - LambdaOrbOpts 

InstallMethod(LambdaOrbOpts, "for a transformation semigroup",
[IsTransformationSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a partial perm semigroup",
[IsBipartitionSemigroup], s-> rec(forflatplainlists:=true));

# new for 1.0 - LambdaAct and RhoAct
###############################################################################

InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnIntegerSetsWithT);

InstallMethod(RhoAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnKerT);

InstallMethod(LambdaAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnRightSignedPartitionWithBipartition);

InstallMethod(RhoAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnLeftSignedPartitionWithBipartition);

if IsBound(OnIntegerSetsWithPP) then 
  InstallMethod(LambdaAct, "for a partial perm semi",
  [IsPartialPermSemigroup], x-> OnIntegerSetsWithPP);
  
  InstallMethod(RhoAct, "for a partial perm semi",
  [IsPartialPermSemigroup], 
  function(s)
    return 
      function(set, f) 
        return OnIntegerSetsWithPP(set, f^-1);
      end;
  end);
  
  #InstallMethod(RhoAct, "for an inverse semigp of partial perms",
  #[IsInverseSemigroup and IsPartialPermSemigroup],
  #s-> OnIntegerSetsWithPP);
fi;

# new for 1.0! - LambdaDegree
###############################################################################

InstallMethod(LambdaDegree, "for an acting semigroup", 
[IsActingSemigroup], s-> Length(LambdaDomain(s)));

# new for 1.0! - LambdaDomain
###############################################################################

InstallMethod(LambdaDomain, "for a transformation semi",
[IsTransformationSemigroup], s-> [1..65536]*1);

InstallMethod(LambdaDomain, "for a partial perm semi",
[IsPartialPermSemigroup], s-> [1..65536]*1);

InstallMethod(LambdaDomain, "for a bipartition semi",
[IsBipartitionSemigroup], s-> [65536]);

InstallMethod(RhoDomain, "for a transformation semi",
[IsTransformationSemigroup], s-> [1..65536]*1);

InstallMethod(RhoDomain, "for a partial perm semi",
[IsPartialPermSemigroup], s-> [1..65536]*1);

InstallMethod(RhoDomain, "for a bipartition semi",
[IsBipartitionSemigroup], s-> [65536]);

# new for 1.0! - LambdaFunc
###############################################################################

InstallMethod(LambdaFunc, "for a transformation semigroup",
[IsTransformationSemigroup], x-> RanSetT);

InstallMethod(LambdaFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> RightSignedPartition);

if IsBound(RanSetPP) then
  InstallMethod(LambdaFunc, "for a partial perm semigroup",
    [IsPartialPermSemigroup], x-> RanSetPP);
fi;

# new for 1.0! - LambdaInverse
###############################################################################
#JDM c methods!

InstallMethod(LambdaInverse, "for a transformation semigroup",
[IsTransformationSemigroup], s-> 
  function(im, f)
    local i, j, n, k, out;

    out:=List([1..f[1]], x-> 1);
    
    for i in im do 
      out[f[i+2]]:=i;
    od;

    return TransformationNC(out);
  end);

InstallMethod(LambdaInverse, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> 
function(ran, f)
  return f^-1;
end);

InstallMethod(LambdaInverse, "for a bipartition",
[IsBipartitionSemigroup], s-> 
function(x, f)
  return f^-1;
end);

# new for 1.0! - RhoInverse 
###############################################################################
#JDM c method for this!

# returns an acting semigroup element acting like the inverse of f on 
# the specified rho value. 

#JDM this could be better since where ever we use it we also know what f is
#mapping onto ker!

InstallMethod(RhoInverse, "for a transformation semi",
[IsTransformationSemigroup], s-> 
  function(ker, f)
    local g, n, m, lookup, i, j;
  
    g:=ker{RanT(f)};
    n:=f[1]; 
    m:=MaximumList(ker);
    lookup:=EmptyPlist(n);
    
    i:=0; j:=0;
    repeat 
      i:=i+1;
      if not IsBound(lookup[g[i]]) then 
        lookup[g[i]]:=i;
        j:=j+1;
      fi;
    until j=m;
    return TransformationNC(List([1..n], i-> lookup[ker[i]]));
  end);

InstallMethod(RhoInverse, "for a partial perm semi",
[IsPartialPermSemigroup], s-> 
  function(dom, f)
    return f^-1;
  end);

InstallMethod(RhoInverse, "for a bipartition",
[IsBipartitionSemigroup], s-> 
  function(x, f)
    return f^-1;
  end);


# new for 1.0! - LambdaPerm
###############################################################################
# LambdaPerm(s) returns a permutation from two acting semigroup elements with
# equal LambdaFunc and RhoFunc. This is required to check if one of the two
# elements belongs to the schutz gp of a lambda orb.

InstallMethod(LambdaPerm, "for a transformation semi",
[IsTransformationSemigroup], s-> PermLeftQuoTransformationNC);

#JDM c method for this!

if IsBound(DomPP) and IsBound(RanPP) then 
  InstallMethod(LambdaPerm, "for a partial perm semi",
  [IsPartialPermSemigroup], s-> function(f,g)
    local h;
    h:=f^-1*g;
    return MappingPermListList(DomPP(h), RanPP(h)); 
  end);
fi;

#JDM c method for this!

InstallMethod(LambdaPerm, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> 
  function(a, b)
    local n, p, i;

    n:=a[1]/2; #degree
    p:=[1..a[2]]; #rank
    for i in [1..n] do 
      p[a[n+i+2]]:=b[n+i+2];
    od;
    return PermList(p);
  end);

# new for 1.0! - LambdaConjugator
###############################################################################
# returns a permutation mapping LambdaFunc(s)(f) to LambdaFunc(s)(g) so that 
# gf^-1(i)=p(i) when RhoFunc(s)(f)=RhoFunc(s)(g)!!

#JDM c method for both of these...
# this is really a generalised LambdaPerm, so maybe it should be combined with 
# LambdaPerm?

InstallMethod(LambdaConjugator, "for a transformation semi",
[IsActingSemigroup], s-> 
  function(f, g) 
    return MappingPermListList(RanT(f), RanT(g));
  end);

if IsBound(RanPP) then 
  InstallMethod(LambdaConjugator, "for a partial perm semi",
  [IsPartialPermSemigroup], s-> 
    function(f, g)
      return MappingPermListList(RanPP(f), RanPP(g));
    end);
fi;

# new for 1.0! - LambdaRank and RhoRank
###############################################################################

InstallMethod(LambdaRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(RhoRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> MaximumList);

InstallMethod(LambdaRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> y-> Number(y{[y[1]+2..Length(y)]}, x-> x=1));

InstallMethod(RhoRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

# new for 1.0! - RhoFunc
###############################################################################

InstallMethod(RhoFunc, "for a trans semi",
[IsTransformationSemigroup], x-> KerT);

InstallMethod(RhoFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> LeftSignedPartition);

if IsBound(DomPP) then
  InstallMethod(RhoFunc, "for a partial perm semi",
   [IsPartialPermSemigroup], x-> DomPP);
fi;

# new for 1.0! - IdempotentLambdaRhoTester - "for a trans semigp"
##############################################################################
#JDM this should be revised.

InstallMethod(IdempotentLambdaRhoTester, "for a trans semigp", 
[IsTransformationSemigroup], s-> function(x, y) 
return IsInjectiveTransOnList(y, x); end);

# new for 1.0! - IdempotentLambdaRhoTester - "for a partial perm semigp"
##############################################################################

InstallMethod(IdempotentLambdaRhoTester, "for a partial perm semigp", 
[IsPartialPermSemigroup], s-> EQ);

# new for 1.0! - IdempotentLambdaRhoCreator - "for a trans semigp"
##############################################################################
#JDM we should update/replace IdempotentNC.

InstallMethod(IdempotentLambdaRhoCreator, "for a trans semigp",
[IsTransformationSemigroup], s-> 
function(x,y)
return IdempotentNC(y,x); end);

# new for 1.0! - IdempotentLambdaRhoCreator - "for a partial perm semigp"
##############################################################################

InstallMethod(IdempotentLambdaRhoCreator, "for a partial perm semigp",
[IsPartialPermSemigroup], s-> PartialPermNC);


