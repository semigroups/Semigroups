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

#

InstallMethod(IsActingSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x-> IsActingSemigroup(ParentSemigroup(x)));

# the number of points in the action

InstallOtherMethod(ActionDegree, "for an acting semigroup",
[IsActingSemigroup], s-> ActionDegree(Representative(s)));

InstallOtherMethod(ActionDegree, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> DegreeOfPartialPermCollection(s));

#

InstallMethod(ActionDegree, "for a transformation",
[IsTransformation], DegreeOfTransformation);

InstallMethod(ActionDegree, "for a partial perm",
[IsPartialPerm], x-> x[2]);

InstallMethod(ActionDegree, "for a bipartition",
[IsBipartition], x-> x[1]/2);

# the number of points in the range of the action

InstallMethod(ActionRank, "for a transformation",
[IsTransformation], RankOfTransformation);

InstallMethod(ActionRank, "for a partial perm",
[IsPartialPerm], x-> x[2]);

InstallMethod(ActionRank, "for a bipartition",
[IsBipartition], x-> x[2]);

# options passed to LambdaOrb(s) when it is created

InstallMethod(LambdaOrbOpts, "for a transformation semigroup",
[IsTransformationSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a partial perm semigroup",
[IsBipartitionSemigroup], s-> rec(forflatplainlists:=true));

# the lambda and rho acts

InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnSets);

InstallMethod(RhoAct, "for a transformation semi",
[IsTransformationSemigroup], x-> ON_KERNEL_ANTI_ACTION);

InstallMethod(LambdaAct, "for a partial perm semi",
[IsPartialPermSemigroup], x-> OnIntegerSetsWithPP);

# JDM new c method for this!
InstallMethod(RhoAct, "for a partial perm semi",
[IsPartialPermSemigroup], s->       
  function(set, f) 
    return OnIntegerSetsWithPP(set, f^-1);
  end);

InstallMethod(LambdaAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnRightSignedPartition);

InstallMethod(RhoAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnLeftSignedPartition);

# the seed or dummy start point for LambdaOrb

InstallMethod(LambdaOrbSeed, "for a transformation semi",
[IsTransformationSemigroup], s-> [65536]);

InstallMethod(LambdaOrbSeed, "for a partial perm semi",
[IsPartialPermSemigroup], s-> [65536]);

InstallMethod(LambdaOrbSeed, "for a bipartition semi",
[IsBipartitionSemigroup], s-> [65536]);

# the seed or dummy start point for RhoOrb

InstallMethod(RhoOrbSeed, "for a transformation semi",
[IsTransformationSemigroup], s->[65536]);

InstallMethod(RhoOrbSeed, "for a partial perm semi",
[IsPartialPermSemigroup], s-> [65536]);

InstallMethod(RhoOrbSeed, "for a bipartition semi",
[IsBipartitionSemigroup], s-> [65536]);

# the function calculating the lambda or rho value of an element

InstallMethod(LambdaFunc, "for a transformation semigroup",
[IsTransformationSemigroup], x-> IMAGE_SET_TRANS);

InstallMethod(LambdaFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> RightSignedPartition);

InstallMethod(LambdaFunc, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> RanSetPP);

InstallMethod(RhoFunc, "for a trans semi",
[IsTransformationSemigroup], x-> FLAT_KERNEL_TRANS);

InstallMethod(RhoFunc, "for a partial perm semi",
[IsPartialPermSemigroup], x-> DomPP);

InstallMethod(RhoFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> LeftSignedPartition);

# the function used to calculate the rank of lambda or rho value

InstallMethod(LambdaRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(LambdaRank, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> y-> Number(y{[y[1]+2..2*y[1]+1]}, x-> x=1));

InstallMethod(RhoRank, "for a transformation semigroup", 
[IsTransformationSemigroup], x-> MaximumList);

InstallMethod(RhoRank, "for a semigroup of partial perms", 
[IsPartialPermSemigroup], x-> Length);

InstallMethod(RhoRank, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> y-> Number(y{[y[1]+2..2*y[1]+1]}, x-> x=1));

# if g=LambdaInverse(X, f) and X^f=Y, then Y^g=X and g acts on the right 
# like the inverse of f on Y.

InstallMethod(LambdaInverse, "for a transformation semigroup",
[IsTransformationSemigroup], s-> INV_LIST_TRANS);

InstallMethod(LambdaInverse, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> function(x, f) return InvPP(f); end); 

#JDM c method
InstallMethod(LambdaInverse, "for a bipartition",
[IsBipartitionSemigroup], s-> function(x, f) return f^-1; end);

# if g=RhoInverse(X, f) and f^X=Y (this is a left action), then g^Y=X and g
# acts on the left like the inverse of g on Y. 

InstallMethod(RhoInverse, "for a transformation semi",
[IsTransformationSemigroup], s-> INV_KER_TRANS);
#  function(ker, f)
#    local g, n, m, lookup, i, j;
#  
#    g:=ker{IMAGE_TRANS(f)};
#    n:=DegreeOfTransformation(f); 
#    m:=MaximumList(ker);
#    lookup:=EmptyPlist(n);
#    
#    i:=0; j:=0;
#    repeat 
#      i:=i+1;
#      if not IsBound(lookup[g[i]]) then 
#        lookup[g[i]]:=i;
#        j:=j+1;
#      fi;
#    until j=m;
#    return TransformationNC(List([1..n], i-> lookup[ker[i]]));
#  end);

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

# LambdaPerm(s) returns a permutation from two acting semigroup elements with
# equal LambdaFunc and RhoFunc. This is required to check if one of the two
# elements belongs to the schutz gp of a lambda orb.

InstallMethod(LambdaPerm, "for a transformation semi",
[IsTransformationSemigroup], s-> PERM_LEFT_QUO_TRANS_NC);

#JDM c method for this!
InstallMethod(LambdaPerm, "for a partial perm semi",
[IsPartialPermSemigroup], s-> function(f,g)
  local h;
  h:=f^-1*g;
  return MappingPermListList(DomPP(h), RanPP(h)); 
end);

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

# returns a permutation mapping LambdaFunc(s)(f) to LambdaFunc(s)(g) so that 
# gf^-1(i)=p(i) when RhoFunc(s)(f)=RhoFunc(s)(g)!!

InstallMethod(LambdaConjugator, "for a transformation semigroup",
[IsTransformationSemigroup], s-> TRANS_IMG_CONJ);

# c method
InstallMethod(LambdaConjugator, "for a partial perm semi",
[IsPartialPermSemigroup], s-> 
function(f, g)
  return MappingPermListList(RanPP(f), RanPP(g));
end);

# the function used to test if there is an idempotent with the specified 
# lambda and rho values.

InstallMethod(IdempotentTester, "for a trans semigp", 
[IsTransformationSemigroup], s-> 
function(img, ker)
  return IS_INJECTIVE_LIST_TRANS(img, ker) and Length(img)=MaximumList(ker);
end);

InstallMethod(IdempotentTester, "for a partial perm semigp", 
[IsPartialPermSemigroup], s-> EQ);

# the function used to create an idempotent with the specified lambda and rho
# values. 

InstallMethod(IdempotentCreator, "for a trans semigp",
[IsTransformationSemigroup], s-> IDEM_IMG_KER_NC);

InstallMethod(IdempotentCreator, "for a partial perm semigp",
[IsPartialPermSemigroup], s-> PartialPermNC);

# GroupElementAction will be \* for transformation and partial perm semigroups 
# and something else for semigroups of bipartitions.

#EOF
