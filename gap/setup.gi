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
[IsGreensClass], x-> IsActingSemigroup(Parent(x)));

# the number of points in the action

InstallOtherMethod(ActionDegree, 
"for associative element with action collection",
[IsAssociativeElementWithActionCollection], 
s-> ActionDegree(Representative(s)));

InstallOtherMethod(ActionDegree, "for a partial perm collection",
[IsPartialPermCollection], DegreeOfPartialPermCollection);

#

InstallMethod(ActionDegree, "for a transformation",
[IsTransformation], DegreeOfTransformation);

InstallMethod(ActionDegree, "for a partial perm",
[IsPartialPerm], RankOfPartialPerm);

InstallMethod(ActionDegree, "for a bipartition",
[IsBipartition], x-> x[1]/2);

# the number of points in the range of the action

InstallMethod(ActionRank, "for a transformation",
[IsTransformation], RankOfTransformation);

InstallMethod(ActionRank, "for a partial perm",
[IsPartialPerm], RankOfPartialPerm);

InstallMethod(ActionRank, "for a bipartition",
[IsBipartition], function(x)
  local y;

  y:=LeftSignedPartition(x);
  return Number(y{[Length(y)-y[1]+1..Length(y)]}, x-> x=1);
end);

# the minimum possible rank of an element

InstallMethod(MinActionRank, "for a transformation semigroup",
[IsTransformationSemigroup], x-> 1);

InstallMethod(MinActionRank, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> 0);

InstallMethod(MinActionRank, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> 0);

# options passed to LambdaOrb(s) when it is created

InstallMethod(LambdaOrbOpts, "for a transformation semigroup",
[IsTransformationSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(LambdaOrbOpts, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(RhoOrbOpts, "for a transformation semigroup",
[IsTransformationSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(RhoOrbOpts, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> rec(forflatplainlists:=true));

InstallMethod(RhoOrbOpts, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> rec(forflatplainlists:=true));

# the lambda and rho acts

InstallMethod(LambdaAct, "for a transformation semi",
[IsTransformationSemigroup], x-> OnPosIntSetsTrans);

InstallMethod(RhoAct, "for a transformation semi",
[IsTransformationSemigroup], x-> ON_KERNEL_ANTI_ACTION);

InstallMethod(LambdaAct, "for a partial perm semi",
[IsPartialPermSemigroup], x-> OnPosIntSetsPartialPerm);

# JDM new c method for this!
InstallMethod(RhoAct, "for a partial perm semi",
[IsPartialPermSemigroup], s->       
  function(set, f) 
    return OnSets(set, f^-1);
  end);

InstallMethod(LambdaAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnRightSignedPartition);

InstallMethod(RhoAct, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> OnLeftSignedPartition);

# the seed or dummy start point for LambdaOrb

InstallMethod(LambdaOrbSeed, "for a transformation semi",
[IsTransformationSemigroup], s-> [0]);

InstallMethod(LambdaOrbSeed, "for a partial perm semi",
[IsPartialPermSemigroup], s-> [0]);

InstallMethod(LambdaOrbSeed, "for a bipartition semi",
[IsBipartitionSemigroup], s-> [65536]);

# the seed or dummy start point for RhoOrb

InstallMethod(RhoOrbSeed, "for a transformation semi",
[IsTransformationSemigroup], s->[0]);

InstallMethod(RhoOrbSeed, "for a partial perm semi",
[IsPartialPermSemigroup], s-> [0]);

InstallMethod(RhoOrbSeed, "for a bipartition semi",
[IsBipartitionSemigroup], s-> [65536]);

# the function calculating the lambda or rho value of an element

InstallMethod(LambdaFunc, "for a transformation semigroup",
[IsTransformationSemigroup], x-> IMAGE_SET_TRANS);

InstallMethod(LambdaFunc, "for a partial perm semigroup",
[IsPartialPermSemigroup], x-> IMAGE_SET_PPERM);

InstallMethod(LambdaFunc, "for a bipartition semigroup",
[IsBipartitionSemigroup], x-> RightSignedPartition);

InstallMethod(RhoFunc, "for a trans semi",
[IsTransformationSemigroup], x-> FLAT_KERNEL_TRANS);

InstallMethod(RhoFunc, "for a partial perm semi",
[IsPartialPermSemigroup], x-> DOMAIN_PPERM);

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
[IsPartialPermSemigroup], s-> function(x, f) return f^-1; end); 

#JDM c method this is not correct!
# do the example of PartitionMonoid(3); then the lambdaorbmult(o,3,4)[1] does
# not move the point o[3] to o[4]. 
InstallMethod(LambdaInverse, "for a bipartition",
[IsBipartitionSemigroup], s-> INV_SIGNED_PART_BIPART);

# if g=RhoInverse(X, f) and f^X=Y (this is a left action), then g^Y=X and g
# acts on the left like the inverse of g on Y. 

InstallMethod(RhoInverse, "for a transformation semi",
[IsTransformationSemigroup], s-> INV_KER_TRANS);

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

InstallMethod(LambdaPerm, "for a partial perm semi",
[IsPartialPermSemigroup], s-> PERM_LEFT_QUO_PPERM_NC);

#JDM c method for this!
InstallMethod(LambdaPerm, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> 
  function(a, b)
    local n, p, aa, bb, i;
    n:=a[1]/2; #degree
    p:=[1..n]; #nr classes
    aa:=a{[n+3..2*n+2]};
    bb:=b{[n+3..2*n+2]};
    #return MappingPermListList(aa,bb);
    for i in [1..n] do 
      p[Position(aa,aa[i])]:=Position(bb, aa[i]);
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
  return MappingPermListList(IMAGE_PPERM(f), IMAGE_PPERM(g));
end);

# this method is identical to the method for LambdaPerm, I'm not sure it does
# the correct thing.

InstallMethod(LambdaConjugator, "for a bipartition semigroup",
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

# the function used to test if there is an idempotent with the specified 
# lambda and rho values.

InstallMethod(IdempotentTester, "for a transformation semigroup", 
[IsTransformationSemigroup], s-> 
function(img, ker)
  return IS_INJECTIVE_LIST_TRANS(img, ker) and Length(img)=MaximumList(ker);
end);

InstallMethod(IdempotentTester, "for a partial perm semigroup", 
[IsPartialPermSemigroup], s-> EQ);

# currently returns false negatives, but apparently not false positives

InstallMethod(IdempotentTester, "for a bipartition semigroup",
[IsBipartitionSemigroup], s-> 
function(left, right)
  local n, nr_left, nr_right, fuse, conn, fuseit, x, y, seen, i;

  n:=Length(left)-left[1]-1;
  nr_left:=left[1];
  nr_right:=right[1];
  
  fuse:=[1..nr_left+nr_right];
  conn:=left{[2+n..Length(left)]};

  fuseit:=function(i)
    while fuse[i]<i do 
      i:=fuse[i];
    od;
    return i;
  end;

  for i in [1..n] do 
    x:=fuseit(left[i+1]);
    y:=fuseit(right[i+1]+nr_left);
    if x<>y then 
      if x<y then 
        fuse[y]:=x;
      else
        fuse[x]:=y;
      fi;
    fi;
  od;
  
  for i in [1..nr_left] do 
    if left[n+1+i]=1 then   
      conn[fuseit(i)]:=1;
    fi;
  od;

  seen:=[1..nr_left]*0;
  for i in [1..nr_right] do 
    if right[i+n+1]=1 then 
      i:=fuseit(i+nr_left);
      if seen[i]=1 or conn[i]=0 then 
        return false;
      else
        seen[i]:=1;
      fi;
    fi;
  od;
  return true;
end);

#foo:=function(f)
#  return tester(LeftSignedPartition(f), RightSignedPartition(f));
#end;
#
#foo2:=function(f)
#  return tester2(ImageSetOfTransformation(f), FlatKernelOfTransformation(f));
#end;

# the function used to create an idempotent with the specified lambda and rho
# values. 

InstallMethod(IdempotentCreator, "for a transformation semigroup",
[IsTransformationSemigroup], s-> IDEM_IMG_KER_NC);

InstallMethod(IdempotentCreator, "for a partial perm semigp",
[IsPartialPermSemigroup], s-> PartialPermNC);

InstallMethod(IdempotentCreator, "for a bipartition semigroup",
[IsBipartitionSemigroup], s->
function(left, right)
  local n, nr_left, nr_right, fuse, fuseit, x, y, c, tab, next, i;
  
  n:=Length(left)-left[1]-1;
  nr_left:=left[1];
  nr_right:=right[1];
  
  fuse:=[1..nr_left+nr_right];

  fuseit:=function(i)
    while fuse[i]<i do 
      i:=fuse[i];
    od;
    return i;
  end;

  for i in [1..n] do 
    x:=fuseit(left[i+1]);
    y:=fuseit(right[i+1]+nr_left);
    if x<>y then 
      if x<y then 
        fuse[y]:=x;
      else
        fuse[x]:=y;
      fi;
    fi;
  od;
  
  c := left{[2..n+1]};
  next := nr_left+1;
  tab:=[1..nr_right]*0;

  for i in [1..n] do
    if right[right[i+1]+n+1]=0 then 
      if tab[right[i+1]]=0 then 
        tab[right[i+1]]:=next;
        next:=next+1;
      fi;
      Add(c, tab[right[i+1]]); 
    else
      x := fuseit(right[i+1]+nr_left);
      x:=First([1..nr_left], i-> fuseit(i)=x and left[i+n+1]=1);
      Add(c, x);
    fi;
  od;
  return BipartitionByIntRepNC(c);
end);

#foo:=function(f)
#  return creator(LeftSignedPartition(f), RightSignedPartition(f));
#end;

# GroupElementAction will be \* for transformation and partial perm semigroups 
# and something else for semigroups of bipartitions.

InstallMethod(GroupElementAction, 
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s-> PROD);

InstallMethod(GroupElementAction, 
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
s-> PROD);

# IsInSubgroupOfSemigroup returns <true> if the element <f> is an element of 
# a subgroup of any semigroup containing it.

InstallMethod(IsInSubgroupOfSemigroup, "for a transformation",
[IsTransformation],
function(f) 
  return IdempotentTester(IMAGE_SET_TRANS(f), FLAT_KERNEL_TRANS(f));
end);

InstallMethod(IsInSubgroupOfSemigroup, "for a partial perm",
[IsPartialPerm],
function(f) 
  return DOMAIN_PPERM(f)=IMAGE_SET_PPERM(f);
end);

InstallMethod(IsInSubgroupOfSemigroup, "for a bipartition",
[IsBipartition],
function(f) 
  return RightSignedPartition(f)=LeftSignedPartition(f);
end);

# IsActingSemigroupWithFixedDegreeMultiplication should be <true> if and only
# if it is only possible to multiply elements of the type in the semigroup with
# the equal degrees.

InstallTrueMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
IsTransformationSemigroup);

InstallMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
"for a partial perm semigroup", [IsPartialPermSemigroup], ReturnFalse);

InstallTrueMethod(IsActingSemigroupWithFixedDegreeMultiplication, 
IsBipartitionSemigroup);

#EOF
