#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Note about the difference between One and MultiplicativeNeutralElement 
# (the same goes for Zero and MultplicativeZero):
#
# One(s) returns One(Representative(s)) if it belongs to s, so that 
# One(s)=Transformation([1..DegreeOfTransformationSemigroup(s)]) if s is a
# transformation semigroup and it returns fail otherwise, or it returns 
# PartialPerm([1..DegreeOfPartialPermSemigroup]) if this belongs to s. 
#
# MultiplicativeNeutralElement on the other hand returns the element of s that
# acts as the identity, note that this can be equal to One(s) but it can also
# not be equal t One(s). 
#
# A semigroup satisfies IsMonoidAsSemigroup(s) if
# MultiplicativeNeutralElement(x)<>fail, so it could be that One(s) returns
# fail but IsMonoidAsSemigroup is still true. 

# Note that a semigroup satisfies IsTransformationMonoid or IsPartialPermMonoid
# only if One(s)<>fail. 

#

InstallMethod(AntiIsomorphismTransformationSemigroup, 
"for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function ( s )
local  en, gens, mapfun;

  en := EnumeratorSorted( s );
  mapfun := function ( a )
  return
  Transformation( Concatenation( List( [ 1 .. Length( en ) ], function ( i )
  return Position( en, a* en[i]); end ), [ Position( en, a ) ] ) );
  end;
  gens := List( GeneratorsOfSemigroup( s ), function ( x )
  return mapfun( x );
  end );
  return MagmaHomomorphismByFunctionNC( s, Semigroup( gens ), mapfun );
end);

#JDM incomplete

InstallMethod(EmbeddingNC, "for a perm group and an acting semigroup",
[IsPermGroup, IsActingSemigroup],
function(g, s)
  local convert, creator, one, t, emb, conj;
 
  if IsTransformationSemigroup(s) then 
    convert:=x-> AsTransformation(x, ActionDegree(s));
  elif IsPartialPermSemigroup(s) then 
    convert:=x-> AsPartialPerm(x, DomainOfPartialPermCollection(s));
    # JDM this won't work in general if Points(s) is not the correct set to act
    # on.
  fi;

  if IsMonoid(s) then 
    creator:=Monoid;
  else
    creator:=Semigroup;
  fi;

  if NrMovedPoints(g)<=ActionDegree(s) then 
    conj:=MappingPermListList(MovedPoints(g), [1..NrMovedPoints(g)]);
    emb:=x-> convert(x^conj);
    t:=creator(List(GeneratorsOfGroup(g), emb));
  else
    Error("the number of moved points of the group is greater than the ",
    "degree of the semigroup");
    return;
  fi;

  return MappingByFunction(g, t, emb, AsPermutation); 
end);

#GGG

InstallMethod(GroupOfUnits, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, m, g, emb, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  m:=Size(r); g:=Group(AsPermutation(Random(r)));

  while Size(g)<m do
    g:=ClosureGroup(g, AsPermutation(Random(r)));
  od;
  
  emb:=EmbeddingNC(g, s);
  u:=Range(emb);
  SetIsomorphismPermGroup(u, InverseGeneralMapping(emb));
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(IdempotentGeneratedSubsemigroup, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Semigroup(Idempotents(s), rec(small:=true)));

#

InstallMethod(IdempotentGeneratedSubsemigroup, 
"for an acting semigroup with inverses",
[IsActingSemigroupWithInverseOp],
s-> InverseSemigroup(Idempotents(s), rec(small:=true)));

# new for 0.7! - InjectionPrincipalFactor - "for a D-class"
#############################################################################

InstallMethod(InjectionPrincipalFactor, "for a D-class",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, inj, zero, bound_r, bound_l, inv_l, inv_r,
  f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("not yet implemented,");
    return;
  elif NrIdempotents(d)=NrHClasses(d) then
    return IsomorphismReesMatrixSemigroup(d);
  fi;

  g:=GroupHClass(d);
  rep:=Representative(g);
  g:=Range(IsomorphismPermGroup(g));

  rreps:=HClassReps(LClass(d, rep));
  lreps:=HClassReps(RClass(d, rep));
  mat:=[];

  inj:=InjectionZeroMagma(g);
  SetIsTotal(inj, true);
  SetIsSingleValued(inj, true);
  g:=Range(inj);
  zero:=MultiplicativeZero(g);
  bound_r:=List([1..Length(rreps)], ReturnFalse);
  bound_l:=List([1..Length(lreps)], ReturnFalse);
  inv_l:=EmptyPlist(Length(lreps));
  inv_r:=EmptyPlist(Length(rreps));

  for i in [1..Length(lreps)] do
    mat[i]:=[];
    for j in [1..Length(rreps)] do
      f:=lreps[i]*rreps[j];
      if f in d then
        mat[i][j]:=AsPermutation(f);
        if not bound_r[j] then
          bound_r[j]:=true;
          inv_r[j]:=mat[i][j]^-1*lreps[i];
        fi;
        if not bound_l[i] then
          bound_l[i]:=true;
        inv_l[i]:=rreps[j]*mat[i][j]^-1;
        fi;
        mat[i][j]:=mat[i][j]^inj;
      else
        mat[i][j]:=zero;
      fi;
    od;
  od;

  rms:=ReesZeroMatrixSemigroup(g, mat);
  iso:=function(f)
    local o, i, j;
    o:=LambdaOrb(d);
    i:=Position(o, LambdaFunc(ParentSemigroup(d))(f));

    if i=fail then
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
    fi;
    j:=Position(o, RhoFunc(ParentSemigroup(d))(f));
    if j=fail then
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return RMSElementNC(rms, j,
      AsPermutation(inv_r[j]*f*inv_l[i])^inj, i);
  end;

  inv:=function(x)
    local i, a, j;
    i:=RowOfRMSElement(x);
    a:=Images(InverseGeneralMapping(inj), UnderlyingElementOfRMSElement(x))[1];
    j:=ColumnOfRMSElement(x);
    return rreps[i]*a*lreps[j];
  end;

  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);

  return hom;
end);

#

InstallMethod(IrredundantGeneratingSubset,
"for an associative element with action collection",
[IsAssociativeElementWithActionCollection],
function(coll)
  local gens, j, out, i, redund, f;
  
  if IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll) then
    coll:=ShallowCopy(GeneratorsOfSemigroup(coll));
  fi;
  
  gens:=Set(ShallowCopy(coll)); j:=Length(gens);
  coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);
 
  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;

  repeat
    i:=i+1; f:=coll[i];
    if InfoLevel(InfoSemigroups)>=3 then 
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund),
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then
      if f in Semigroup(Difference(gens, [f])) then
        AddSet(redund, f); gens:=Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund)+Length(out)=j;

  if InfoLevel(InfoSemigroups)>1 then
    Print("\n");
  fi;
  return out;
end);

#

InstallOtherMethod(IsomorphismReesMatrixSemigroup, "for D-class",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) or not NrIdempotents(d)=NrHClasses(d) then
    Error("every H-class of the D-class should be a group,",
    " try InjectionPrincipalFactor instead,");
    return;
  fi;

  g:=GroupHClass(d);

  rep:=Representative(g); 
  g:=Range(IsomorphismPermGroup(g));

  rreps:=HClassReps(LClass(d, rep)); 
  lreps:=HClassReps(RClass(d, rep));
  mat:=[];
  
  for i in [1..Length(lreps)] do 
    mat[i]:=[];
    for j in [1..Length(rreps)] do 
      mat[i][j]:=AsPermutation(lreps[i]*rreps[j]);
    od;
  od;

  rms:=ReesMatrixSemigroup(g, mat);
  
  iso:=function(f)
    local o, i, j;
    o:=LambdaOrb(d);
    i:=Position(o, LambdaFunc(ParentSemigroup(d))(f));
    if i=fail then 
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
    fi;
    j:=Position(o, RhoFunc(ParentSemigroup(d))(f)); 
    if j=fail then 
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return RMSElementNC(rms, j,
      AsPermutation(rreps[j])^-1*AsPermutation(f)*
      AsPermutation(lreps[i])^-1, i);
  end;

  inv:=function(x)
    local i, a, j;
    i:=RowOfReesMatrixSemigroupElement(x);
    a:=UnderlyingElementOfReesMatrixSemigroupElement(x);
    j:=ColumnOfReesMatrixSemigroupElement(x);
    return rreps[i]*a*lreps[j];
  end;

  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);

  return hom;
end);

# new for 1.0! - InversesOfSemigroupElement - "for acting semigroup and elt"
#############################################################################

InstallMethod(InversesOfSemigroupElement, "for acting semigroup and elt",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)

  if f in s then
    return InversesOfSemigroupElementNC(s, f);
  fi;

  return fail;
end);

# mod for 1.0! - InversesOfSemigroupElementNC - "for acting semigroup and elt"
#############################################################################

InstallMethod(InversesOfSemigroupElementNC, 
"for an acting semigroup and acting elt",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, rank_f, lambda, rhorank, tester, j, o, rhos, grades, rho_f,
   lambdarank, creator, inv, out, k, g, rho, i, x;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  rank_f:=LambdaRank(s)(f); 
  lambda:=LambdaFunc(s)(f);
  rhorank:=RhoRank(s);
  tester:=IdempotentTester(s);
  j:=0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  # JDM is this really true?
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
    rhos:=EmptyPlist(Length(o));
    for rho in o do
      if rhorank(rho)=rank_f and tester(lambda, rho) then
        j:=j+1;
        rhos[j]:=rho;
      fi;
    od;
  else
    o:=Orb(s, RhoOrbSeed(s), RhoAct(s),
      rec(  forflatplainlists:=true, #JDM probably don't want to assume this..
            treehashsize:=SemigroupsOptionsRec.hashlen.M,
            gradingfunc:=function(o, x) return rhorank(x); end,
            onlygrades:=function(x, y) return x>=rank_f; end,
            onlygradesdata:=fail));
    Enumerate(o, infinity);
    grades:=Grades(o);
    rhos:=EmptyPlist(Length(o));
    for i in [2..Length(o)] do 
      if grades[i]=rank_f and tester(lambda, o[i]) then 
        j:=j+1;
        rhos[j]:=o[i];
      fi;
    od;
  fi;
  ShrinkAllocationPlist(rhos);
  
  rho_f:=RhoFunc(s)(f);
  lambdarank:=LambdaRank(s);
  creator:=IdempotentCreator(s);
  inv:=LambdaInverse(s);
  
  out:=[]; k:=0; 
 
  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    for x in o do
      if lambdarank(x)=rank_f and tester(x, rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  else
    o:=Orb(s, LambdaOrbSeed(s), LambdaAct(s),
      rec(  forflatplainlists:=true, #JDM probably don't want to assume this..
            treehashsize:=SemigroupsOptionsRec.hashlen.M,
            gradingfunc:=function(o, x) return lambdarank(x); end,
            onlygrades:=function(x, y) return x>=rank_f; end,
            onlygradesdata:=fail));
    Enumerate(o, infinity);
    grades:=Grades(o);
    for x in o do
      if grades[i]=rank_f and tester(x, rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(x, f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  fi; 
 
  return out;
end);

#JDM expand so that this is really an isomorphism and that the range knows some
#of the properties that the domain does. 

InstallMethod(IsomorphismBipartitionSemigroup, 
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  
  return MappingByFunction(s, Semigroup(List(GeneratorsOfSemigroup(s),     
   AsBipartition)), AsBipartition, AsTransformation);
end);

# mod for 1.0! - IsomorphismTransformationSemigroup - "for a perm group"
#############################################################################
    
InstallOtherMethod(IsomorphismTransformationSemigroup, "for a perm group",
[IsPermGroup], g-> IsomorphismTransformationSemigroup(g, NrMovedPoints(g)));

# mod for 1.0! - IsomorphismTransformationSemigroup
#############################################################################

InstallOtherMethod(IsomorphismTransformationSemigroup,
"for a perm group and pos int",
[IsPermGroup, IsPosInt], 
function(g, n)
  local conj, iso, s;

  if n<NrMovedPoints(g) then 
    Error("the number of moved points of the perm group exceeds the",
    " second argument,");
    return;
  fi;
  
  conj:=MappingPermListList(MovedPoints(g), [1..NrMovedPoints(g)]);
  iso:=x-> AsTransformation(x^conj, n);
  s:=Semigroup(List(GeneratorsOfGroup(g), iso));

  return MappingByFunction(g, s, iso, AsPermutation);
end);

#MMM

# new for 1.0! - MultiplicativeNeutralElement - "for an acting semigroup"
###########################################################################

InstallOtherMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, rank, n, f, r;

  gens:=Generators(s);
  n:=Maximum(List(gens, ActionRank));

  if n=ActionDegree(s) then
    return One(s);
  fi;

  f:=First(gens, f-> rank(f)=n);

  r:=GreensRClassOfElementNC(s, f); #NC? JDM 

  if not NrIdempotents(r)=1 then
    Info(InfoSemigroups, 2, "the number of idempotents in the R-class of the",
    " first maximum rank");
    Info(InfoSemigroups, 2, " generator is not 1");
    return fail;
  fi;

  f:=Idempotents(r)[1];

  if ForAll(gens, x-> x*f=x and f*x=x) then
    return f;
  fi;

  Info(InfoSemigroups, 2, "the unique idempotent in the R-class of the first",
  " maximum rank");
  Info(InfoSemigroups, 2, " generator is not the identity");
  return fail;
end);

# it just so happens that the MultiplicativeNeutralElement of a semigroup of
# partial permutations has to coincide with the One. This is not the case for
# transformation semigroups

InstallOtherMethod(MultiplicativeNeutralElement, "for a partial perm semi",
[IsPartialPermSemigroup], One);

#

InstallOtherMethod(MultiplicativeZero, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local min, o, rank, i, pos, f, m, rank_i, min_found, n;
  
  min:=MinActionRank(s);
  o:=LambdaOrb(s);
  rank:=LambdaRank(s);
  
  #is there an element in s with minimum possible rank
  if IsPartialPermSemigroup(s) or IsTransformationSemigroup(s) then 
    i:=0;
    repeat 
     i:=i+1;
     pos:=EnumeratePosition(o, [i], false);
    until pos<>fail or i=ActionDegree(s);
  else
    pos:=LookForInOrb(o, function(o, x) return rank(x)=min; end, 2);
  fi;
  if pos<>fail then
    f:=EvaluateWord(GeneratorsOfSemigroup(s), 
     TraceSchreierTreeForward(o, pos));
  fi;

  # lambda orb is closed, find an element with minimum rank
  if not IsBound(f) then 
    min_found:=rank(o[2]); pos:=1; i:=0; 
    
    while min_found>min and i<Length(o) do 
      i:=i+1;
      rank_i:=rank(o[i]);
      if rank_i<min_found then 
        min_found:=rank_i;
        pos:=i;
      fi;
    od;
    f:=EvaluateWord(GeneratorsOfSemigroup(s), TraceSchreierTreeForward(o, pos));
  fi;

  if IsIdempotent(f) and Size(GreensRClassOfElementNC(s, f))=1 then
    return f;
  fi;

  return fail;
end);

#

InstallOtherMethod(IsomorphismReesMatrixSemigroup, "for a simple inverse semi",
[IsPartialPermSemigroup and IsInverseSemigroup and IsSimpleSemigroup],
function(s)
  return IsomorphismReesMatrixSemigroup(DClass(s, Representative(s)));
end);

#

InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsMonoidAsSemigroup(s) then 
    Error( "Usage: trans. semigroup satisfying IsMonoidAsSemigroup," );
    return;
  fi;

  return MappingByFunction(s, Monoid(Difference(Generators(s),
  [TransformationNC([1..DegreeOfTransformationSemigroup(s)])])), x-> x, x-> x);
end);

#

InstallOtherMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
 
   if not IsGroupAsSemigroup(s)  then
     Error( "Usage: trans. semigroup satisfying IsGroupAsSemigroup,");
     return; 
   fi;
 
   return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
    AsPermutation, x-> AsTransformation(x, ActionDegree(s)));
 end);

# to attributes

#JDM this should be improved: inverse of the function is missing, and a similar
#approach as used in the method for IsomorphismTransformationSemigroup for a
#semigroup of binary relations should be used to reduce the number of points
#acted on. 

InstallOtherMethod(IsomorphismTransformationSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup], 
function(S)        
  local n, F, T;
  n:=Length(GeneratorsOfSemigroup(S)[1][1]);
  F:=BaseDomain(GeneratorsOfSemigroup(S)[1]);        
  T:=Semigroup(List(GeneratorsOfSemigroup(S), x-> 
   TransformationOp(x, Elements(F^n), OnRight)));        
  return MappingByFunction(S, T,
   x-> TransformationOp(x, Elements(F^Size(F)), OnRight));
end);

# to attributes

InstallOtherMethod(IsomorphismTransformationSemigroup, "for semigp of bin rels",
[IsBinaryRelationSemigroup], 
function(s)        
  local n, pts, o, t, pos, i;

  n:=DegreeOfBinaryRelation(Generators(s)[1]);
  pts:=EmptyPlist(2^n);

  for i in [1..n] do 
    o:=Orb(s, [i], OnPoints); #JDM multiseed orb
    Enumerate(o);
    pts:=Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  t:=Semigroup(List(Generators(s), x-> TransformationOpNC(x, pts, OnPoints)));
  
  return MappingByFunction(s, t, x-> TransformationOpNC(x, pts, OnPoints),
  x-> BinaryRelationOnPoints(List([1..n], i-> pts[pos[i]^x])));
end);

#

InstallOtherMethod(IsomorphismPermGroup, "for a partial perm semigroup", 
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsGroupAsSemigroup(s)  then
    Error( "Usage: partial perm. semigroup satisfying IsGroupAsSemigroup,");
    return; 
  fi;

  return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
   AsPermutation, x-> AsPartialPerm(x, DomainOfPartialPermCollection(s)));
end);

#

InstallMethod(MinimalIdeal, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, gens, max, o, i, bound, f;

  n:=ActionDegree(s);
  gens:=Generators(s);
  max:=Maximum(List(gens, ActionDegree));

  if max=n then 
    bound:=2^n;
  else
    bound:=Sum([1..max], x-> Binomial(n, x));
  fi;

  o:=Orb(gens, [1..n], OnSets, rec( schreier:=true,
   gradingfunc:=function(o, x) return Length(x); end,
    onlygrades:=[1..max],
     lookingfor:=function(o, x) return Length(x)=1; end));
   
  Enumerate(o, bound);

  if IsPosInt(PositionOfFound(o)) then 
    i:=PositionOfFound(o);
  else
    i:=Position(Grades(o), Minimum(Grades(o))); 
  fi;

  f:=EvaluateWord(gens, TraceSchreierTreeForward(o, i));
  i:=Semigroup(Elements(GreensDClassOfElementNC(s, f)));

  #i:=SemigroupIdealByGenerators(s, [f]);
  SetIsSimpleSemigroup(i, true);
  #SetIsMinimalIdeal(i, true);
  #SetUnderlyingDClassOfMinIdeal(i, GreensDClassOfElement(s, f));
  #return i;
  return i;#JDM temp. 
end);

#

InstallMethod(MinimalIdeal, "for a partial perm semi",
[IsPartialPermSemigroup],
function(s)
  local n, gens, max, bound, o, i, f, I;

  n:=ActionDegree(s);
  gens:=Generators(s);
  max:=Maximum(List(gens, ActionDegree));

  if max=n then
    bound:=2^n;
  else
    bound:=Sum([1..max], x-> Binomial(n, x));
  fi;

  o:=Orb(gens, DomainOfPartialPermCollection(s), OnIntegerSetsWithPP, 
    rec( schreier:=true,
         gradingfunc:=function(o, x) return Length(x); end,
         onlygrades:=[0..max],
         lookingfor:=function(o, x) return Length(x)=0; end));
  
  Enumerate(o, bound);

  if IsPosInt(PositionOfFound(o)) then
    i:=PositionOfFound(o);
  else
    i:=Position(Grades(o), Minimum(Grades(o)));
  fi;

  f:=EvaluateWord(gens, TraceSchreierTreeForward(o, i));
  I:=InverseSemigroup(Elements(GreensDClassOfElementNC(s, f)));
  SetIsGroupAsSemigroup(I, true);
  return I;
end);

#PPP

# new for 0.5! - PosetOfIdempotents - "for a transformation semigroup"
#############################################################################

InstallMethod(PosetOfIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], ReturnFail);

# new for 0.7! - PrimitiveIdempotents - "for an inverse semigroup"
#############################################################################

InstallMethod(PrimitiveIdempotents, "for an inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local zero, rank;
  zero:=MultiplicativeZero(s);
  if zero=fail then 
    rank:=zero[2];
  else
    rank:=Set(List(OrbSCC(LongOrb(s)), x-> Length(LongOrb(s)[x[1]])))[2];
  fi;

  return Idempotents(s, rank);
end);

# new for 0.7! - PrincipalFactor - "for a D-class"
#############################################################################

InstallMethod(PrincipalFactor, "for a D-class", 
[IsGreensDClass], 
d-> Range(InjectionPrincipalFactor(d)));

#SSS

# fix for 0.5! - SmallGeneratingSet - "for a trans. semi."
#############################################################################

InstallOtherMethod(SmallGeneratingSet, "for a trans. semi.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s -> Generators(Semigroup(Generators(s), rec(small:=true, schreier:=false))));

# new for 0.7! - SmallGeneratingSet - "for an inverse semi"
#############################################################################

InstallOtherMethod(SmallGeneratingSet, "for a trans. semi.", 
[IsPartialPermSemigroup and IsInverseSemigroup],
s -> Generators(InverseSemigroup(Generators(s), rec(small:=true, 
 schreier:=false))));

# new for 0.2! - StructureDescription - "for a Brandt trans. semigroup"
############################################################################

InstallOtherMethod(StructureDescription, "for a Brandt trans. semigroup",
[IsTransformationSemigroup and IsBrandtSemigroup],
function(s)
  local iter, d;
  
  iter:=IteratorOfDClasses(s);
  repeat 
    d:=NextIterator(iter);
  until Size(d)>1;
  
  return Concatenation("B(", StructureDescription(GroupHClass(d)), ", ",
  String(NrRClasses(d)), ")");
end);

# new for 0.7! - StructureDescription - "for a trans. semi. as group"
############################################################################

InstallOtherMethod(StructureDescription, "for a group as semigroup",
[IsTransformationSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));

# new for 0.7! - StructureDescription - "for a part. perm. semi. as group""
############################################################################

InstallOtherMethod(StructureDescription, "for a group as semigroup",
[IsPartialPermSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));



#EOF
