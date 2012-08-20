#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#AAA

# new for 0.5! - AntiIsomorphismTransformationSemigroup - "for a trans. semi."
###########################################################################

InstallMethod(AntiIsomorphismTransformationSemigroup, "for a semigroup",
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

#EEE

# new for 1.0! - EmbeddingNC - "for a perm group and a semigroup"
###########################################################################

InstallMethod(EmbeddingNC, "for a perm group and an acting semigroup",
[IsPermGroup, IsActingSemigroup],
function(g, s)
  local convert, creator, one, t, emb, conj;
 
  if IsTransformationSemigroup(s) then 
    convert:=x-> AsTransformation(x, Degree(s));
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

  if NrMovedPoints(g)<=Degree(s) then 
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

# mod for 1.0! - GroupOfUnits - "for an acting semigroup"
###########################################################################

InstallMethod(GroupOfUnits, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, m, g, emb, u;

  if not IsMonoidAsSemigroup(s) then
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

#III

# mod for 0.7! - IdempotentGeneratedSubsemigp - "for a semi"
###########################################################################

InstallMethod(IdempotentGeneratedSubsemigp, "for a semigroup",
[IsSemigroup and HasGeneratorsOfSemigroup],
s-> Semigroup(Idempotents(s), rec(small:=true)));

InstallMethod(IdempotentGeneratedSubsemigp, "for a semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
s-> InverseSemigroup(Idempotents(s), rec(small:=true)));

# new for 0.7! - InjectionPrincipalFactor - "for a D-class"
#############################################################################

InstallMethod(InjectionPrincipalFactor, "for a D-class",
[IsGreensDClass],
function(d)
  local g, rep, rreps, lreps, mat, inj, zero, bound_r, bound_l, inv_l, inv_r,
  f, rms, iso, inv, hom, i, j;

  if not IsActingSemigroupGreensClass(d) then
    Error("usage: a D-class of an acting semigroup,"); 
    return;
  fi;

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
    i:=Position(o, LambdaFunc(ParentAttr(d))(f));

    if i=fail then
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    o:=RhoOrb(d);
    j:=Position(o, RhoFunc(ParentAttr(d))(f));
    if j=fail then
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return ReesZeroMatrixSemigroupElementNC(rms, j,
      AsPermutation(inv_r[j]*f*inv_l[i])^inj, i);
  end;

  inv:=function(x)
    local i, a, j;
    i:=RowIndexOfReesZeroMatrixSemigroupElement(x);
    a:=Images(InverseGeneralMapping(inj),
     UnderlyingElementOfReesZeroMatrixSemigroupElement(x))[1];
    j:=ColumnIndexOfReesZeroMatrixSemigroupElement(x);
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
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsActingElt],
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
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsActingElt],
function(s, f)
  local regular, rank_f, lambda, rhorank, tester, j, o, rhos, grades, rho_f, lambdarank, creator, inv, out, k, g, rho, i, x;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  rank_f:=Rank(f); 
  lambda:=LambdaFunc(s)(f);
  rhorank:=RhoRank(s);
  tester:=IdempotentLambdaRhoTester(s);
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
    o:=Orb(s, [1..65536], RhoAct(s),
      rec(  forflatplainlists:=true, #JDM probably don't want to assume this..
            treehashsize:=CitrusOptionsRec.hashlen.M,
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
  creator:=IdempotentLambdaRhoCreator(s);
  inv:=LambdaInverse(s);
  
  out:=[]; k:=0; 
 
  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    for x in o do
      if lambdarank(x)=rank_f and tester(x, rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(x, f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  else
    o:=Orb(s, [1..65536], LambdaAct(s),
      rec(  forflatplainlists:=true, #JDM probably don't want to assume this..
            treehashsize:=CitrusOptionsRec.hashlen.M,
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
  iso:=x-> AsTransformationNC(x^conj, n);
  s:=Semigroup(List(GeneratorsOfGroup(g), iso));

  return MappingByFunction(g, s, iso, AsPermutation);
end);

#MMM

# new for 1.0! - MultiplicativeNeutralElement - "for an acting semigroup"
###########################################################################

InstallOtherMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, n, f, r;

  gens:=Generators(s);
  n:=Maximum(List(gens, Rank));

  if n=Degree(s) then
    return One(s);
  fi;

  f:=First(gens, f-> Rank(f)=n);

  r:=GreensRClassOfElementNC(s, f); #NC? JDM 

  if not NrIdempotents(r)=1 then
    Info(InfoCitrus, 2, "the number of idempotents in the R-class of the",
    " first maximum rank");
    Info(InfoCitrus, 2, " generator is not 1");
    return fail;
  fi;

  f:=Idempotents(r)[1];

  if ForAll(gens, x-> x*f=x and f*x=x) then
    return f;
  fi;

  Info(InfoCitrus, 2, "the unique idempotent in the R-class of the first",
  " maximum rank");
  Info(InfoCitrus, 2, " generator is not the identity");
  return fail;
end);

# it just so happens that the MultiplicativeNeutralElement of a semigroup of
# partial permutations has to coincide with the One. This is not the case for
# transformation semigroups

InstallOtherMethod(MultiplicativeNeutralElement, "for a partial perm semi",
[IsPartialPermSemigroup], One);

# new for 0.1! - MultiplicativeZero - "for an acting semigroup"
###########################################################################

InstallOtherMethod(MultiplicativeZero, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local o, min, rank, pos, f, m, i, j;

  o:=LambdaOrb(s);
  
  if IsPartialPermSemigroup(s) then 
    min:=0;
  elif IsTransformationSemigroup(s) then 
    min:=1;
  fi;

  rank:=LambdaRank(s);
  
  if not IsClosed(o) then 
    o!.looking:=true; o!.lookingfor:=function(o, x) return rank(x)=min; end;
    o!.lookfunc:=o!.lookingfor;
    Enumerate(o);
    pos:=PositionOfFound(o);
    o!.found:=false; o!.looking:=false;
    Unbind(o!.lookingfor); Unbind(o!.lookfunc); 
    if pos<>false then
      f:=EvaluateWord(GeneratorsOfSemigroup(s), 
       TraceSchreierTreeForward(o, pos));
    fi;
  fi;

  if not IsBound(f) then 
    m:=rank(o[1]);
    pos:=1;
    i:=0;

    while m>min and i<Length(o) do 
      i:=i+1;
      j:=rank(o[i]);
      if j<m then 
        m:=j;
        pos:=i;
      fi;
    od;
    f:=EvaluateWord(GeneratorsOfSemigroup(s), TraceSchreierTreeForward(o, pos));
  fi;

  if f^2=f and Size(GreensRClassOfElementNC(s, f))=1 then
    return f;
  fi;

  return fail;
end);



