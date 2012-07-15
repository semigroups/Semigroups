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

#GGG

# mod for 1.0! - GroupOfUnits - "for an acting semigroup"
###########################################################################

InstallMethod(GroupOfUnits, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, m, g, iso, u;

  if not IsMonoidAsSemigroup(s) then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  m:=Size(r); g:=Group(AsPermutation(Random(r)));

  while Size(g)<m do
    g:=ClosureGroup(g, AsPermutation(Random(r)));
  od;

  if IsTransformationSemigroup(s) then
    iso:=IsomorphismTransformationSemigroup(g);
  elif IsPartialPermSemigroup(s) then
    Error("not yet implemented");
  else
    Error("usage: a semigroup of transformations or partial perms,");
    return;
  fi;

  u:=Range(iso);
  SetIsomorphismPermGroup(u, InverseGeneralMapping(iso));
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

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

#MMM

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



