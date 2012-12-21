############################################################################# 
## 
#W  semigroups.gi 
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# basic things

# JDM move to lib

InstallMethod(ViewObj, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 10,
function(s)
  local n;
 
  if not (IsPartialPermSemigroup(s) or IsTransformationSemigroup(s) or 
    IsBipartitionSemigroup(s) or IsMatrixSemigroup(s) or
    IsBinaryRelationSemigroup(s)) then 
    TryNextMethod();
  fi;

  if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    Print("<inverse ");
  elif HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
    Print("<regular ");
  else
    Print("<");
  fi;
  
  if IsPartialPermSemigroup(s) then 
    Print("partial perm ");
  elif IsTransformationSemigroup(s) then 
    Print("transformation ");
  elif IsBipartitionSemigroup(s) then 
    Print("bipartition ");
  elif IsMatrixSemigroup(s) then 
    Print("matrix ");
  elif IsBinaryRelationSemigroup(s) then 
    Print("binary relation ");
  fi;
  
  if IsMonoid(s) then 
    Print("monoid ");
  else 
    Print("semigroup ");
  fi;

  if IsTransformationSemigroup(s) then 
    Print("of degree ", DegreeOfTransformationSemigroup(s), " ");
  elif IsPartialPermSemigroup(s) then 
    Print("of degree ", DegreeOfPartialPermSemigroup(s), " ");
  elif IsMatrixSemigroup(s) then
    n:=Length(GeneratorsOfSemigroup(s)[1][1]);
    Print(n, "x", n, " over ", BaseDomain(GeneratorsOfSemigroup(s)[1][1]), " ");
  elif IsBipartitionSemigroup(s) then 
    Print("of degree ", DegreeOfBipartitionSemigroup(s)/2, " "); 
  elif IsBinaryRelationSemigroup(s) then 
    Print("of degree ", Length(Successors(Generators(s)[1])), " ");
  fi;

  Print("with ", Length(Generators(s)));
  Print(" generator");

  if Length(Generators(s))>1 then 
    Print("s");
  fi;
  Print(">");
end);

#

InstallOtherMethod(Generators, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    if IsMonoid(s) then 
      return GeneratorsOfInverseMonoid(s);
    fi;
    return GeneratorsOfInverseSemigroup(s);
  fi;

  if IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

# move to lib JDM

InstallOtherMethod(IsSubsemigroup, 
"for semigroup with generators and semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup, 
IsSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  return ForAll(GeneratorsOfSemigroup(t), x-> x in s);
end);

#

InstallOtherMethod(IsSubsemigroup, 
"for a partial perm semi and inverse  semigroup of partial perms",
[IsPartialPermSemigroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(s, t)
  return ForAll(Generators(t), x-> x in s);
end);

# JDM move to lib

InstallOtherMethod(IsSubset, 
"for semigroup and associative element collection",
[IsSemigroup, IsAssociativeElementCollection],
function(s, coll)
  return ForAll(coll, x-> x in s);
end);

# JDM move to lib

InstallMethod(\=, 
"for semigroup with generators and semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup,
 IsSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  return ForAll(GeneratorsOfSemigroup(s), x-> x in t) and
   ForAll(GeneratorsOfSemigroup(t), x-> x in s);
end);

# creating semigroups, monoids, inverse semigroups, etc

InstallMethod(MagmaByGenerators, 
"for an associative element with action collection",
[IsAssociativeElementWithActionCollection],
function(gens)
  local M;
  
  M:=Objectify( NewType( FamilyObj( gens ), 
   IsMagma and IsAttributeStoringRep ), rec(opts:=SemigroupsOptionsRec));

  SetGeneratorsOfMagma( M, AsList( gens ) );
  return M;
end);

# JDM move to lib

MakeReadWriteGlobal("Semigroup");
UnbindGlobal("Semigroup");

BindGlobal("Semigroup", 
function ( arg )
  local out, i;
  if Length( arg ) = 1 and IsMatrix( arg[1] )  then
    return SemigroupByGenerators( [ arg[1] ] );
  elif Length( arg ) = 1 and IsList( arg[1] ) and 0 < Length( arg[1] )  then
    return SemigroupByGenerators( arg[1] );
  elif IsAssociativeElement(arg[1]) or IsAssociativeElementCollection(arg[1]) then 
    out:=[];
    for i in [1..Length(arg)] do 
      if IsAssociativeElement(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsAssociativeElementCollection(arg[i]) then 
        if IsSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return SemigroupByGenerators(Concatenation(out), arg[i]);
      else
        Error( "Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>)," );
        return;
      fi;
    od;
    return SemigroupByGenerators(Concatenation(out));
  elif 0 < Length( arg )  then
    return SemigroupByGenerators( arg );
  else
    Error( "Usage: Semigroup(<gen>,...),Semigroup(<gens>),Semigroup(<D>),");
    return;
  fi;
end);

#

InstallOtherMethod(SemigroupByGenerators, "for an acting elt collection",
[IsAssociativeElementWithActionCollection],
function(gens)
   return SemigroupByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallOtherMethod(SemigroupByGenerators, 
"for an associative element with action collection and record",
[IsAssociativeElementWithActionCollection, IsRecord],
function(gens, opts)
  local n, M, L, i, closure_opts, s, filts, f;

  opts:=SemigroupOptions(opts);

  if opts.small and Length(gens)>1 then 
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;

    n:=ActionDegree(gens);

    #remove the identity
    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2])=n then
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=Semigroup(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then
          s:=ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;

  filts:=IsSemigroup and IsAttributeStoringRep;

  if opts.acting then 
    filts:=filts and IsActingSemigroup;
  fi;

  s:=Objectify( NewType( FamilyObj( gens ), filts ), rec(opts:=opts));
  
  if opts.regular then 
    SetIsRegularSemigroup(s, true);
  fi;
 
  SetGeneratorsOfMagma( s, AsList( gens ) );
  return s;
end);

# JDM move to lib

MakeReadWriteGlobal("Monoid");
UnbindGlobal("Monoid");

BindGlobal("Monoid", 
function ( arg )
  local out, i;
  if Length( arg ) = 1 and IsMatrix( arg[1] )  then
    return MonoidByGenerators( [ arg[1] ] );
  elif Length( arg ) = 2 and IsMatrix( arg[1] )  then
    return MonoidByGenerators( arg );
  elif Length( arg ) = 1 and IsList( arg[1] ) and 0 < Length( arg[1] )  then
    return MonoidByGenerators( arg[1] );
  elif Length( arg ) = 2 and IsList( arg[1] )  then
    return MonoidByGenerators( arg[1], arg[2] );
  elif IsAssociativeElement(arg[1]) or IsAssociativeElementCollection(arg[1])
   then 
    out:=[];
    for i in [1..Length(arg)] do 
      if IsAssociativeElement(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsAssociativeElementCollection(arg[i]) then 
        if IsSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return MonoidByGenerators(Concatenation(out), arg[i]);
      else
        Error( "Usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)," );
        return;
      fi;
    od;
    return MonoidByGenerators(Concatenation(out));
  elif 0 < Length( arg )  then
    return MonoidByGenerators( arg );
  else
    Error( "Usage: Monoid(<gen>,...), Monoid(<gens>), Monoid(<D>)," );
    return;
  fi;
end);

#

InstallOtherMethod(MonoidByGenerators, 
"for an associative element collection",
[IsAssociativeElementWithActionCollection],
function(gens)
  return MonoidByGenerators(gens, SemigroupsOptionsRec);
end);

#

InstallOtherMethod(MonoidByGenerators, 
"for an asssociative element with action collection and record",
[IsAssociativeElementWithActionCollection, IsRecord],
function(gens, record)
  local n, i, closure_opts, s, filts, f;
  
  record:=SemigroupOptions(record);

  if record.small and Length(gens)>1 then #small gen. set
    
    gens:=ShallowCopy(gens);
    gens:=SSortedList(gens); #remove duplicates 
    gens:=Permuted(gens, Random(SymmetricGroup(Length(gens))));;
    Sort(gens, function(x, y) return ActionRank(x)>ActionRank(y); end);;

    n:=ActionDegree(gens);

    if gens[1]![1]=[1..n] and ActionRank(gens[2])=n then #remove id
      Remove(gens, 1);
    fi;

    i:=0;
    closure_opts:=rec(small:=false, hashlen:=record.hashlen);
    s:=Monoid(gens[1], closure_opts);

    if InfoLevel(InfoSemigroups)>1 then
      n:=Length(gens);
      for i in [1..n] do
        if not gens[i] in s then 
          s:=ClosureSemigroupNC(s, [gens[i]], closure_opts);
        fi;
        Print("at \t", i, " of \t", n, "; \t", Length(Generators(s)),
        " generators so far");
      od;
      Print("\n");
    else
      for f in gens do
        if not f in s then 
          s:=ClosureSemigroupNC(s, [f], closure_opts);
        fi;
      od;
    fi;
    return s;
  fi;    

  filts:=IsMonoid and IsAttributeStoringRep;

  if record.acting then 
    filts:=filts and IsActingSemigroup;
  fi;

  s:=Objectify( NewType( FamilyObj( gens ), filts ), rec(opts:=record));

  if record.regular then 
    SetIsRegularSemigroup(s, true);
  fi;

  SetGeneratorsOfMagmaWithOne( s, AsList( gens ) );
  return s;
end);

# maybe move to lib JDM (problem is that so far pperms are the only elements
# that can be used to produce inverse semigroups)

InstallGlobalFunction(InverseMonoid,
function( arg )
  local out, i;

  if IsPartialPerm(arg[1]) or IsPartialPermCollection(arg[1]) then 
    out:=[]; 
    for i in [1..Length(arg)] do 
      if IsPartialPerm(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsPartialPermCollection(arg[i]) then 
        if IsPartialPermSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return InverseMonoidByGenerators(Concatenation(out), arg[i]);
      else
        Error( "usage: InverseMonoid(<gen>,...), InverseMonoid(<gens>),"
        ,  "InverseMonoid(<D>)," );
        return;
      fi;
    od;
    return InverseMonoidByGenerators(Concatenation(out));
  fi;
  Error( "usage: InverseMonoid(<gen>,...),InverseMonoid(<gens>),",
   "InverseMonoid(<D>),");
  return;
end);

#

InstallGlobalFunction(InverseSemigroup,
function( arg )
  local out, i;

  if IsPartialPerm(arg[1]) or IsPartialPermCollection(arg[1]) then 
    out:=[]; 
    for i in [1..Length(arg)] do 
      if IsPartialPerm(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsPartialPermCollection(arg[i]) then 
        if IsPartialPermSemigroup(arg[i]) then
          out[i]:=Generators(arg[i]);
        else
          out[i]:=arg[i];
        fi;
      elif i=Length(arg) and IsRecord(arg[i]) then 
        return InverseSemigroupByGenerators(Concatenation(out), arg[i]);
      else
        Error( "usage: InverseSemigroup(<gen>,...), InverseSemigroup(<gens>),"
        ,  "InverseSemigroup(<D>)," );
        return;
      fi;
    od;
    return InverseSemigroupByGenerators(Concatenation(out));
  fi;
  Error( "usage: InverseSemigroup(<gen>,...),InverseSemigroup(<gens>),",
   "InverseSemigroup(<D>),");
  return;
end);

#

InstallMethod(InverseMonoidByGenerators, "for partial perm coll", 
[IsPartialPermCollection],
function(coll)
local one, gens, f;

  gens:=ShallowCopy(coll);

  for f in coll do
    if not DomPP(f)=RanSetPP(f) then 
      Add(gens, f^-1);
    fi;
  od;

  return InverseMonoidByGeneratorsNC(gens, coll, SemigroupsOptionsRec);
end);

#

InstallMethod(InverseSemigroupByGenerators, "for partial perm coll", 
[IsPartialPermCollection],
function(coll)
  local gens, f;

  gens:=ShallowCopy(coll);
  
  for f in coll do
    if not DomPP(f)=RanSetPP(f) then 
      Add(gens, f^-1);
    fi;
  od;

  return InverseSemigroupByGeneratorsNC(gens, coll, SemigroupsOptionsRec);
end);

#

InstallOtherMethod(InverseMonoidByGenerators, 
"for partial perm coll and record",
[IsPartialPermCollection, IsRecord],
function(coll, opts)
  local n, one, f, gens;
  
  if not IsBound(opts.small) then
    opts.small:=SemigroupsOptionsRec.small;
  fi;

  if not IsBound(opts.hashlen) then
    opts.hashlen:=SemigroupsOptionsRec.hashlen;
  elif IsPosInt(opts.hashlen) then
    n:=opts.hashlen;
    opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)),
     L:=NextPrimeInt(n));
  elif not IsRecord(opts.hashlen) then
    Error("the component hashlen should be a positive integer or a record,");
    return;
  fi;

  if not opts.small then
    gens:=ShallowCopy(coll);
      
    for f in coll do
      if not DomPP(f)=RanSetPP(f) then
        Add(gens, f^-1); 
      fi;
    od;
  else
    gens:=coll;
  fi;

  return InverseMonoidByGeneratorsNC(gens, coll, opts);
end);

#

InstallOtherMethod(InverseSemigroupByGenerators, 
"for partial perm coll and record",
[IsPartialPermCollection, IsRecord],
function(coll, opts)
  local n, f, gens;

  if not IsBound(opts.small) then
    opts.small:=SemigroupsOptionsRec.small;
  fi;

  if not IsBound(opts.hashlen) then
    opts.hashlen:=SemigroupsOptionsRec.hashlen;
  elif IsPosInt(opts.hashlen) then
    n:=opts.hashlen;
    opts.hashlen:=rec(S:=NextPrimeInt(Int(n/100)), M:=NextPrimeInt(Int(n/4)),
     L:=NextPrimeInt(n));
  elif not IsRecord(opts.hashlen) then
    Error("the component hashlen should be a positive integer or a record,");
    return;
  fi;

  if not opts.small then
    gens:=ShallowCopy(coll);
      
    for f in coll do
      if not DomPP(f)=RanSetPP(f) then
        Add(gens, f^-1);
      fi;
    od;
  else
    gens:=coll;
  fi;

  return InverseSemigroupByGeneratorsNC(gens, coll, opts);
end);

#

InstallMethod(InverseMonoidByGeneratorsNC, 
"for partial perm coll, partial perm coll, and record",
[IsPartialPermCollection, IsPartialPermCollection, IsRecord],
function(gens, coll, opts)
  local closure_opts, s, filts, f;

  if opts.small and Length(gens)>1 then 
    coll:=SSortedList(ShallowCopy(coll));
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));;
    Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);;
    
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=InverseMonoid(coll[1], closure_opts);
    
    for f in coll do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;

  filts:=IsMagmaWithOne and IsInverseSemigroup and IsAttributeStoringRep;
  if opts.acting then 
    filts:=filts and IsActingSemigroupWithInverseOp;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), filts), rec(opts:=opts));

  SetDomainOfPartialPermCollection(s, Union(List(gens, DomPP)));
  SetRangeOfPartialPermCollection(s, DomainOfPartialPermCollection(s));
  SetGeneratorsOfMagmaWithOne(s, gens);
  SetGeneratorsOfInverseSemigroup(s, Concatenation([One(s)], coll));
  SetGeneratorsOfInverseMonoid(s, coll);
  return s;
end);

#

InstallMethod(InverseSemigroupByGeneratorsNC, 
"for partial perm coll, partial perm coll, and record",
[IsPartialPermCollection, IsPartialPermCollection, IsRecord],
function(gens, coll, opts)
  local closure_opts, s, filts, f;

  if opts.small and Length(gens)>1 then 
    coll:=SSortedList(ShallowCopy(coll));
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));;
    Sort(coll, function(x, y) return x[2]>y[2]; end);;
    
    closure_opts:=rec(small:=false, hashlen:=opts.hashlen);
    s:=InverseSemigroup(coll[1], closure_opts);
    
    for f in coll do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;
  
  filts:=IsMagma and IsInverseSemigroup and IsAttributeStoringRep;
  if opts.acting then 
    filts:=filts and IsActingSemigroupWithInverseOp;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), filts), rec(opts:=opts));

  SetGeneratorsOfMagma(s, gens);
  SetGeneratorsOfInverseSemigroup(s, coll);
  return s;
end);

#

InstallGlobalFunction(RegularSemigroup, 
function(arg)
  Add(arg, rec(regular:=true));
  return CallFuncList(Semigroup, arg);
end);

#

InstallMethod(SubsemigroupByProperty, 
"for an acting semigroup with generators and function",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction], 
function(S, func)
  local limit, n;

  if HasSize(S) then 
    limit:=Size(S);
  else
    n:=ActionDegree(S);
    limit:=n^n; #JDM this line wrong in general!!
  fi;

  return SubsemigroupByProperty(S, func, limit);
end);

#

InstallOtherMethod(SubsemigroupByProperty, 
"for an acting semigroup with generators, function, and positive integer",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
 
  iter:=Iterator(S);
  T:=Semigroup(NextIterator(iter));
  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureSemigroup(T, f);
    fi;
  od;

  return T;
end);

#JDM generalise

InstallMethod(SubsemigroupByProperty, 
"for an inverse semigroup of partial perms and function",
[IsPartialPermSemigroup and IsInverseSemigroup, IsFunction], 
function(S, func)
  local limit, n;

  if HasSize(S) then 
    limit:=Size(S);
  else
    n:=NrMovedPoints(S);
    limit:=Sum(List([0..n], r-> Binomial(n,r)^2*Factorial(r)));
  fi;

  return SubsemigroupByProperty(S, func, limit);
end);

#JDM generalise

InstallOtherMethod(SubsemigroupByProperty, "for a part perm semi, func, rec",
[IsPartialPermSemigroup and IsInverseSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
  
  iter:=Iterator(S);
  T:=InverseSemigroup(NextIterator(iter));
  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureInverseSemigroup(T, f);
    fi;
  od;
  return T;
end);

# closure

InstallOtherMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithActionCollection],
function(s, coll) 
  return ClosureInverseSemigroup(s, coll, s!.opts);
end);

#

InstallOtherMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithAction],
function(s, f) 
  return ClosureInverseSemigroup(s, [f], s!.opts);
end);

#

InstallOtherMethod(ClosureInverseSemigroup, 
"for acting semigroup with inverse op and an associative element coll",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithAction, IsRecord],
function(s, f, record) 
  return ClosureInverseSemigroup(s, [f], record);
end);

#

InstallMethod(ClosureInverseSemigroup, 
"for an acting semigroup with inverse op, ass. elt. coll, and record",
[IsActingSemigroupWithInverseOp, IsAssociativeElementWithActionCollection, IsRecord],
function(s, coll, record)
  local n;

  if not ElementsFamily(FamilyObj(s))=FamilyObj(Representative(coll)) then 
    Error("the semigroup and collection of elements are not of the same type,");
    return;
  fi;

  if IsSemigroup(coll) then 
    coll:=GeneratorsOfSemigroup(coll);
  fi;

  return ClosureInverseSemigroupNC(s, Filtered(coll, x-> not x in s),
   SemigroupOptions(record));
end);

#

InstallGlobalFunction(ClosureInverseSemigroupNC,
function(s, coll, opts)
  local t, coll_copy, o, f;
 
  if coll=[] then
    Info(InfoSemigroups, 2, "the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;

  coll_copy:=ShallowCopy(coll);
  for f in coll do
    if not LambdaFunc(s)(f)=RhoFunc(s)(f) then 
      Add(coll_copy, f^-1);
    fi;
  od;  
  
  o:=StructuralCopy(LambdaOrb(s));
  AddGeneratorsToOrbit(o, coll_copy);

  t:=InverseSemigroupByGeneratorsNC(o!.gens, 
   Concatenation(Generators(s), coll), opts);

  #JDM is the following enough?!

  if IsBound(o!.scc) then 
    Unbind(o!.scc); Unbind(o!.trees); Unbind(o!.scc_lookup);
  fi;
  
  if IsBound(o!.mults) then 
    Unbind(o!.mults); 
  fi;
  
  if IsBound(o!.schutz) then 
    Unbind(o!.schutz);
  fi;
  
  o!.finished:=false;
  SetLambdaOrb(t, o);
  return t;
end);

#

InstallOtherMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action coll",
[IsActingSemigroup, IsAssociativeElementWithActionCollection],
function(s, coll)
  return ClosureSemigroup(s, coll, s!.opts);
end);

#

InstallOtherMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action",
[IsActingSemigroup, IsAssociativeElementWithAction],
function(s, f)
  return ClosureSemigroup(s, [f], s!.opts);
end);

#

InstallOtherMethod(ClosureSemigroup, 
"for an acting semigroup and associative element with action",
[IsActingSemigroup, IsAssociativeElementWithAction, IsRecord],
function(s, f, record)
  return ClosureSemigroup(s, [f], SemigroupOptions(record));
end);

#

InstallMethod(ClosureSemigroup, 
"for an acting semigroup, associative element with action coll, and record",
[IsActingSemigroup, IsAssociativeElementWithActionCollection, IsRecord],
function(s, coll, record)

  record.small:=false;

  if IsActingSemigroup(coll) then 
    coll:=Generators(coll);
  fi;

  if ActionDegree(s)<>ActionDegree(Representative(coll)) then 
    Error("usage: the degree of the semigroup and collection must be equal,");
    return;
  fi;

  return ClosureSemigroupNC(s, Filtered(coll, x-> not x in s),
   SemigroupOptions(record));
end);

# coll should consist of elements not in s

#JDM this needs to be reworked. 

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll, opts)
  local t, old_data, n, max_rank, orbits, lens, data_ht, data, data_len, images, old_lens, old_orbits, gens, ht, o, r, j, scc, reps, out, old_reps, old_data_list, old_reps_len, old_o, new_data, d, g, m, z, i, k, val, y;
 
  if coll=[] then 
    Info(InfoSemigroups, 2, "All the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;
  
  if IsTransformationMonoid(s) then 
    t:=Monoid(s, coll, opts);
  else
    t:=Semigroup(s, coll, opts);
  fi;

  #no schreier###############################################################
  
  #old_data:=OrbitsOfImages(s);
  #n:=LambdaDegree(t);
  
  # set up orbits of images of t

  max_rank:=Maximum(List(coll, ActionRank)); 
  gens:=List(Generators(t), x-> x![1]);
  orbits:=EmptyPlist(n); 
  lens:=[1..n]*0;
  data_ht:=HTCreate([1,1,1,1,1,1], rec(forflatplainlists:=true,
   hashlen:=old_data!.data_ht!.len));
  data:=EmptyPlist(Length(old_data!.data)); 
  data_len:=0;
  images:=HTCreate(SSortedList(gens[1]), rec(forflatplainlists:=true,
   hashlen:=old_data!.images!.len));
  old_lens:=old_data!.lens; old_orbits:=old_data!.orbits;

  # initialize R-class reps orbit
  
  ht:=StructuralCopy(old_data!.ht); o:=ht!.o; 
  r:=Length(o);
  
  for i in [1..Length(coll)] do 
    j:=HTAdd(ht, coll[i]![1], r+i);
    o[r+i]:=ht!.els[j];
  od;
  
  # process orbits of large images
 
  for j in [n, n-1..max_rank+1] do
    if old_lens[j]>0 then
      lens[j]:=old_lens[j];
      orbits[j]:=EmptyPlist(lens[j]);
      for k in [1..lens[j]] do
        o:=StructuralCopy(old_orbits[j][k]);
        o!.onlygradesdata:=images;
        AddGeneratorsToOrbit(o, coll);
        scc:=o!.scc; reps:=o!.reps;

        for m in [1..Length(scc)] do
          for val in [1..Length(reps[m])] do
            for n in [1..Length(reps[m][val])] do
              data_len:=data_len+1;
              out:=[j, k, scc[m][1], m, val, n];
              HTAdd(data_ht, out, data_len);
              data[data_len]:=out;
            od;
          od;
        od;  
        for i in o do 
          HTAdd(images, i, k);
        od;  
        orbits[j][k]:=o;
      od;
    fi;
  od;

  # process orbits of small images

  old_reps:=EmptyPlist(Length(old_data!.data));
  old_data_list:=EmptyPlist(Length(old_data!.data));
  old_reps_len:=0;

  for j in [max_rank, max_rank-1..1] do 
    if old_lens[j]>0 then 
      orbits[j]:=[];
      for k in [1..old_lens[j]] do
        old_o:=old_orbits[j][k];
        if HTValue(images, old_o[1])=fail then 
          lens[j]:=lens[j]+1;
          o:=StructuralCopy(old_o);
          o!.onlygradesdata:=images;
          AddGeneratorsToOrbit(o, coll);
          Unbind(o!.scc); Unbind(o!.rev);

          r:=Length(OrbSCC(o));

          o!.trees:=EmptyPlist(r);
          o!.reverse:=EmptyPlist(r);
          o!.reps:=List([1..r], x-> []);
          o!.kernels_ht:=[];
          o!.perms:=EmptyPlist(Length(o));
          o!.schutz:=EmptyPlist(r);
          o!.nr_idempotents:=List([1..r], m-> []);  
          for i in o do 
            HTAdd(images, i, lens[j]);
          od;
          orbits[j][lens[j]]:=o;
        fi;
        Append(old_reps, Concatenation(Concatenation(old_o!.reps)));
      od;
    fi;
  od;

  # set orbits of images of t

  #new_data:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
  # rec(finished:=false, orbits:=orbits, lens:=lens, images:=images,
  #  at:=old_data!.at, gens:=gens, ht:=ht, data_ht:=data_ht, data:=data));

  #SetOrbitsOfImages(t, new_data); 
 
  # process old R-reps 

  for i in [1..Length(old_reps)] do 
    #j:=InOrbitsOfImages(old_reps[i], false, 
    # [fail, fail, fail, fail, fail, 0, fail], orbits, images);
    #if not j[1] then
    #  AddToOrbitsOfImages(t, old_reps[i], j[2], new_data, false);
    #fi;
  od;
  
  # install new pts in the orbit
  
  coll:=List(coll, x-> x![1]); n:=Length(Generators(s)); 

  for i in [1..Length(data)] do 
    d:=data[i];
    g:=orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
    m:=Length(coll); j:=Length(ht!.o);
    for y in [1..m] do 
      z:=g{coll[y]};
      if HTValue(ht, z)=fail then
        j:=j+1; z:=HTAdd(ht, z, j); ht!.o[j]:=ht!.els[z];
      fi;
    od;
  od;
  
  # process kernel orbits here too!

  return t;
end);


#random

InstallMethod(RandomBipartitionSemigroup, "for a pos int and pos int",
[IsPosInt, IsPosInt],
function(m, n)
  return Semigroup(List([1..m], x-> RandomBipartition(n)));
end);

#

InstallMethod(RandomBipartitionMonoid, "for a pos int and pos int",
[IsPosInt, IsPosInt],
function(m, n)
  return Monoid(List([1..m], x-> RandomBipartition(n)));
end);

#

InstallMethod(RandomMatrixSemigroup, "for a ring and pos int",
[IsRing, IsPosInt, IsPosInt], 
function(R, m, n)
  return Semigroup(List([1..m], x-> RandomMat(n, n, R)));
end);

#

InstallMethod(RandomBinaryRelationMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  local s;

  s:=Monoid(List([1..m], x-> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true); 
  return s;
end);

#

InstallMethod(RandomBinaryRelationSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  local s;

  s:=Semigroup(List([1..m], x-> RandomBinaryRelationOnPoints(n)));
  SetIsBinaryRelationSemigroup(s, true);
  return s;
end);

InstallMethod(RandomBlockGroup, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  return Semigroup(Set(List([1..m], x-> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  return InverseMonoid(Set(List([1..m], x-> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomInverseSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(m,n)
  return InverseSemigroup(Set(List([1..m], x-> RandomPartialPerm(n))));
end);

#

InstallMethod(RandomTransformationSemigroup, "for pos int and pos int",
[IsPosInt, IsPosInt], 
function(m,n)
  return Semigroup(Set(List([1..m], x-> RandomTransformation(n))));
end);

#

InstallMethod(RandomTransformationMonoid, "for a pos int and pos int",
[IsPosInt, IsPosInt], 
function(m,n)
  return Monoid(Set(List([1..m], x-> RandomTransformation(n))));
end);

#EOF
