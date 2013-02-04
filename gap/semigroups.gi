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

# 4 parts to the display: property, type, name, attributes.
# property: simple
# type:     transformation
# name:     monoid

InstallMethod(ViewObj, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 10,
function(s)
  local type, name, property, n;
 
  if not (IsPartialPermSemigroup(s) or IsTransformationSemigroup(s) or 
    IsBipartitionSemigroup(s) or IsMatrixSemigroup(s) or
    IsBinaryRelationSemigroup(s)) then 
    TryNextMethod();
  fi;

  # type
  if IsPartialPermSemigroup(s) then 
    type:="partial perm ";
  elif IsTransformationSemigroup(s) then 
    type:="transformation ";
  elif IsBipartitionSemigroup(s) then 
    type:="bipartition ";
  elif IsMatrixSemigroup(s) then 
    type:="matrix ";
  elif IsBinaryRelationSemigroup(s) then 
    type:="binary relation ";
  fi;
 
  # name
  if HasIsGroupAsSemigroup(s) and IsGroupAsSemigroup(s) then 
    name:="group ";
  elif HasIsZeroGroup(s) and IsZeroGroup(s) then 
    name:="0-group";
  elif IsMonoid(s) then 
    name:="monoid ";
  else 
    name:="semigroup ";
  fi;
  
  #properties
  property:="";

  if HasIsTrivial(s) and IsTrivial(s) then 
    Append(property, "trivial ");
  else 
    if HasIsCommutativeSemigroup(s) and IsCommutativeSemigroup(s) then 
      Append(property, "commutative ");
    fi;

    if (HasIsHTrivial(s) and IsHTrivial(s)) 
      and not (HasIsBrandtSemigroup(s) and IsBrandtSemigroup(s)) then 
      Append(property, "aperiodic ");
    fi;

  fi;

  if HasIsTrivial(s) and IsTrivial(s) then 
  elif HasIsGroupAsSemigroup(s) and IsGroupAsSemigroup(s) then 
  elif HasIsZeroGroup(s) and IsZeroGroup(s) then 
  elif HasIsBrandtSemigroup(s) and IsBrandtSemigroup(s) then 
    Append(property, "Brandt ");
  elif HasIsZeroSimpleSemigroup(s) and IsZeroSimpleSemigroup(s) then 
    Append(property, "0-simple ");
  elif HasIsCliffordSemigroup(s) and IsCliffordSemigroup(s) then
    Append(property, "Clifford ");
  elif HasIsInverseSemigroup(s) and IsInverseSemigroup(s) then 
    if HasIsFactorisableSemigroup(s) and IsFactorisableSemigroup(s) then 
      Append(property, "factorisable ");
    fi;
    Append(property, "inverse ");
  elif HasIsLeftZeroSemigroup(s) and IsLeftZeroSemigroup(s) then 
    Append(property, "left zero ");
  elif HasIsRightZeroSemigroup(s) and IsRightZeroSemigroup(s) then 
    Append(property, "right zero ");
  elif HasIsLeftSimple(s) and IsLeftSimple(s) then 
    Append(property, "left simple ");
  elif HasIsRightSimple(s) and IsRightSimple(s) then 
    Append(property, "right simple ");
  elif HasIsSimpleSemigroup(s) and IsSimpleSemigroup(s) then 
    Append(property, "simple ");
  #elif HasIsCompletelyRegularSemigroup(s) and IsCompletelyRegularSemigroup(s)
  # then 
  #  Append(property, "completely regular ");
  elif HasIsOrthodoxSemigroup(s) and IsOrthodoxSemigroup(s) then 
    Append(property, "orthodox ");
  elif HasIsRegularSemigroup(s) then 
    if IsRegularSemigroup(s) then 
      Append(property, "regular ");
    else
      Append(property, "non-regular ");
    fi;
  elif HasIsAdequateSemigroup(s) and IsAdequateSemigroup(s) then
    Append(property, "adequate ");
  fi;

  Print("<", property, type, name);

  if IsMatrixSemigroup(s) then
    n:=Length(GeneratorsOfSemigroup(s)[1][1]);
    Print(n, "x", n, " over ", BaseDomain(GeneratorsOfSemigroup(s)[1][1]), " ");
  fi;
  
  if HasSize(s) then 
    if SizeScreen()[1]-Length(property)-Length(type)-Length(name)-40
      > Length(String(Size(s))) then 
      Print("of size ", Size(s), ", ");
    fi;
  elif not IsMatrixSemigroup(s) then 
    Print("of ");
  fi;

  if IsTransformationSemigroup(s) then 
    Print("degree ", DegreeOfTransformationSemigroup(s), " ");
  elif IsPartialPermSemigroup(s) then 
    Print("degree ", DegreeOfPartialPermSemigroup(s), " ");
  elif IsBipartitionSemigroup(s) then 
    Print("degree ", DegreeOfBipartitionSemigroup(s)/2, " "); 
  elif IsBinaryRelationSemigroup(s) then 
    Print("degree ", Length(Successors(Generators(s)[1])), " ");
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

  if HasGeneratorsOfInverseMonoid(s) then 
    return GeneratorsOfInverseMonoid(s);
  elif HasGeneratorsOfInverseSemigroup(s) then 
    return GeneratorsOfInverseSemigroup(s);
  elif IsMonoid(s) then
    return GeneratorsOfMonoid(s);
  fi;

  return GeneratorsOfSemigroup(s);
end);

#

InstallMethod(\.,"for a semigroup with generators and pos int",
[IsSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, n)
  s:=GeneratorsOfSemigroup(s);
  n:=NameRNam(n);
  n:=Int(n);
  if n=fail or Length(s)<n then
    Error("the second argument should be a positive integer not greater than",
     " the number of generators of the semigroup in the first argument");
  fi;
  return s[n];
end);

#

InstallMethod(\., "for a monoid with generators and pos int",
[IsMonoid and HasGeneratorsOfMonoid, IsPosInt],
function(s, n)
  s:=GeneratorsOfMonoid(s);
  n:=NameRNam(n);
  n:=Int(n);
  if n=fail or Length(s)<n then
    Error("the second argument should be a positive integer not greater than",
     " the number of generators of the semigroup in the first argument");
  fi;
  return s[n];
end);

#

InstallMethod(\.,"for an inverse semigroup with generators and pos int",
[IsInverseSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, n)
  s:=GeneratorsOfInverseSemigroup(s);
  n:=NameRNam(n);
  n:=Int(n);
  if n=fail or Length(s)<n then
    Error("the second argument should be a positive integer not greater than",
     " the number of generators of the semigroup in the first argument");
  fi;
  return s[n];
end);


#

InstallMethod(\., "for an inverse monoid with generators and pos int",
[IsInverseMonoid and HasGeneratorsOfInverseMonoid, IsPosInt],
function(s, n)
  s:=GeneratorsOfInverseMonoid(s);
  n:=NameRNam(n);
  n:=Int(n);
  if n=fail or Length(s)<n then
    Error("the second argument should be a positive integer not greater than",
     " the number of generators of the semigroup in the first argument");
  fi;
  return s[n];
end);



# returns a semigroup generating set for the inverse semigroup generated by
# <coll>.

InstallOtherMethod(GeneratorsOfSemigroup, 
"for an associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithSemigroupInverseCollection and IsAssociativeElementWithActionCollection],
function(coll)
  local gens, f;

  gens:=ShallowCopy(coll);
  for f in coll do
    if not IsInSubgroupOfSemigroup(f) then 
      Add(gens, f^-1);
    fi;
  od;

  return gens;
end);

# move to lib JDM move to lib

InstallOtherMethod(IsSubsemigroup, 
"for semigroup and semigroup with generators",
[IsSemigroup, IsSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  return ForAll(GeneratorsOfSemigroup(t), x-> x in s);
end);

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

InstallOtherMethod(SemigroupByGenerators, 
"for an associative element with action collection",
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
        " generators so far\n");
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
    if IsOne(gens[1]) and IsBound(gens[2]) and ActionRank(gens[2])=n then
      #remove id
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

  if IsAssociativeElementWithSemigroupInverse(arg[1]) 
   or IsAssociativeElementWithSemigroupInverseCollection(arg[1]) then 
    out:=[]; 
    for i in [1..Length(arg)] do 
      if IsAssociativeElementWithSemigroupInverse(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsAssociativeElementWithSemigroupInverseCollection(arg[i]) then 
        if IsActingSemigroup(arg[i]) then
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

  if IsAssociativeElementWithSemigroupInverse(arg[1]) 
   or IsAssociativeElementWithSemigroupInverseCollection(arg[1]) then 
    out:=[]; 
    for i in [1..Length(arg)] do 
      if IsAssociativeElementWithSemigroupInverse(arg[i]) then 
        out[i]:=[arg[i]];
      elif IsAssociativeElementWithSemigroupInverseCollection(arg[i]) then 
        if IsActingSemigroup(arg[i]) then
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

InstallMethod(InverseMonoidByGenerators, 
"for associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithSemigroupInverseCollection and
IsAssociativeElementWithActionCollection],
function(coll)
  return InverseMonoidByGeneratorsNC(GeneratorsOfSemigroup(coll), coll,
   SemigroupsOptionsRec);
end);

#

InstallMethod(InverseSemigroupByGenerators, 
"for associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithSemigroupInverseCollection and
IsAssociativeElementWithActionCollection],
function(coll)
  return InverseSemigroupByGeneratorsNC(GeneratorsOfSemigroup(coll), coll,
   SemigroupsOptionsRec);
end);

#

InstallOtherMethod(InverseMonoidByGenerators, 
"for associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithSemigroupInverseCollection and IsAssociativeElementWithActionCollection, IsRecord],
function(coll, record)
  local gens;
  
  record:=SemigroupOptions(record);
  
  if not record.small then
    gens:=GeneratorsOfSemigroup(coll);
  else
    gens:=coll;
  fi;

  return InverseMonoidByGeneratorsNC(gens, coll, record);
end);

#

InstallOtherMethod(InverseSemigroupByGenerators, 
"for associative element with semigroup inverse and action collection", 
[IsAssociativeElementWithSemigroupInverseCollection and IsAssociativeElementWithActionCollection, IsRecord],
function(coll, record)
  local gens;
  
  record:=SemigroupOptions(record);
  
  if not record.small then
    gens:=GeneratorsOfSemigroup(coll);
  else
    gens:=coll;
  fi;

  return InverseSemigroupByGeneratorsNC(gens, coll, record);
end);

# <gens> are a semigroup generating set for the inverse semigroup generated by
# <coll>.

InstallMethod(InverseMonoidByGeneratorsNC, 
"for associative element with semigroup inverse and action collections, and record",  
[IsAssociativeElementWithSemigroupInverseCollection and
IsAssociativeElementWithActionCollection,
IsAssociativeElementWithSemigroupInverseCollection and
IsAssociativeElementWithActionCollection, IsRecord],
function(gens, coll, record)
  local closure_opts, s, filts, f;

  if record.small and Length(gens)>1 then 
    coll:=SSortedList(ShallowCopy(coll));
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));;
    Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);;
    
    closure_opts:=rec(small:=false, hashlen:=record.hashlen);
    s:=InverseMonoid(coll[1], closure_opts);
    
    for f in coll do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;

  filts:=IsMagmaWithOne and IsInverseSemigroup and IsAttributeStoringRep;
  
  if record.acting then 
    filts:=filts and IsActingSemigroupWithInverseOp;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), filts), rec(opts:=record));

  SetGeneratorsOfMagmaWithOne(s, gens);
  SetGeneratorsOfInverseSemigroup(s, Concatenation([One(s)], coll));
  SetGeneratorsOfInverseMonoid(s, coll);
  return s;
end);

# <gens> are a semigroup generating set for the inverse semigroup generated by
# <coll>.

InstallMethod(InverseSemigroupByGeneratorsNC, 
"for partial perm coll, partial perm coll, and record",
[IsAssociativeElementWithSemigroupInverseCollection and
IsAssociativeElementWithActionCollection,
IsAssociativeElementWithSemigroupInverseCollection and
IsAssociativeElementWithActionCollection, IsRecord],
function(gens, coll, record)
  local closure_opts, s, filts, f;

  if record.small and Length(gens)>1 then 
    coll:=SSortedList(ShallowCopy(coll));
    coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));;
    Sort(coll, function(x, y) return x[2]>y[2]; end);;
    
    closure_opts:=rec(small:=false, hashlen:=record.hashlen);
    s:=InverseSemigroup(coll[1], closure_opts);
    for f in coll do
      if not f in s then 
        s:=ClosureInverseSemigroupNC(s, [f], closure_opts);
      fi;
    od;
    return s;
  fi;
  
  filts:=IsMagma and IsInverseSemigroup and IsAttributeStoringRep;
  
  if record.acting then 
    filts:=filts and IsActingSemigroupWithInverseOp;
  fi;

  s:=Objectify( NewType (FamilyObj( gens ), filts), rec(opts:=record));

  SetGeneratorsOfMagma(s, gens);
  SetGeneratorsOfInverseSemigroup(s, coll);
  return s;
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
function(s, coll, record)
  local t, coll_copy, o, f;
 
  if coll=[] then
    Info(InfoSemigroups, 2, "the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;

  coll_copy:=GeneratorsOfSemigroup(coll);
  o:=StructuralCopy(LambdaOrb(s));
  AddGeneratorsToOrbit(o, coll_copy);

  t:=InverseSemigroupByGeneratorsNC(o!.gens, 
   Concatenation(Generators(s), coll), record);

  #remove everything related to strongly connected components
  Unbind(o!.scc); Unbind(o!.trees); Unbind(o!.scc_lookup);
  Unbind(o!.mults); Unbind(o!.schutz); Unbind(o!.reverse);
  Unbind(o!.rev); Unbind(o!.truth); Unbind(o!.schutzstab); Unbind(o!.slp);

  o!.semi:=t;
  o!.scc_reps:=[One(Generators(t))];
  
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
"for an acting semigroup and associative element with action coll",
[IsActingSemigroup, IsList and IsEmpty],
function(s, coll)
  return s;
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
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(Representative(coll)) then 
    Error("the semigroup and collection of elements are not of the same type,");
    return;
  fi;

  record.small:=false;

  if IsActingSemigroup(coll) then 
    coll:=Generators(coll);
  fi;

  if IsActingSemigroupWithFixedDegreeMultiplication(s) and
    ActionDegree(s)<>ActionDegree(Representative(coll)) then 
    Error("usage: the degree of the semigroup and collection must be equal,");
    return;
  fi;

  return ClosureSemigroupNC(s, Filtered(coll, x-> not x in s),
   SemigroupOptions(record));
end);

# coll should consist of elements not in s

InstallGlobalFunction(ClosureSemigroupNC,
function(s, coll, opts)
  local t, old_o, o, new_data, old_data, max_rank, ht, new_orb, old_orb, new_nr, old_nr, graph, old_graph, reps, repslookup, orblookup1, orblookup2, repslens, lenreps, new_schreierpos, old_schreierpos, new_schreiergen, old_schreiergen, new_schreiermult, old_schreiermult, gens, nr_new_gens, nr_old_gens, lambda, lambdaact, lambdaperm, rho, lambdarhoht, oht, scc, old_scc, lookup, old_lookup, old_to_new, htadd, htvalue, i, x, pos, m, rank, y, rhoy, val, schutz, tmp, old, j, n;
 
  if coll=[] then 
    Info(InfoSemigroups, 2, "all the elements in the collection belong to the ",
    " semigroup,");
    return s;
  fi;
  

  # init the semigroup or monoid
  if IsMonoid(s) then 
    t:=Monoid(s, coll, opts);
  else
    t:=Semigroup(s, coll, opts);
  fi;
  
  # if nothing is known about s, then return t
  if not HasLambdaOrb(s) then 
    return t;
  fi;
  
  # set up lambda orb for t
  old_o:=LambdaOrb(s);
  o:=StructuralCopy(old_o);
  AddGeneratorsToOrbit(o, coll);

  # unbind everything related to strongly connected components, since 
  # even if the orbit length doesn't change the strongly connected components
  # might
  Unbind(o!.scc); Unbind(o!.trees); Unbind(o!.scc_lookup);
  Unbind(o!.mults); Unbind(o!.schutz); Unbind(o!.reverse); 
  Unbind(o!.rev); Unbind(o!.truth); Unbind(o!.schutzstab); Unbind(o!.slp); 
  
  o!.semi:=t;
  o!.scc_reps:=[One(Generators(t))];

  SetLambdaOrb(t, o); 
  
  if not HasSemigroupData(s) or SemigroupData(s)!.pos=0 then 
    return t;
  fi;
  
  # get new and old R-rep orbit data
  new_data:=SemigroupData(t);
  old_data:=SemigroupData(s);
  max_rank:=MaximumList(List(coll, ActionRank)); 

  ht:=new_data!.ht;       
  # so far found R-reps
  
  new_orb:=new_data!.orbit;   
  old_orb:=old_data!.orbit;   
  # the so far found R-reps data 
  
  new_nr:=Length(new_orb);
  old_nr:=Length(old_orb);
  # points in orb in position at most i have descendants
  
  graph:=new_data!.graph; 
  old_graph:=old_data!.graph;
  graph[1]:=ShallowCopy(old_graph[1]);
  # orbit graph of orbit of R-classes under left mult 

  reps:=new_data!.reps;   
  # reps grouped by equal lambda and rho value 
  # HTValue(lambdarhoht, Concatenation(lambda(x), rho(x))
  
  repslookup:=new_data!.repslookup; 
  # Position(orb, reps[i][j])=repslookup[i][j] = HTValue(ht, reps[i][j])
 
  orblookup1:=new_data!.orblookup1; 
  # orblookup1[i] position in reps containing orb[i][4] (the R-rep)
  
  orblookup2:=new_data!.orblookup2;
  # orblookup2[i] position in reps[orblookup1[i]] 
  # containing orb[i][4] (the R-rep)

  repslens:=new_data!.repslens;
  # Length(reps[i])=repslens[i] 

  lenreps:=new_data!.lenreps;       
  # lenreps=Length(reps)
  
  # schreier
  new_schreierpos:=new_data!.schreierpos;
  old_schreierpos:=old_data!.schreierpos;
  new_schreiergen:=new_data!.schreiergen;
  old_schreiergen:=old_data!.schreiergen;
  new_schreiermult:=new_data!.schreiermult;
  old_schreiermult:=old_data!.schreiermult;

  # generators
  gens:=new_data!.gens;
  nr_new_gens:=Length(gens);
  nr_old_gens:=Length(old_data!.gens);

  # lambda/rho
  lambda:=LambdaFunc(s);
  lambdaact:=LambdaAct(s);
  lambdaperm:=LambdaPerm(s);
  rho:=RhoFunc(s);
  lambdarhoht:=LambdaRhoHT(t);

  oht:=o!.ht;
  scc:=OrbSCC(o);
  old_scc:=OrbSCC(old_o);
  lookup:=o!.scc_lookup; 
  old_lookup:=old_o!.scc_lookup;
  
  # look up for old_to_new[i]:=Position(new_orb, old_orb[i]);
  # i.e. position of old R-rep in new_orb
  
  old_to_new:=EmptyPlist(old_nr);
  old_to_new[1]:=1;

  if IsBound(HTAdd_TreeHash_C) then
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;

  i:=1;   
  
  # install old R-class reps in new_orb
  while new_nr<=old_nr and i<old_nr do
    i:=i+1;
    x:=old_orb[i][4];
    pos:=old_schreiermult[i];
    m:=lookup[pos];
    rank:=ActionRank(x);
    
    if rank>max_rank or scc[m][1]=old_scc[old_lookup[pos]][1] then 
      y:=x;
    elif pos=old_scc[old_lookup[pos]][1] then 
      y:=x*LambdaOrbMult(o, m, pos)[2];
    else
      # x has rectified lambda value but pos refers to the unrectified value
      y:=x*LambdaOrbMult(old_o, old_lookup[pos], pos)[1]
       *LambdaOrbMult(o, m, pos)[2];
    fi;
   
    rhoy:=[m];
    Append(rhoy, rho(y));
    val:=htvalue(lambdarhoht, rhoy);

    if val=fail or rank>max_rank then #new rho value, and hence new R-rep
      lenreps:=lenreps+1;
      htadd(lambdarhoht, rhoy, lenreps);
      new_nr:=new_nr+1;
      reps[lenreps]:=[y];
      repslookup[lenreps]:=[new_nr];
      orblookup1[new_nr]:=lenreps;
      orblookup2[new_nr]:=1;
      repslens[lenreps]:=1;
      x:=[t, m, o, y, false, new_nr];
    else              # old rho value
      x:=[t, m, o, y, false, new_nr+1];
      #check membership in schutz gp via stab chain
      schutz:=LambdaOrbStabChain(o, m);

      if schutz=true then # schutz gp is symmetric group
        old_to_new[i]:=repslookup[val][1];
        continue;
      else
        if schutz=false then # schutz gp is trivial
          tmp:=htvalue(ht, y);
          if tmp<>fail then
            old_to_new[i]:=tmp;
            continue;
          fi;
        else # schutz gp neither trivial nor symmetric group
          old:=false;
          for n in [1..repslens[val]] do
            if SiftedPermutation(schutz, lambdaperm(reps[val][n], y))=()
              then
              old:=true;
              old_to_new[i]:=repslookup[val][n];
              break;
            fi;
          od;
          if old then
            continue;
          fi;
        fi;
        new_nr:=new_nr+1;
        repslens[val]:=repslens[val]+1;
        reps[val][repslens[val]]:=y;
        repslookup[val][repslens[val]]:=new_nr;
        orblookup1[new_nr]:=val;
        orblookup2[new_nr]:=repslens[val];
      fi;
    fi;
    new_orb[new_nr]:=x;
    graph[new_nr]:=ShallowCopy(old_graph[i]);
    new_schreierpos[new_nr]:=old_to_new[old_schreierpos[i]];
    # orb[nr] is obtained from orb[i]
    new_schreiergen[new_nr]:=old_schreiergen[i];     
    # by multiplying by gens[j]
    new_schreiermult[new_nr]:=pos;  # and ends up in position <pos> of 
                                    # its lambda orb
    htadd(ht, y, new_nr);
    old_to_new[i]:=new_nr;
  od;
  
  # process the orbit graph

  for i in [1..new_nr] do 
    for j in [1..Length(graph[i])] do 
      graph[i][j]:=old_to_new[graph[i][j]];
    od;
  od;

  # apply new generators to old R-reps
  new_data!.genstoapply:=[nr_old_gens+1..nr_new_gens];
  new_data!.pos:=0;
  new_data!.stopper:=old_to_new[old_data!.pos];
  new_data!.lenreps:=lenreps;
  Enumerate(new_data, infinity, ReturnFalse);

  new_data!.pos:=old_to_new[old_data!.pos];
  new_data!.stopper:=false;
  new_data!.genstoapply:=[1..nr_new_gens];
  
  return t;
end);

#subsemigroups

# <limit> is the max size of the subsemigroup.

InstallOtherMethod(SubsemigroupByProperty, 
"for an acting semigroup with generators, function, and positive integer",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
 
  iter:=Iterator(S);
  
  repeat 
    f:=NextIterator(iter);
  until func(f) or IsDoneIterator(iter);
  
  if not func(f) then 
    return fail; # JDM should really return the empty semigroup
  fi;

  T:=Semigroup(f);

  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureSemigroup(T, f);
    fi;
  od;
  SetParent(T, S);
  return T;
end);

# <limit> is the max size of the subsemigroup.

InstallOtherMethod(InverseSubsemigroupByProperty, 
"for acting semigroup with inverse op & generators, function, positive integer",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, IsFunction, IsPosInt], 
function(S, func, limit)
  local iter, T, f;
 
  iter:=Iterator(S);
  
  repeat 
    f:=NextIterator(iter);
  until func(f) or IsDoneIterator(iter);
  
  if not func(f) then 
    return fail; # JDM should really return the empty semigroup
  fi;

  T:=InverseSemigroup(f);

  while Size(T)<limit and not IsDoneIterator(iter) do 
    f:=NextIterator(iter);
    if func(f) then 
      T:=ClosureInverseSemigroup(T, f);
    fi;
  od;
  SetParent(T, S);
  return T;
end);

#

InstallMethod(SubsemigroupByProperty, 
"for an acting semigroup with generators and function",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsFunction], 
function(S, func)
  return SubsemigroupByProperty(S, func, Size(S));
end);

#

InstallOtherMethod(InverseSubsemigroupByProperty, 
"for acting semigroup with inverse op & generators and function",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, IsFunction], 
function(S, func)
  return InverseSubsemigroupByProperty(S, func, Size(S));
end);

# JDM also InverseSubmonoidNC..

InstallMethod(InverseSubsemigroupNC, 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, 
IsAssociativeElementWithSemigroupInverseCollection],
function(s, gens)
  local t;

  t:=InverseSemigroup(gens);
  SetParent(t, s);
  return t;
end);

#

InstallMethod(InverseSubsemigroup, 
"for an acting semigroup with inverse op & generators and element coll",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, 
IsAssociativeElementWithSemigroupInverseCollection],
function(s, gens)
  if ForAll(gens, x-> x in s) then 
    return InverseSubsemigroupNC(s, gens);
  fi;
  Error("the specified elements do not belong to the first argument,");
  return;
end);

#

InstallMethod(InverseSubmonoidNC, 
"for an acting semigroup with inverse op & generators and element coll",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, 
IsAssociativeElementWithSemigroupInverseCollection],
function(s, gens)
  local t;

  t:=InverseMonoid(gens);
  SetParent(t, s);
  return t;
end);

#

InstallMethod(InverseSubmonoid, 
"for an acting semigroup with inverse op & generators and element coll",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup, 
IsAssociativeElementWithSemigroupInverseCollection],
function(s, gens)
  if IsMonoid(s) and ForAll(gens, x-> x in s) then 
    if DegreeOfPartialPermCollection(gens)<DegreeOfPartialPermSemigroup(s) then 
      Append(gens, One(s));
    fi;
    return InverseSubmonoidNC(s, gens);
  fi;
  Error("the specified elements do not belong to the first argument,");
  return;
end);


#miscellaneous

InstallGlobalFunction(RegularSemigroup, 
function(arg)
  if not IsRecord(arg[Length(arg)]) then 
    Add(arg, rec(regular:=true));
  else
    arg[Length(arg)].regular:=true;
  fi;
  return CallFuncList(Semigroup, arg);
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
