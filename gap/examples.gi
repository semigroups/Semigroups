############################################################################# 
## 
#W  examples.gi
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

#FFF

# new for 0.7! - FullMatrixSemigroup - "for a pos int and pos int"
################################################################################

InstallMethod(FullMatrixSemigroup, "for pos int and pos int",  
[IsPosInt, IsPosInt],
function(d,q)
  local g, S;

  g:=List([1..d], x-> List([1..d], function(y) if y=x and not y=d then
   return Z(q)^0; else return 0*Z(q); fi; end));
  g:=OneMutable(GeneratorsOfGroup(GL(d,q))[1]);
  g[d][d]:=Z(q)*0;

  S:=Monoid(Concatenation(GeneratorsOfGroup(GL(d,q)), [g]));
  SetIsMatrixSemigroup(S, true);
  SetIsFullMatrixSemigroup(S, true);
  SetIsGeneralLinearSemigroup(S, true);

  return S;
end);

#GGG

# new for 0.7! - GeneralLinearSemigroup - "for a pos int and pos int"
################################################################################

InstallMethod(GeneralLinearSemigroup, "for 2 pos ints", 
[IsPosInt, IsPosInt], FullMatrixSemigroup);

# new for 0.7! - IsFullMatrixSemigroup - "for a pos int and pos int"
################################################################################

InstallMethod(IsFullMatrixSemigroup, "for a semigroup", 
[IsSemigroup], ReturnFalse);

# new for 0.7! - IsGeneralLinearSemigroup - "for a pos int and pos int"
################################################################################

InstallOtherMethod(IsGeneralLinearSemigroup, "for a semigroup",
[IsSemigroup], ReturnFalse);

# new for 0.7! - MonoidOfMultiplicationByN - "for a pos int"
################################################################################
# undoc
# from the semigroupe manual... JDM is this right?

InstallMethod(MonoidOfMultiplicationByN, "for a pos int",
[IsPosInt],
function(n)
  local out, i;

  out:=EmptyPlist(n);
  for i in [1..n] do 
    out[2*i-1]:=i;
    out[2*i]:=i;
  od;

  return Monoid(Transformation(out{[1..n]}),Transformation(out{[n+1..2*n]}));
end);

#OOO

# new for 0.7! - O - "for a pos int"
################################################################################

InstallMethod(O, "for a pos int",
[IsPosInt],
function(n)
  local gens, i;

  gens:=EmptyPlist(n);
  gens[1]:=Transformation(Concatenation([1], [1..n-1]));

  for i in [1..n-1] do
    gens[i+1]:=[1..n];
    gens[i+1][i]:=i+1;
    gens[i+1]:=TransformationNC(gens[i+1]);
  od; 

  return Monoid(gens);
end);

#PPP

# new for 0.7! - POI - "for a pos int"
################################################################################

InstallMethod(POI, "for a pos int",
[IsPosInt],
function(n)
  local out, i;

  out:=EmptyPlist(n);
  out[1]:=PartialPermNC([0..n-1]);
  for i in [0..n-2] do 
    out[i+2]:=[1..n];
    out[i+2][(n-i)-1]:=n-i; out[i+2][n-i]:=0;
    out[i+2]:=PartialPermNC(out[i+2]);
  od;
  return InverseMonoid(out); 
end);

# new for 0.7! - POPI - "for a pos int"
################################################################################

InstallMethod(POPI, "for a pos int",
[IsPosInt],
function(n)
  return InverseMonoid(PartialPermNC(Concatenation([2..n],[1])), 
   PartialPermNC(Concatenation([1..n-2],[n])));
end);

# new for 0.7! - PowerSemigroup - "for a group"
################################################################################
# undoc

InstallMethod(PowerSemigroup, "for a group",
[IsGroup],
function(g)
  local act, dom, gens, s, i, f;

  act:=function(A, B) return Union(List(A, x-> x*B)); end;
  dom:=Combinations(Elements(g));
  Sort(dom, function(x,y) return Length(x)<Length(y); end);
  gens:=[TransformationActionNC(dom[1], dom, act)];
  s:=Semigroup(gens);
  i:=2;

  while Size(s)<2^Size(g) do  
    i:=i+1;
    f:=TransformationActionNC(dom[i], dom, act);
    if not f in s then 
      Add(gens, f);
      s:=Semigroup(gens);
    fi;
  od;
  return s;
end);

#SSS

# new for 0.7! - SingularSemigp - "for a pos int"
################################################################################

InstallMethod(SingularSemigroup, "for a pos int",
[IsPosInt],
function(n)
  local img, x, S, T;
  img:=Concatenation([1..n-1], [n-1]);
  x:=TransformationNC(img);
  S:=FullTransformationSemigroup(n);
  T:=SubsemigroupNC(S, Idempotents(GreensDClassOfElementNC(S, x)));
  return T;
end);

# new for 0.7! - SymmetricInverseSemigp - "for a pos int"
################################################################################

InstallMethod(SymmetricInverseSemigp, "for a pos int",
[IsPosInt],
function(n)

  if n=0 then
    return InverseSemigroup(PartialPermNC([]));
  fi;

  return InverseSemigroup(List(GeneratorsOfGroup(SymmetricGroup(n)), x->
   PartialPermNC(ListPerm(x, n))), PartialPermNC([0..n-1]*1));
end);

#VVV

# new for 0.7! - ViewObj - "for full matrix semigroup"
################################################################################

InstallMethod(ViewObj, "for full matrix semigroup",
[IsFullMatrixSemigroup], 10,
function( obj )        
  local n;
  n:=Length(GeneratorsOfMonoid(obj)[1][1]);
  Print( "<full matrix semigroup ",n, "x", n, " over ",
   BaseDomain(GeneratorsOfMonoid(obj)[1][1]), ">");         
  return;
end); 

#EOF
