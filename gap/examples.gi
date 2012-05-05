
#FFF

InstallMethod(FullMatrixSemigroup, "for 2 pos ints",  
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
  SetIsFinite(S, true);
  #SetSize(S, q^(d^2));

  return S;
end);

#GGG

InstallMethod(GeneralLinearSemigroup, "for 2 pos ints", 
[IsPosInt, IsPosInt], FullMatrixSemigroup);

InstallMethod(IsFullMatrixSemigroup, "for a semigroup", 
[IsSemigroup], ReturnFalse);

InstallOtherMethod(IsGeneralLinearSemigroup, "for a semigroup",
[IsSemigroup], ReturnFalse);

# from the semigroupe manual... JDM is this right?
InstallGlobalFunction(MonoidOfMultiplicationByN,
function(n)
  local out, i;

  out:=EmptyPlist(n);
  for i in [1..n] do 
    out[2*i-1]:=i;
    out[2*i]:=i;
  od;

  return Monoid(Transformation(out{[1..n]}),Transformation(out{[n+1..2*n]}));
end);

#PPP

InstallGlobalFunction(POI, 
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

InstallGlobalFunction(POPI,
function(n)
  return InverseMonoid(PartialPermNC(Concatenation([2..n],[1])), 
   PartialPermNC(Concatenation([1..n-2],[n])));
end);

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



InstallMethod( ViewObj, "for full matrix semigroup",
[IsFullMatrixSemigroup], 10,
function( obj )        
  local n;
  n:=Length(GeneratorsOfMonoid(obj)[1][1]);
  Print( "<full matrix semigroup ",n, "x", n, " over ",
   BaseDomain(GeneratorsOfMonoid(obj)[1][1]), ">");         
  return;
end); 

#EOF
