

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


