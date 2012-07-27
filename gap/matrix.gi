


InstallOtherMethod(OneMutable, "for ring element coll coll coll",
[IsRingElementCollCollColl], x-> One(Representative(x)));

InstallMethod(ViewObj, "for a matrix semigroup",
[IsMatrixSemigroup], 
function(obj)
  local n;
  n:=Length(Generators(obj)[1][1]);
  Print( "<matrix semigroup ",n, "x", n, " over ",
   BaseDomain(Generators(obj)[1][1]), ">");
  return;
end);
