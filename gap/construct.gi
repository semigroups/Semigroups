


InstallOtherMethod(DirectProductOp, 
"for a trans. monoid and trans. monoid",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup,
IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  local close, m, dom, act, gens;

  #JDM something about monoids....

  close:=function(gens)
    local x, y;

    for x in gens do 
      y:=x^2;
      if not y in gens then 
        Add(gens, y);
      fi;
    od;
    return gens;
  end;
 
  m:=DegreeOfTransformationSemigroup(s);
  dom:=[1..m+DegreeOfTransformationSemigroup(t)]*1;
  act:=function(x, f)
    if x<=m then 
      return x;
    fi;
    return (x-m)^f+m;
  end;
  
  gens:=List(GeneratorsOfSemigroup(t), x-> TransformationOp(x, dom, act));
  Append(gens, List(GeneratorsOfSemigroup(s),
  x-> AsTransformation(x, m+DegreeOfTransformationSemigroup(t))));
  return Semigroup(gens);
end); 




