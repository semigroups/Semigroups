


InstallOtherMethod(DirectProductOp, 
"for a trans. monoid and trans. monoid",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup,
IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  local close, m, dom, act, gens;

  #JDM something about monoids....
  if IsMonoid(s) then 

    m:=DegreeOfTransformationSemigroup(s);
  
    dom:=[1..m+DegreeOfTransformationSemigroup(t)]*1;
  
    act:=function(x, f)
      if x<=m then 
        return x;
      fi;
      return (x-m)^f+m;
    end;
  
    gens:=List(GeneratorsOfMonoid(t), x-> TransformationOp(x, dom, act));
    Append(gens, List(GeneratorsOfSemigroup(s),
    x-> AsTransformation(x, m+DegreeOfTransformationSemigroup(t))));
    return Monoid(gens);
  elif IsMonoidAsSemigroup(s) then
    
  fi;

end); 




