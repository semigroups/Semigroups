trace := function(m)
  local trace, i;
  if IsMatrix(m) then 
    trace := 0;
    for i in [1 .. Minimum(Length(m), Length(m[1]))] do
      trace := trace + m[i][i];
    od;
    return trace;
  else
    Error("the argument is not an matrix,");
  fi;
end; 
