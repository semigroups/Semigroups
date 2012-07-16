#############################################################################
##
#W  regular.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## Methods for Green's classes of regular acting semigroups

# new for 1.0! - \in - "for a regular acting semigroup and elt"
##############################################################################

InstallMethod(\in, "for an acting elt and regular acting semigroup",
[IsActingElt, IsActingSemigroup and IsRegularSemigroup], 
function(f, s)
  local lambda_o, rho_o, lambda, rho, lambda_l, rho_l, m, schutz, scc, g;
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(f) then 
    Error("the element and semigroup are not of the same type,");
    return;
  fi;

  if HasAsSSortedList(s) then 
    return f in AsSSortedList(s); 
  fi;

  lambda_o:=LambdaOrb(s);
  rho_o:=RhoOrb(s);
  lambda:=LambdaFunc(s)(f);
  rho:=RhoFunc(s)(f); 

  if IsClosed(lambda_o) then
    lambda_l:=Position(lambda_o, lambda);
    if lambda_l=fail or (lambda_l=1 and not IsMonoidAsSemigroup(s)) then
      return false;
    fi;
    rho_l:=Position(rho_o, rho);
    if rho_l=fail then
      return false;
    fi;
  elif IsClosed(rho_o) then 
    rho_l:=Position(rho_o, rho);
    if rho_l=fail or (rho_l=1 and not IsMonoidAsSemigroup(s)) then
      return false;
    fi;
    lambda_l:=Position(lambda_o, lambda);
    if rho_l=fail then
      return false;
    fi;
  else
    lambda_l:=Position(lambda_o, lambda);
    if lambda_l=fail then
      lambda_o!.looking:=true; 
      lambda_o!.lookingfor:=function(o, x) return x=lambda; end;
      lambda_o!.lookfunc:=lambda_o!.lookingfor;
      Enumerate(lambda_o);
      lambda_l:=PositionOfFound(lambda_o);
      lambda_o!.found:=false; lambda_o!.looking:=false;
      Unbind(lambda_o!.lookingfor); Unbind(lambda_o!.lookfunc);

      if lambda_l=false then
        return false;
      fi;
    fi;

    if lambda_l=1 and not IsMonoidAsSemigroup(s) then
      return false;
    fi;

    rho_l:=Position(rho_o, rho);

    if rho_l=fail then
      rho_o!.looking:=true; 
      rho_o!.lookingfor:=function(o, x) return x=rho; end;
      rho_o!.lookfunc:=rho_o!.lookingfor;
      Enumerate(rho_o);
      rho_l:=PositionOfFound(rho_o);
      rho_o!.found:=false; rho_o!.looking:=false;
      Unbind(rho_o!.lookingfor); Unbind(rho_o!.lookfunc);

      if rho_l=false then
        return false;
      fi;
    fi;
  fi;

  if not IsClosed(lambda_o) then  
    lambda_o:=GradedLambdaOrb(s, f, false);
    Enumerate(lambda_o, infinity);
    lambda_l:=1;
  fi;
 
  m:=OrbSCCLookup(lambda_o)[lambda_l];
  schutz:=LambdaOrbStabChain(lambda_o, m);

  if schutz=true then
    return true;
  fi;

  scc:=OrbSCC(lambda_o)[m];
  g:=f;
  
  if lambda_l<>scc[1] then 
    g:=g*LambdaOrbMults(lambda_o, m)[lambda_l][2];
  fi;

  if IsIdempotent(g) then 
    return true;
  elif schutz=false then
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(One(g), g))=();
end);

