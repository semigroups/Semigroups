

# new for 0.7! - \^ - "for a partial perm and neg int"
#############################################################################

InstallMethod(\^, "for a partial perm and neg int",
[IsPartialPerm, IsNegInt],
function(f, r)
  if not f[1]=0 then
    return Objectify(PartialPermType, InvPartPerm_C(f))^-r;
  else
    return f;
  fi;
end);

InstallMethod( ELM_LIST, "for a part perm and a pos int",
[IsPartialPerm, IsPosInt], ELM_LIST_PP);

InstallMethod( ELMS_LIST, "for a partial perm and a small dense list",
[IsPartialPerm, IsDenseList and IsSmallList ], ELMS_LIST_PP );

