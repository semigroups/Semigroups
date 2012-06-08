
BindGlobal("PartialTransFamily", NewFamily("PartialTransFamily",
 IsPartialTrans, CanEasilySortElements, CanEasilySortElements));

BindGlobal("PartialTransType", NewType(PartialTransFamily,
 IsPartialTrans and IsDataObjectRep and IsActingElt));

BindGlobal("TransFamily", NewFamily("TransFamily",
 IsPartialTrans, CanEasilySortElements, CanEasilySortElements));

BindGlobal("TransType", NewType(TransFamily,
 IsPartialTrans and IsTrans and IsDataObjectRep and IsActingElt));

# new for 0.7! - ELM_LIST - "for a part trans and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a part trans and a pos int",
[IsPartialTrans, IsPosInt], ELM_LIST_PT);

# new for 0.7! - ELMS_LIST - "for a part trans and small dense list"
############################################################################

InstallOtherMethod(ELMS_LIST, "for a partial trans and a small dense list",
[IsPartialTrans, IsDenseList and IsSmallList ], ELMS_LIST_PT );

# new for 0.7! - InternalRepOfPartialTrans - "for a partial trans" 
############################################################################# 
 
InstallGlobalFunction(InternalRepOfPartialTrans,  
function(f) 
 
  if not IsPartialTrans(f) then 
    Error("the argument should be a partial trans,"); 
    return; 
  elif IsPartialPerm(f) then 
    return InternalRepOfPartialPerm(f);
  fi; 

  if f[1]=0 then 
    return f{[1..7]};
  fi;
  
  return f{[1..7+2*f[1]+2*f[2]+f[3]]}; 
end); 

# new for 0.7! - InternalRepOfTransformation - "for a transformation" 
############################################################################# 
 
InstallGlobalFunction(InternalRepOfTransformation,  
function(f) 
 
  if not IsTrans(f) then 
    Error("the argument should be a trans,"); 
    return; 
  fi; 

  return f{[1..4+2*f[1]+f[2]]}; 
end); 


# new for 0.7! - PrintObj - "for a partial trans" 
############################################################################# 
 
InstallMethod(PrintObj, "for a partial trans", 
[IsPartialTrans], 
function(f) 
  local dom, ran; 
 
  if f[1]=0 then 
    Print("<empty mapping>"); 
    return; 
  fi;
 
  dom:=DomPT(f); ran:=RanPT(f); 
 
  if f[3]>2 then # JDM printing of [2,4] is [2, 4..4]! 
    if IsRange(dom) then  
      ConvertToRangeRep(dom); 
    fi; 
    if IsRange(ran) then  
      ConvertToRangeRep(ran); 
    fi; 
  fi; 
  if f[3]<20 then  
       
    if dom=ran then  
      Print("<identity on ", dom, ">"); 
      return; 
    fi; 
    Print(dom, " -> ", ran); 
    return; 
  fi; 
  Print("<partial transformation on ", f[2], " pts with rank ", f[3], ">"); 
  return; 
end);

# new for 0.7! - PrintObj - "for a trans" 
############################################################################# 
 
InstallMethod(PrintObj, "for a trans", 
[IsTrans], 
function(f) 
 
  if f[1]<20 then  
    Print("<transformation ", RanT(f), ">");
    return;
  fi; 
  Print("<transformation on ", f[1], " pts with rank ", f[2], ">"); 
  return; 
end);

