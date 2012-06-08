
MakeReadWriteGlobal("TransformationFamily");
UnbindGlobal("TransformationFamily");
MakeReadWriteGlobal("TransformationType");
UnbindGlobal("TransformationType");

BindGlobal("TransformationFamily", NewFamily("TransformationFamily",
 IsTransformation, CanEasilySortElements, CanEasilySortElements));

BindGlobal("TransformationType", NewType(TransformationFamily,
 IsTransformation and IsDataObjectRep and IsActingElt));

# new for 0.7! - ELM_LIST - "for a trans and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a trans and a pos int",
[IsTransformation, IsPosInt], ELM_LIST_T);

# new for 0.7! - ELMS_LIST - "for a trans and small dense list"
############################################################################

InstallOtherMethod(ELMS_LIST, "for a trans and a small dense list",
[IsTransformation, IsDenseList and IsSmallList ], ELMS_LIST_T );

# new for 0.7! - InternalRepOfTransformation - "for a transformation" 
############################################################################# 
 
InstallGlobalFunction(InternalRepOfTransformation,  
function(f) 
 
  if not IsTransformation(f) then 
    Error("the argument should be a transformation,"); 
    return; 
  fi; 

  return f{[1..4+2*f[1]+f[2]]}; 
end); 

# new for 0.7! - PrintObj - "for a transformation" 
############################################################################# 
 
InstallMethod(PrintObj, "for a transformation",
[IsTransformation], 
function(f) 
 
  if f[1]<20 then  
    Print("<transformation ", RanT(f), ">");
    return;
  fi; 
  Print("<transformation on ", f[1], " pts with rank ", f[2], ">"); 
  return; 
end);

