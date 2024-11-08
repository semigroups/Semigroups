#
# gapbind_demo: Minimal demo of how to use gapbind14
#
# Reading the declaration part of the package.
#
if LoadKernelExtension("gapbind_demo") = false then
    Error("failed to load gapbind_demo kernel extension");
fi;

if not IsBoundGlobal("IsTGapBind14Obj") then
  DeclareCategory("IsTGapBind14Obj", IsObject);
  BindGlobal("TheTypeTGapBind14Obj",
             NewType(NewFamily("TGapBind14ObjFamily"), IsTGapBind14Obj));
fi;

ReadPackage( "gapbind_demo", "gap/gapbind_demo.gd");
