#
# gapbind_demo: Minimal demo of how to use gapbind14
#
# Reading the declaration part of the package.
#
_PATH_SO:=Filename(DirectoriesPackagePrograms("gapbind_demo"), "gapbind_demo.so");
if _PATH_SO <> fail then
    LoadDynamicModule(_PATH_SO);
fi;
Unbind(_PATH_SO);

if not IsBoundGlobal("IsTGapBind14Obj") then
  DeclareCategory("IsTGapBind14Obj", IsObject);
  BindGlobal("TheTypeTGapBind14Obj",
             NewType(NewFamily("TGapBind14ObjFamily"), IsTGapBind14Obj));
fi;

ReadPackage( "gapbind_demo", "gap/gapbind_demo.gd");
