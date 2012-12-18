############################################################################# 
## 
#W  options.g 
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

BindGlobal("SemigroupsOptionsRec", 
  rec(  small:=false,
        hashlen:=rec(S:=251, M:=6257, L:=25013),
        regular:=false,
        acting:=true
      ));

MakeReadWriteGlobal("SemigroupsOptionsRec");

