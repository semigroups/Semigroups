############################################################################# 
## 
#W  options.g 
#Y  Copyright (C) 2011-12                                 James D. Mitchell
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

# new for 0.6! - CitrusOptionsRec - global variable

BindGlobal("CitrusOptionsRec", 
  rec(  schreier:=true, 
        small:=false,
        hashlen:=rec(S:=251, M:=6257, L:=25013)
      ));

MakeReadWriteGlobal("CitrusOptionsRec");

