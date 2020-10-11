#############################################################################
##
##  obsolete.gi
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

BindGlobal("_PrintObsolete", function(old, arg...)
  Print("#I  `", old, "` is no longer supported\n",
        "#I  use `", Concatenation(List(arg, String)), "` instead!\n");
end);
