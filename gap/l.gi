#############################################################################
##
#W  greens_l_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to kernels/l-classes!

# - should be cleaned up like r.gi!!! In particular, standardise the inputs!

# Conventions

# -low-level function make as few functions calls as possible, higher level ones
# must use LClassSchutzGp... etc.

# - d_schutz should be moved from OrbitsOfImages to OrbitsOfKernels.

###########################################################################
# 

InstallGlobalFunction(LClassData, function(list)
return Objectify(NewType(NewFamily("Green's L Class Data", IsGreensLClassData), 
IsGreensLClassData and IsGreensLClassDataRep), list);
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassKernelOrbitFromData,
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
end);

############################################################################

InstallGlobalFunction(LClassRelsFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.rels;
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassRepFromData,
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassSCCFromData,
function(s,d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassSchutzGpFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][2];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassStabChainFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);


# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
local O, s;

s:=UnderlyingSemigroupOfIterator(iter);
O:=OrbitsOfKernels(s);

Print( "<iterator of L-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfKernels(s), " elements, ", NrLClassesOrbitsOfKernels(s), 
 " L-classes>");
return;
end);

#############################################################################
#

InstallMethod( PrintObj, "for object in `IsGreensLClassData'",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep,  " )" );
end );



# new for 4.0!
#############################################################################
# returns the size of the semigroup so far
# JDM this method should be in d.gi!

InstallGlobalFunction(SizeOrbitsOfKernels, 
function(s)
local i, c, o, m, val;

i:=0;

#Info(InfoMonoidGreens, 4, "SizeOrbitsOfKernels");

c:=OrbitsOfKernels(s)!.orbits;

for o in Concatenation(Compacted(c)) do 
  for m in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[m]) then 
      #for val in [1..Length(o!.schutz[m])] do 
      	#for l in o!.schutz[j][k] do 
      		#Error("");
      		#i:=i+Size(o!.schutz[m][val][2])*Length(o!.scc[m])*
      		# Length(o!.reps[m][val]);
      		i:=i+Size(o!.schutz[m][2])*
      		 Length(o!.scc[m])*Length(Concatenation(o!.reps[m]));
      	#od;
      #od;
    fi;
  od;
od;

#if OrbitsOfKernels(s)!.finished and not HasSize(s) then 
#	SetSize(s, i);
#fi; JDM

return i;
end);

#############################################################################
# 

InstallMethod( ViewObj, "for L-class data",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.relts,
", ", obj!.invrelts,", ", obj!.schutz, " )" );
end );
