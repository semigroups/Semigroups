##############################################################################
##
##  smallestimage.g         GRAPE Library               Steve Linton
##
##  Copyright (C) Steve Linton 2003
##
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

BindGlobal("SmallestImageSet",function(arg)
    local   best,  h,  k,  n,  paths,  level,  min,  goodpaths,  
            orbnums,  orbmins,  gens,  path,  cands,  remset,  bestpt,  
            x,  q,  rep,  num,  pt,  gen,  img,  besto,  newpaths,  
            cases,  case,  newpath, g, set;
    
# Function by Steve Linton. 
# Slightly modified by Leonard Soicher, and renamed from `SmallestImage' 
# to `SmallestImageSet'.
    # This algorithm is iterative. At level i it
    # computes the lex-least i-set which can be an image
    # of any subset of set, and all the essentially different ways
    # in which this can happen. The lex-least image of set must arise
    # by extending one of these
    #
    # in concrete terms, best is that i-set, h is the sequence
    # stabilizer of best and paths is a list of records
    #
    # each record has components: mappedpts indicating which points in set
    # are mapped tuple-wise onto best; remainder is set\mappedpts;
    # perm is an element of g mapping mappedpts to best
    # substab is a subgroup of the stabilizer of mappedpts (ptwise) 
    # and remainder (setwise)
    # paths should contain entries with every possible sequence
    # mappedpts, subject to the action of the setwise stabilizer Stab(g, set)
    #
    # Each iteration is done in two phases. In the first we examine
    # each entry in paths to see what is the smallest point to which
    # any entry of remainder^perm can be mapped by h, and which entries of
    # remainder can be mapped there. We remember in goodpaths the ones that achieve
    # the global smallest point, which we add to best
    #
    # In the second part, we take each entry on goodpaths and see what
    # essentially different (under substab) ways there are to extend it
    # we add those to newpaths.
    
    if Length(arg) < 2 then
        Error("SmallestImageSet: must have at least 2 parameters");
    fi;
    
    g := arg[1];
    set := arg[2];
    
    if not IsPermGroup(g) or not IsSet(set) then
        Error("usage: SmallestImageSet( <PermGroup>, <Set> [, <PermGroup> ] )");
    fi;
         
    if set = [] then
        return [];
    fi;
    set := Set(set);
    if IsTrivial(g) then
        return set;
    fi;
    best := [];
    h := g;
    if Length(arg) >= 3 then
        k := arg[3];
        if k = false then
            k := Group(());
        fi;
        if not IsPermGroup(k) then
           Error("<k> must be a permutation group");
        fi;
    else
        k := Stabilizer(g,set,OnSets);
    fi;
    
    n := Maximum(set[Length(set)],LargestMovedPoint(g));
    paths := [rec(mappedpts := [], 
                  remainder := set, 
                  perm := (), 
                  substab := k)];
    for level in [1..Length(set)] do
        if Size(h) = 1 then
            Append(best, Minimum(List(paths, path -> OnSets(path.remainder, path.perm))));
            return best;
        fi;
        min := infinity;
        goodpaths := [];
        orbnums := ListWithIdenticalEntries(n,-1);
        orbmins := [];
        gens := GeneratorsOfGroup(h);
        for path in paths do
            if Size(path.substab) = 1 then
                cands := path.remainder;
            else
                cands := List(OrbitsDomain(path.substab, path.remainder), o->o[1]);
            fi;
            remset := OnTuples(cands,path.perm);
            
            #
            # We need to decide the smallest image of anything in remset
            # under h. We build up a orbit numbers data structure
            # lazily so that we only do the orbit calculations we need
            # and only do them once.
            #
            
            bestpt := infinity;
            for x in remset do
                if orbnums[x] = -1 then
                    
                    #
                    # Need a new orbit. Also require the smallest point
                    # as the rep.
                    #
                    q := [x];
                    rep := x;
                    num := Length(orbmins)+1;
                    orbnums[x] := num;
                    for pt in q do
                        for gen in gens do
                            img := pt^gen;
                            if orbnums[img] = -1 then
                                orbnums[img] := num;
                                Add(q,img);
                                if img < rep then
                                    rep := img;
                                fi;
                            fi;
                        od;
                    od;
                    orbmins[num] := rep;
                else
                    num := orbnums[x];
                    rep := orbmins[num];
                fi;
                
                #
                # Does this help?
                #
                if rep < bestpt then
                    besto := num;
                    bestpt := rep;
                fi;
            od;
            
            if bestpt < min then
                #
                # Improved the global bound, forget all previous candidates
                #
                goodpaths := [];
                min := bestpt;
            fi;
            
            if bestpt = min then
                
                #
                # rmemeber this case
                #
                
                path.relevant :=  Filtered(path.remainder, x->
                                          orbnums[x^path.perm] = besto);
                Add(goodpaths, path);
            fi;
        od;
        
        #
        # Here goodpaths contains the options that need further exploration.
        # min is the next point in the smallest image
        #
        
        Add(best,min);
        if level = Length(set) then
            return best;
        fi;
        
        newpaths := [];
        for path in goodpaths do
            
            #
            # We can reduce the search by using some residual symmetry
            #
            if IsTrivial(path.substab) then
                cases := path.relevant;
            else
                cases := List(Orbits(path.substab,path.relevant), o->o[1]);
            fi;
            
            
            for case in cases do
                newpath := StructuralCopy(path);
                Add(newpath.mappedpts,case);
                #
                # use Representative action this way round so that the
                # non-changing point is first. Dramatically reduces the number
                # of base changes
                newpath.perm  := newpath.perm / RepresentativeAction(h,min,case^path.perm);
                RemoveSet(newpath.remainder,case);
                newpath.substab := Stabilizer(newpath.substab,case);
                Add(newpaths, newpath);
            od;
        od;
        paths := newpaths;
        h := Stabilizer(h,min);
    od;
end);
