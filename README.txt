##
## README.txt
## Version 3.1.1
## Mon Jun  9 17:02:20 BST 2008
##

This is the README file of the MONOID package version 3.1.1 for 
computing with transformation semigroups. MONOID 3.1.1 is an updated version of 
the package with the same name for GAP 3; see 

http://schmidt.nuigalway.ie/monoid/index.html

for more information about the original MONOID by Goetz 
Pfeiffer and Steve A. Linton, Edmund F. Robertson and Nik Ruskuc.

MONOID 3.1.1 retains all the functionality of the original MONOID package. 
In particular, MONOID 3.1.1 contains more efficient methods than those  available
in the GAP library for computing orbits, calculating Green's 
classes, finding the size, the elements, and testing membership in 
transformation semigroups. After MONOID has been loaded many of these methods 
are automatically used in preference to those in the library and do not need to 
be called explicitly by the user. 

In addition, there are new methods for testing if a semigroup satisfies a 
particular property, such as if it is regular, simple, inverse, or completely 
regular; computing the automorphism group of a transformation semigroup;
homomorphisms and isomorphism between some types of semigroup; and functions to 
create some well-known transformation semigroups. 

The MONOID package is written in GAP code only but relies on the GRAPE package 
in the methods for computing the automorphism group of a semigroup. The 
following functions can only be used fully if GRAPE is fully installed.  That
is, `AutomorphismGroup' with argument a transformation semigroup or a Rees zero 
matrix semigroup, `RightTransStabAutoGroup' for a Rees zero matrix 
semigroup, `RZMSGraph', `RZMSInducedFunction', `RZMStoRZMSInducedFunction', and 
`IsomorphismSemigroups' with both arguments Rees zero matrix semigroups.  Please 
see the manual entries for these functions for further information.
Installation of GRAPE is described in the README file of the GRAPE distribution 
and in the section entitled `Installing the GRAPE Package' of the GRAPE manual. 
See

http://www.maths.qmul.ac.uk/~leonard/grape/

or the main GAP webpages for more information. 

Getting MONOID
--------------

You can download the package from:

http://www-history.mcs.st-and.ac.uk/~jamesm/monoid/index.html 

in .tar.gz, and .tar.bz2 formats.

Contents
--------

The MONOID folder containing this README.txt file should contain the directories 
and files: `doc', `examples', `gap', `init.g', `PackageInfo.g', `read.g', 
`README.txt', `tmp', and `tst'.

Installation
------------

1) unpack the file (for example, by using `gunzip monoid3r1p1.tar.gz' and then 
`tar -xf monoid3r1p1.tar' in UNIX).

2) move the resultant directory `MONOID' into the `pkg' directory of your GAP 
directory (the directory containing the directories `lib', `doc', `pkg', and so 
on).

3) if you want to make use of the methods for finding automorphism groups of 
semigroups, as mentioned above, make sure that the GRAPE package is fully 
installed in a UNIX environment.

4) start GAP in the usual way.

5) type `LoadPackage("monoid");'. 

Example Installation
--------------------

unix> gunzip monoid3r1p1.tar.gz 
unix> tar -xf monoid3r1p1.tar 
unix> mv MONOID GAPROOT/pkg
unix> gap

[ ... ]

gap> LoadPackage("MONOID");
Loading  MONOID 3.1.1
by James Mitchell (http://www-groups.mcs.st-and.ac.uk/~jamesm)
For help, type: ?the monoid package
true
gap> 

Presuming that the above steps can be completed successfully you will be running 
the MONOID package!

Further Information
-------------------

For details on how to use the MONOID package see the package documentation 
(prepared using the GAPDOC package) in the `doc' subdirectory (view either the 
html file `chap0.html' via a webbrowser or the file `manual.pdf' via a PDF 
viewer).

If you use the MONOID package, please let me know by sending me an email.  
If you notice any features missing that you think are important or if you find a 
bug, I would very much appreciate it if you would let me know.

James Mitchell, St Andrews, 10th of January 2008.

#****************************************************************************
#
# Copyright (C) 2008 James Mitchell 
# <jdm3@st-and.ac.uk>
#
#  Distributed under the terms of the GNU General Public License (GPL)
#
#    This code is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty
#    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#  See the GNU General Public License for more details; the full text
#  is available at:
#
#  http://www.gnu.org/licenses/
#