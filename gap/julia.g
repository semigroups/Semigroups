#############################################################################
##
##  julia.g
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##  This file is a GAP package extension (see the "Extensions" component of
##  PackageInfo.g): it is only read if the GAP package JuliaInterface is
##  available. It is currently a placeholder, and does not yet do anything.
##
##  The long term aim is to use this extension mechanism to call the
##  functionality of libsemigroups from GAP via the Julia package
##  Semigroups.jl (https://github.com/libsemigroups/Semigroups.jl), as a
##  replacement for (parts of) the Semigroups kernel module. See
##  https://github.com/semigroups/Semigroups/issues/1220 for more details.
##
##  The JuliaInterface itself can be found at
##  https://github.com/oscar-system/GAP.jl/tree/master/pkg/JuliaInterface

SEMIGROUPS.JuliaInterfaceLoaded := true;
