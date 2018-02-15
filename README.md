### README - Semigroups package for GAP

#### Copyright (C) 2011-18 James D. Mitchell et al. <br/>Licensing information is available in the LICENSE file.
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.592893.svg)](https://doi.org/10.5281/zenodo.592893)[![Build Status](https://travis-ci.org/gap-packages/Semigroups.svg?branch=stable-3.0)](https://travis-ci.org/gap-packages/Semigroups)

## Getting Semigroups

To get the latest version of the package download the archive file `semigroups-x.x.x.tar.gz` from the [Semigroups](https://gap-packages.github.io/Semigroups) webpage, and inside the `pkg` subdirectory of your GAP installation unpack `semigroups-x.x.x.tar.gz`, by, for example, doing:

    gunzip semigroups-x.x.x.tar.gz; tar xvf semigroups-x.x.x.tar
   
This will create a subdirectory `semigroups-x.x.x`.

## Issues

For questions, remarks, suggestions, and issues please use the 
[issue tracker](https://github.com/gap-packages/Semigroups/issues).

## Installation

It is assumed that you have a working copy of GAP with version number 4.9.0 or higher.  The  most  up-to-date  version  of  GAP  and instructions on how to install it can be obtained from the main [GAP](http://www.gap-system.org) page.

The  following  is  a  summary of the steps that should lead to a successful installation of [Semigroups](https://gap-packages.github.io/Semigroups):

* get the [IO](http://gap-packages.github.io/io/) package version 4.5.1 or higher

* get the [Orb](http://gap-packages.github.io/orb/) package version 4.8.0 or higher. 
  Both [Orb](http://gap-packages.github.io/orb/) and [Semigroups](https://gap-packages.github.io/Semigroups) perform better if [Orb](http://gap-packages.github.io/orb/) is compiled, so compile [Orb](http://gap-packages.github.io/orb/)!

* ensure that the [Digraphs](http://gap-packages.github.io/Digraphs/) package version 0.12.0 or higher is available.  [Digraphs](http://gap-packages.github.io/Digraphs/) must be compiled before [Semigroups](https://gap-packages.github.io/Semigroups) can be
loaded.

* get the [genss](http://gap-packages.github.io/genss/) package version 1.5 or higher 

* download the package archive `semigroups-x.x.x.tar.gz` from the [Semigroups](https://gap-packages.github.io/Semigroups) webpage.

* unzip and untar the file `semigroups-x.x.x.tar.gz`, for example, using
 
    ``` 
    gunzip semigroups-x.x.x.tar.gz; tar xvf semigroups-x.x.x.tar
    ```
  
    this should create a directory called `semigroups-x.x.x`.

* locate the `pkg` directory of your GAP directory, which contains the directories `lib`, `doc` and so on. Move the directory `semigroups-x.x.x` into the `pkg` directory.
    
* from version 3.0.0, it is necessary to compile the [Semigroups](https://gap-packages.github.io/Semigroups) package. [Semigroups](https://gap-packages.github.io/Semigroups) uses the [libsemigroups](https://james-d-mitchell.github.io/libsemigroups/) C++ library, which requires a compiler implementing the C++11 standard. To compile, inside the `pkg/semigroups-x.x.x` directory, type
    * `./configure`
    * `make`

    If you are using GCC to compile [Semigroups](https://gap-packages.github.io/Semigroups), then version 5.0 or higher is required. Trying to compile [Semigroups](https://gap-packages.github.io/Semigroups) with an earlier version of GCC will result in an error at compile time.  [Semigroups](https://gap-packages.github.io/Semigroups) supports GCC version 5.0 or higher, and clang version 5.0 or higher.

    Further information about this step can be found in manual section entitled "Compiling the kernel module"

* start GAP in the usual way.

* type `LoadPackage("semigroups");`

Enjoy!
