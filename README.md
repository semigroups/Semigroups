### README - Semigroups package for GAP

#### Copyright (C) 2011-16 James D. Mitchell et al. <br/>Licensing information is available in the LICENSE file.   
  
## Getting Semigroups

To get the latest version of the package download the archive file `semigroups-x.x.x.tar.gz` from the [Semigroups](https://gap-packages.github.io/Semigroups) webpage, and inside the `pkg` subdirectory of your GAP installation unpack `semigroups-x.x.x.tar.gz`, by, for example, doing:

    gunzip semigroups-x.x.x.tar.gz; tar xvf semigroups-x.x.x.tar
   
This will create a subdirectory `semigroups-x.x.x`.

## Issues

For questions, remarks, suggestions, and issues please use the 
[issue tracker](https://github.com/gap-packages/Semigroups/issues).

## Installation

It is assumed that you have a working copy of GAP with version number 4.8.0 or higher.  The  most  up-to-date  version  of  GAP  and instructions on how to install it can be obtained from the main [GAP](http://www.gap-system.org) page.

The  following  is  a  summary of the steps that should lead to a successful installation of [Semigroups](https://gap-packages.github.io/Semigroups):

* get the [IO](http://gap-system.github.io/io/) package version 4.4.4 or higher
 
* get the [Orb](http://gap-system.github.io/orb/) package version 4.7.3 or higher. 
  Both [Orb](http://gap-system.github.io/orb/) and [Semigroups](https://gap-packages.github.io/Semigroups) perform better if [Orb](http://gap-system.github.io/orb/) is compiled, so compile [Orb](http://gap-system.github.io/orb/)!

* **this step is optional:** certain functions in [Semigroups](https://gap-packages.github.io/Semigroups) require the [Grape](http://www.maths.qmul.ac.uk/~leonard/grape/) package to be available and fully compiled; a full list of these functions can be found in the first chapter of the manual.  To use these functions make sure that the [Grape](http://www.maths.qmul.ac.uk/~leonard/grape/) package version 4.5 or higher is available.
        
  If [Grape](http://www.maths.qmul.ac.uk/~leonard/grape/) is not fully installed, then [Semigroups](https://gap-packages.github.io/Semigroups) can be used as normal with  the  exception that the functions listed in the first chapter of the manual will not work. 

* **this step is optional:** the non-deterministic version of the function `Normalizer` requires the [genss](http://gap-system.github.io/genss/) package to be loaded. If you want to use this function, then please ensure that the [genss](http://gap-system.github.io/genss/) package version 1.5 or higher is available. 

* download the package archive `semigroups-x.x.x.tar.gz` from the [Semigroups](https://gap-packages.github.io/Semigroups) webpage.

* unzip and untar the file `semigroups-x.x.x.tar.gz`, for example, using
 
    ``` 
    gunzip semigroups-x.x.x.tar.gz; tar xvf semigroups-x.x.x.tar
    ```
  
    this should create a directory called `semigroups-x.x.x`.

* locate  the  `pkg`  directory  of your GAP directory, which contains the directories `lib`, `doc` and so on. Move the directory `semigroups-x.x.x` into the `pkg` directory (if it is not there already).

* start GAP in the usual way.

* type `LoadPackage("semigroups");`

Enjoy!
