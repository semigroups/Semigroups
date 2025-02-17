# README - Semigroups package for GAP

#### Copyright (C) 2011-2025 James D. Mitchell et al.<br />Licensing information is available in the LICENSE file.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.592893.svg)](https://doi.org/10.5281/zenodo.592893)

## Getting Semigroups

To get the latest version of the package download the archive file
`semigroups-x.x.x.tar.gz` from the [Semigroups][] webpage, and inside the `pkg`
subdirectory of your GAP installation unpack `semigroups-x.x.x.tar.gz`, by, for
example, doing:

    gunzip semigroups-x.x.x.tar.gz; tar xvf semigroups-x.x.x.tar

This will create a subdirectory `semigroups-x.x.x`.

## Issues

For questions, remarks, suggestions, and issues please use the
[issue tracker](https://github.com/gap-packages/Semigroups/issues).

## Installation

It is assumed that you have a working copy of GAP with version number 4.12.0 or
higher.  The most up-to-date version of GAP and instructions on how to install it
can be obtained from the main [GAP](https://www.gap-system.org) page.

The following is a summary of the steps that should lead to a successful
installation of [Semigroups][]:

* get the [datastructures](https://gap-packages.github.io/datastructures)
  package version 0.2.5 or higher

* ensure that the [Digraphs][] package version 1.2.0 or higher is available.
  [Digraphs][] must be compiled before [Semigroups][] can be loaded.

* get the [genss](https://gap-packages.github.io/genss) package version 1.6.5 or
  higher

* get the [images](https://gap-packages.github.io/images) package version 1.3.0 or

* get the [IO](https://gap-packages.github.io/io) package version 4.5.1 or higher

* get the [orb][] package version 4.8.2 or higher

Both [orb][] and [Semigroups][] perform better when [orb][] is compiled, so compile
[orb][]!

* download the package archive `semigroups-x.x.x.tar.gz` from the
  [Semigroups][] webpage.

* unzip and untar the file `semigroups-x.x.x.tar.gz`, for example, using

    ```
    gunzip semigroups-x.x.x.tar.gz; tar xvf semigroups-x.x.x.tar
    ```

    this should create a directory called `semigroups-x.x.x`.

* locate the `pkg` directory of your GAP directory, which contains the
  directories `lib`, `doc`, and so on. Move the directory `semigroups-x.x.x`
  into the `pkg` directory.

* from version 3.0.0, it is necessary to compile the [Semigroups][] package.
  [Semigroups][] uses the [libsemigroups][] C++ library, which requires a compiler
  implementing the C++14 standard.

  You may either build [libsemigroups][] along with [Semigroups][], or have it
  installed at a custom or standard location, as explained in its
  documentation.  To compile in the former case, inside the
  `pkg/semigroups-x.x.x` directory, type
    * `./configure`
    * `make`

  For the latter case, you need a working [pkg-config][] tool; assuming
  [libsemigroups][] was installed at location `/foo/bar`, type

    * `PKG_CONFIG_PATH=/foo/bar/lib/pkgconfig ./configure --with-external-libsemigroups`
    * `make`

  If [pkg-config][] has `/foo/bar` among its default locations, then
  `PKG_CONFIG_PATH=/foo/bar/lib/pkgconfig` part above may be omitted).

  If you are using GCC to compile [Semigroups][], then version 5.0 or higher is
  required. Trying to compile [Semigroups][] with an earlier version of GCC will
  result in an error at compile time.  [Semigroups][] supports GCC version 5.0 or
  higher, and clang version 5.0 or higher.

  Further information about this step can be found in manual section entitled
  “Compiling the kernel module”.

* start GAP in the usual way.
* type `LoadPackage("semigroups");`

Enjoy!

[Semigroups]: https://semigroups.github.io/Semigroups
[libsemigroups]: https://libsemigroups.rtfd.io/
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[orb]: https://gap-packages.github.io/orb
[Digraphs]: https://digraphs.github.io/Digraphs
