![ORDERPACK](docs/images/orderpack.gif)
## NAME
   orderpack(3f) - a clone of orderpack2.0 for sorting and ordering
   (LICENSE:PD)

## DESCRIPTION

This module is a clone of Orderpack 2.0 from Michel Olagnon that has
been restructured so as to be useable as an fpm(1) package.

ORDERPACK 2.0 provides Fortran procedures for General and Specialized
Ranking and Sorting Routines.

It includes Fortran 90 source code for Unconditional, Unique, and Partial
Ranking, Sorting, and Permutation.

Authors: Michel Olagnon
date: 2000-2012

## BUILDING THE MODULE USING make(1) ![gmake](docs/images/gnu.gif)
     git clone https://github.com/urbanjost/orderpack.git
     cd orderpack/src
     # change Makefile if not using one of the listed compilers
     
     # for gfortran
     make clean
     make gfortran
     
     # for ifort
     make clean
     make ifort

     # for nvfortran
     make clean
     make nvfortran

This will compile the Fortran module and basic example
programs that exercise the routine.

## BUILD and TEST with FPM ![-](docs/images/fpm_logo.gif)

   Download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
        git clone https://github.com/urbanjost/orderpack.git
        cd orderpack
        fpm build
```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        orderpack        = { git = "https://github.com/urbanjost/orderpack.git" }
```
## DOCUMENTATION   ![docs](docs/images/docs.gif)

### USER
   - original documentation of [procedures](https://urbanjost.github.io/orderpack/).
<!--
   - A single page that uses javascript to combine all the HTML
     descriptions of the man-pages is at 
     [BOOK_orderpack](https://urbanjost.github.io/orderpack/BOOK_orderpack.html).

   - a simple index to the man-pages in HTML form for the
   [routines](https://urbanjost.github.io/orderpack/man3.html) 
   and [programs](https://urbanjost.github.io/orderpack/man1.html) 

   - There are man-pages in the repository download in the docs/ directory
     that may be installed on ULS (Unix-Like Systems).

   - ![man-pages](docs/images/manpages.gif)
      + [manpages.zip](https://urbanjost.github.io/orderpack/manpages.zip)
      + [manpages.tgz](https://urbanjost.github.io/orderpack/manpages.tgz)
-->

   - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### DEVELOPER
   - [ford(1) output](https://urbanjost.github.io/orderpack/fpm-ford/index.html).
<!--
   - [doxygen(1) output](https://urbanjost.github.io/orderpack/doxygen_out/html/index.html).
-->

   - [github action status](docs/STATUS.md) 
---

## REFERENCES ![-](docs/images/ref.gif)

   * [ORDERPACK2.0](http://www.fortran-2000.com/rank/)
   * [ORDERPACK2.0](https://forge-dga.jouy.inra.fr/svn/qtlmap/trunk/lib/orderpack-2.0/index.html)
   * [Wikipedia](https://en.m.wikipedia.org/wiki/Sorting_algorithm)

---
