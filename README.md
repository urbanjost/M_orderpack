![ORDERPACK](docs/images/orderpack.gif)
## Name
   orderpack(3f) - general and specialized ranking and sorting routines
   (LICENSE:CC0-1.0)

## Description

This repository is a clone of ORDERPACK 2.0 from Michel Olagnon that
has been restructured so as to be useable as an fpm(1) package; placed
in a git(1) repository; and has a growing set of unit tests.

ORDERPACK 2.0 provides Fortran procedures for General and Specialized
Ranking and Sorting Routines.  It includes Fortran 90 source code for
Unconditional, Unique, and Partial Ranking, Sorting, and Permutation.

    Authors: Michel Olagnon
    date: 2000-2012

Suggestions, testing, test cases, feedback, and assistance welcome for
the update.
---
## Recent Events and Issues (See Also : CHANGELOG)

### 2022-04-19
   - uniinv, mulcnt now support CHARACTER variables

### 2022-04-18
   - Main sorting routines inssor(), mrgref(), mrgrnk(), refsor() can now take a CHARACTER array as an argument.

     Ongoing support of CHARACTER types continues.
   - All routines now have a **preliminary** man-page containing an example program, which is also rendered into HTML
   - All the example programs from the man-pages are also available as demo programs in the example directory.
   - A (preliminary, but functional)  make(1) file has been added
   - Developer Note: all routines are now using the prep(1) preprocessor and have been converted to templates to simplify 
     maintaining code that allows multiple input types.

  There appear to be issues with the original code, but may have been introduced
  (but are being looked at):

- UNIPAR returns non-unique values when less unique values in input than requested in output with gfortran,nvfortran; OK with ifort
- INDMED has issues that may only manifest when debug options are used when compiling
```text
    At line 499 of file ./src/M_indmed.f90
    Fortran runtime error: Array bound mismatch for dimension 1 of array 'iwrkt' (7/5)
```
---

## Building the module using make![gmake](docs/images/gnu.gif)

This will compile the Fortran module and basic example programs that exercise the routines:

```bash
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
```

---
## Build and test with![fpm](docs/images/fpm_logo.gif)

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
---
## Documentation ![docs](docs/images/docs.gif)

### User
   - **UNDER CONSTRUCTION** an index to man-pages in HTML form for all the
     [routines](https://urbanjost.github.io/orderpack/man3.html)
   - documentation derived from the original [procedure documentation](https://urbanjost.github.io/orderpack/),
     which will gradually be replaced by the above.
<!--
   and [programs](https://urbanjost.github.io/orderpack/man1.html)

   - A single page that uses javascript to combine all the HTML
     descriptions of the man-pages is at
     [BOOK_orderpack](https://urbanjost.github.io/orderpack/BOOK_orderpack.html).

   - There are man-pages in the repository download in the docs/ directory
     that may be installed on ULS (Unix-Like Systems).

   - ![man-pages](docs/images/manpages.gif)
      + [manpages.zip](https://urbanjost.github.io/orderpack/manpages.zip)
      + [manpages.tgz](https://urbanjost.github.io/orderpack/manpages.tgz)
-->
   - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes
---
### Developer
   - [ford(1) output](https://urbanjost.github.io/orderpack/fpm-ford/index.html).
   - [doxygen(1) output](https://urbanjost.github.io/orderpack/doxygen_out/html/index.html).
   - [github action status](docs/STATUS.md)
---
## See also ![-](docs/images/demos.gif)
   * [parallel mrgrnk](https://github.com/cphyc/Fortran-parallel-sort)
---
## References ![-](docs/images/ref.gif)

   * [ORDERPACK2.0](http://www.fortran-2000.com/rank/)
   * [ORDERPACK2.0](https://forge-dga.jouy.inra.fr/svn/qtlmap/trunk/lib/orderpack-2.0/index.html)
   * [Wikipedia](https://en.m.wikipedia.org/wiki/Sorting_algorithm)
---
