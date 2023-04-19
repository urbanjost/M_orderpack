![_M_ORDERPACK](docs/images/M_orderpack.gif)
## Name
   **M_orderpack**(3f) - general and specialized ranking and sorting routines

## Description

This repository is a derivative of ORDERPACK 2.0 from Michel Olagnon
that additionally

 - supports CHARACTER variables as well as numeric variables on many of
   the procedures.
 - has been restructured so as to be useable as an fpm(1) package.
 - is placed in a git(1) repository for version control.
 - had man-pages and working demonstration programs of each procedure
   added.
 - contains a growing set of unit tests.

M_ORDERPACK 2.1 performs both conventional sorting and ranking as well as
the rarer specialized ordering tasks such as partial sorting, partial
ranking, unique sorting, unique ranking, inverse unique ranking, and
more. These partial sort and ranking routines can greatly accelerate
many computations when users need only the M largest or smallest elements
out of a N-element vector.

All the specialized procedures have a range over which they far outperform
a basic sort, and most have a range where they dramatically underperform.
If you are not limited by memory requirements or have no issues with
runtimes the simplest solution may be just to use SORT(3f) and RANK(3f).

Otherwise, your solution method may very well depend on the size of the
input arrays, whether the data is already close to the required order,
or how costly it is to create work arrays or an index array.

So, if you want the smallest value in an array call the intrinsic
MINVAL(3f), not ORDERVAL(3f).

**Authors**: 

- Michel Olagnon date: 2000-2013 wrote the original ORDERPACK 2.0
- John S. Urban, date: 2022 derived M_ORDERPACK 2.1 from ORDERPACK 2.0

**LICENSE**:

CC0-1.0

---
## Recent Events and Issues (See Also : CHANGELOG)

### 2022-04-18
  There appear to be issues with the original code, but may have been introduced
  (but are being looked at):

- UNIPAR returns non-unique values when less unique values in input than
  requested in output with gfortran,nvfortran; OK with ifort
---

## Building the module using make![gmake](docs/images/gnu.gif)

This will compile the Fortran module and basic example programs that exercise the routines:

```bash
     git clone https://github.com/urbanjost/M_orderpack.git


     cd M_orderpack/src
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
   Note that to specifically get release 2.0.0 you would use
```bash
     git clone --branch 2.0.0 https://github.com/urbanjost/M_orderpack.git
```
---
## Build and test with![fpm](docs/images/fpm_logo.gif)

   Download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )
```bash
        git clone https://github.com/urbanjost/M_orderpack.git
        cd M_orderpack
        fpm build
```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        M_orderpack        = { git = "https://github.com/urbanjost/M_orderpack.git" }
```
---
## Documentation ![docs](docs/images/docs.gif)

### User
   - [routines](https://urbanjost.github.io/M_orderpack/man3.html) 
     are described in HTML form using the format of man-pages.
<!--
     and [programs](https://urbanjost.github.io/M_orderpack/man1.html)
   - A single page that uses javascript to combine all the HTML
     descriptions of the man-pages is at
     [BOOK_M_orderpack](https://urbanjost.github.io/M_orderpack/BOOK_M_orderpack.html).
-->

   - ![man-pages](docs/images/manpages.gif)
     There are man-pages in the repository download in the docs/ directory
     that may be installed on ULS (Unix-Like Systems).

      + [manpages.zip](https://urbanjost.github.io/M_orderpack/manpages.zip)
      + [manpages.tgz](https://urbanjost.github.io/M_orderpack/manpages.tgz)

   - fpm plugin fpm-m_orderpack(1). If you build the optional program
     source in app/fpm-m_orderpack.f90.plugin and place it in your search-path
     then commands like
```bash
     fpm m_orderpack manual|more
```
     will display the help-text manual for the procedures. For more
     information enter
```bash
     fpm m_orderpack --help
```

   - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes
---
### Developer
   - [ford(1) output](https://urbanjost.github.io/M_orderpack/fpm-ford/index.html).
   - [doxygen(1) output](https://urbanjost.github.io/M_orderpack/doxygen_out/html/index.html).
   - [github action status](docs/STATUS.md)
---
## See also ![-](docs/images/demos.gif)
   - [parallel mrgrnk](https://github.com/cphyc/Fortran-parallel-sort)

   * [M_sort](https://github.com/urbanjost/M_sort)
   * [ORDERPACK2.0](http://www.fortran-2000.com/rank/)
   * [M_orderpack](https://github.com/urbanjost/M_orderpack)
   * [sortff](https://gitlab.com/everythingfunctional/sortff)
#### general packages containing sorting
   * [commonTools](https://github.com/wtdailey/commonTools)
   * [Beliavsky List](https://github.com/Beliavsky/Fortran-code-on-GitHub#sorting)
---
## References ![-](docs/images/ref.gif)

   * [ORDERPACK2.0](http://www.fortran-2000.com/rank/)
   * [ORDERPACK2.0](https://forge-dga.jouy.inra.fr/svn/qtlmap/trunk/lib/orderpack-2.0/index.html)
   * [sorting(Wikipedia)](https://en.m.wikipedia.org/wiki/Sorting_algorithm)
   * [median(Rosetta Code)](http://www.rosettacode.org/wiki/Averages/Median)
   * [median(Wikipedia)](https://en.wikipedia.org/wiki/Median)
   * [quartile(Wikipedia)](https://en.wikipedia.org/wiki/Quartile)
---
