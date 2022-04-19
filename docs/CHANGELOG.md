## orderpack Changelog

The intent of this log is to keep everyone in the loop about what's new
in the orderpack project. It is a curated, chronologically ordered list
of notifications of notable events such as bug fixes, new features,
and usage changes.

   - [x] git repository on WWW (github)
   - [x] annotated source files with an open license
   - [ ] unit test
   - [x] make(1) build
   - [x] fpm(1) build
   - [x] user manual (on-line)
   - [x] man-page
   - [x] app program
   - [x] demo program for public procedures
   - [x] developer documents (ford(1))
   - [x] CI/CD(Continious Integration/Development) verification (github actions)
   - [ ] registered in fpm(1) repository

---
**2022-04-18**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
   - Main sorting routines inssor(), mrgref(), mrgrnk(), refsor() can now take a CHARACTER array as an argument.
   - All routines now have a man-page containing an example program, which is also rendered into HTML
   - All the example programs from the man-pages are also available as demo programs in the example directory.
   - Developer Note: all routines are now using the prep(1) preprocessor and have been converted to templates to simplify 
     maintaining code that allows multiple input types.
---
**2022-04-13**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
     initial release on github
---
**2012-11-22**  Michel Olagnon  <https://github.com/urbanjost>
### :green_circle: ADD:
      - MEDIAN was added 22nd November 2012, to deal with the case of an even number of samples.
**2012-01-DD**  Michel Olagnon  <https://github.com/urbanjost>
### :orange_circle: DIFF:
      - MRGREF was slightly modified as of January 2012 to make the sort stable.

**2011-03-DD**  Michel Olagnon  <https://github.com/urbanjost>
### :red_circle: FIX:
      - Bugs were corrected as of fall 2010 in UNIRNK, UNIINV (The routine
        tried to access the 4th value when there are only 3) and in RNKPAR
        (The routine fails when the 3 first values are equal). Please
        download the corrected versions.
      - RAPKNR was added first of February 2011.

      - Similar bugs were corrected as of March 2011 in RNKPAR and RAPKNR
        (The routines may fail when ranking 3 values out of 4). Please
        download the corrected versions.
---
<!--
### :orange_circle: DIFF:
       + renamed ADVICE(3f) to ALERT(3f)
### :green_circle: ADD:
       + advice(3f) was added to provide a standardized message format simply.
### :red_circle: FIX:
       + </bo> did not work on several terminal types, changed it to a more
         universally accepted value.
-->
