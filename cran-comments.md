## Test environments

- Local Ubuntu 20.04 installation (R 4.1.0)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- r-hub:
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Oracle Solaris 10, x86, 32 bit, R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Check status summary:
                  NOTE OK
  Source packages    0  1
  Reverse depends    1  0

Check results summary:
gtfsio ... OK
rdepends_tidytransit ... NOTE
* checking installed package size ... NOTE
* checking dependencies in R code ... NOTE
* checking data for non-ASCII characters ... NOTE

None of the notes in {tidytransit} check are related to {gtfsio}.
