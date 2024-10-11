## Test environments

- Local Windows Server 2022 installation (R 4.4.1)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Check status summary:
                  WARNING OK
  Source packages       0  1
  Reverse depends       2  0

Check results summary:
gtfsio ... OK
rdepends_gtfstools ... WARNING
* checking CRAN incoming feasibility ... [106s] WARNING
rdepends_tidytransit ... WARNING
* checking CRAN incoming feasibility ... [97s] WARNING
* checking installed package size ... NOTE

None of the notes and warnings in {gtfstools} and {tidytransit} checks are related to {gtfsio}. The revdep checks were run with tools::check_packages_in_dir(check_args = "--as-cran"), thus raising an "insufficient package version" warning in the dependencies check.

These are the results without the --as-cran flag:

Check status summary:
                  NOTE OK
  Source packages    0  1
  Reverse depends    1  1

Check results summary:
gtfsio ... OK
rdepends_gtfstools ... OK
rdepends_tidytransit ... NOTE
* checking installed package size ... NOTE
