## Test environments

- Local Ubuntu 20.04 installation (R 4.1.2)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- win-builder (devel, release, oldrel)
- r-hub:
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Oracle Solaris 10, x86, 32 bit, R-release

## R CMD check results

0 errors | 0 warnings | 1 note

> Found the following (possibly) invalid URLs:
>   URL: https://doi.org/10.5281/zenodo.5650829
>     From: inst/CITATION
>     Status: 404
>     Message: Not Found
>   URL: https://zenodo.org/record/5650829
>     From: README.md
>     Status: 404
>     Message: Not Found
> 
> Found the following (possibly) invalid DOIs:
>   DOI: 10.5281/zenodo.5650829
>     From: inst/CITATION
>     Status: Not Found
>     Message: 404

I have reserved a DOI in Zenodo to use in the CITATION file, but I haven't yet finished the submission because I'm awaiting for the approval on CRAN to upload the package `.tar.gz`. As soon as the package gets approved I'll upload the file and finish the submission, thus creating the DOI for good and getting rid of this note.

## Reverse dependencies

Check status summary:
                  WARN NOTE
  Source packages    0    1
  Reverse depends    2    0

Check results summary:
gtfsio ... NOTE
* checking CRAN incoming feasibility ... NOTE
rdepends_gtfs2gps ... WARN
* checking CRAN incoming feasibility ... WARNING
* checking examples ... NOTE
rdepends_tidytransit ... WARN
* checking CRAN incoming feasibility ... WARNING
* checking installed package size ... NOTE

None of the notes and warnings in {gtfs2gps} and {tidytransit} checks are related to {gtfsio}. The revdep checks were run with tools::check_packages_in_dir(check_args = "--as-cran"), thus raising an "insufficient package version" warning in the dependencies check. {gtfsio}'s note is explained above.

These are the results without the --as-cran flag:

                  NOTE OK
  Source packages    0  1
  Reverse depends    1  1

Check results summary:
gtfsio ... OK
rdepends_gtfs2gps ... OK
rdepends_tidytransit ... NOTE
* checking installed package size ... NOTE
* checking dependencies in R code ... NOTE
* checking data for non-ASCII characters ... NOTE
