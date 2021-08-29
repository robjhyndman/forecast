# Find all revdep packages
packages <- revdepcheck::cran_revdeps("forecast")
# Install them
remotes::install_cran(packages)
# Check revdeps
revdepcheck::revdep_add(packages=c("bmgarch","JFE"))
revdepcheck::revdep_check()
# Check forecast
devtools::check_win_oldrelease(args = "--compact-vignettes=both")
devtools::check_win_release(args = "--compact-vignettes=both")
devtools::check_win_devel(args = "--compact-vignettes=both")
devtools::release(check=FALSE, args = "--compact-vignettes=both")
