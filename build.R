devtools::check_win_oldrelease(args = "--compact-vignettes=both")
devtools::check_win_release(args = "--compact-vignettes=both")
devtools::check_win_devel(args = "--compact-vignettes=both")

devtools::release(check=TRUE, args = "--compact-vignettes=both")
