# Maintenance -----

# run all testthat tests (after reloading with current code) - do all pass?
devtools::test()

# check code coverage - are all functions covered 100%?
detach("package:CodelistGenerator", unload=TRUE)
devtools::test_coverage()
# for more coverage details uncomment the following
# covr::report()
# cov <- covr::package_coverage(here::here())
# covr::zero_coverage(cov)

# run all examples - do they all run without error?
devtools::run_examples()

# check spelling throughout - any obvious typos to fix?
devtools::spell_check()
# spelling::update_wordlist() # if they are not true spelling mistakes we can add them to our wordlist

# Check documentation (as R CMD check)
devtools::check_man()

# fuller checks - any warnings or notes to be fixed?
# note, warning about qpdf can be ignored
devtools::check() # updates the documentation, then builds and
devtools::check_rhub() # All os

# have you followed the style guide?
# note you can use stlyer to fix formatting
# ignore formatting warning for .datatable.aware
#  doesn´t pick up vars used in glue (So ignore warnign about duration)
lintr::lint_package()

devtools::build_readme()
devtools::document() #  Use roxygen to document a package.

# data for vignettes is precomputed, so first let´s do that (which might take a little while)
source(here::here("extras", "precompute_vignette_data.R"))
devtools::build_vignettes()

# ------------

# Setting up -----
# usethis::use_version()
# usethis::use_pipe()
# use_r("clean_words")
# use_r("get_candidate_codes")
# use_r("show_mappings")
# usethis::use_r("get_vocab_version")

# usethis::use_package("checkmate")
# usethis::use_package("dplyr")
# usethis::use_package("dtplyr")
# usethis::use_package("tidyr")
# usethis::use_package("stringr")
# usethis::use_package("glue")

# use_testthat()
# use_test("clean_words")
# usethis::use_test("show_mappings")
# usethis::use_test("get_candidate_codes")
# usethis::use_test("get_vocab_version")

# Coverage
# usethis::use_coverage()
# usethis::use_github_action("test-coverage")






# myPkgs <- c("CodelisttGenerator")
# pdb <- available.packages()
# tools::package_dependencies(myPkgs, db = pdb) # all arguments at default
#
# BiocManager::install("ComplexHeatmap")
# library(pkgndep)
# pkg <- pkgndep(here::here())
# plot(pkg, fix_size = FALSE)
# heaviness(pkg)


# devtools::load_all()
# usethis::use_github_pages()
# usethis::use_pkgdown_github_pages()

# usethis::use_vignette("Introduction_to_CodelistGenerator")
# usethis::use_vignette("Options_for_CodelistGenerator")




# use_mit_license()



# use_readme_rmd()
#
# devtools::load_all()

# usethis::use_github_action("lint")
#
# covr::package_coverage(type = "all")
# usethis::use_coverage(type = "codecov")
#
# usethis::use_github_action_check_standard()



