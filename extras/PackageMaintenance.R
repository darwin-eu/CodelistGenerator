
# Please work through the below before pushing any changes to github

# 1) run all testthat tests (after reloading with current code) - do all pass?
devtools::test()

# 2) check code coverage - are all functions covered 100%?
detach("package:CodelistGenerator", unload=TRUE)
devtools::test_coverage()
# for more coverage details uncomment the following
# covr::report()
# cov <- covr::package_coverage(here::here())
# covr::zero_coverage(cov)

# 3) run all examples - do they all run without error?
devtools::run_examples()

# 4) check spelling throughout - any obvious typos to fix?
devtools::spell_check()
# spelling::update_wordlist() # if they are not true spelling mistakes we can add them to our wordlist

# 5) Check documentation (as R CMD check)
devtools::check_man()

# 6) fuller checks - any warnings or notes to be fixed?
# note, warning about qpdf can be ignored
devtools::check() # updates the documentation, then builds and
devtools::check_rhub() # All os

# 7) have you followed the style guide?
# note you can use stlyer to fix formatting
# ignore formatting warning for .datatable.aware
# doesn´t pick up vars used in glue (So ignore warningn about duration)
lintr::lint_package(".",
             linters = lintr::linters_with_defaults(
              lintr::object_name_linter(styles = "camelCase")
             )
)

# 8) Rebuild readme
devtools::build_readme()

# 9) Document package
devtools::document() #  Use roxygen to document a package.

# 10) Regenerate vignettes
# data for vignettes is precomputed, so first let´s do that
# this take a little while
source(here::here("extras", "precomputeVignetteData.R"))
devtools::build_vignettes()

# 11) check website locally
# you can check the website locally before pushing by running
pkgdown::build_site()
