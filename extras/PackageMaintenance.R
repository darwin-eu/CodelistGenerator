
devtools::build_readme()
devtools::document()
devtools::spell_check()
devtools::test()
devtools::check()

# devtools::load_all()
usethis::use_github_pages()
usethis::use_pkgdown_github_pages()

# render precomputed vignettes
knitr::knit("vignettes/Introduction_to_CodelistGenerator.Rmd.to.precompute",
            output = "vignettes/Introduction_to_CodelistGenerator.Rmd")

# usethis::use_vignette("Introduction_to_CodelistGenerator")

# usethis::use_version()

# use_r("clean_words")
# use_r("get_candidate_codes")
# use_r("show_mappings")

# usethis::use_package("checkmate")
# usethis::use_package("dplyr")
# usethis::use_package("dtplyr")
# usethis::use_package("tidyr")
# usethis::use_package("stringr")

# use_mit_license()

# use_testthat()
# use_test("clean_words")
#
# use_readme_rmd()
#
# load_all()


