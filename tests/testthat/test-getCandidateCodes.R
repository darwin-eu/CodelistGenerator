test_that("tests with mock db", {
  backends <- c("database",
                "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # check tidyWords
    expect_message(tidyWords(c("café")))
    expect_message(x <- tidyWords(c("/hip")))
    expect_identical(x, "hip")
    expect_message(x <- tidyWords(c("/hép/")))
    expect_identical(x, "hp")
    expect_message(x <- tidyWords(c("?")))
    expect_identical(x,as.character())
    expect_message(x <- tidyWords(c("?", "hello")))
    expect_identical(x,"hello")

    # tests
    # test keywords search - exact
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = FALSE
    )
    expect_true((nrow(codes) == 1 &
                   codes$concept_name[1] == "Musculoskeletal disorder"))
    # variable names
    expect_true(all(c(
      "concept_id", "concept_name",
      "domain_id", "vocabulary_id"
    ) %in%
      names(codes)))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c(
        "knee osteoarthritis",
        "hip osteoarthritis"
      ),
      domains = "Condition",
      includeDescendants = FALSE
    )
    expect_true(nrow(codes) == 2)
    expect_true("Osteoarthritis of knee" %in%
                  (codes |>
                     dplyr::pull("concept_name")))
    expect_true("Osteoarthritis of hip" %in%
                  (codes |>
                     dplyr::pull("concept_name")))

    # exact matches
    cdm$concept <- cdm$concept |>
      dplyr::union_all(
        cdm$concept |>
          dplyr::filter(concept_name == "Knee osteoarthritis") |>
          dplyr::mutate("standard_concept" = "S",
                        "concept_id" = 27L)
      )
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "knee osteo",
      domains = "Condition",
      includeDescendants = FALSE
    )
    codes1 <- getCandidateCodes(
      cdm = cdm,
      keywords = "/knee osteo/",
      domains = "Condition",
      includeDescendants = FALSE
    )
    codes2 <- getCandidateCodes(
      cdm = cdm,
      keywords = "/\bknee osteo/\b",
      domains = "Condition",
      includeDescendants = FALSE
    )
    codes3 <- getCandidateCodes(
      cdm = cdm,
      keywords = "/\bknee osteoarthritis/\b",
      domains = "Condition",
      includeDescendants = FALSE
    )
    codes4 <- getCandidateCodes(
      cdm = cdm,
      keywords = "knee osteoarthritis",
      domains = "Condition",
      includeDescendants = FALSE
    )
    expect_identical(codes$concept_id, c(4L, 27L))
    expect_identical(codes1$concept_id, c(27L))
    expect_identical(codes2$concept_id, integer(0))
    expect_identical(codes3$concept_id, c(27L))
    expect_identical(codes4$concept_id, codes$concept_id)

    # test include descendants
    cdm <- mockVocabRef()
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = TRUE
    )
    expect_true((nrow(codes) == 5 &
                   all(codes$concept_id %in% c(1:5)) &
                   all(!codes$concept_id %in% c(6, 7))))

    # test include ancestor
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Arthritis"),
      domains = "Condition",
      includeAncestor = TRUE
    )
    expect_true(any(codes$concept_name %in% "Musculoskeletal disorder"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Osteoarthritis of knee"),
      domains = "Condition",
      includeAncestor = TRUE
    )
    # nb includeAncestor should only include one level above
    expect_true(!any(codes$concept_name %in% "Musculoskeletal disorder"))
    expect_true(any(codes$concept_name %in% "Arthritis"))

    # test standardConcept
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Arthritis",
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard"),
      includeDescendants = TRUE
    )
    expect_true((nrow(codes) == 5 &
                   all(codes$concept_id %in% c(3, 4, 5, 8, 17)) &
                   all(!codes$concept_id %in% c(1, 2, 7))))

    # test searchInSynonyms
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "osteoarthrosis",
      domains = "Condition",
      searchInSynonyms = TRUE
    )
    expect_true(any(codes$concept_name %in% "Arthritis"))

    # test exclusion
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = "Osteoarthritis of hip",
      domains = "Condition"
    )
    expect_true(any(!codes$concept_name %in% "Osteoarthritis of hip"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("Osteoarthritis of hip", "something else", "shoulder"),
      domains = "Condition"
    )
    expect_true(all(codes$concept_name != "Osteoarthritis of hip"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("something else", "shoulder", "Osteoarthritis of hip"),
      domains = "Condition"
    )
    expect_true(all(codes$concept_name != "Osteoarthritis of hip"))

    # test exclusion + partial match
    codes1 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = NULL,
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    )

    expect_message(codes2 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("hip of", "/Knee Osteoarthritis"),
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    ))
    expect_equal(
      {attr(codes1, "search_strategy") <- NULL; codes1 |> dplyr::filter(.data$concept_id %in% c(3L, 17L))},
      {attr(codes2, "search_strategy") <- NULL; codes2}
    )

    codes3 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("hip of", "/Knee Osteoarthritis/"),
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    )

    codes4 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("hip of", "/ee Osteoarthritis/"),
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    )
    expect_equal(
      {attr(codes1, "search_strategy") <- NULL; codes1 |> dplyr::filter(!.data$concept_id %in% c(5L, 8L))},
      {attr(codes3, "search_strategy") <- NULL; codes3}
    )
    expect_equal(
      {attr(codes3, "search_strategy") <- NULL; codes3},
      {attr(codes4, "search_strategy") <- NULL; codes4}
    )

    codes5 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("hip of", "/\bee Osteoarthritis/\b"),
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    )
    expect_equal(
      {attr(codes1, "search_strategy") <- NULL; codes1 |> dplyr::filter(!.data$concept_id %in% c(5L))},
      {attr(codes5, "search_strategy") <- NULL; codes5}
    )

    codes6 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("hip of", "/\bknee Osteoarthritis/\b"),
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    )
    expect_equal(
      {attr(codes4, "search_strategy") <- NULL; codes4},
      {attr(codes6, "search_strategy") <- NULL; codes6}
    )

    # test exclusion + symbols (to ignore)
    expect_message(codes7 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("?"),
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard")
    ))
    expect_equal(
      {attr(codes1, "search_strategy") <- NULL; codes1},
      {attr(codes7, "search_strategy") <- NULL; codes7}
    )

    # test non-standard
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Musculoskeletal", "Degenerative arthropathy"),
      searchNonStandard = TRUE,
      includeDescendants = FALSE,
      domains = "Condition"
    )
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Degenerative arthropathy"),
      searchNonStandard = TRUE,
      includeDescendants = FALSE,
      domains = "Condition"
    )
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    # all options
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis", "knee"),
      domains = c("Condition", "observation"),
      standardConcept = "Standard",
      searchInSynonyms = TRUE,
      searchNonStandard = TRUE,
      exclude = "Childhood asthma",
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(nrow(codes) >= 1)

    # test search in drug
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "adalimumab",
      domains = "Drug",
      searchInSynonyms = TRUE,
      includeDescendants = FALSE,
      includeAncestor = TRUE
    )
    expect_true(codes$concept_id == "10")

    # search for drug and condition
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis", "adalimumab"),
      domains = c("Condition", "Drug"),
      searchInSynonyms = TRUE,
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(any(codes$concept_id %in% "10"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis", "adalimumab"),
      domains = NULL,
      searchInSynonyms = TRUE,
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(all((codes$domain_id |> sort() |> unique()) == c("Condition", "Drug", "Observation")))

    ## Edge cases
    # check empty candidate set
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "asthmaX",
      domains = "Condition"
    )
    expect_true(nrow(codes) == 0)

    # keywords that don´t exist
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Musculoskeletal disorder", "XXXXX"),
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    )
    expect_true("1" %in% codes$concept_id)

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "XXXXX",
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    )
    expect_true(nrow(codes) == 0)

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "a", exclude = "a",
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    )
    expect_true(nrow(codes) == 0)

    ## Expected errors
    # keyword should be a character
    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = 35,
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    ))

    expect_error(getCandidateCodes(
      cdm = "Not a cdm ref",
      keywords = "a",
      exclude = NULL,
      includeDescendants = TRUE,
      includeAncestor = FALSE
    ))

    # domain that doesn´t exist
    expect_warning(getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = c("Condition", "Some other table")
    ))

    # standardConcept that doesn´t exist
    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      standardConcept = c("Standard", "Something that doesn´t exist"),
      includeDescendants = FALSE
    ))

    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      standardConcept = "Something that doesn´t exist",
      includeDescendants = FALSE
    ))

    cdm1 <- cdm
    cdm1$concept <- NULL
    expect_error(getCandidateCodes(
      cdm = cdm1,
      keywords = "Musculoskeletal disorder"
    ))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("tests with mock db - multiple domains", {
  skip_on_cran()
  backends <- c("database",
                "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    # test keywords search - exact
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true((nrow(codes) == 4 &
                   all(codes$concept_id %in% c(3:5, 9)) &
                   all(!codes$concept_id %in% c(1, 2, 6, 7, 8))))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "h o osteoarthritis",
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true(all(
      nrow(codes) == 3 &
        c(4,5,9) %in% codes$concept_id))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("tests with eunomia", {

  skip_on_cran()
  skip_on_ci()
  db <- DBI::dbConnect(duckdb::duckdb(),
                       dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = "main",
    writeSchema = "main"
  )

  codes <- getCandidateCodes(cdm=cdm,
                             keywords= "sinusitis",
                             domains = c("Condition", "observation"),
                             searchInSynonyms = TRUE,
                             searchNonStandard = TRUE,
                             includeAncestor = TRUE)

  expect_true(sum(is.na(codes$concept_name)) == 0)

  expect_equal(codes |>
                 dplyr::pull("concept_id"),
               unique(codes |>
                        dplyr::pull("concept_id")))

  codes1 <- getCandidateCodes(cdm=cdm,
                              keywords= c("Protein","disease"),
                              domains = NULL,
                              searchInSynonyms = TRUE,
                              searchNonStandard = TRUE,
                              includeAncestor = TRUE)
  codes1 <- codes1 |> dplyr::arrange(concept_id)
  expect_warning(codes2 <- getCandidateCodes(cdm=cdm,
                                             keywords= c("Protein","disease"),
                                             domains = c("Condition", "Drug", "Procedure", "Device", "Observation", "Measurement"),
                                             searchInSynonyms = TRUE,
                                             searchNonStandard = TRUE,
                                             includeAncestor = TRUE))
  codes2 <- codes2 |> dplyr::arrange(concept_id)

  expect_equal(
    {attr(codes1, "search_strategy") <- NULL; codes1},
    {attr(codes2, "search_strategy") <- NULL; codes2}
  )

})
