# Argument descriptions repeated > 1:

#' Helper for consistent documentation of `codelistNameDoc`
#' @param codelistName Name or names of codelist in x. If NULL, all codelist present
#' in x will be considered.
#'
#' @name codelistNameDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `.options`.
#'
#' @param .options Named list with additional formatting options.
#' visOmopResults::tableOptions() shows allowed arguments and
#' their default values.
#'
#' @name .optionsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `ageGroup`.
#'
#' @param ageGroup If not NULL, a list of ageGroup vectors of length two.
#'
#' @name ageGroupDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `byConcept`.
#'
#' @param byConcept TRUE or FALSE. If TRUE code use will be summarised by
#' concept.
#'
#' @name byConceptDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `bySex`.
#'
#' @param bySex TRUE or FALSE. If TRUE code use will be summarised by sex.
#'
#' @name bySexDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `byYear`.
#'
#' @param byYear TRUE or FALSE. If TRUE code use will be summarised by year.
#'
#' @name byYearDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `cdm`.
#'
#' @param cdm A cdm reference to an OMOP CDM dataset. If data is held within a
#' database, the vocabulary tables should be in the same schema as the clinical
#' tables (person, observation period, and so on).
#'
#' @name cdmDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `countBy`.
#'
#' @param countBy Either "record" for record-level counts or "person" for
#' person-level counts.
#'
#' @name countByDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `domain`.
#'
#' @param domain Character vector with one or more of the OMOP CDM domains.
#' The results will be restricted to the given domains. Check the available
#' ones by running availableDomains(). If NULL, all supported domains are included:
#' Condition, Drug, Procedure, Device, Observation, and Measurement.
#'
#' @name domainDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `doseForm`.
#'
#' @param doseForm Only codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form. Use 'doseForms()' to see the available dose forms.
#'
#' @name doseFormDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `doseUnit`.
#'
#' @param doseUnit Only codes with the specified dose unit
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose unit Use 'availableDoseUnits()' to see the available dose units.
#'
#' @name doseUnitDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `groupColumn`.
#'
#' @param groupColumn Variables to use as group labels. Allowed columns are:
#' "cdm_name", "codelist_name", "domain_id", "standard_concept_name",
#' "standard_concept_id", "estimate_name", "standard_concept", "vocabulary_id".
#' These cannot be used in header.
#'
#' @name groupColumnDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `groupColumn`.
#'
#' @param groupColumn Variables to use as group labels. Allowed columns are:
#' "cdm_name", "codelist_name", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
#' results are stratified, "year", "sex", "age_group" can also be used.
#' These cannot be used in header.
#'
#' @name groupColumnStrataDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `header`.
#'
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "codelist_name", "domain_id", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "standard_concept", "vocabulary_id".
#' Alternatively, it can include other names to use as overall header labels.
#'
#' @name headerDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `header`.
#'
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "codelist_name", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
#' results are stratified, "year", "sex", "age_group" can also be used.
#' Alternatively, it can include other names to use as overall header labels.
#'
#' @name headerStrataDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `hide`.
#'
#' @param hide Table columns to exclude, options are:  "cdm_name",
#' "codelist_name", "domain_id", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "standard_concept", "vocabulary_id". These cannot be used in
#' header or groupColumn.
#'
#' @name hideDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `style`.
#'
#' @param style A character string or custom R code to define the visual formatting
#' of the table. This argument can be provided in two ways: (1) Pre-defined Styles
#' (Character String): Use a name for a built-in style (e.g., "darwin"). See
#' visOmopResults::tableStyle() for available options. (2) Custome Code (Advanced):
#' Supply a block of custom R code. This code must be specific to the table type.
#' See visOmopResults::tableStyleCode() for structural examples.
#'
#' @name tableStyleDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `hide`.
#'
#' @param hide Table columns to exclude, options are: "cdm_name",
#' "codelist_name", "year", "sex", "age_group", "standard_concept_name",
#' "standard_concept_id", "estimate_name", "source_concept_name",
#' "source_concept_id", "domain_id". If results are stratified, "year", "sex",
#' "age_group" can also be used. These cannot be used in header or groupColumn.
#'
#' @name hideStrataDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `includeDescendants`.
#'
#' @param includeDescendants Either TRUE or FALSE. If TRUE descendant concepts
#' of identified concepts will be included in the candidate codelist. If FALSE
#' only direct mappings from ICD-10 codes to standard codes will be returned.
#'
#' @name includeDescendantsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `ingredientRange`.
#'
#' @param ingredientRange Used to restrict descendant codes to those
#' associated with a specific number of drug ingredients. Must be a vector of
#' length two with the first element the minimum number of ingredients allowed
#' and the second the maximum. A value of c(2, 2) would restrict to only
#' concepts associated with two ingredients.
#'
#' @name ingredientRangeDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `keepOriginal`.
#'
#' @param keepOriginal Whether to keep the original codelist (TRUE) or just return
#' the stratified ones (FALSE).
#'
#' @name keepOriginalDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `keepOriginal`.
#'
#' @param keepOriginal Whether to keep the original codelist (TRUE) or just return
#' the subset ones (FALSE).
#'
#' @name keepOriginalDocSubset
#' @keywords internal
NULL

#' Helper for consistent documentation of `level`.
#'
#' @param level ATC level. Can be one or more of "ATC 1st", "ATC 2nd",
#' "ATC 3rd", "ATC 4th", and "ATC 5th".
#'
#' @name levelATCDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `level`.
#'
#' @param level Can be either "ICD10 Chapter", "ICD10 SubChapter",
#' "ICD10 Hierarchy", or "ICD10 Code".
#'
#' @name levelICD10Doc
#' @keywords internal
NULL

#' Helper for consistent documentation of `minimumCount`.
#'
#' @param minimumCount Any codes with a frequency under this will be removed.
#'
#' @name minimumCountDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `nameStyle`.
#'
#' @param nameStyle Name style to apply to returned list. Can be one of
#' `"{concept_code}"`,`"{concept_id}"`, `"{concept_name}"`, or a combination (i.e.,
#' `"{concept_code}_{concept_name}"`).
#'
#' @name nameStyleDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `routeCategory`.
#'
#' @param routeCategory Only codes with the specified route will be
#' returned. If NULL, descendant codes will be returned regardless of route
#' category. Use getRoutes() to find the available route categories.
#'
#' @name routeCategoryDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `standardConcept`.
#'
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#'
#' @name standardConceptDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `table`.
#'
#' @param table cdm table of interest.
#'
#' @name tableDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `type`.
#'
#' @param type Can be "codelist", "codelist_with_details" or
#' "concept_set_expression".
#'
#' @name typeBroadDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `type`.
#'
#' @param type Can be "codelist" or "codelist_with_details".
#'
#' @name typeNarrowDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `type`.
#'
#' @param type Type of desired formatted table. To see supported formats
#' use visOmopResults::tableType().
#'
#' @name typeTableDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `x`.
#'
#' @param x A codelist.
#'
#' @name xDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `x` where input can be codelist or cohort.
#'
#' @param x A codelist or cohort table name.
#'
#' @name xDocCohort
#' @keywords internal
NULL
