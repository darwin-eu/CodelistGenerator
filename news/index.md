# Changelog

## CodelistGenerator 4.0.1

CRAN release: 2026-01-08

- Fix edge case for summariseOrphanCodes() when no records found

## CodelistGenerator 4.0.0

CRAN release: 2025-12-17

- Add functions asCodelist(), asCodelistWithDetails(),
  asConceptSetExpression()
- Add functions associatedConceptClassIds(), associatedDomains(),
  associatedDoseForms(), associatedDoseUnits(),
  associatedDrugIngredients(), associatedRelationshipIds(),
  associatedRouteCategories(), and associatedVocabularies()
- Add functions availableConceptClassIds(), availableDomains(),
  availableDoseForms(), availableDoseUnits(),
  availableDrugIngredients(), availableRelationshipIds(),
  availableRouteCategories(), and availableVocabularies()
- Add functions intersectCodelists(), unionCodelists(), and
  excludeConcepts()
- Add functions stratifyByBrand(), stratifyByDomain(),
  stratifyByDoseForm(), stratifyByVocabulary()
- Add functions subsetOnDoseForm(), subsetOnIngredientRange(),
  subsetOnVocabulary()
- Add function searchStrategy()
- Add function vocabularyVersion()
- Remove availableICD10(), availableIngredients(),
  buildAchillesTables(), codesInUse(), getConceptClassId(),
  getDomains(), getDoseForm(), getDoseUnit(), getICD10StandardCodes(),
  getRelationshipId(), getRouteCategories(), getVocabVersion(),
  getVocabularies()

## CodelistGenerator 3.5.0

CRAN release: 2025-04-10

- Add reexports from omopgenerics
- Improve consistency in function documentation

## CodelistGenerator 3.4.1

CRAN release: 2025-03-26

- Remove dependency on RJSONIO in favour of jsonlite

## CodelistGenerator 3.4.0

CRAN release: 2025-02-19

- Depend on omopgenerics 1.0.0
- Account for non-UTF8 concept names

## CodelistGenerator 3.3.2

CRAN release: 2025-01-28

- Change examples and vignettes for CDMConnector 1.7.0
- Depend on visOmopResults 1.0.0

## CodelistGenerator 3.3.1

CRAN release: 2024-11-26

- Update to use base pipe

## CodelistGenerator 3.3.0

CRAN release: 2024-11-10

- Add function subsetOnDomain()
- Add parameter .options in table functions
- Fix for breaking change in omopgenerics 0.4.0 and visOmopResults 0.5.0

## CodelistGenerator 3.2.1

CRAN release: 2024-10-17

- Add nameStyle argument for getDrugIngredientCodes()

## CodelistGenerator 3.2.0

CRAN release: 2024-09-24

- Add functions availableIngredients(), availableATC()
- Align table arguments with visOmopResults

## CodelistGenerator 3.1.0

CRAN release: 2024-07-12

- Add functions getRouteCategories() and subsetOnRouteCategory()
- Export route data
- Add doseUnit, route, and routeCategory as arguments in getATCCodes()
  and getDrugIngredientCodes()
- Add functions stratifyByConcept(), stratifyByDoseUnit(),
  subsetOnDoseUnit(), stratifyByRouteCategory(), and
  subsetByRouteCategory()
- Add type argument to functions returning codelists
- Improve performance of summariseOrphanCodes()

## CodelistGenerator 3.0

CRAN release: 2024-05-31

- Add function getRelationshipId
- Add functions summariseAchillesCodeUse (replaces achillesCodeUse),
  summariseOrphanCodes (replaces findOrphanCodes), tableAchillesCodeUse,
  tableCodeUse, tableCohortCodeUse, tableOrphanCodes.

## CodelistGenerator 2.2.3

CRAN release: 2024-03-08

- Fix for forthcoming breaking change in dependency omopgenerics

## CodelistGenerator 2.2.2

CRAN release: 2024-02-14

- Fix for edge case with multiple exclusion criteria

## CodelistGenerator 2.2.1

CRAN release: 2024-02-07

- Working with omopgenerics

## CodelistGenerator 2.2.0

CRAN release: 2024-01-25

- Added functions findOrphanCodes, restrictToCodesInUse,
  sourceCodesInUse.
- Speed improvements in getCandidateCodes from doing search in place
  (e.g. on database side).
- Dropped explicit support of an Arrow cdm.

## CodelistGenerator 2.1.1

CRAN release: 2023-11-20

- Improved support of device domain.

## CodelistGenerator 2.0.0

CRAN release: 2023-10-09

- Simplified the interface of getCandidateCodes, with a number of
  arguments removed.
- Added function summariseCohortCodeUse.

## CodelistGenerator 1.7.0

CRAN release: 2023-08-16

- Added function codesFromCohort.

## CodelistGenerator 1.6.0

CRAN release: 2023-07-07

- Improved getICD10StandardCodes function.
- Added function codesFromConceptSet.

## CodelistGenerator 1.5.0

CRAN release: 2023-06-13

- Require CDMConnector v1.0.0 or above.

## CodelistGenerator 1.4.0

CRAN release: 2023-06-06

- Added function summariseCodeUse.

## CodelistGenerator 1.3.0

CRAN release: 2023-05-30

- Added function getICD10StandardCodes.

## CodelistGenerator 1.2.0

CRAN release: 2023-05-04

- Added functions getATCCodes and getDrugIngredientCodes.

## CodelistGenerator 1.1.0

CRAN release: 2023-04-01

- Added exactMatch and includeSequela options to getCandidateCodes
  function.

## CodelistGenerator 1.0.0

CRAN release: 2023-02-07

- Added a `NEWS.md` file to track changes to the package.
