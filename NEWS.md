# CodelistGenerator 2.2.3
* Fix for forthcoming breaking change in dependency omopgenerics

# CodelistGenerator 2.2.2
* Fix for edge case with multiple exclusion criteria

# CodelistGenerator 2.2.1
* Working with omopgenerics

# CodelistGenerator 2.2.0
* Added functions findOrphanCodes, restrictToCodesInUse, sourceCodesInUse.
* Speed improvements in getCandidateCodes from doing search in place (e.g. on database side).
* Dropped explicit support of an Arrow cdm.

# CodelistGenerator 2.1.1
* Improved support of device domain.

# CodelistGenerator 2.0.0
* Simplified the interface of getCandidateCodes, with a number of arguments removed.
* Added function summariseCohortCodeUse.

# CodelistGenerator 1.7.0
* Added function codesFromCohort.

# CodelistGenerator 1.6.0
* Improved getICD10StandardCodes function.
* Added function codesFromConceptSet.

# CodelistGenerator 1.5.0
* Require CDMConnector v1.0.0 or above.

# CodelistGenerator 1.4.0
* Added function summariseCodeUse.

# CodelistGenerator 1.3.0
* Added function getICD10StandardCodes.

# CodelistGenerator 1.2.0
* Added functions getATCCodes and getDrugIngredientCodes. 

# CodelistGenerator 1.1.0
* Added exactMatch and includeSequela options to getCandidateCodes function.

# CodelistGenerator 1.0.0
* Added a `NEWS.md` file to track changes to the package.
