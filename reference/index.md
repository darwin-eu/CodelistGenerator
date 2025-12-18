# Package index

### Search for codes following a systematic search strategy

- [`getCandidateCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getCandidateCodes.md)
  : Perform a systematic search to identify a candidate codelist using
  the OMOP CDM vocabulary tables.

- [`searchStrategy()`](https://darwin-eu.github.io/CodelistGenerator/reference/searchStrategy.md)
  :

  Report the search strategy used to identify codes when using the
  [`getCandidateCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getCandidateCodes.md)
  function

### Create vocabulary-based codelists

- [`getDrugIngredientCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getDrugIngredientCodes.md)
  : Get descendant codes of drug ingredients
- [`getATCCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getATCCodes.md)
  : Get the descendant codes of Anatomical Therapeutic Chemical (ATC)
  classification codes

### Run codelist diagnostics

- [`summariseAchillesCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseAchillesCodeUse.md)
  : Summarise code use from achilles counts.
- [`summariseCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseCodeUse.md)
  : Summarise code use in patient-level data.
- [`summariseCohortCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseCohortCodeUse.md)
  : Summarise code use among a cohort in the cdm reference
- [`summariseOrphanCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseOrphanCodes.md)
  : Find orphan codes related to a codelist using achilles counts and,
  if available, PHOEBE concept recommendations

### Present codelist diagnostics results in a table

- [`tableAchillesCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/tableAchillesCodeUse.md)
  : Format the result of summariseAchillesCodeUse into a table
- [`tableCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/tableCodeUse.md)
  : Format the result of summariseCodeUse into a table.
- [`tableCohortCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/tableCohortCodeUse.md)
  : Format the result of summariseCohortCodeUse into a table.
- [`tableOrphanCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/tableOrphanCodes.md)
  : Format the result of summariseOrphanCodes into a table

### Extract codelists from JSON files

- [`codesFromCohort()`](https://darwin-eu.github.io/CodelistGenerator/reference/codesFromCohort.md)
  : Get concept ids from JSON files containing cohort definitions

- [`codesFromConceptSet()`](https://darwin-eu.github.io/CodelistGenerator/reference/codesFromConceptSet.md)
  **\[deprecated\]** :

  Get concept ids from JSON files containing concept sets
  **\[deprecated\]**

### Combine codelists

- [`unionCodelists()`](https://darwin-eu.github.io/CodelistGenerator/reference/unionCodelists.md)
  : Generate a codelist from the union of different codelists. The
  generated codelist will come out in alphabetical order.
- [`intersectCodelists()`](https://darwin-eu.github.io/CodelistGenerator/reference/intersectCodelists.md)
  : Generate a codelist from the intersection of different codelists.
  The generated codelist will come out in alphabetical order.

### Subset codelists

- [`subsetToCodesInUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetToCodesInUse.md)
  : Filter a codelist to keep only the codes being used in patient
  records
- [`subsetOnRouteCategory()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetOnRouteCategory.md)
  : Subset a codelist to only those with a particular route category
- [`subsetOnDoseUnit()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetOnDoseUnit.md)
  : Subset a codelist to only those with a particular dose unit.
- [`subsetOnDomain()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetOnDomain.md)
  : Subset a codelist to only those codes from a particular domain.
- [`subsetOnVocabulary()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetOnVocabulary.md)
  : Subset a codelist to only those codes from a particular vocabulary.
- [`subsetOnIngredientRange()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetOnIngredientRange.md)
  : Subset a codelist to only those codes with a range of number of
  ingredients
- [`subsetOnDoseForm()`](https://darwin-eu.github.io/CodelistGenerator/reference/subsetOnDoseForm.md)
  : Subset a codelist to only those codes from a particular domain.

### Stratify codelists

- [`stratifyByRouteCategory()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByRouteCategory.md)
  : Stratify a codelist by route category.
- [`stratifyByDoseUnit()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByDoseUnit.md)
  : Stratify a codelist by dose unit.
- [`stratifyByDomain()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByDomain.md)
  : Stratify a codelist by domain category.
- [`stratifyByVocabulary()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByVocabulary.md)
  : Subset a codelist to only those codes from a particular domain.
- [`stratifyByDoseForm()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByDoseForm.md)
  : Stratify a codelist by dose form.
- [`stratifyByConcept()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByConcept.md)
  : Stratify a codelist by the concepts included within it.
- [`stratifyByBrand()`](https://darwin-eu.github.io/CodelistGenerator/reference/stratifyByBrand.md)
  : Stratify a codelist by brand category.

### Codelist utility functions

- [`addConcepts()`](https://darwin-eu.github.io/CodelistGenerator/reference/addConcepts.md)
  : Add concepts to a codelist
- [`excludeConcepts()`](https://darwin-eu.github.io/CodelistGenerator/reference/excludeConcepts.md)
  : Exclude concepts from a codelist
- [`compareCodelists()`](https://darwin-eu.github.io/CodelistGenerator/reference/compareCodelists.md)
  : Compare overlap between two sets of codes
- [`getMappings()`](https://darwin-eu.github.io/CodelistGenerator/reference/getMappings.md)
  : Show mappings from non-standard vocabularies to standard.
- [`asCodelist()`](https://darwin-eu.github.io/CodelistGenerator/reference/asCodelist.md)
  : Coerce to a codelist
- [`asCodelistWithDetails()`](https://darwin-eu.github.io/CodelistGenerator/reference/asCodelistWithDetails.md)
  : Coerce to a codelist with details
- [`asConceptSetExpression()`](https://darwin-eu.github.io/CodelistGenerator/reference/asConceptSetExpression.md)
  : Coerce to a concept set expression

### Vocabulary utility functions

- [`vocabularyVersion()`](https://darwin-eu.github.io/CodelistGenerator/reference/vocabularyVersion.md)
  : Get the available version of the vocabulary used in the cdm
- [`getDescendants()`](https://darwin-eu.github.io/CodelistGenerator/reference/getDescendants.md)
  : Get descendant codes for a given concept

### Identify concepts available in the OMOP CDM

- [`availableATC()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableATC.md)
  : Get the names of all available Anatomical Therapeutic Chemical (ATC)
  classification codes
- [`availableConceptClassIds()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableConceptClassIds.md)
  : Get the available concept classes used in a given set of domains
- [`availableDomains()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableDomains.md)
  : Get the domains available in the cdm
- [`availableDoseForms()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableDoseForms.md)
  : Get the dose forms for drug concepts
- [`availableDoseUnits()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableDoseUnits.md)
  : Get available dose units
- [`availableDrugIngredients()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableDrugIngredients.md)
  : Get the names of all available drug ingredients
- [`availableRouteCategories()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableRouteCategories.md)
  : Get available drug routes
- [`availableRelationshipIds()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableRelationshipIds.md)
  : Get available relationships between concepts
- [`availableVocabularies()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableVocabularies.md)
  : Get the available vocabularies available in the cdm

### Identify concepts associated with a codelist

- [`associatedConceptClassIds()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedConceptClassIds.md)
  : Get the concept classes associated with a codelist
- [`associatedDomains()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedDomains.md)
  : Get the domains associated with a codelist
- [`associatedDoseForms()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedDoseForms.md)
  : Get the dose forms associated with drug concepts in a codelist
- [`associatedDoseUnits()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedDoseUnits.md)
  : Get available dose units
- [`associatedDrugIngredients()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedDrugIngredients.md)
  : Get the names of drug ingredients associated with codelist
- [`associatedRouteCategories()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedRouteCategories.md)
  : Get drug routes associated with a codelist
- [`associatedRelationshipIds()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedRelationshipIds.md)
  : Get available relationships with concepts in a codelist
- [`associatedVocabularies()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedVocabularies.md)
  : Get the vocabularies associated with a codelist

### Datasets

- [`doseFormToRoute`](https://darwin-eu.github.io/CodelistGenerator/reference/doseFormToRoute.md)
  : Table showing the route category associated with each dose form.

### Benchmark

- [`benchmarkCodelistGenerator()`](https://darwin-eu.github.io/CodelistGenerator/reference/benchmarkCodelistGenerator.md)
  : Run benchmark of codelistGenerator analyses

### Create a mock dataset that contains vocabulary tables

- [`mockVocabRef()`](https://darwin-eu.github.io/CodelistGenerator/reference/mockVocabRef.md)
  : Generate example vocabulary database
