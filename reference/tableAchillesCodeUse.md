# Format the result of summariseAchillesCodeUse into a table

Format the result of summariseAchillesCodeUse into a table

## Usage

``` r
tableAchillesCodeUse(
  result,
  type = "gt",
  header = c("cdm_name", "estimate_name"),
  groupColumn = character(),
  hide = character(),
  style = "default",
  .options = list()
)
```

## Arguments

- result:

  A `<summarised_result>` with results of the type "achilles_code_use".

- type:

  Type of desired formatted table. To see supported formats use
  visOmopResults::tableType().

- header:

  A vector specifying the elements to include in the header. The order
  of elements matters, with the first being the topmost header. The
  header vector can contain one of the following variables: "cdm_name",
  "codelist_name", "domain_id", "standard_concept_name",
  "standard_concept_id", "estimate_name", "standard_concept",
  "vocabulary_id". Alternatively, it can include other names to use as
  overall header labels.

- groupColumn:

  Variables to use as group labels. Allowed columns are: "cdm_name",
  "codelist_name", "domain_id", "standard_concept_name",
  "standard_concept_id", "estimate_name", "standard_concept",
  "vocabulary_id". These cannot be used in header.

- hide:

  Table columns to exclude, options are: "cdm_name", "codelist_name",
  "domain_id", "standard_concept_name", "standard_concept_id",
  "estimate_name", "standard_concept", "vocabulary_id". These cannot be
  used in header or groupColumn.

- style:

  A character string or custom R code to define the visual formatting of
  the table. This argument can be provided in two ways: (1) Pre-defined
  Styles (Character String): Use a name for a built-in style (e.g.,
  "darwin"). See visOmopResults::tableStyle() for available options. (2)
  Custome Code (Advanced): Supply a block of custom R code. This code
  must be specific to the table type. See
  visOmopResults::tableStyleCode() for structural examples.

- .options:

  Named list with additional formatting options.
  visOmopResults::tableOptions() shows allowed arguments and their
  default values.

## Value

A table with a formatted version of the summariseCohortCodeUse result.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)

cdm <- mockVocabRef("database")
oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
result_achilles <- summariseAchillesCodeUse(newCodelist(list(oa = oa$concept_id)),
                                            cdm = cdm)
#> 
tableAchillesCodeUse(result_achilles)


  

```

Database name

mock

Codelist name

Domain ID

Standard concept name

Standard concept ID

Vocabulary ID

Source concept name

Estimate name

Record count

Person count

oa

condition

Osteoarthritis of knee

4

SNOMED

NA

400

400

Osteoarthritis of hip

5

SNOMED

NA

200

200

CDMConnector::[cdmDisconnect](https://darwin-eu.github.io/omopgenerics/reference/cdmDisconnect.html)(cdm)
\# }
