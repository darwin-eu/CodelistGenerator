# Get the names of all available Anatomical Therapeutic Chemical (ATC) classification codes

Get the names of all available Anatomical Therapeutic Chemical (ATC)
classification codes

## Usage

``` r
availableATC(cdm, level = c("ATC 1st"))
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- level:

  ATC level. Can be one or more of "ATC 1st", "ATC 2nd", "ATC 3rd", "ATC
  4th", and "ATC 5th".

## Value

A vector containing the names of ATC codes for the chosen level(s) found
in the concept table of cdm.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get ATC 1st level classification codes
availableATC(cdm, level = "ATC 1st")
#> [1] "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS"
#> [2] "RESPIRATORY SYSTEM"                        

# Get all ATC classification codes
availableATC(cdm, level = c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5th"))
#>  [1] "combinations of electrolytes; parenteral"  
#>  [2] "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS"
#>  [3] "ANTINEOPLASTIC AGENTS"                     
#>  [4] "THROAT PREPARATIONS"                       
#>  [5] "DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES"     
#>  [6] "COUGH AND COLD PREPARATIONS"               
#>  [7] "ANTIHISTAMINES FOR SYSTEMIC USE"           
#>  [8] "OTHER RESPIRATORY SYSTEM PRODUCTS"         
#>  [9] "ENDOCRINE THERAPY"                         
#> [10] "IMMUNOSTIMULANTS"                          
#> [11] "IMMUNOSUPPRESSANTS"                        
#> [12] "RESPIRATORY SYSTEM"                        
#> [13] "NASAL PREPARATIONS"                        
# }
```
