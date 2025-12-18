# Exploring the OMOP CDM vocabulary tables

In this vignette, we will explore the functions that help us delve into
the vocabularies used in our database. These functions allow us to
explore the different vocabularies and concepts characteristics.

First of all, we will load the required packages and a eunomia database.

``` r
library(DBI)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)

# Connect to the database and create the cdm object
con <- dbConnect(duckdb::duckdb(), 
                 eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
                  cdmName = "Eunomia Synpuf",
                  cdmSchema   = "main",
                  writeSchema = "main", 
                  achillesSchema = "main")
```

Note that we have included [achilles
tables](https://github.com/OHDSI/Achilles) in our cdm reference, which
are used for some of the analyses.

## Vocabulary characteristics

We can first start by getting the vocabulary version of our CDM object:

``` r
vocabularyVersion(cdm)
#> [1] "v5.0 06-AUG-21"
```

And the available vocabularies, which correspond to the column
*vocabulary_id* from the concept table:

``` r
availableVocabularies(cdm)
#>  [1] "APC"                  "BDPM"                 "CMS Place of Service"
#>  [4] "CPT4"                 "Currency"             "DRG"                 
#>  [7] "Ethnicity"            "Gemscript"            "Gender"              
#> [10] "HCPCS"                "HES Specialty"        "ICD9Proc"            
#> [13] "LOINC"                "MDC"                  "NDC"                 
#> [16] "NUCC"                 "OPCS4"                "PCORNet"             
#> [19] "Provider"             "Race"                 "Relationship"        
#> [22] "Revenue Code"         "RxNorm"               "RxNorm Extension"    
#> [25] "SNOMED"               "UCUM"                 "Visit"
```

## Domains

We can also explore the domains that our CDM object has, which is the
column *domain_id* from the concept table:

``` r
availableDomains(cdm)
#>  [1] "Condition"           "Currency"            "Device"             
#>  [4] "Drug"                "Ethnicity"           "Gender"             
#>  [7] "Meas Value"          "Meas Value Operator" "Measurement"        
#> [10] "Metadata"            "Observation"         "Procedure"          
#> [13] "Provider"            "Race"                "Relationship"       
#> [16] "Revenue Code"        "Route"               "Spec Anatomic Site" 
#> [19] "Spec Disease Status" "Specimen"            "Unit"               
#> [22] "Visit"
```

or restrict the search among *standard* concepts:

``` r
availableDomains(cdm,
                 standardConcept = "Standard")
#>  [1] "Condition"           "Currency"            "Device"             
#>  [4] "Drug"                "Ethnicity"           "Gender"             
#>  [7] "Meas Value"          "Meas Value Operator" "Measurement"        
#> [10] "Metadata"            "Observation"         "Procedure"          
#> [13] "Provider"            "Race"                "Relationship"       
#> [16] "Revenue Code"        "Route"               "Spec Anatomic Site" 
#> [19] "Spec Disease Status" "Specimen"            "Unit"               
#> [22] "Visit"
```

## Concept class

We can further explore the different classes that we have (reported in
*concept_class_id* column from the concept table):

``` r
availableConceptClassIds(cdm)
#>  [1] "2-dig nonbill code"   "3-dig billing code"   "3-dig nonbill code"  
#>  [4] "4-dig billing code"   "Admitting Source"     "Answer"              
#>  [7] "APC"                  "Attribute"            "Body Structure"      
#> [10] "Branded Drug"         "Branded Drug Box"     "Branded Drug Comp"   
#> [13] "Branded Drug Form"    "Branded Pack"         "Branded Pack Box"    
#> [16] "Canonical Unit"       "Claims Attachment"    "Clinical Drug"       
#> [19] "Clinical Drug Box"    "Clinical Drug Comp"   "Clinical Drug Form"  
#> [22] "Clinical Finding"     "Clinical Observation" "Clinical Pack"       
#> [25] "Clinical Pack Box"    "Context-dependent"    "CPT4"                
#> [28] "CPT4 Hierarchy"       "CPT4 Modifier"        "Currency"            
#> [31] "Device"               "Discharge Status"     "Disposition"         
#> [34] "Doc Kind"             "Doc Role"             "Doc Setting"         
#> [37] "Doc Subject Matter"   "Doc Type of Service"  "Dose Form"           
#> [40] "Ethnicity"            "Event"                "Gemscript"           
#> [43] "Gemscript THIN"       "Gender"               "HCPCS"               
#> [46] "HCPCS Modifier"       "Ingredient"           "Lab Test"            
#> [49] "Linkage Assertion"    "Location"             "Marketed Product"    
#> [52] "MDC"                  "Morph Abnormality"    "MS-DRG"              
#> [55] "Observable Entity"    "Observation"          "Organism"            
#> [58] "Pharma/Biol Product"  "Physical Force"       "Physical Object"     
#> [61] "Physician Specialty"  "Procedure"            "Provider"            
#> [64] "Qualifier Value"      "Quant Branded Box"    "Quant Branded Drug"  
#> [67] "Quant Clinical Box"   "Quant Clinical Drug"  "Race"                
#> [70] "Relationship"         "Revenue Code"         "Social Context"      
#> [73] "Specimen"             "Staging / Scales"     "Substance"           
#> [76] "Survey"               "Unit"                 "Visit"
```

Or restrict the search among *non-standard* concepts with *condition*
domain:

``` r
availableConceptClassIds(cdm, 
                         standardConcept = "Non-standard", 
                         domain = "Condition")
#>  [1] "3-char billing code"  "3-char nonbill code"  "3-dig billing code"  
#>  [4] "3-dig billing E code" "3-dig billing V code" "3-dig nonbill code"  
#>  [7] "3-dig nonbill E code" "3-dig nonbill V code" "4-char billing code" 
#> [10] "4-char nonbill code"  "4-dig billing code"   "4-dig billing E code"
#> [13] "4-dig billing V code" "4-dig nonbill code"   "4-dig nonbill V code"
#> [16] "5-char billing code"  "5-char nonbill code"  "5-dig billing code"  
#> [19] "5-dig billing V code" "6-char billing code"  "6-char nonbill code" 
#> [22] "7-char billing code"  "Admin Concept"        "Clinical Finding"    
#> [25] "Context-dependent"    "Event"                "ICD10 code"          
#> [28] "ICD10 Hierarchy"      "ICD10 SubChapter"     "ICD9CM code"         
#> [31] "Morph Abnormality"    "Navi Concept"         "Observable Entity"   
#> [34] "Organism"             "OXMIS"                "Pharma/Biol Product" 
#> [37] "Physical Object"      "Procedure"            "Qualifier Value"     
#> [40] "Read"                 "SMQ"                  "Social Context"      
#> [43] "Staging / Scales"     "Substance"
```

## Relationships

We can also explore the different relationships that are present in our
CDM:

``` r
availableRelationshipIds(cdm)
#>  [1] "Asso finding of"   "Asso with finding" "Due to of"        
#>  [4] "Finding asso with" "Followed by"       "Follows"          
#>  [7] "Has asso finding"  "Has due to"        "Has manifestation"
#> [10] "Is a"              "Manifestation of"  "Mapped from"      
#> [13] "Maps to"           "Occurs after"      "Occurs before"    
#> [16] "Subsumes"
```

Or narrow the search among *standard* concepts with domain
*observation*:

``` r
availableRelationshipIds(cdm,
                         standardConcept1 = "Standard",
                         standardConcept2 = "Standard",
                         domains1 = "Observation",
                         domains2 = "Observation")
#>   [1] "Access of"            "Admin method of"      "Asso finding of"     
#>   [4] "Asso morph of"        "Asso proc of"         "Asso with finding"   
#>   [7] "Basic dose form of"   "Causative agent of"   "Characterizes"       
#>  [10] "Clinical course of"   "Component of"         "Contained in panel"  
#>  [13] "CPT4 - SNOMED cat"    "CPT4 - SNOMED eq"     "Dir morph of"        
#>  [16] "Dir subst of"         "Disp dose form of"    "Dose form of"        
#>  [19] "DRG - MDC cat"        "Due to of"            "Energy used by"      
#>  [22] "Finding asso with"    "Finding context of"   "Finding inform of"   
#>  [25] "Focus of"             "Has access"           "Has admin method"    
#>  [28] "Has asso finding"     "Has asso morph"       "Has asso proc"       
#>  [31] "Has basic dose form"  "Has causative agent"  "Has clinical course" 
#>  [34] "Has component"        "Has dir morph"        "Has dir subst"       
#>  [37] "Has disp dose form"   "Has dose form"        "Has due to"          
#>  [40] "Has finding context"  "Has focus"            "Has intended site"   
#>  [43] "Has intent"           "Has interprets"       "Has laterality"      
#>  [46] "Has method"           "Has modification"     "Has occurrence"      
#>  [49] "Has pathology"        "Has priority"         "Has proc context"    
#>  [52] "Has proc duration"    "Has proc morph"       "Has process output"  
#>  [55] "Has property"         "Has recipient cat"    "Has relat context"   
#>  [58] "Has release charact"  "Has scale type"       "Has severity"        
#>  [61] "Has spec active ing"  "Has state of matter"  "Has technique"       
#>  [64] "Has temp finding"     "Has temporal context" "Has time aspect"     
#>  [67] "Has transformation"   "Intended site of"     "Intent of"           
#>  [70] "Interprets of"        "Is a"                 "Is characterized by" 
#>  [73] "Laterality of"        "Mapped from"          "Maps to"             
#>  [76] "Maps to value"        "MDC cat - DRG"        "Method of"           
#>  [79] "Modification of"      "Occurrence of"        "Panel contains"      
#>  [82] "Pathology of"         "Plays role"           "Priority of"         
#>  [85] "Proc context of"      "Proc duration of"     "Proc morph of"       
#>  [88] "Process output of"    "Property of"          "Recipient cat of"    
#>  [91] "Relat context of"     "Relative to"          "Relative to of"      
#>  [94] "Release charact of"   "Role played by"       "Scale type of"       
#>  [97] "Severity of"          "SNOMED - CPT4 eq"     "SNOMED cat - CPT4"   
#> [100] "Spec active ing of"   "State of matter of"   "Subst used by"       
#> [103] "Subsumes"             "Technique of"         "Temp related to"     
#> [106] "Temporal context of"  "Time aspect of"       "Transformation of"   
#> [109] "Using energy"         "Using finding inform" "Using subst"         
#> [112] "Value mapped from"
```
