# ChunhuaLab_2016BBW
#### aka: Project: EliTES (Eligibility criteria Tracking and Estimation System)

Code and Data for 2016 JBI publication: "Black Box Warning Prediction by Mining Collective Patient Selection Patterns from Linked Public Data Resources"

The purpose of this project is to track the trend of clinical trial eligibility criteria and
1) assess trend shift and possible bias
2) try to estimate the future drug performance
3) generate the eligibility patients' profile and real-world patients' profile, find the gap between those two


# Project structure

####Note: This github reppository contains most code and some data used by this project. Limited by file size, some big datasets and algorithms/softwares (all open to public) are not uploaded. Detailed information will be provided upon request.

1 ./data
  * contains some datasets used by this project
  
2 ./script
  * all scripts (mainly python and R) used by this project
  * main.py main script
  * parameters.py all parameters specified here
  * ./pylib python libraries used in the project
  * ./rlib R library used in the project (for Rscript user, one can import the src.Rproj to run all R code) 
  
3 ./result
  * some (middle) results

## Code summary

### Data Pre-Processing

####step1: using locally saved drugs.com data to find all drug list
  * `tfidf.drugList_occursAtLeastOnceInAllYears()`

####step2: map drugs.com name to PDR to ensure drugs can be mapped to BBW info
  * `tfidf.drugList_findBBWInfo(candiDrugList)`

####step3: map drugs.com-PDR name to NDC and marketing information
  * `map.mappingMarketingDate_fromDrugList(candiDrugList_BBW.keys(), fh_ndc_source)`
  * `map.mappingMarketingDate_fromDrugList(candiDrugList_robust.keys(), fh_ndc_source)`
  * `map.mappingMarketingDate_fromDrugList(candiDrugList, fh_ndc_source)`

####step4: mapping drugs to NDC for marketing date
  * `ctgov.retrieveOnlineDrugTrialInfo_fromDrugNameAndMarketingDateDic(candiDrugList_BBW_dated, fh_ctgov_drugTrialContent_csv+'_BBW', fh_ctgov_drugTrialContent_tab+'_BBW', fh_ctgov_drugTrialList_tab+'_BBW', fh_ctgov_drugTrialTmp+'_BBW')`
  * `ctgov.retrieveOnlineDrugTrialInfo_fromDrugNameAndMarketingDateDic(candiDrugList_robust_dated, fh_ctgov_drugTrialContent_csv+'_ROBUST', fh_ctgov_drugTrialContent_tab+'_ROBUST', fh_ctgov_drugTrialList_tab+'_ROBUST', fh_ctgov_drugTrialTmp+'_ROBUST')`

####step5: find eligibile drugs with lower limit trial numbers per period
  * `tfidf.selectEligibleDrugs(ctList_robust_pre, ctList_robust_post, para_lowerLimit_CTperPerid)`
  * `tfidf.selectEligibleDrugs(ctList_BBW_pre, ctList_BBW_post, para_lowerLimit_CTperPerid)`

### Data Analysis (R)
  * Please follow the code in 'r_main.R'




## Methods summary: with existing MySQL database
query: return trial list [[CT1 for drug1,CT2 for drug 1...],[CT for drug2]...] with the search of a certain drug name list
    
