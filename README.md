# ChunhuaLab_2016BBW
Code and Data for 2016 JBI publication: "Black Box Warning Prediction by Mining Collective Patient Selection Patterns from Linked Public Data Resources"

The purpose of this project is to track the trend of clinical trial eligibility criteria and
1) assess trend shift and possible bias
2) try to estimate the future drug performance
3) generate the eligibility patients' profile and real-world patients' profile, find the gap between those two



Methods summary: # with existing MySQL database
query: return trial list [[CT1 for drug1,CT2 for drug 1...],[CT for drug2]...] with the search of a certain drug name list
    (from clinicaltrials.gov search engine, not from database)
extractRule/extractConcept
mappingUMLS
compareRules/compareConcepts/compareTrials
aggregateRules/aggregateConcepts

The program is reconstructed by Handong Ma (hm2588 at columbia dot edu) with previous work of Prof. Chunhua Weng's lab
