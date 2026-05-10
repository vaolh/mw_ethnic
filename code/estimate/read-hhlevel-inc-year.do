*************************************************
************ Read HH-level Income (Year) ********
*************************************************

*** REPLICATION FILE: read-hhlevel-inc-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-06

*** Loads the HH × year ENIGH cross-section from
*** code/build/enigh-hhlevel-inc-year.do.
***
*** Canonical names produced by the build:
***   - HH treatment: zlfn, post, treat_post
***   - HH outcomes:  lnw, lni, ing_*, ing_*_nom, ictpc-equivalent (mon)
***   - HH head:      head_age, head_sex, head_etnia, head_indspeaker,
***                   head_educ, head_employed
***   - HH composition: hh_size, n_kids, n_adults, n_workers, n_indigenous,
***                     n_hli, n_employed, n_informal, mean_age, mean_educ,
***                     share_female
***   - Geo / weights: ubica_geo, state, factor, upm, est_dis, smg
***
*** Sets the global $controls used by the DiD scripts that consume this file.

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-hhlevel-inc-year.dta", clear

*************************************************
**** Build squared age + HH-level dummies *******
*************************************************

cap drop head_agesq
gen double head_agesq = head_age * head_age

*** Binary indicators consistent with the did-wage-ethnic columns (head-based).
cap drop hli_hh
cap drop indig_hh
gen byte hli_hh   = (head_indspeaker == 1) if !missing(head_indspeaker)
gen byte indig_hh = (head_etnia      == 1) if !missing(head_etnia)
label variable hli_hh   "Household head speaks an indigenous language"
label variable indig_hh "Household head self-identifies as indigenous"

*************************************************
***************** Controls **********************
*************************************************

global controls i.head_sex head_age head_agesq head_educ hh_size n_kids n_workers
