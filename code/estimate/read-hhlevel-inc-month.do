*************************************************
************ Read HH-level Income (Month) *******
*************************************************

*** REPLICATION FILE: read-hhlevel-inc-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-06

*** Loads the HH × month ENIGH income panel from
*** code/build/enigh-hhlevel-inc-month.do.
***
*** Canonical names produced by the build:
***   - HH treatment: zlfn, post, treat_post
***   - HH outcomes:  lnw, lni, ing_*, ing_*_nom, lnnwi, lngt, lnr, lnfc,
***                   lnn, lnv, lno, lnmon
***   - HH composition: hh_size, n_kids, n_adults, n_workers, n_indigenous,
***                     n_hli, n_employed, n_informal, mean_age, mean_educ,
***                     share_female
***   - Geo / weights: ubica_geo, state, factor, upm, est_dis, smg
***   - Time: year, month, time (calendar)
***
*** No head_* vars at HH × month — ethnicity is approximated by
*** "any HLI member in HH" and "any indig member in HH".

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-hhlevel-inc-month.dta", clear

*************************************************
**** HH-level ethnicity indicators ***************
*************************************************

cap drop hli_hh
cap drop indig_hh
gen byte hli_hh   = (n_hli         > 0) if !missing(n_hli)
gen byte indig_hh = (n_indigenous  > 0) if !missing(n_indigenous)
label variable hli_hh   "Household has at least one HLI member"
label variable indig_hh "Household has at least one self-identified indigenous member"

*************************************************
***************** Controls **********************
*************************************************

global controls hh_size n_kids n_adults n_workers mean_age mean_educ share_female
