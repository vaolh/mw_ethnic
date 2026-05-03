*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off
set linesize 250
set varabbrev off

*** REPLICATION FILE: enigh-household-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-03

*** Builds the household × month ENIGH panel.
*** Each row = one household per calendar month within the survey reference
*** period (typically Feb–Sep of each ENIGH wave).
***   - HH income aggregated from enigh-month (sum across earners)
***   - HH composition aggregated from enigh-month rows in that month
***   - HH-invariant attributes from concentradohogar (replicated to each month)
***
*** Output: ../../data/clean/enigh/enigh-household-month.dta
*** Companion R script: enigh-household-month.R

cap mkdir log
log using "log/enigh-household-month.log", replace text

include _helpers.do

*************************************************
*********** Aggregate from enigh-month ***********
*************************************************

use "../../data/clean/enigh/enigh-month.dta", clear
display _n "Loaded enigh-month: N = " _N

local income_bases wages non_wage_income gov_transfers rentas fin_capital ///
                   negocio ventas other lab mon

*** Indicator constructions used in composition counts.
gen byte _is_kid          = (edad < 15) if !missing(edad)
gen byte _is_adult        = (edad >= 15) if !missing(edad)
gen byte _is_worker       = (employed == 1)
gen byte _is_indig        = (etnia == 1)
gen byte _is_hli          = (indspeaker == 1)
gen byte _is_female       = (gender == 0)
gen byte _is_informal     = (sar_afore1 == 0 & employed == 1)

*** Aggregate at HH × month level.
collapse (sum) ing_wages_nom ing_wages ///
               ing_non_wage_income_nom ing_non_wage_income ///
               ing_gov_transfers_nom ing_gov_transfers ///
               ing_rentas_nom ing_rentas ///
               ing_fin_capital_nom ing_fin_capital ///
               ing_negocio_nom ing_negocio ///
               ing_ventas_nom ing_ventas ///
               ing_other_nom ing_other ///
               ing_lab_nom ing_lab ///
               ing_mon_nom ing_mon ///
               n_kids       = _is_kid ///
               n_adults     = _is_adult ///
               n_workers    = _is_worker ///
               n_indigenous = _is_indig ///
               n_hli        = _is_hli ///
               n_employed   = _is_worker ///
               n_informal   = _is_informal ///
         (mean) mean_age   = edad ///
                mean_educ  = years_of_study ///
                share_female = _is_female ///
         (firstnm) ubica_geo state ent_name reg_num reg_name macro_num macro_name ///
                   tam_loc factor upm est_dis zlfn post treat_post smg ///
                   tipo_viv mat_pared mat_techos mat_pisos antiguedad cuart_dorm ///
                   num_cuarto disp_agua dotac_agua excusado disp_elect combustible ///
                   eli_basura tenencia renta estim_pago pago_viv tot_resid ///
                   tot_hom tot_muj tot_hog est_socio deflator time, ///
         by(folioviv foliohog year month)

label variable n_kids       "# household members aged < 15"
label variable n_adults     "# household members aged >= 15"
label variable n_workers    "# employed members"
label variable n_employed   "# employed members"
label variable n_indigenous "# self-identified indigenous members"
label variable n_hli        "# indigenous-language-speaking members"
label variable n_informal   "# informal workers (employed & no SAR/AFORE)"
label variable mean_age     "mean age of HH members (>12)"
label variable mean_educ    "mean years of schooling (HH members >12)"
label variable share_female "share of female members"

gen hh_size = n_kids + n_adults
label variable hh_size      "household size (# residents from enigh-month)"

*** Logs on HH-level real income (parallel arrays).
local _bases   wages           non_wage_income gov_transfers rentas    fin_capital negocio ventas  other   lab     mon
local _lognames lnw            lnnwi           lngt          lnr       lnfc        lnn     lnv     lno     lni     lnmon
local _N : word count `_bases'
forvalues k = 1/`_N' {
    local b   : word `k' of `_bases'
    local lnm : word `k' of `_lognames'
    cap drop `lnm'
    gen double `lnm' = ln(ing_`b') if ing_`b' > 0 & !missing(ing_`b')
}

*************************************************
**************** Save output *********************
*************************************************

order folioviv foliohog year month time ubica_geo state ent_name ///
      reg_num reg_name macro_num macro_name ///
      tam_loc factor upm est_dis zlfn post treat_post ///
      hh_size n_kids n_adults n_workers n_employed n_informal ///
      n_indigenous n_hli mean_age mean_educ share_female ///
      ing_wages_nom ing_wages ing_non_wage_income_nom ing_non_wage_income ///
      ing_gov_transfers_nom ing_gov_transfers ing_rentas_nom ing_rentas ///
      ing_fin_capital_nom ing_fin_capital ing_negocio_nom ing_negocio ///
      ing_ventas_nom ing_ventas ing_other_nom ing_other ///
      ing_lab_nom ing_lab ing_mon_nom ing_mon ///
      lnw lnnwi lngt lnr lnfc lnn lnv lno lni lnmon ///
      deflator

sort folioviv foliohog year month
compress
save "../../data/clean/enigh/enigh-household-month.dta", replace

display _n "Saved enigh-household-month.dta with " _N " HH-month obs and " c(k) " variables."

cap log close
