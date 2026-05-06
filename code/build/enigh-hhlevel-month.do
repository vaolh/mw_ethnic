*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off
set linesize 250
set varabbrev off

*** REPLICATION FILE: enigh-hhlevel-month.do
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
*** Output: ../../data/clean/enigh/enigh-hhlevel-month.dta
*** Companion R script: enigh-hhlevel-month.R

cap mkdir log
log using "log/enigh-hhlevel-month.log", replace text

include _helpers.do

*************************************************
*********** Aggregate from enigh-month ***********
*************************************************

use "../../data/clean/enigh/enigh-indlevel-month.dta", clear
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
*** HH expenditure aggregated from gastoshogar ***
*************************************************

*** For HH × month, distribute trimestral expenditure (gasto_tri) evenly
*** across the 3 reference months when mes_dia is missing; attribute to the
*** specific month when mes_dia is valid. We aggregate by (HH × month) and
*** merge into the panel.

tempfile defl_tf
load_deflators_to "`defl_tf'"

tempfile defl_tf2
save `defl_tf2'

tempfile hh_panel_pre_gas
save `hh_panel_pre_gas'

local YEARS 2016 2018 2020 2022 2024
tempfile hh_gas_month_all
local first_iter = 1
foreach yr of local YEARS {
    use "../../data/source/enigh/gastoshogar`yr'.dta", clear
    cap destring gasto_tri,  replace
    cap destring gas_nm_tri, replace
    classify_gasto_clave
    *** Extract month from mes_dia (MMDD); when "0000" or empty, distribute
    *** evenly across the 3 months centered on the survey reference window.
    cap confirm string variable mes_dia
    if _rc tostring mes_dia, replace
    gen byte month_from_dia = real(substr(mes_dia, 1, 2))
    *** ENIGH reference period is the survey trimester. We allocate 1/3 of
    *** the trimestral amount to each of months {month_from_dia, +1, -1}
    *** if the date is valid; otherwise drop (we keep the year-level total
    *** in enigh-hhlevel-year.dta which uses the full quarterly aggregate).
    drop if missing(month_from_dia) | month_from_dia < 1 | month_from_dia > 12
    *** Each trimestral observation becomes 1 monthly attribution.
    gen double gas_nom_m  = gasto_tri  / 3
    gen double gas_nm_m   = gas_nm_tri / 3
    *** Aggregate to (HH × month).
    collapse (sum) gas_nom_m gas_nm_m, ///
        by(folioviv foliohog month_from_dia)
    rename month_from_dia month
    rename gas_nom_m gas_total_nom_month
    rename gas_nm_m  gas_total_nm_nom_month
    *** Deflate by Aug-`yr' INPC.
    quietly {
        preserve
        use "`defl_tf'", clear
        keep if year == `yr' & month == 8
        summarize deflator
        local ago_def = r(mean)
        restore
    }
    if `ago_def' == 0 | missing(`ago_def') local ago_def = 1
    gen double gas_total_real_month    = gas_total_nom_month    / `ago_def'
    gen double gas_total_nm_real_month = gas_total_nm_nom_month / `ago_def'
    gen year = `yr'
    if `first_iter' {
        save `hh_gas_month_all', replace
        local first_iter = 0
    }
    else {
        append using `hh_gas_month_all', force
        save `hh_gas_month_all', replace
    }
}

use `hh_panel_pre_gas', clear
merge 1:1 folioviv foliohog year month using `hh_gas_month_all'
drop if _merge == 2
drop _merge

label variable gas_total_nom_month     "nominal HH expenditure this month (pesos)"
label variable gas_total_real_month    "real (Aug-2024) HH expenditure this month"
label variable gas_total_nm_nom_month  "nominal HH non-monetary expenditure this month"
label variable gas_total_nm_real_month "real HH non-monetary expenditure this month"

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
      gas_total_nom_month gas_total_real_month ///
      gas_total_nm_nom_month gas_total_nm_real_month ///
      lnw lnnwi lngt lnr lnfc lnn lnv lno lni lnmon ///
      deflator

*** Apply value labels to every categorical column.
apply_all_labels

sort folioviv foliohog year month
compress
save "../../data/clean/enigh/enigh-hhlevel-month.dta", replace

display _n "Saved enigh-hhlevel-month.dta with " _N " HH-month obs and " c(k) " variables."

cap log close
