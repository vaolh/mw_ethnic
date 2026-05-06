*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off
set linesize 250
set varabbrev off

*** REPLICATION FILE: enigh-hhlevel-clave-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-04

*** Builds the household × clave_gasto × calendar-month long expenditure panel.
*** One row per (folioviv, foliohog, clave, month) when mes_dia is valid.
*** Includes:
***   - Real and nominal expenditure (gas_nom, gas_real, gas_nm_nom, gas_nm_real)
***   - clave (6-digit string), capitulo (2-digit COICOP), gasto_group (custom)
***   - clave_label merged from data/clean/enigh/clave_gasto_labels.csv
***   - Survey weights, treatment, geography from concentradohogar
***
*** Output: ../../data/clean/enigh/enigh-hhlevel-clave-month.dta
*** Companion R script: enigh-hhlevel-clave-month.R

cap mkdir log
log using "log/enigh-hhlevel-clave-month.log", replace text

include _helpers.do

local YEARS 2016 2018 2020 2022 2024

*************************************************
**************** INPC Deflators ******************
*************************************************

tempfile defl_tf
load_deflators_to "`defl_tf'"

*************************************************
*************** Build per-year ******************
*************************************************

tempfile clave_panel_all
local first_iter = 1

foreach yr of local YEARS {
    display _n "Processing gastoshogar `yr' …"

    *** Load gastoshogar and add capitulo + gasto_group columns.
    use "../../data/source/enigh/gastoshogar`yr'.dta", clear
    cap destring gasto_tri,  replace
    cap destring gas_nm_tri, replace
    classify_gasto_clave

    *** Extract calendar month from mes_dia ("MMDD"; missing == "0000").
    cap confirm string variable mes_dia
    if _rc tostring mes_dia, replace
    gen byte month = real(substr(mes_dia, 1, 2))
    drop if missing(month) | month < 1 | month > 12

    *** Aggregate to (HH × clave × month). Each trimestral observation is
    *** spread across the 3 reference months evenly when mes_dia is valid.
    gen double gas_nom    = gasto_tri  / 3
    gen double gas_nm_nom = gas_nm_tri / 3

    collapse (sum) gas_nom gas_nm_nom ///
             (firstnm) capitulo gasto_group, ///
             by(folioviv foliohog clave month)

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
    gen double gas_real    = gas_nom    / `ago_def'
    gen double gas_nm_real = gas_nm_nom / `ago_def'
    gen double deflator    = `ago_def'

    *** Build calendar time = first day of month in survey year.
    gen mes_str = string(month, "%02.0f")
    gen time    = date(string(`yr') + "-" + mes_str, "YM")
    format time %td
    drop mes_str
    gen year = `yr'

    *** Merge HH-level treatment + survey design from concentradohogar.
    tempfile gh_panel
    save `gh_panel', replace
    use "../../data/source/enigh/concentradohogar`yr'.dta", clear
    cap confirm string variable ubica_geo
    if _rc tostring ubica_geo, replace
    keep folioviv foliohog ubica_geo factor smg upm est_dis
    cap destring factor smg, replace
    duplicates drop folioviv foliohog, force
    clean_ubica_geo `yr'
    apply_state_region
    apply_zlfn
    tempfile hh_meta
    save `hh_meta', replace

    use `gh_panel', clear
    merge m:1 folioviv foliohog using `hh_meta'
    drop if _merge == 2
    drop _merge

    if `first_iter' {
        save `clave_panel_all', replace
        local first_iter = 0
    }
    else {
        append using `clave_panel_all', force
        save `clave_panel_all', replace
    }
}

use `clave_panel_all', clear

*** Treatment / time indicators.
gen byte post = (year > 2018)
gen byte treat_post = zlfn * post
label variable post       "Post-Treatment (year > 2018)"
label variable treat_post "ZLFN × Post"

*************************************************
*** Merge clave labels (1,763 codes from docs) ***
*************************************************

tempfile lbl_tf
preserve
import delimited using "../../data/clean/enigh/clave_gasto_labels.csv", ///
    clear varnames(1) stringcols(_all) encoding("utf-8")
rename label clave_label
duplicates drop clave, force
save `lbl_tf', replace
restore

merge m:1 clave using `lbl_tf'
drop if _merge == 2
drop _merge

label variable clave         "ENIGH expenditure clave (6-digit code)"
label variable clave_label   "ENIGH clave description (Spanish, from docs)"
label variable capitulo      "Capítulo (COICOP first 2 digits of clave)"
label variable gasto_group   "Custom group: food / durables / services / housing / transport / health / restaurants / etc."
label variable gas_nom       "nominal monthly expenditure (allocated from trimestral, pesos)"
label variable gas_real      "real (Aug-2024) monthly expenditure (pesos)"
label variable gas_nm_nom    "nominal monthly non-monetary expenditure (pesos)"
label variable gas_nm_real   "real monthly non-monetary expenditure (pesos)"
label variable deflator      "INPC deflator (Aug-yr / Aug-2024)"

*************************************************
**************** Save output *********************
*************************************************

apply_all_labels

order folioviv foliohog year month time ubica_geo state ent_name ///
      reg_num reg_name macro_num macro_name ///
      factor upm est_dis zlfn post treat_post smg ///
      clave clave_label capitulo gasto_group ///
      gas_nom gas_real gas_nm_nom gas_nm_real deflator

sort folioviv foliohog clave year month
compress
save "../../data/clean/enigh/enigh-hhlevel-clave-month.dta", replace

display _n "Saved enigh-hhlevel-clave-month.dta with " _N " HH-clave-month obs and " c(k) " variables."

cap log close
