*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off
set linesize 250
set varabbrev off

*** REPLICATION FILE: enigh-hhlevel-inc-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-06

*** Builds the household × year ENIGH income cross-section.
*** Each row = one household (folioviv + foliohog) per ENIGH wave.
***   - HH income aggregated from enigh-indlevel-inc-month (sum across earners and months)
***   - HH expenditure from concentradohogar (gas_nm_pa, gas_food, gas_nonfood)
***   - Housing characteristics from viviendas (already in enigh-indlevel-inc-month)
***   - Composition: n_indigenous, n_workers, n_kids, n_employed, n_informal,
***     mean_age, mean_educ, share_female (computed from enigh-indlevel-inc-month rows)
***   - Head characteristics from poblacion (parentesco == 101)
***   - CONEVAL poverty: hli, ictpc, ict, informal, pea, ss_dir
***
*** Output: ../../data/clean/enigh/enigh-hhlevel-inc-year.dta
*** Companion R script: enigh-hhlevel-inc-year.R

cap mkdir log
log using "log/enigh-hhlevel-inc-year.log", replace text

include _helpers.do

local REF_PERIOD_MONTHS = 6
local YEARS 2016 2018 2020 2022 2024

*************************************************
*********** Aggregate from enigh-month ***********
*************************************************

use "../../data/clean/enigh/enigh-indlevel-inc-month.dta", clear
display _n "Loaded enigh-month: N = " _N

*** First, collapse to person × year (sum income over the 6 months).
local income_bases wages non_wage_income gov_transfers rentas fin_capital ///
                   negocio ventas other lab mon
local income_cols ""
foreach b of local income_bases {
    local income_cols "`income_cols' ing_`b'_nom ing_`b'"
}

preserve
collapse (sum) `income_cols' ///
         (firstnm) ubica_geo state ent_name reg_num reg_name macro_num macro_name ///
                   tam_loc factor upm est_dis zlfn post treat_post smg ///
                   gender female edad edad_pob years_of_study hoursworked ///
                   employed school_attendance etnia indspeaker indund parentesco ///
                   tipo_viv mat_pared mat_techos mat_pisos antiguedad cuart_dorm ///
                   num_cuarto disp_agua dotac_agua excusado disp_elect combustible ///
                   eli_basura tenencia renta estim_pago pago_viv tot_resid ///
                   tot_hom tot_muj tot_hog est_socio sar_afore1 indep1 ind1 ///
                   gaspers_tri gaspers_ntri, ///
         by(folioviv foliohog numren year)

foreach b of local income_bases {
    replace ing_`b'_nom = ing_`b'_nom / `REF_PERIOD_MONTHS'
    replace ing_`b'      = ing_`b'      / `REF_PERIOD_MONTHS'
}
tempfile py_data
save `py_data'
restore

*** -- Now collapse from person × year to HH × year.
*** Sum income across earners; flag composition counts.
use `py_data', clear

*** Indicator constructions used in composition counts.
gen byte _is_kid          = (edad < 15) if !missing(edad)
gen byte _is_adult        = (edad >= 15) if !missing(edad)
gen byte _is_worker       = (employed == 1)
gen byte _is_indig        = (etnia == 1)
gen byte _is_hli          = (indspeaker == 1)
gen byte _is_female       = (gender == 0)
gen byte _is_informal     = (sar_afore1 == 0 & employed == 1)

*** Aggregate at HH level.
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
                   tot_hom tot_muj tot_hog est_socio gaspers_tri gaspers_ntri, ///
         by(folioviv foliohog year)

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

tempfile hh_panel
save `hh_panel'

*************************************************
**** HH expenditure aggregated from gastoshogar **
*************************************************

*** Replaces the previous concentradohogar block — we now sum the raw
*** clave-level expenditure records into capítulo (10 official COICOP groups)
*** and custom groups (food, durables, services, ...). aggregate_gastos_hh_year
*** is defined in _helpers.do.

tempfile defl_tf
load_deflators_to "`defl_tf'"

tempfile hh_exp_all
local first_iter = 1
foreach yr of local YEARS {
    tempfile _hh_yr
    aggregate_gastos_hh_year `yr' "`defl_tf'" "`_hh_yr'"
    if `first_iter' {
        use "`_hh_yr'", clear
        save `hh_exp_all', replace
        local first_iter = 0
    }
    else {
        append using "`_hh_yr'", force
        save `hh_exp_all', replace
    }
}

use `hh_panel', clear
merge 1:1 folioviv foliohog year using `hh_exp_all'
drop if _merge == 2
drop _merge

*** Persist the merged panel before the head/class loops overwrite memory.
tempfile hh_panel_v2
save `hh_panel_v2'

*************************************************
**************** HH head from poblacion **********
*************************************************

tempfile hh_head_all
local first_iter = 1
foreach yr of local YEARS {
    use "../../data/source/enigh/poblacion`yr'.dta", clear
    rename _all, lower
    keep folioviv foliohog numren parentesco sexo edad etnia hablaind ///
         nivelaprob gradoaprob trabajo_mp
    cap destring parentesco sexo edad nivelaprob gradoaprob, replace
    keep if parentesco == 101
    duplicates drop folioviv foliohog, force
    gen byte head_age      = edad
    gen byte head_sex      = (sexo == 1)
    gen byte head_etnia    = (etnia == "1")
    gen byte head_indspeaker = (hablaind == "1")
    gen byte head_employed = (trabajo_mp == "1")
    build_years_of_study
    rename years_of_study head_educ
    keep folioviv foliohog head_age head_sex head_etnia head_indspeaker ///
         head_employed head_educ
    gen year = `yr'
    if `first_iter' {
        save `hh_head_all', replace
        local first_iter = 0
    }
    else {
        append using `hh_head_all', force
        save `hh_head_all', replace
    }
}

use `hh_panel_v2', clear
merge 1:1 folioviv foliohog year using `hh_head_all'
drop if _merge == 2
drop _merge

tempfile hh_panel_v3
save `hh_panel_v3'

*************************************************
**************** HH type from hogares ************
*************************************************

tempfile hh_class_all
local first_iter = 1
foreach yr of local YEARS {
    use "../../data/source/enigh/hogares`yr'.dta", clear
    rename _all, lower
    cap confirm variable clase_hog
    if _rc continue
    keep folioviv foliohog clase_hog
    duplicates drop folioviv foliohog, force
    cap destring clase_hog, replace
    gen year = `yr'
    if `first_iter' {
        save `hh_class_all', replace
        local first_iter = 0
    }
    else {
        append using `hh_class_all', force
        save `hh_class_all', replace
    }
}

use `hh_panel_v3', clear
capture confirm file `hh_class_all'
if _rc == 0 {
    merge 1:1 folioviv foliohog year using `hh_class_all'
    drop if _merge == 2
    drop _merge
}

*************************************************
**************** Save output *********************
*************************************************

*** Logs on HH-level real income (computed at the very end).
cap drop lnw
gen double lnw   = ln(ing_wages)            if ing_wages > 0           & !missing(ing_wages)
cap drop lnnwi
gen double lnnwi = ln(ing_non_wage_income)  if ing_non_wage_income > 0 & !missing(ing_non_wage_income)
cap drop lngt
gen double lngt  = ln(ing_gov_transfers)    if ing_gov_transfers > 0   & !missing(ing_gov_transfers)
cap drop lnr
gen double lnr   = ln(ing_rentas)           if ing_rentas > 0          & !missing(ing_rentas)
cap drop lnfc
gen double lnfc  = ln(ing_fin_capital)      if ing_fin_capital > 0     & !missing(ing_fin_capital)
cap drop lnn
gen double lnn   = ln(ing_negocio)          if ing_negocio > 0         & !missing(ing_negocio)
cap drop lnv
gen double lnv   = ln(ing_ventas)           if ing_ventas > 0          & !missing(ing_ventas)
cap drop lno
gen double lno   = ln(ing_other)            if ing_other > 0           & !missing(ing_other)
cap drop lni
gen double lni   = ln(ing_lab)              if ing_lab > 0             & !missing(ing_lab)
cap drop lnmon
gen double lnmon = ln(ing_mon)              if ing_mon > 0             & !missing(ing_mon)

order folioviv foliohog year ubica_geo state ent_name ///
      reg_num reg_name macro_num macro_name ///
      tam_loc factor upm est_dis zlfn post treat_post ///
      hh_size n_kids n_adults n_workers n_employed n_informal ///
      n_indigenous n_hli mean_age mean_educ share_female ///
      head_age head_sex head_etnia head_indspeaker head_employed head_educ ///
      ing_wages_nom ing_wages ing_non_wage_income_nom ing_non_wage_income ///
      ing_gov_transfers_nom ing_gov_transfers ing_rentas_nom ing_rentas ///
      ing_fin_capital_nom ing_fin_capital ing_negocio_nom ing_negocio ///
      ing_ventas_nom ing_ventas ing_other_nom ing_other ///
      ing_lab_nom ing_lab ing_mon_nom ing_mon ///
      gas_total_nom gas_total_real gas_total_nm_nom gas_total_nm_real ///
      gas_food_nom gas_food_real gas_durables_nom gas_durables_real ///
      gas_services_nom gas_services_real gas_housing_nom gas_housing_real ///
      gas_health_nom gas_health_real gas_transport_nom gas_transport_real ///
      lnw lnnwi lngt lnr lnfc lnn lnv lno lni lnmon

*** Apply value labels to every categorical column.
apply_all_labels

sort folioviv foliohog year
compress
save "../../data/clean/enigh/enigh-hhlevel-inc-year.dta", replace

display _n "Saved enigh-hhlevel-inc-year.dta with " _N " HH-year obs and " c(k) " variables."

cap log close
