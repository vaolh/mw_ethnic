*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off
set linesize 250
set varabbrev off

*** REPLICATION FILE: enigh-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-03

*** Builds the individual × year ENIGH cross-section by collapsing
*** enigh-month.dta over the 6-month reference period and merging
*** CONEVAL poverty / formality flags.
***
*** Companion R script: enigh-year.R (must produce same N + within 1e-6).

cap mkdir log
log using "log/enigh-year.log", replace text

include _helpers.do

local REF_PERIOD_MONTHS = 6
local YEARS 2016 2018 2020 2022 2024

*************************************************
*************** Load enigh-month *****************
*************************************************

use "../../data/clean/enigh/enigh-month.dta", clear
display _n "Loaded enigh-month: N = " _N " obs, vars = " c(k)

*************************************************
************** Aggregate to year *****************
*************************************************

*** Income variables: SUM over months / 6 = monthly average over reference period.
local income_bases wages non_wage_income gov_transfers rentas fin_capital ///
                   negocio ventas other lab mon
local income_cols ""
foreach b of local income_bases {
    local income_cols "`income_cols' ing_`b'_nom ing_`b'"
}

*** Capture invariant individual attributes via firstnm.
unab all_present : *
local invariant_cols ""
foreach v in ubica_geo state ent_name reg_num reg_name macro_num macro_name ///
             tam_loc factor upm est_dis ///
             zlfn post treat_post ///
             gender female edad edad_pob edadsq years_of_study hoursworked ///
             employed school_attendance motherhome fatherhome ///
             etnia indspeaker indund smg ///
             scian1 sinco1 ind1 subor1 indep1 personal1 pago1 ///
             contrato1 tipocontr1 hours1 ///
             scian2 sinco2 ind2 subor2 indep2 personal2 pago2 ///
             contrato2 tipocontr2 hours2 ///
             incapacidad1 aguinaldo1 vacaciones1 utilidades1 credito_vivienda1 ///
             guarderias1 cuidados_parentales1 sar_afore1 seguro_vida1 prestamos1 ///
             prima_vacacional1 becas1 comedor1 fonacot1 despensa1 ///
             servicios_publicos1 pension_invalidez1 pension_familia1 ///
             otras_prestaciones1 sin_prestaciones1 ///
             incapacidad2 aguinaldo2 vacaciones2 utilidades2 credito_vivienda2 ///
             guarderias2 cuidados_parentales2 sar_afore2 seguro_vida2 prestamos2 ///
             prima_vacacional2 becas2 comedor2 fonacot2 despensa2 ///
             servicios_publicos2 pension_invalidez2 pension_familia2 ///
             otras_prestaciones2 sin_prestaciones2 ///
             tipo_viv mat_pared mat_techos mat_pisos antiguedad cuart_dorm ///
             num_cuarto disp_agua dotac_agua excusado disp_elect combustible ///
             eli_basura tenencia renta estim_pago pago_viv ///
             tot_resid tot_hom tot_muj tot_hog est_socio ///
             gaspers_tri gaspers_ntri parentesco num_trabaj {
    cap confirm variable `v'
    if _rc == 0 {
        local invariant_cols "`invariant_cols' `v'"
    }
}

collapse ///
    (sum) `income_cols' ///
    (firstnm) `invariant_cols', ///
    by(folioviv foliohog numren year)

*** Divide income totals by reference period length to get monthly average.
foreach b of local income_bases {
    replace ing_`b'_nom = ing_`b'_nom / `REF_PERIOD_MONTHS'
    replace ing_`b'      = ing_`b'      / `REF_PERIOD_MONTHS'
}

*** Recompute logs from real income (parallel arrays).
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
**************** CONEVAL merge *******************
*************************************************

*** Load INPC deflators to deflate CONEVAL ictpc/ict/nomon by August INPC.
tempfile defl_tf
load_deflators_to "`defl_tf'"

tempfile coneval_all
local first_iter = 1
foreach yr of local YEARS {
    capture confirm file "../../data/source/coneval/pobreza`yr'.dta"
    if _rc {
        display as error "CONEVAL file missing for `yr', skipping."
        continue
    }
    preserve
    use "../../data/source/coneval/pobreza`yr'.dta", clear
    rename _all, lower
    keep if pea == 1 | pea == 2
    *** Get August INPC deflator for `yr'.
    quietly: use "`defl_tf'", clear
    keep if year == `yr' & month == 8
    quietly: summarize deflator
    local ago_def = r(mean)
    if `ago_def' == 0 | missing(`ago_def') local ago_def = 1
    use "../../data/source/coneval/pobreza`yr'.dta", clear
    rename _all, lower
    keep if pea == 1 | pea == 2
    cap replace hli = 2 if missing(hli)
    cap gen byte informal = (pea == 1 & ss_dir == 0)
    foreach v in ictpc ict nomon reg_esp pago_esp {
        cap confirm variable `v'
        if _rc == 0 {
            cap gen double `v'_nom = `v'
            replace `v' = `v' / `ago_def'
        }
    }
    *** Drop CONEVAL income aggregates that we already compute from ENIGH.
    foreach v in ing_mon ing_lab ing_ren ing_tra {
        cap drop `v'
    }
    gen year = `yr'
    *** Restrict to canonical CONEVAL columns; tolerate missing ones.
    local cv_keep folioviv foliohog numren year ///
         hli hlio hlm informal pea ss_dir ///
         ictpc ict nomon reg_esp pago_esp ///
         ictpc_nom ict_nom nomon_nom ///
         clas_emp tam_emp sector
    local cv_present ""
    foreach v of local cv_keep {
        cap confirm variable `v'
        if _rc == 0 local cv_present "`cv_present' `v'"
    }
    keep `cv_present'
    cap quietly: replace folioviv = string(real(folioviv), "%010.0f") if !missing(folioviv)
    if `first_iter' {
        save `coneval_all', replace
        local first_iter = 0
    }
    else {
        append using `coneval_all', force
        save `coneval_all', replace
    }
    restore
}

*** Merge CONEVAL into the year-level cross-section.
capture confirm file `coneval_all'
if _rc == 0 {
    merge 1:1 folioviv foliohog numren year using `coneval_all'
    drop if _merge == 2
    drop _merge
}

*************************************************
*************** Distributional ranks *************
*************************************************

*** Build weighted deciles and centiles per year, by year-conditional sort.
foreach pair in "ictpc 10 deciles_ictpc" "ictpc 100 centiles_ictpc" ///
                "ing_lab 10 deciles_inglab" "ing_lab 100 centiles_inglab" {
    tokenize "`pair'"
    local var "`1'"
    local n   "`2'"
    local out "`3'"
    cap drop `out' _cumw _totw
    bysort year (`var'): gen double _cumw = sum(factor) ///
        if !missing(`var') & factor > 0 & !missing(factor)
    by year: egen double _totw = max(_cumw)
    gen byte `out' = ceil(`n' * _cumw / _totw) ///
        if !missing(_cumw) & !missing(_totw) & _totw > 0
    replace `out' = `n' if `out' > `n' & !missing(`out')
    drop _cumw _totw
}

*************************************************
**************** Save output *********************
*************************************************

compress
save "../../data/clean/enigh/enigh-year.dta", replace

display _n "Saved enigh-year.dta with " _N " observations and " c(k) " variables."

cap log close
