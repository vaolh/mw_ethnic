*************************************************
*** Build clave→subgroup crosswalk ***************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: code/build/build-clave-crosswalk.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-11

*** Enumerates every unique clave across all gastoshogar*.dta files,
*** applies the range-based seed rules from data/clean/enigh/clave_subgroups_seed.csv
*** (first-match-wins, lex string comparison), and writes the unified
*** crosswalk as a Stata .dta. This runs ONCE during the build and
*** caches the result, so classify_gasto_clave inside the per-year
*** expenditure build can do a fast m:1 merge against ~2k unique
*** claves rather than re-applying the seed rules five times.
***
*** Output: data/clean/enigh/clave_crosswalk.dta
***   one row per clave with columns:
***     clave          — string
***     capitulo_norm  — 2-digit harmonized capítulo
***     subgroup       — concepto subgroup
***     subgroup_label — Spanish display label
***
*** Fails (exit 9) if any clave is unmatched by the seed.

cap mkdir log
log using "log/build-clave-crosswalk.log", replace text

local YEARS 2016 2018 2020 2022 2024

*************************************************
**** Enumerate unique claves across waves ********
*************************************************

tempfile uclaves
clear
gen str10 clave = ""
save `uclaves', replace emptyok

foreach yr of local YEARS {
    di as text "scanning gastoshogar`yr'.dta …"
    use clave using "../../data/source/enigh/gastoshogar`yr'.dta", clear
    duplicates drop clave, force
    append using `uclaves'
    duplicates drop clave, force
    save `uclaves', replace
}

use `uclaves', clear

*** Drop pre-aggregated T-codes upfront (mirrors classify_gasto_clave).
drop if substr(clave, 1, 1) == "T"

di as text "unique non-T claves found: " _N

*************************************************
**** Apply seed rules (first-match-wins) *********
*************************************************

preserve
qui import delimited "../../data/clean/enigh/clave_subgroups_seed.csv", ///
    stringcols(_all) varnames(1) clear
local nrules = _N
forval r = 1/`nrules' {
    local _s`r'  = clave_start[`r']
    local _e`r'  = clave_end[`r']
    local _c`r'  = capitulo_norm[`r']
    local _sg`r' = subgroup[`r']
    local _sl`r' = subgroup_label[`r']
}
restore

gen str10 capitulo_norm  = ""
gen str40 subgroup       = ""
gen str80 subgroup_label = ""

forval r = 1/`nrules' {
    qui replace capitulo_norm  = "`_c`r''"  ///
        if missing(subgroup) & "`_s`r''" <= clave & clave <= "`_e`r''"
    qui replace subgroup_label = "`_sl`r''" ///
        if missing(subgroup) & "`_s`r''" <= clave & clave <= "`_e`r''"
    qui replace subgroup       = "`_sg`r''" ///
        if missing(subgroup) & "`_s`r''" <= clave & clave <= "`_e`r''"
}

*** Coverage gate.
cap assert !missing(subgroup)
if _rc {
    di as error "build-clave-crosswalk: unmatched clave(s); " ///
                "extend data/clean/enigh/clave_subgroups_seed.csv"
    list clave if missing(subgroup), clean noobs
    exit 9
}

di as text "all " _N " unique claves matched by seed rules"
tab capitulo_norm, missing

*************************************************
**** Save crosswalk *******************************
*************************************************

label variable clave          "ENIGH expenditure clave"
label variable capitulo_norm  "Capítulo harmonizado (2 dígitos)"
label variable subgroup       "Concepto subgroup (~65 buckets)"
label variable subgroup_label "Subgroup display label (Spanish)"

sort clave
compress
save "../../data/clean/enigh/clave_crosswalk.dta", replace

di as text "saved data/clean/enigh/clave_crosswalk.dta (" _N " rows)"

cap log close
