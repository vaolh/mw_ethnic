*************************************************
**** Read HH × Subgroup × Year Expenditure *******
*************************************************

*** REPLICATION FILE: read-hhlevel-exp-subgroup-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-10

*** Year-cadence version of read-hhlevel-exp-subgroup-month.do. Collapses
*** the long expenditure panel to (HH × year × subgroup), then reshapes
*** wide so each row is (HH × year) with one column per subgroup × outcome.
*** Same harmonized ~65-subgroup partition from build-clave-crosswalk.py.

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-hhlevel-exp-month.dta", clear

drop if capitulo == "17"          // Q* capital flows — not consumption
drop if subgroup == "other_uncategorized"

*************************************************
**** Collapse claves -> subgroup × HH × year ****
*************************************************

collapse (sum) gas_real gas_nm_real ///
         (firstnm) zlfn post treat_post ubica_geo factor smg upm est_dis ///
                   state ent_name reg_num reg_name macro_num macro_name, ///
         by(folioviv foliohog year subgroup)

bysort folioviv foliohog year: egen double gas_total_real = total(gas_real)
gen double share      = gas_real / gas_total_real
replace share = 0 if missing(share)
gen double lngas_real = ln(gas_real + 1)

keep folioviv foliohog year subgroup ///
     gas_real lngas_real share ///
     zlfn post treat_post ubica_geo factor smg upm est_dis ///
     state ent_name reg_num reg_name macro_num macro_name ///
     gas_total_real

reshape wide gas_real lngas_real share, ///
        i(folioviv foliohog year) j(subgroup) string

ds gas_real*
local gasvars `r(varlist)'
foreach v of local gasvars {
    local sub = subinstr("`v'", "gas_real", "", 1)
    rename `v' gas_`sub'_real
}
ds lngas_real*
local lnvars `r(varlist)'
foreach v of local lnvars {
    local sub = subinstr("`v'", "lngas_real", "", 1)
    rename `v' lngas_`sub'_real
}
ds share*
local shvars `r(varlist)'
foreach v of local shvars {
    if "`v'" == "share_female" continue
    local sub = subinstr("`v'", "share", "", 1)
    rename `v' share_`sub'
}

ds share_*
local rawsubs `r(varlist)'
local SUBGROUPS ""
foreach v of local rawsubs {
    if "`v'" == "share_female" continue
    local sub = subinstr("`v'", "share_", "", 1)
    local SUBGROUPS `SUBGROUPS' `sub'
}
global SUBGROUPS `SUBGROUPS'
display as text "SUBGROUPS (" wordcount("$SUBGROUPS") "): " "$SUBGROUPS"

foreach s of global SUBGROUPS {
    cap replace gas_`s'_real   = 0 if missing(gas_`s'_real)
    cap replace lngas_`s'_real = 0 if missing(lngas_`s'_real)
    cap replace share_`s'      = 0 if missing(share_`s')
}

gen double lngas_total_real = ln(gas_total_real + 1)
label variable lngas_total_real "log(total real HH expenditure + 1)"

*************************************************
**** Merge HH composition + ethnicity ************
*************************************************

*** Year-cadence demographic controls come from the inc-year HH file.
*** Aggregate to (HH × year) first (collapse to first match per HH-year).
merge 1:1 folioviv foliohog year using ///
    "../../data/clean/enigh/enigh-hhlevel-inc-year.dta", ///
    keepusing(hh_size n_kids n_adults n_workers n_employed n_informal ///
              n_indigenous n_hli mean_age mean_educ share_female) ///
    keep(match) nogen

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
