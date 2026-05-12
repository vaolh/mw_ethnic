*************************************************
*** Read HH × Subgroup × Month Expenditure *******
*************************************************

*** REPLICATION FILE: read-hhlevel-exp-subgroup-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-10

*** Loads the long expenditure panel (one row per HH × clave × month) from
*** code/build/enigh-hhlevel-exp-month.do, then collapses to the harmonized
*** ~65-subgroup partition built by build-clave-crosswalk.py. Reshapes to
*** wide format so each row is (HH × year × month) with one column per
*** subgroup for both real-expenditure level (gas_<sub>_real) and share of
*** household total (share_<sub>). Merges HH demographics + treatment from
*** enigh-hhlevel-inc-month.dta.
***
*** Output dataset in memory has, per (HH × year × month) row:
***   folioviv foliohog year month time                       (key)
***   gas_total_real lngas_total_real                          (totals)
***   gas_<sub>_real, lngas_<sub>_real, share_<sub>            (~65 × 3)
***   zlfn post treat_post ubica_geo factor smg                (treatment)
***   hli_hh indig_hh                                          (strata)
***   $controls = hh_size n_kids n_adults n_workers
***                mean_age mean_educ share_female
***
*** Macros set on exit:
***   SUBGROUPS — space-separated list of all ~65 subgroup names
***   $controls — global of HH composition control vars
***
*** Note: cap 17 (Q* claves = financial capital flows) is excluded from
*** the consumption analysis to keep outcomes interpretable as expenditure.

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-hhlevel-exp-month.dta", clear

*** Drop capital-flow rows (Q* / cap 17) — these are not consumption.
drop if capitulo == "17"

*** Drop the placeholder "other_uncategorized" subgroup — empty-label codes
*** that carry no substantive content.
drop if subgroup == "other_uncategorized"

*************************************************
**** Collapse claves -> subgroup × HH × month *****
*************************************************

*** Sum within (HH, year, month, subgroup) — one row per HH-month-subgroup.
collapse (sum) gas_real gas_nm_real ///
         (firstnm) zlfn post treat_post ubica_geo factor smg upm est_dis ///
                   state ent_name reg_num reg_name macro_num macro_name time, ///
         by(folioviv foliohog year month subgroup)

*************************************************
**** Compute HH-month totals ********************
*************************************************

*** Total real expenditure per (HH, month) — denominator for shares.
bysort folioviv foliohog year month: egen double gas_total_real = total(gas_real)

*** Expenditure share within each (HH, month).
gen double share = gas_real / gas_total_real
replace share = 0 if missing(share)  // zero-total months → zero share

*** Logged real outcome (log+1 to handle zero-spending subgroup-months).
gen double lngas_real = ln(gas_real + 1)

*************************************************
**** Reshape wide: one col per subgroup ********
*************************************************

*** Build a sortable subgroup-string key (Stata reshape needs ASCII names).
*** subgroup is already snake_case ASCII from the crosswalk.

keep folioviv foliohog year month time subgroup ///
     gas_real lngas_real share ///
     zlfn post treat_post ubica_geo factor smg upm est_dis ///
     state ent_name reg_num reg_name macro_num macro_name ///
     gas_total_real

reshape wide gas_real lngas_real share, ///
        i(folioviv foliohog year month) j(subgroup) string

*** Rename to project naming convention: gas_real<sub> → gas_<sub>_real, etc.
*** Stata's reshape produces `gas_real<subname>`; we want `gas_<subname>_real`
*** for readability. Loop over all subgroup names.
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
    if "`v'" == "share_female" continue  // legacy control, leave alone
    local sub = subinstr("`v'", "share", "", 1)
    rename `v' share_`sub'
}

*** Build the SUBGROUPS macro for downstream DiD loops. Derive from the
*** column names (more robust than re-reading the crosswalk here).
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

*** Zero-fill: subgroup-months with no claves get missing — convert to 0
*** since "no expenditure recorded" semantically = 0 spending.
foreach s of global SUBGROUPS {
    cap replace gas_`s'_real   = 0 if missing(gas_`s'_real)
    cap replace lngas_`s'_real = 0 if missing(lngas_`s'_real)
    cap replace share_`s'      = 0 if missing(share_`s')
}

*** Log of total HH expenditure (back-compat with total-only specs).
gen double lngas_total_real = ln(gas_total_real + 1)
label variable lngas_total_real "log(total real HH expenditure + 1)"

*************************************************
**** Merge HH composition + ethnicity ************
*************************************************

merge 1:1 folioviv foliohog year month using ///
    "../../data/clean/enigh/enigh-hhlevel-inc-month.dta", ///
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
