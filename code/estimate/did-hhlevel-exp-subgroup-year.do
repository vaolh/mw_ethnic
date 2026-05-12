*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-hhlevel-exp-subgroup-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-10

*** Subgroup-level DiD on the ENIGH HH-year expenditure panel.
***
*** For each of the ~62 substantive subgroups (excluding cap 17 financial
*** capital and cap 18 uncategorized), runs five stratified regressions:
***
***   col 1: All HH
***   col 2: hli_hh == 0  (no HLI-speaking members)
***   col 3: indig_hh == 0 (no self-identified indigenous members)
***   col 4: hli_hh == 1  (≥1 HLI-speaking member)
***   col 5: indig_hh == 1 (≥1 self-identified indigenous member)
***
*** Two outcomes per subgroup:
***   lngas_<sub>_real  — log(real expenditure + 1) on this subgroup
***   share_<sub>       — share of total HH expenditure on this subgroup
***
*** Specification (matches did-indlevel-inc-year, did-exp-hhlevel-month):
***   reghdfe y i.zlfn##i.post $controls, absorb(ubica_geo year) ///
***       vce(cluster ubica_geo)
***
*** Outputs one table per (capitulo × outcome):
***   paper/tables/did-exp-subgroup-<cap>-<outcome>-year.tex
***
*** Reads: read-hhlevel-exp-subgroup-year.do (sets $SUBGROUPS, $controls).

cap mkdir log
log using "log/did-hhlevel-exp-subgroup-year.log", replace text

include _helpers.do
do read-hhlevel-exp-subgroup-year.do

local TABDIR ../../paper/tables/

*************************************************
**** Subgroup → capitulo lookup ******************
*************************************************

*** Load crosswalk (dedup by subgroup) so we can group output tables by
*** capitulo. Loaded as a local-only frame; main dataset is unaffected.
preserve
use "../../data/clean/enigh/clave_crosswalk.dta", clear
duplicates drop subgroup, force
keep subgroup capitulo_norm subgroup_label
tempfile sub_meta
save `sub_meta', replace
restore

*** Build per-capitulo subgroup lists.
preserve
use `sub_meta', clear
levelsof capitulo_norm, local(CAPS) clean
foreach c of local CAPS {
    levelsof subgroup if capitulo_norm == "`c'", local(SUB_`c') clean
}
restore

*************************************************
**** DiD loop ************************************
*************************************************

estimates clear

*** Helper: run 5 stratified regressions for (outcome y, subgroup s) and
*** store as `_<y>_<s>_{all,hli0,ind0,hli1,ind1}`.
local OUTCOMES lngas share

foreach c of local CAPS {
    *** Skip capitulos excluded from the substantive analysis.
    if "`c'" == "17" | "`c'" == "18" continue

    *** Skip capitulos with no subgroups loaded into memory (defensive).
    local CSUBS `"`SUB_`c''"'
    local k = wordcount("`CSUBS'")
    if `k' == 0 continue

    foreach y of local OUTCOMES {
        estimates clear
        local cols ""
        local titles ""

        foreach s of local CSUBS {
            local yvar = cond("`y'" == "lngas", "lngas_`s'_real", "share_`s'")
            cap confirm variable `yvar'
            if _rc {
                di as text "skipping `yvar' (not in memory)"
                continue
            }

            *** All HH
            reghdfe `yvar' i.zlfn##i.post $controls, ///
                absorb(ubica_geo year) vce(cluster ubica_geo)
            eststo `s'_all
            estadd local stratum "All"

            *** hli_hh == 0
            reghdfe `yvar' i.zlfn##i.post $controls if hli_hh == 0, ///
                absorb(ubica_geo year) vce(cluster ubica_geo)
            eststo `s'_hli0
            estadd local stratum "HLI=0"

            *** indig_hh == 0
            reghdfe `yvar' i.zlfn##i.post $controls if indig_hh == 0, ///
                absorb(ubica_geo year) vce(cluster ubica_geo)
            eststo `s'_ind0
            estadd local stratum "Indig=0"

            *** hli_hh == 1
            cap reghdfe `yvar' i.zlfn##i.post $controls if hli_hh == 1, ///
                absorb(ubica_geo year) vce(cluster ubica_geo)
            if !_rc {
                eststo `s'_hli1
                estadd local stratum "HLI=1"
            }

            *** indig_hh == 1
            cap reghdfe `yvar' i.zlfn##i.post $controls if indig_hh == 1, ///
                absorb(ubica_geo year) vce(cluster ubica_geo)
            if !_rc {
                eststo `s'_ind1
                estadd local stratum "Indig=1"
            }

            local cols `cols' `s'_all `s'_hli0 `s'_ind0 `s'_hli1 `s'_ind1
            local titles `"`titles' "`s'"  "`s'"  "`s'"  "`s'"  "`s'""'
        }

        *** Skip if no models produced.
        if "`cols'" == "" continue

        *** Build mgroups labels (one per subgroup) and the per-col mtitles
        *** (5 strata × k subgroups). mgroups spans 5 cols with subgroup name.
        local mg_labels ""
        local mg_pattern ""
        local col_mtitles ""
        foreach s of local CSUBS {
            local yvar = cond("`y'" == "lngas", "lngas_`s'_real", "share_`s'")
            cap confirm variable `yvar'
            if _rc continue
            local mg_labels `"`mg_labels' "`s'""'
            local mg_pattern `mg_pattern' 1 0 0 0 0
            local col_mtitles `"`col_mtitles' "All" "HLI=0" "Indig=0" "HLI=1" "Indig=1""'
        }

        esttab `cols' using "`TABDIR'did-exp-subgroup-`c'-`y'-year.tex", ///
            replace label fragment booktabs ///
            mgroups(`mg_labels', pattern(`mg_pattern') ///
                    prefix(\multicolumn{5}{c}{) suffix(}) span ///
                    erepeat(\cmidrule(lr){@span})) ///
            mtitles(`col_mtitles') ///
            nonumbers collabels(none) ///
            cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
            starlevels(* 0.10 ** 0.05 *** 0.01) ///
            keep(1.zlfn#1.post) ///
            coeflabel(1.zlfn#1.post "ZLFN $\times$ Post") ///
            stats(N, fmt(%11.0gc) label("Observations")) ///
            nogaps
        di as text "wrote did-exp-subgroup-`c'-`y'-year.tex (cap=`c', outcome=`y', subgroups=`k')"
    }
}

cap log close
