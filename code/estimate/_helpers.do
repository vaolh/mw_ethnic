*************************************************
**************** Helpers (estimate) *************
*************************************************

*** REPLICATION FILE: code/estimate/_helpers.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-07

*** Defines reusable Stata programs used across the DiD .do files in
*** code/estimate/.
***
***   savecoeffs namelist, cvar(<varname>) interaction_pattern(<token>) savedir(<path>)
***       — extracts every coefficient that contains <token> from each stored
***         estimate, saves to <savedir>/coefficients_<cvar>.{dta,csv}.
***
***   yearcoefs, EQname(<stored_est>)
***       — after `eststo <eqname>` of an `i.zlfn##ib2018.year` event-study
***         regression, builds matrices `__year_b` (1×5) and `__year_se` (1×5)
***         with positions [2016, 2018=0, 2020, 2022, 2024] in column order.
***         Coefplot can then plot the 2018 baseline as zero with no CI band.
***         Use as: coefplot matrix(__year_b), se(__year_se) ...
***
***   monthcoefs, EQname(<stored_est>)
***       — after `eststo <eqname>` of an `i.zlfn##ib21397.time` event-study
***         regression on the HH × month panel, builds matrices `__month_b`
***         (1×15) and `__month_se` (1×15) with positions for the 15 actual
***         ENIGH interview months (Aug/Sep/Oct of each of 5 waves), with
***         aug2018 (time=21397, position 4) set as the baseline = 0.
***         Necessary because `coefplot graph1, keep(...)` mis-positions
***         surviving coefficients sequentially at 1..N rather than at their
***         semantic time positions, dropping all post-2018 points into the
***         pre-2018 portion of the x-axis.
***         Use as: coefplot matrix(__month_b), se(__month_se), ...
***
*** Adapted from a Brazil-minwage `program.do` (savecoeffs + exporttable).
*** The original `exporttable` is intentionally NOT ported here: esttab's
*** native option strings (mtitle, refcat, coeflabel) do not pass cleanly
*** through a Stata wrapper without losing the embedded quoting that
*** controls per-column titles. The DiD scripts call esttab directly to
*** preserve readable, debuggable formatting.

*************************************************
**************** savecoeffs *********************
*************************************************

*************************************************
**************** yearcoefs **********************
*************************************************

cap prog drop yearcoefs
program define yearcoefs
    syntax, EQname(string)
    qui estimates restore `eqname'
    cap matrix drop __year_b
    cap matrix drop __year_se
    matrix __year_b  = J(1, 5, 0)
    matrix __year_se = J(1, 5, 0)
    matrix __year_b[1, 1]  = _b[1.zlfn#2016.year]
    matrix __year_b[1, 3]  = _b[1.zlfn#2020.year]
    matrix __year_b[1, 4]  = _b[1.zlfn#2022.year]
    matrix __year_b[1, 5]  = _b[1.zlfn#2024.year]
    matrix __year_se[1, 1] = _se[1.zlfn#2016.year]
    matrix __year_se[1, 3] = _se[1.zlfn#2020.year]
    matrix __year_se[1, 4] = _se[1.zlfn#2022.year]
    matrix __year_se[1, 5] = _se[1.zlfn#2024.year]
    matrix colnames __year_b  = c2016 c2018 c2020 c2022 c2024
    matrix colnames __year_se = c2016 c2018 c2020 c2022 c2024
end

*************************************************
**************** monthcoefs *********************
*************************************************

*** The 15 ENIGH interview months across 2016-2024 in Stata daily-date encoding
*** (days since 01jan1960). Each wave samples Aug/Sep/Oct (the ENIGH reference
*** quarter), so 5 waves × 3 months = 15 categories. aug2018 (time=21397) is
*** the omitted baseline.
***
***   Position | Date     | Stata time code | Wave
***   ---------+----------+-----------------+------
***          1 | aug2016  |          20667  | 2016
***          2 | sep2016  |          20698  | 2016
***          3 | oct2016  |          20728  | 2016
***          4 | aug2018  |          21397  | 2018 (BASELINE = 0)
***          5 | sep2018  |          21428  | 2018
***          6 | oct2018  |          21458  | 2018
***          7 | aug2020  |          22128  | 2020
***          8 | sep2020  |          22159  | 2020
***          9 | oct2020  |          22189  | 2020
***         10 | aug2022  |          22858  | 2022
***         11 | sep2022  |          22889  | 2022
***         12 | oct2022  |          22919  | 2022
***         13 | aug2024  |          23589  | 2024
***         14 | sep2024  |          23620  | 2024
***         15 | oct2024  |          23650  | 2024

cap prog drop monthcoefs
program define monthcoefs
    syntax, EQname(string)
    qui estimates restore `eqname'
    cap matrix drop __month_b
    cap matrix drop __month_se
    matrix __month_b  = J(1, 15, 0)
    matrix __month_se = J(1, 15, 0)
    matrix __month_b[1,  1] = _b[1.zlfn#20667.time]
    matrix __month_b[1,  2] = _b[1.zlfn#20698.time]
    matrix __month_b[1,  3] = _b[1.zlfn#20728.time]
    *** position 4 (aug2018 = 21397) is the baseline = 0 — leave at J() init
    matrix __month_b[1,  5] = _b[1.zlfn#21428.time]
    matrix __month_b[1,  6] = _b[1.zlfn#21458.time]
    matrix __month_b[1,  7] = _b[1.zlfn#22128.time]
    matrix __month_b[1,  8] = _b[1.zlfn#22159.time]
    matrix __month_b[1,  9] = _b[1.zlfn#22189.time]
    matrix __month_b[1, 10] = _b[1.zlfn#22858.time]
    matrix __month_b[1, 11] = _b[1.zlfn#22889.time]
    matrix __month_b[1, 12] = _b[1.zlfn#22919.time]
    matrix __month_b[1, 13] = _b[1.zlfn#23589.time]
    matrix __month_b[1, 14] = _b[1.zlfn#23620.time]
    matrix __month_b[1, 15] = _b[1.zlfn#23650.time]
    matrix __month_se[1,  1] = _se[1.zlfn#20667.time]
    matrix __month_se[1,  2] = _se[1.zlfn#20698.time]
    matrix __month_se[1,  3] = _se[1.zlfn#20728.time]
    matrix __month_se[1,  5] = _se[1.zlfn#21428.time]
    matrix __month_se[1,  6] = _se[1.zlfn#21458.time]
    matrix __month_se[1,  7] = _se[1.zlfn#22128.time]
    matrix __month_se[1,  8] = _se[1.zlfn#22159.time]
    matrix __month_se[1,  9] = _se[1.zlfn#22189.time]
    matrix __month_se[1, 10] = _se[1.zlfn#22858.time]
    matrix __month_se[1, 11] = _se[1.zlfn#22889.time]
    matrix __month_se[1, 12] = _se[1.zlfn#22919.time]
    matrix __month_se[1, 13] = _se[1.zlfn#23589.time]
    matrix __month_se[1, 14] = _se[1.zlfn#23620.time]
    matrix __month_se[1, 15] = _se[1.zlfn#23650.time]
    matrix colnames __month_b  = aug2016 sep2016 oct2016 aug2018 sep2018 oct2018 ///
                                  aug2020 sep2020 oct2020 aug2022 sep2022 oct2022 ///
                                  aug2024 sep2024 oct2024
    matrix colnames __month_se = aug2016 sep2016 oct2016 aug2018 sep2018 oct2018 ///
                                  aug2020 sep2020 oct2020 aug2022 sep2022 oct2022 ///
                                  aug2024 sep2024 oct2024
end

*************************************************
**************** savecoeffs *********************
*************************************************

cap prog drop savecoeffs
program define savecoeffs
    syntax namelist, CVARname(string) INTERACTION_pattern(string) SAVEdir(string)

    *** Get coefficient names from the first stored estimate.
    local first : word 1 of `namelist'
    qui estimates restore `first'
    local allcoefs : colnames e(b)

    *** Filter to those containing both the heterogeneity variable name and
    *** the interaction-pattern token (e.g. "1.zlfn#1.post").
    local intcoefs
    foreach c of local allcoefs {
        if strpos("`c'", "`interaction_pattern'") > 0 & strpos("`c'", "`cvarname'") > 0 {
            local intcoefs `intcoefs' `c'
        }
    }

    local ncoefs : word count `intcoefs'
    if `ncoefs' == 0 {
        di as error "savecoeffs: no interaction coefficients found for `cvarname' / `interaction_pattern'"
        exit 198
    }

    *** Capture value labels BEFORE preserve (preserve restores data state but
    *** value-label associations are lost across clear).
    forval i = 1/`ncoefs' {
        local thiscoef : word `i' of `intcoefs'
        local rc = regexm("`thiscoef'", "^([0-9]+)[bo]?\.")
        local numpart = regexs(1)
        local lbl`i' : label (`cvarname') `numpart'
        local id`i' = `numpart'
    }

    preserve
    clear
    qui set obs `ncoefs'
    gen str80 coefname           = ""
    gen int   `cvarname'_id      = .
    gen str80 `cvarname'_label   = ""

    forval i = 1/`ncoefs' {
        local thiscoef : word `i' of `intcoefs'
        qui replace coefname           = "`thiscoef'" in `i'
        qui replace `cvarname'_id      = `id`i''      in `i'
        qui replace `cvarname'_label   = "`lbl`i''"   in `i'
    }

    foreach estnm of local namelist {
        qui estimates restore `estnm'
        tempname bvec vvec
        matrix `bvec' = e(b)
        matrix `vvec' = vecdiag(e(V))

        gen double b_`estnm'  = .
        gen double se_`estnm' = .
        gen double N_`estnm'  = e(N)

        forval i = 1/`ncoefs' {
            local thiscoef : word `i' of `intcoefs'
            local colidx = colnumb(`bvec', "`thiscoef'")
            if `colidx' != . {
                qui replace b_`estnm'  = `bvec'[1, `colidx']        in `i'
                qui replace se_`estnm' = sqrt(`vvec'[1, `colidx']) in `i'
            }
        }
    }

    order `cvarname'_id `cvarname'_label
    drop coefname
    sort `cvarname'_id

    cap mkdir "`savedir'"
    qui save             "`savedir'/coefficients_`cvarname'.dta", replace
    qui export delimited "`savedir'/coefficients_`cvarname'.csv", replace
    di as text "savecoeffs: wrote coefficients_`cvarname'.{dta,csv} to `savedir'"
    restore
end
