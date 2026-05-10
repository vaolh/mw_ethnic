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
