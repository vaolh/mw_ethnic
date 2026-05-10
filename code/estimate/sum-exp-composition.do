*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: sum-exp-composition.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-10

*** Pre-DiD diagnostic: stacked horizontal bar of HH expenditure shares by
*** gasto_group (project-defined semantic grouping ~14 categories: food /
*** durables / services / housing / transport / health / restaurants / etc.),
*** restricted to post-treatment data and broken out by HLI × ZLFN.
***
*** WHY POST-ONLY: the build's classify_gasto_clave function maps standard
*** COICOP 2-digit codes (01..18) to named groups. The 2024 ENIGH wave uses
*** these codes; pre-2024 waves use different codes (A0/A1/A2/B0) that all
*** fall into "other". Until the build harmonizes pre-2024 codes (Phase 3),
*** the composition shift across pre/post cannot be plotted — we instead
*** show the post-treatment composition by ethnicity × ZLFN cell.
***
*** Each row of the input = (HH × clave × month). We sum gas_real by
*** gasto_group within each ethnicity × ZLFN cell, then convert to shares.

cap mkdir log
log using "log/sum-exp-composition.log", replace text

include _helpers.do

cap mkdir "../../paper/figures/sum"

*************************************************
**** Load + filter to post + named groups ********
*************************************************

use "../../data/clean/enigh/enigh-hhlevel-exp-month.dta", clear
keep if post == 1
drop if missing(gasto_group) | gasto_group == "other"

*** Merge in HH ethnicity from inc-month panel.
merge m:1 folioviv foliohog year month using ///
    "../../data/clean/enigh/enigh-hhlevel-inc-month.dta", ///
    keepusing(n_indigenous n_hli) keep(match) nogen
gen byte hli_hh   = (n_hli         > 0) if !missing(n_hli)
gen byte indig_hh = (n_indigenous  > 0) if !missing(n_indigenous)

*** Define cell index (1-4): hli × zlfn (post only).
gen byte cell = .
replace cell = 1 if hli_hh == 0 & zlfn == 0
replace cell = 2 if hli_hh == 0 & zlfn == 1
replace cell = 3 if hli_hh == 1 & zlfn == 0
replace cell = 4 if hli_hh == 1 & zlfn == 1
label define cell ///
    1 "non-HLI / non-ZLFN"  2 "non-HLI / ZLFN" ///
    3 "HLI / non-ZLFN"      4 "HLI / ZLFN"
label values cell cell

drop if missing(cell)

*************************************************
**** Compute (gasto_group, cell) shares **********
*************************************************

*** Total within each cell (denominator for share).
preserve
collapse (sum) gas_real, by(cell)
rename gas_real gas_total_cell
tempfile totals
save `totals'
restore

*** Sum spending by (gasto_group × cell), then divide by cell totals.
collapse (sum) gas_real, by(gasto_group cell)
merge m:1 cell using `totals', nogen
gen double share = gas_real / gas_total_cell

*************************************************
**** Reshape wide and stacked horizontal bar *****
*************************************************

drop gas_real gas_total_cell
reshape wide share, i(cell) j(gasto_group) string

*** All `share*` columns now: each row = a cell, each column = a gasto_group.
ds share*
local sharevars `r(varlist)'

graph hbar (asis) `sharevars', ///
    over(cell, label(labsize(small))) ///
    stack ///
    blabel(none) ///
    ytitle("Share of categorized HH expenditure (post-2018)", size(small)) ///
    legend(rows(3) size(small) symxsize(*0.6) symysize(*0.6) ///
        label(1 "alcohol-tobac") label(2 "clothing") label(3 "comms") ///
        label(4 "durables") label(5 "education") label(6 "financial") ///
        label(7 "food") label(8 "health") label(9 "housing") ///
        label(10 "personal") label(11 "recreation") label(12 "restaurants") ///
        label(13 "services") label(14 "transport")) ///
    graphregion(color(white)) bgcolor(white)
graph export "../../paper/figures/sum/sum-exp-composition.png", replace width(4000) height(3000)

cap log close
