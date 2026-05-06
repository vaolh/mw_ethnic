*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-wage-deciles
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-28

log using "log/did-wage-deciles.log", replace text

*************************************************
***** DiD: Wages by Income Decile ***************
**** Columns: All / Formal / Informal ***********
**** Rows:    Deciles 1–9 (10 = baseline) *******
*************************************************

foreach ds in year month {

do read-indlevel-`ds'.do
estimates clear

*************************************************
************* Informality Proxy *****************
*************************************************

*** Year data: informal comes from CONEVAL merge (pea==1 & ss_dir==0)
*** Month data: proxy via absence of SAR/AFORE on main job

if "`ds'" == "month" {
    cap gen informal = (sar_afore1 != 1) if employed == 1
}

*************************************************
************** Income Deciles *******************
*************************************************

xtile decile = lni, nq(10)

*************************************************
*** Build keeplist and coeflabels from loop ****
*************************************************

local keeplist ""
local coeflabels ""
forvalues d = 1/9 {
    local keeplist   `keeplist'   `d'.decile#1.zlfn#1.post
    local coeflabels `coeflabels' `d'.decile#1.zlfn#1.post "\hspace{1em}Decile `d'"
}

*************************************************
******* Regressions: All Workers ****************
*************************************************

reghdfe lnw ibn.decile##(i.zlfn##i.post) $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c1
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*************************************************
****** Regressions: Formal Workers **************
*************************************************

reghdfe lnw ibn.decile##(i.zlfn##i.post) $controls if informal == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c2
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*************************************************
****** Regressions: Informal Workers ************
*************************************************

reghdfe lnw ibn.decile##(i.zlfn##i.post) $controls if informal == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c3
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*************************************************
*************** Export TeX Table ****************
*************************************************

esttab c1 c2 c3 ///
    using "../../paper/tables/did-wage-deciles-`ds'.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-4}) prefoot(\midrule) ///
    postfoot(\bottomrule \bottomrule) booktabs ///
    nonumbers mtitle("(1)" "(2)" "(3)") collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.decile#1.zlfn#1.post "ZLFN $\times$ 2016--2024", nolabel) ///
    keep(`keeplist') ///
    coeflabel(`coeflabels') ///
    stats(N controls hastimefe hasmunicfe, ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

}

cap log close
