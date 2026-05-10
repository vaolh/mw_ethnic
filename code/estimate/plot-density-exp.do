*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: plot-density-exp.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-09

*** Kernel densities of log total HH monthly expenditure, by ZLFN × ethnicity,
*** separately for pre (year <= 2018) and post (year > 2018). Visual diagnostic
*** for distributional shifts beyond mean effects captured by DiD.
***
*** 4 figures: hli-pre, hli-post, indig-pre, indig-post (each compares ZLFN
*** vs non-ZLFN within the ethnicity subsample).

cap mkdir log
log using "log/plot-density-exp.log", replace text

include _helpers.do
do read-hhlevel-exp-month.do

cap mkdir "../../paper/figures/density"

*************************************************
**** HLI: pre-treatment kernel densities *********
*************************************************

twoway ///
    (kdensity lngas_total_real if zlfn == 0 & hli_hh == 0 & post == 0, lcolor("31 119 180") lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & hli_hh == 0 & post == 0, lcolor("31 119 180") lpattern(dash) lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 0 & hli_hh == 1 & post == 0, lcolor("214 39 40")  lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & hli_hh == 1 & post == 0, lcolor("214 39 40")  lpattern(dash) lwidth(medthick)), ///
    legend(order(1 "Non-ZLFN, non-HLI" 2 "ZLFN, non-HLI" 3 "Non-ZLFN, HLI" 4 "ZLFN, HLI") rows(2)) ///
    xtitle("log(HH monthly expenditure + 1)") ytitle("Density") ///
    graphregion(color(white)) bgcolor(white) ///
    ylabel(, grid glcolor(gs14) glwidth(thin))
graph export "../../paper/figures/density/plot-density-exp-hli-pre.png", replace width(4000) height(3000)

twoway ///
    (kdensity lngas_total_real if zlfn == 0 & hli_hh == 0 & post == 1, lcolor("31 119 180") lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & hli_hh == 0 & post == 1, lcolor("31 119 180") lpattern(dash) lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 0 & hli_hh == 1 & post == 1, lcolor("214 39 40")  lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & hli_hh == 1 & post == 1, lcolor("214 39 40")  lpattern(dash) lwidth(medthick)), ///
    legend(order(1 "Non-ZLFN, non-HLI" 2 "ZLFN, non-HLI" 3 "Non-ZLFN, HLI" 4 "ZLFN, HLI") rows(2)) ///
    xtitle("log(HH monthly expenditure + 1)") ytitle("Density") ///
    graphregion(color(white)) bgcolor(white) ///
    ylabel(, grid glcolor(gs14) glwidth(thin))
graph export "../../paper/figures/density/plot-density-exp-hli-post.png", replace width(4000) height(3000)

*************************************************
**** Indig: pre / post-treatment kernel ***********
*************************************************

twoway ///
    (kdensity lngas_total_real if zlfn == 0 & indig_hh == 0 & post == 0, lcolor("31 119 180") lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & indig_hh == 0 & post == 0, lcolor("31 119 180") lpattern(dash) lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 0 & indig_hh == 1 & post == 0, lcolor("214 39 40")  lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & indig_hh == 1 & post == 0, lcolor("214 39 40")  lpattern(dash) lwidth(medthick)), ///
    legend(order(1 "Non-ZLFN, non-Indig" 2 "ZLFN, non-Indig" 3 "Non-ZLFN, Indig" 4 "ZLFN, Indig") rows(2)) ///
    xtitle("log(HH monthly expenditure + 1)") ytitle("Density") ///
    graphregion(color(white)) bgcolor(white) ///
    ylabel(, grid glcolor(gs14) glwidth(thin))
graph export "../../paper/figures/density/plot-density-exp-indig-pre.png", replace width(4000) height(3000)

twoway ///
    (kdensity lngas_total_real if zlfn == 0 & indig_hh == 0 & post == 1, lcolor("31 119 180") lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & indig_hh == 0 & post == 1, lcolor("31 119 180") lpattern(dash) lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 0 & indig_hh == 1 & post == 1, lcolor("214 39 40")  lwidth(medthick)) ///
    (kdensity lngas_total_real if zlfn == 1 & indig_hh == 1 & post == 1, lcolor("214 39 40")  lpattern(dash) lwidth(medthick)), ///
    legend(order(1 "Non-ZLFN, non-Indig" 2 "ZLFN, non-Indig" 3 "Non-ZLFN, Indig" 4 "ZLFN, Indig") rows(2)) ///
    xtitle("log(HH monthly expenditure + 1)") ytitle("Density") ///
    graphregion(color(white)) bgcolor(white) ///
    ylabel(, grid glcolor(gs14) glwidth(thin))
graph export "../../paper/figures/density/plot-density-exp-indig-post.png", replace width(4000) height(3000)

cap log close
