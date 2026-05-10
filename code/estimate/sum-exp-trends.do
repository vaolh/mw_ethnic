*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: sum-exp-trends.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-09

*** Pre-DiD diagnostic: mean log total HH monthly expenditure by year, split
*** by ZLFN treatment × ethnicity. One panel per ethnicity dimension (HLI head
*** based / self-identified indigenous). Each panel has 4 lines: ZLFN×ind,
*** ZLFN×nonind, nonZLFN×ind, nonZLFN×nonind.
***
*** Visual parallel-trends check: pre-2019 lines should track each other.

cap mkdir log
log using "log/sum-exp-trends.log", replace text

include _helpers.do
do read-hhlevel-exp-month.do

cap mkdir "../../paper/figures/sum"

*************************************************
**** Collapse to (year × zlfn × ethnicity) means *
*************************************************

preserve
collapse (mean) lngas_total_real, by(year zlfn hli_hh)
gen byte cat = .
replace cat = 1 if zlfn == 0 & hli_hh == 0
replace cat = 2 if zlfn == 0 & hli_hh == 1
replace cat = 3 if zlfn == 1 & hli_hh == 0
replace cat = 4 if zlfn == 1 & hli_hh == 1
label define cat 1 "Non-ZLFN, non-HLI" 2 "Non-ZLFN, HLI" 3 "ZLFN, non-HLI" 4 "ZLFN, HLI"
label values cat cat

twoway ///
    (connected lngas_total_real year if cat == 1, lcolor("31 119 180")  mcolor("31 119 180")  msymbol(circle)) ///
    (connected lngas_total_real year if cat == 2, lcolor("214 39 40")   mcolor("214 39 40")   msymbol(circle)) ///
    (connected lngas_total_real year if cat == 3, lcolor("31 119 180")  mcolor("31 119 180")  msymbol(triangle) lpattern(dash)) ///
    (connected lngas_total_real year if cat == 4, lcolor("214 39 40")   mcolor("214 39 40")   msymbol(triangle) lpattern(dash)), ///
    xline(2019, lcolor(gs10) lpattern(dash)) ///
    legend(order(1 "Non-ZLFN, non-HLI" 2 "Non-ZLFN, HLI" 3 "ZLFN, non-HLI" 4 "ZLFN, HLI") rows(2)) ///
    ytitle("Mean log(HH expenditure + 1)") xtitle("Year") ///
    xlabel(2016 2018 2020 2022 2024) ///
    graphregion(color(white)) bgcolor(white) ///
    ylabel(, grid glcolor(gs14) glwidth(thin))
graph export "../../paper/figures/sum/sum-exp-trends-hli.png", replace width(4000) height(3000)
restore

preserve
collapse (mean) lngas_total_real, by(year zlfn indig_hh)
gen byte cat = .
replace cat = 1 if zlfn == 0 & indig_hh == 0
replace cat = 2 if zlfn == 0 & indig_hh == 1
replace cat = 3 if zlfn == 1 & indig_hh == 0
replace cat = 4 if zlfn == 1 & indig_hh == 1
label define cat 1 "Non-ZLFN, non-Indig" 2 "Non-ZLFN, Indig" 3 "ZLFN, non-Indig" 4 "ZLFN, Indig"
label values cat cat

twoway ///
    (connected lngas_total_real year if cat == 1, lcolor("31 119 180")  mcolor("31 119 180")  msymbol(circle)) ///
    (connected lngas_total_real year if cat == 2, lcolor("214 39 40")   mcolor("214 39 40")   msymbol(circle)) ///
    (connected lngas_total_real year if cat == 3, lcolor("31 119 180")  mcolor("31 119 180")  msymbol(triangle) lpattern(dash)) ///
    (connected lngas_total_real year if cat == 4, lcolor("214 39 40")   mcolor("214 39 40")   msymbol(triangle) lpattern(dash)), ///
    xline(2019, lcolor(gs10) lpattern(dash)) ///
    legend(order(1 "Non-ZLFN, non-Indig" 2 "Non-ZLFN, Indig" 3 "ZLFN, non-Indig" 4 "ZLFN, Indig") rows(2)) ///
    ytitle("Mean log(HH expenditure + 1)") xtitle("Year") ///
    xlabel(2016 2018 2020 2022 2024) ///
    graphregion(color(white)) bgcolor(white) ///
    ylabel(, grid glcolor(gs14) glwidth(thin))
graph export "../../paper/figures/sum/sum-exp-trends-indig.png", replace width(4000) height(3000)
restore

cap log close
