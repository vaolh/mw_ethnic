*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-hhlevel-inc-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-07

*** Household-level YEAR DiDs.
*** Output (in paper/tables/):
***   1. did-wage-hhlevel-year.tex     (Topic A: log HH wages × head ethnicity)
***   2. did-income-hhlevel-year.tex   (Topic B: log HH income × head ethnicity)
***
*** Reads: read-hhlevel-inc-year.do (sets $controls + hli_hh / indig_hh)

cap mkdir log
log using "log/did-hhlevel-inc-year.log", replace text

include _helpers.do
do read-hhlevel-inc-year.do

local TABDIR ../../paper/tables/

*************************************************
**** Topic A: HH log wages by head ethnicity ****
*************************************************

estimates clear

reghdfe lnw i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a1
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if hli_hh == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a2
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if indig_hh == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a3
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if hli_hh == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a4
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if indig_hh == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a5
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

esttab a1 a2 a3 a4 a5 using "`TABDIR'did-wage-hhlevel-year.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) prefoot(\midrule)                                   ///
    postfoot(\bottomrule \bottomrule) booktabs                                           ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none)                      ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                                         ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                                  ///
    refcat(1.zlfn#1.post "ZLFN $\times$", nolabel)                                       ///
    keep(1.zlfn#1.post)                                                                  ///
    coeflabel(1.zlfn#1.post "{2016--2024}")                                              ///
    stats(N controls hastimefe hasmunicfe,                                               ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

*************************************************
**** Topic B: HH log income by head ethnicity ***
*************************************************

estimates clear

reghdfe lni i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b1
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if hli_hh == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b2
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if indig_hh == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b3
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if hli_hh == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b4
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if indig_hh == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b5
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

esttab b1 b2 b3 b4 b5 using "`TABDIR'did-income-hhlevel-year.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) prefoot(\midrule)                                     ///
    postfoot(\bottomrule \bottomrule) booktabs                                             ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none)                        ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                                           ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                                    ///
    refcat(1.zlfn#1.post "ZLFN $\times$", nolabel)                                         ///
    keep(1.zlfn#1.post)                                                                    ///
    coeflabel(1.zlfn#1.post "{2016--2024}")                                                ///
    stats(N controls hastimefe hasmunicfe,                                                 ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

cap log close
