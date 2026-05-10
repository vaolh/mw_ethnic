*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-hhlevel-exp-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-07

*** Household-level monthly TOTAL expenditure DiD by ethnicity stratum.
***
*** Outcome:        lngas_total_real = ln(real HH monthly expenditure + 1)
*** Treatment:      i.zlfn##i.post
*** FE:             ubica_geo + year (matches did-wage-* spec)
*** Cluster:        ubica_geo
*** Strata (cols):  All / hli_hh==0 / indig_hh==0 / hli_hh==1 / indig_hh==1
***
*** Output: paper/tables/did-exp-hhlevel-month.tex
***
*** Reads:  read-hhlevel-exp-month.do (loads + collapses to HH×month total +
***         merges ethnicity from hhlevel-inc-month)

cap mkdir log
log using "log/did-hhlevel-exp-month.log", replace text

include _helpers.do
do read-hhlevel-exp-month.do

local TABDIR ../../paper/tables/

*************************************************
**** DiD: log total HH expenditure ****************
*************************************************

estimates clear

reghdfe lngas_total_real i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo e1
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lngas_total_real i.zlfn##i.post $controls if hli_hh == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo e2
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lngas_total_real i.zlfn##i.post $controls if indig_hh == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo e3
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lngas_total_real i.zlfn##i.post $controls if hli_hh == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo e4
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lngas_total_real i.zlfn##i.post $controls if indig_hh == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo e5
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

esttab e1 e2 e3 e4 e5 using "`TABDIR'did-exp-hhlevel-month.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) prefoot(\midrule)                                  ///
    postfoot(\bottomrule \bottomrule) booktabs                                          ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none)                     ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                                        ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                                 ///
    refcat(1.zlfn#1.post "ZLFN $\times$", nolabel)                                      ///
    keep(1.zlfn#1.post)                                                                 ///
    coeflabel(1.zlfn#1.post "{2016--2024}")                                             ///
    stats(N controls hastimefe hasmunicfe,                                              ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

cap log close
