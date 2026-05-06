*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-income-ethnic
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-23

log using "log/did-income-ethnic.log", replace text

*************************************************
***** DiD: Income by Indigenous Status **********
*************************************************

foreach ds in year month {

do read-indlevel-`ds'.do
estimates clear

* All units
reghdfe lni i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo t1m1
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

* Non-indigenous speakers
reghdfe lni i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo t1m2
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

* Non-indigenous (self-identified)
reghdfe lni i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo t1m3
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

* Indigenous speakers
reghdfe lni i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo t1m4
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

* Indigenous (self-identified)
reghdfe lni i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo t1m5
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*************************************************
*************** Export TeX Table ****************
*************************************************

esttab t1m1 t1m2 t1m3 t1m4 t1m5 ///
    using "../../paper/tables/did-income-ethnic-`ds'.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) prefoot(\midrule) ///
    postfoot(\bottomrule \bottomrule) booktabs ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "ZLFN $\times$", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "{2016-2024}") ///
    stats(N controls hastimefe hasmunicfe, ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell
}

cap log close
