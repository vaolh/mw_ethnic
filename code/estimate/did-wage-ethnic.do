*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-wage-ethnic
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-28

log using "log/did-wage-ethnic.log", replace text

*************************************************
******* DiD: Wages by Ethnicity *****************
**** Col 1:   All (controls) ********************
**** Col 2-3: Non-Ind (HLI / self-id) ***********
**** Col 4-5: Ind (HLI / self-id) ***************
*************************************************

foreach ds in year month {

do read-indlevel-`ds'.do
estimates clear

*** All — with controls
reghdfe lnw i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c1
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*** Non-indigenous speakers — with controls
reghdfe lnw i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c2
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*** Non-indigenous (self-identified) — with controls
reghdfe lnw i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c3
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*** Indigenous speakers — with controls
reghdfe lnw i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c4
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*** Indigenous (self-identified) — with controls
reghdfe lnw i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c5
     estadd local controls       "Y"
     estadd local hastimefe      "Y"
     estadd local hasmunicfe     "Y"

*************************************************
*************** Export TeX Table ****************
*************************************************

esttab c1 c2 c3 c4 c5 ///
    using "../../paper/tables/did-wage-ethnic-`ds'.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) prefoot(\midrule) ///
    postfoot(\bottomrule \bottomrule) booktabs ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "ZLFN $\times$", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "{2016--2024}") ///
    stats(N controls hastimefe hasmunicfe, ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

}

cap log close
