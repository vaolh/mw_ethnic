*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-wage-sector
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-28

log using "log/did-wage-sector.log", replace text

*************************************************
*** DiD: Sector Heterogeneity *******************
**** Panel A: Pre-treatment sector shares *******
**** Panel B: DiD by sector coverage ************
*************************************************

foreach ds in year month {

do read-indlevel-`ds'.do
estimates clear

*************************************************
************ Harmonize Sector Variable **********
*************************************************

*** Year data has `sector'; month data has `ind1'
if "`ds'" == "month" {
    cap gen sector = ind1
    cap gen informal = (sar_afore1 != 1) if employed == 1
}

*** Covered = sectors with MW enforcement
*** Uncovered = agriculture (11), other services incl. domestic (81),
***             unspecified workers (97, 99)
gen covered = !inlist(sector, 11, 81, 97, 99) if !missing(sector)

*************************************************
*** Panel A: Pre-treatment Sector Shares ********
*************************************************

*** Sector distribution before treatment (year <= 2018)
*** By ethnicity (indspeaker)

eststo share_all: quietly estpost tabulate sector if post == 0, nototal
eststo share_nonind: quietly estpost tabulate sector if post == 0 & indspeaker == 0, nototal
eststo share_ind: quietly estpost tabulate sector if post == 0 & indspeaker == 1, nototal

esttab share_all share_nonind share_ind ///
    using "../../paper/tables/sum-sector-shares-`ds'.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-4}) prefoot(\midrule) ///
    postfoot(\bottomrule \bottomrule) booktabs ///
    nonumbers mtitle("(1)" "(2)" "(3)") collabels(none) ///
    cells(pct(fmt(%9.1f))) ///
    stats(N, fmt(%11.0gc) label("Observations")) onecell

*************************************************
*** Panel B: DiD by Coverage × Ethnicity ********
**** Covered sectors ****************************
*************************************************

estimates clear

*** All — covered
reghdfe lnw i.zlfn##i.post $controls if covered == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c1
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Non-ind speakers — covered
reghdfe lnw i.zlfn##i.post $controls if covered == 1 & indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c2
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Non-ind self-id — covered
reghdfe lnw i.zlfn##i.post $controls if covered == 1 & etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c3
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Ind speakers — covered
reghdfe lnw i.zlfn##i.post $controls if covered == 1 & indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c4
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Ind self-id — covered
reghdfe lnw i.zlfn##i.post $controls if covered == 1 & etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo c5
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*************************************************
*** DiD by Coverage × Ethnicity *****************
**** Uncovered sectors **************************
*************************************************

*** All — uncovered
reghdfe lnw i.zlfn##i.post $controls if covered == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo u1
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Non-ind speakers — uncovered
reghdfe lnw i.zlfn##i.post $controls if covered == 0 & indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo u2
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Non-ind self-id — uncovered
reghdfe lnw i.zlfn##i.post $controls if covered == 0 & etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo u3
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Ind speakers — uncovered
reghdfe lnw i.zlfn##i.post $controls if covered == 0 & indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo u4
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*** Ind self-id — uncovered
reghdfe lnw i.zlfn##i.post $controls if covered == 0 & etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo u5
     estadd local controls   "Y"
     estadd local hastimefe  "Y"
     estadd local hasmunicfe "Y"

*************************************************
*************** Export TeX Table ****************
*************************************************

*** Panel A: Covered sectors
esttab c1 c2 c3 c4 c5 ///
    using "../../paper/tables/did-wage-sector-`ds'.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) booktabs ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "\textit{Panel A: Covered Sectors}", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024") ///
    noobs nomtitle

*** Panel B: Uncovered sectors (append)
esttab u1 u2 u3 u4 u5 ///
    using "../../paper/tables/did-wage-sector-`ds'.tex", append label fragment ///
    nolines prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
    nonumbers nomtitle collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "\textit{Panel B: Uncovered Sectors}", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024") ///
    stats(N controls hastimefe hasmunicfe, ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

}

cap log close
