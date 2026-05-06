*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-wage-informality
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-28

log using "log/did-wage-informality.log", replace text

*************************************************
*** DiD: Informality Decomposition **************
**** Panels: Formal / Informal / Self-Emp *******
**** Columns: All / NonInd-HLI / NonInd-Self ****
****          / Ind-HLI / Ind-Self **************
*************************************************

foreach ds in year month {

do read-indlevel-`ds'.do
estimates clear

*************************************************
************* Employment Categories *************
*************************************************

if "`ds'" == "month" {
    cap gen informal = (sar_afore1 != 1) if employed == 1
    cap gen indep = indep1
}

gen formal   = (informal == 0) if !missing(informal)
gen self_emp = (indep == 1)    if !missing(indep)

*************************************************
****** Panel A: Formal Employment Outcome *******
*************************************************

reghdfe formal i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo a1
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo a2
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo a3
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo a4
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo a5
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

*************************************************
****** Panel B: Informal Employment Outcome *****
*************************************************

reghdfe informal i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo b1
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo b2
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo b3
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo b4
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo b5
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

*************************************************
** Panel C: Self-Employment / Own-Account *******
*************************************************

reghdfe self_emp i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo d1
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo d2
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo d3
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo d4
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
     eststo d5
     estadd local controls "Y"
     estadd local hastimefe "Y"
     estadd local hasmunicfe "Y"

*************************************************
*************** Export TeX Table ****************
*************************************************

*** Panel A header + coefficients
esttab a1 a2 a3 a4 a5 ///
    using "../../paper/tables/did-wage-informality-`ds'.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) booktabs ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "\textit{Panel A: Formal Employment}", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024") ///
    noobs nomtitle

*** Panel B (append, no header)
esttab b1 b2 b3 b4 b5 ///
    using "../../paper/tables/did-wage-informality-`ds'.tex", append label fragment ///
    nolines booktabs ///
    nonumbers nomtitle collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "\textit{Panel B: Informal Employment}", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024") ///
    noobs

*** Panel C (append, with footer stats)
esttab d1 d2 d3 d4 d5 ///
    using "../../paper/tables/did-wage-informality-`ds'.tex", append label fragment ///
    nolines prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
    nonumbers nomtitle collabels(none) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    starlevels(* 0.10 ** 0.05 *** 0.01) ///
    refcat(1.zlfn#1.post "\textit{Panel C: Self-Employment}", nolabel) ///
    keep(1.zlfn#1.post) ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024") ///
    stats(N controls hastimefe hasmunicfe, ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

}

cap log close
