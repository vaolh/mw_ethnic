*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: did-indlevel-inc-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-06

*** All individual-level MONTH-cadence DiD tables in one file.
*** Output (in paper/tables/):
***   1. did-wage-ethnic-month.tex          (Topic A: log wages by ethnicity)
***   2. did-income-ethnic-month.tex        (Topic B: log income by ethnicity)
***   3. did-wage-deciles-month.tex         (Topic C: heterogeneity by income decile × formal/informal)
***   4. did-wage-informality-month.tex     (Topic D: formal / informal / self-emp shares)
***   5. did-wage-sector-month.tex          (Topic E: covered / uncovered sectors)
***   6. sum-sector-shares-month.tex        (Topic E supplement: pre-treat sector distribution)
***
*** Reads: code/estimate/read-indlevel-inc-month.do
***        code/estimate/_helpers.do (exporttable)
***
*** Differences from year cadence:
***   - Month dataset has `ind1` (sector code) and `sar_afore1` (proxy for SS dir)
***     instead of `sector` and CONEVAL-derived `informal`. Synthesized below.

cap mkdir log
log using "log/did-indlevel-inc-month.log", replace text

include _helpers.do
do read-indlevel-inc-month.do

local TABDIR ../../paper/tables/

*** Synthesize variables that are CONEVAL-derived in the year file but proxied here.
cap gen informal = (sar_afore1 != 1) if employed == 1
cap gen indep    = indep1
cap gen sector   = ind1

*************************************************
**** Topic A: Wages by Ethnicity *****************
*************************************************

estimates clear

reghdfe lnw i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a1
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a2
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a3
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a4
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo a5
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

esttab a1 a2 a3 a4 a5 using "`TABDIR'did-wage-ethnic-month.tex", replace label fragment ///
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
**** Topic B: Income by Ethnicity ****************
*************************************************

estimates clear

reghdfe lni i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b1
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b2
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b3
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b4
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lni i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo b5
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

esttab b1 b2 b3 b4 b5 using "`TABDIR'did-income-ethnic-month.tex", replace label fragment ///
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

*************************************************
**** Topic C: Wage Deciles ***********************
*************************************************

estimates clear

xtile decile = lni, nq(10)

local keeplist ""
local coeflabels ""
forvalues d = 1/9 {
    local keeplist   `keeplist'   `d'.decile#1.zlfn#1.post
    local coeflabels `coeflabels' `d'.decile#1.zlfn#1.post "\hspace{1em}Decile `d'"
}

reghdfe lnw ibn.decile##(i.zlfn##i.post) $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo c1
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw ibn.decile##(i.zlfn##i.post) $controls if informal == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo c2
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

reghdfe lnw ibn.decile##(i.zlfn##i.post) $controls if informal == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo c3
    estadd local controls    "Y"
    estadd local hastimefe   "Y"
    estadd local hasmunicfe  "Y"

esttab c1 c2 c3 using "`TABDIR'did-wage-deciles-month.tex", replace label fragment ///
    nolines posthead(\cmidrule{2-4}) prefoot(\midrule)                              ///
    postfoot(\bottomrule \bottomrule) booktabs                                      ///
    nonumbers mtitle("(1)" "(2)" "(3)") collabels(none)                             ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                                    ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                             ///
    refcat(1.decile#1.zlfn#1.post "ZLFN $\times$ 2016--2024", nolabel)              ///
    keep(`keeplist')                                                                 ///
    coeflabel(`coeflabels')                                                          ///
    stats(N controls hastimefe hasmunicfe,                                          ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

*************************************************
**** Topic D: Informality decomposition **********
*************************************************

estimates clear

cap gen formal   = (informal == 0) if !missing(informal)
cap gen self_emp = (indep    == 1) if !missing(indep)

reghdfe formal i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paA1
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paA2
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paA3
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paA4
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe formal i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paA5
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paB1
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paB2
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paB3
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paB4
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe informal i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paB5
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paC1
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paC2
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paC3
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paC4
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe self_emp i.zlfn##i.post $controls if etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo paC5
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

local INFOROUT "`TABDIR'did-wage-informality-month.tex"

esttab paA1 paA2 paA3 paA4 paA5 using "`INFOROUT'", replace label fragment ///
    nolines posthead(\cmidrule{2-6}) booktabs                              ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none)        ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                           ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                    ///
    refcat(1.zlfn#1.post "\textit{Panel A: Formal Employment}", nolabel)   ///
    keep(1.zlfn#1.post)                                                    ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024")        ///
    noobs

esttab paB1 paB2 paB3 paB4 paB5 using "`INFOROUT'", append label fragment   ///
    nolines booktabs                                                          ///
    nonumbers nomtitle collabels(none)                                        ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                              ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                       ///
    refcat(1.zlfn#1.post "\textit{Panel B: Informal Employment}", nolabel)    ///
    keep(1.zlfn#1.post)                                                       ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024")           ///
    noobs

esttab paC1 paC2 paC3 paC4 paC5 using "`INFOROUT'", append label fragment   ///
    nolines prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs       ///
    nonumbers nomtitle collabels(none)                                          ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                                ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                         ///
    refcat(1.zlfn#1.post "\textit{Panel C: Self-Employment}", nolabel)          ///
    keep(1.zlfn#1.post)                                                         ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024")             ///
    stats(N controls hastimefe hasmunicfe,                                      ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

*************************************************
**** Topic E: Sector covered / uncovered *********
*************************************************

estimates clear

gen covered = !inlist(sector, 11, 81, 97, 99) if !missing(sector)

eststo share_all:    quietly estpost tabulate sector if post == 0,                   nototal
eststo share_nonind: quietly estpost tabulate sector if post == 0 & indspeaker == 0, nototal
eststo share_ind:    quietly estpost tabulate sector if post == 0 & indspeaker == 1, nototal

esttab share_all share_nonind share_ind using "`TABDIR'sum-sector-shares-month.tex", ///
    replace label fragment                                                            ///
    nolines posthead(\cmidrule{2-4}) prefoot(\midrule)                                ///
    postfoot(\bottomrule \bottomrule) booktabs                                        ///
    nonumbers mtitle("(1)" "(2)" "(3)") collabels(none)                               ///
    cells(pct(fmt(%9.1f)))                                                            ///
    stats(N, fmt(%11.0gc) label("Observations")) onecell

estimates clear

reghdfe lnw i.zlfn##i.post $controls if covered == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo cov1
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 1 & indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo cov2
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 1 & etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo cov3
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 1 & indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo cov4
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 1 & etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo cov5
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo unc1
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 0 & indspeaker == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo unc2
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 0 & etnia == 0, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo unc3
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 0 & indspeaker == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo unc4
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

reghdfe lnw i.zlfn##i.post $controls if covered == 0 & etnia == 1, absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo unc5
    estadd local controls "Y"
    estadd local hastimefe "Y"
    estadd local hasmunicfe "Y"

local SECTOROUT "`TABDIR'did-wage-sector-month.tex"

esttab cov1 cov2 cov3 cov4 cov5 using "`SECTOROUT'", replace label fragment   ///
    nolines posthead(\cmidrule{2-6}) booktabs                                  ///
    nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none)            ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                               ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                        ///
    refcat(1.zlfn#1.post "\textit{Panel A: Covered Sectors}", nolabel)         ///
    keep(1.zlfn#1.post)                                                        ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024")            ///
    noobs

esttab unc1 unc2 unc3 unc4 unc5 using "`SECTOROUT'", append label fragment    ///
    nolines prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs        ///
    nonumbers nomtitle collabels(none)                                          ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))                                ///
    starlevels(* 0.10 ** 0.05 *** 0.01)                                         ///
    refcat(1.zlfn#1.post "\textit{Panel B: Uncovered Sectors}", nolabel)         ///
    keep(1.zlfn#1.post)                                                          ///
    coeflabel(1.zlfn#1.post "\hspace{1em}ZLFN $\times$ 2016--2024")              ///
    stats(N controls hastimefe hasmunicfe,                                       ///
        fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell

cap log close
