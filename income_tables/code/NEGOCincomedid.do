*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: NEGOCincomedid
*** STATA VERSION: 18.5/SE
*** AUTHOR: Victor Alfonso Ortega Le Hénanff
*** EMAIL: vincictor33@gmail.com
*** DATE: 2025-05-05

*************************************************
**************** Load + Globals *****************
*************************************************

global project="/Users/victorortega/Dropbox/mw_ethnic/enighdata/output"
global output="/Users/victorortega/Dropbox/mw_ethnic/tables/output"

use "../input/enighdata.dta", clear
*use "$project/enighdata.dta", clear

*************************************************
******************* Table 1 *********************
*************************************************

*generate post treatment variable
gen post = year > 2018

*generate treatment
gen treat = 1 if lnn > 0 & (lnw == . | lnw == 0)
replace treat = 0 if treat == .

*control for observables
gl controls i.gender edad edadsq years_of_study hoursworked i.employed

*all units
reghdfe lni i.treat##i.post $controls , absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo t1m1
	 estadd local controls 	     "Y"
	 estadd local hastimefe 	 "Y"
	 estadd local hasmunicfe 	 "Y"
	 
*non indigenous
reghdfe lni i.treat##i.post $controls if indspeaker==0, absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo t1m2
	 estadd local controls 	     "Y"
	 estadd local hastimefe 	 "Y"
	 estadd local hasmunicfe 	 "Y"
reghdfe lni i.treat##i.post $controls if indigenous==0, absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo t1m3
	 estadd local controls 	     "Y"
	 estadd local hastimefe 	 "Y"
	 estadd local hasmunicfe 	 "Y"
	 
*indigenous
reghdfe lni i.treat##i.post $controls if indspeaker==1, absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo t1m4
	 estadd local controls 	     "Y"
	 estadd local hastimefe 	 "Y"
	 estadd local hasmunicfe 	 "Y"
reghdfe lni i.treat##i.post $controls if indigenous==1, absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo t1m5
	 estadd local controls 	     "Y"
	 estadd local hastimefe 	 "Y"
	 estadd local hasmunicfe 	 "Y"


esttab 	t1m1 t1m2 t1m3 t1m4 t1m5  ///
			using "../output/table5.tex",  replace label fragment ///
			nolines  posthead(\cmidrule{2-6}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
			nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)") collabels(none)    ///
			cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
			refcat(1.treat#1.post "ZLFN $\times$", nolabel) ///
			keep(1.treat#1.post)   ///
			coeflabel(1.treat#1.post "{2016-2022}") ///
			stats(N controls hastimefe hasmunicfe, ///
			fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Municipal FE")) onecell
