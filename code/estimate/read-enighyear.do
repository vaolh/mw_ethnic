*************************************************
*************** Read ENIGH Year *****************
*************************************************

*** REPLICATION FILE: read-enighyear.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-03

*** Loads the yearly cross-section produced by code/build/enigh-year.do
*** (or code/build/enigh-year.R — both write to the same path with the
*** same canonical variable contract).
***
*** Canonical names (zlfn, post, etnia, indspeaker, indund, female,
*** edad_pob, edadsq, ing_*, ing_*_nom, lnw, lni, …) are produced directly
*** by the build script — no renames needed and no log re-derivation.
*** Sets the global $controls used by every analysis script.

*************************************************
**************** Install packages ****************
*************************************************

cap which estout
if _rc {
    ssc install estout, replace
}
cap which coefplot
if _rc {
    ssc install coefplot, replace
}
cap which reghdfe
if _rc {
    net install ftools,  replace from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")
    net install reghdfe, replace from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")
}

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-year.dta", clear

*************************************************
***************** Controls **********************
*************************************************

global controls i.female edad_pob edadsq years_of_study hoursworked i.employed
