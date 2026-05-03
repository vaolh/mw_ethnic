*************************************************
*************** Read ENIGH Month ****************
*************************************************

*** REPLICATION FILE: read-enighmonth.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-03

*** Loads the monthly panel produced by code/build/enigh-month.do.
*** All canonical names (zlfn, post, etnia, indspeaker, indund, female,
*** edad_pob, edadsq, ing_*, ing_*_nom, lnw, lni, …) are produced directly
*** by the build script — no renames needed.
*** Sets the global $controls used by every analysis script.

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-month.dta", clear

*************************************************
***************** Controls **********************
*************************************************

global controls i.female edad_pob edadsq years_of_study hoursworked i.employed
