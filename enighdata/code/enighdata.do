/*Clean Memory*/

clear
cap clear
cap log close
set more off

/*Install Programs*/

ssc install tabout

*** REPLICATION FILE: enighdata
*** STATA VERSION: 18/SE
*** AUTHOR: Victor Alfonso Ortega Le Hénanff
*** EMAIL: vincictor33@gmail.com
*** DATE: 2025-02-16

use "../input/ingresos2016.dta", clear

save "../output/ingresos2016.dta", replace