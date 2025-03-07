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

use "../input/poblacion2016.dta", clear

egen new_id = concat(folioviv foliohog numren)
keep new_id hablaind sexo edad

save "../output/poblacion2016.dta", replace

use "../input/ingresos2016.dta", clear

egen new_id = concat(folioviv foliohog numren)
gen negocio = inrange(real(substr(clave, 2, .)), 68, 81)

keep if negocio == 1

collapse (sum) ing_1 ing_2 ing_3 ing_4 ing_5 ing_6 ing_tri ///
        (first) mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 negocio, ///
        by(new_id)
		
gen clave = "6"

tempfile negocios
save `negocios'

use "../input/ingresos2016.dta", clear

egen new_id = concat(folioviv foliohog numren)
gen negocio = inrange(real(substr(clave, 2, .)), 68, 81)

append using `negocios'

drop if inrange(real(substr(clave, 2, .)), 68, 81)

order new_id, before(folioviv)

merge m:1 new_id using "../output/poblacion2016.dta"



