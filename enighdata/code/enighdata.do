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

// First dataset preparation
*use "../input/poblacion2016.dta", clear
use "/Users/victorortega/Dropbox/mw_ethnic/downloaddata/output/poblacion2016.dta", clear
egen new_id = concat(folioviv foliohog numren)
keep new_id hablaind lenguaind comprenind etnia sexo edad madre_hog padre_hog asis_esc nivel grado residencia hor_1 trabajo_mp 
tempfile pop_data
save `pop_data'

// Second dataset preparation - negocios
*use "../input/ingresos2016.dta", clear
use "/Users/victorortega/Dropbox/mw_ethnic/downloaddata/output/ingresos2016.dta", clear
egen new_id = concat(folioviv foliohog numren)

gen clave_group = ""

// INGRESOS LABORALES
replace clave_group = "ingresos_laborales" if ///
    inlist(clave, "P001","P002","P003","P004","P005","P006","P007","P008","P009")
replace clave_group = "ingresos_laborales" if ///
    inlist(clave, "P014","P015","P016","P018","P019","P067","P011")

// GOVERNMENT TRANSFERS
replace clave_group = "gov_transfers" if ///
    inlist(clave, "P032","P033","P038","P040","P042","P043","P044","P045") 
replace clave_group = "gov_transfers" if ///
    inlist(clave, "P048","P101","P046","P047","P102")
replace clave_group = "gov_transfers" if ///
    inlist(clave, "P103","P104","P105","P106","P107","P108")

// OTHER
replace clave_group = "other" if ///
    inlist(clave, "P012","P013","P020","P021","P022","P034","P035","P036","P037")
replace clave_group = "other" if ///
    inlist(clave, "P041","P049","P051","P057","P058","P039")
	
// RENTS (RENTA DE LA PROPIEDAD INMOBILIARIA)
replace clave_group = "rentas" if inlist(clave, "P023","P024","P025")

// FINANCIAL CAPITAL
replace clave_group = "fin_capital" if ///
    inlist(clave, "P026","P027","P028","P029","P030","P031","P050")
replace clave_group = "fin_capital" if ///
    inlist(clave, "P065","P066","P052","P053","P064")

// NEGOCIO
replace clave_group = "negocio" if ///
    inlist(clave, "P068","P069","P070","P071","P072","P073","P074")  
replace clave_group = "negocio" if ///
    inlist(clave, "P078","P079","P080","P081","P075","P076","P077")

// VENTAS
replace clave_group = "ventas" if ///
    inlist(clave, "P054","P055","P056","P059","P060","P061","P062","P063")

keep if negocio == 1
collapse (sum) ing_1 ing_2 ing_3 ing_4 ing_5 ing_6 ing_tri ///
        (first) mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 negocio, ///
        by(new_id)
gen clave = "6"
tempfile negocios

*use "../input/ingresos2016.dta", clear
use "/Users/victorortega/Dropbox/mw_ethnic/downloaddata/output/ingresos2016.dta", clear
egen new_id = concat(folioviv foliohog numren)
gen negocio = inrange(real(substr(clave, 2, .)), 68, 81)
append using `negocios'
drop if inrange(real(substr(clave, 2, .)), 68, 81)

merge m:1 new_id using `pop_data'

reshape long mes_ ing_, i(new_id clave) j(obs)
rename ing_ income
gen month = mes_
drop mes_ obs
order new_id month, before(folioviv)
drop if clave != "6" & clave != "P001"
