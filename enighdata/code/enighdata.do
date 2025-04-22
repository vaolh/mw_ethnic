*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*************************************************
*************** Install Programs ****************
*************************************************

ssc install tabout

global project="/Users/victorortega/Dropbox/mw_ethnic/downloaddata/output"

*** REPLICATION FILE: enighdata
*** STATA VERSION: 18/SE
*** AUTHOR: Victor Alfonso Ortega Le Hénanff
*** EMAIL: vincictor33@gmail.com
*** DATE: 2025-02-16

*************************************************
************** 2016 ENIGH dataset ***************
*************************************************

******** Poblacion and concentradohogar *********

*use "../input/poblacion2016.dta", clear
use "$project/poblacion2016.dta", clear
egen new_id = concat(folioviv foliohog numren)
keep new_id hablaind lenguaind comprenind etnia sexo edad madre_hog padre_hog asis_esc nivel grado residencia hor_1 trabajo_mp 
tempfile pop_data_2016
save `pop_data_2016'

*use "../input/concentradohogar2016.dta", clear
use "$project/concentradohogar2016.dta", clear
collapse (first) ubica_geo, by(folioviv)
tempfile hog_data_2016
save `hog_data_2016'

************ Ingresos ENIGH dataset *************

*use "../input/ingresos2016.dta", clear
use "$project/ingresos2016.dta", clear
egen new_id = concat(folioviv foliohog numren)

gen clave_group = ""

*https://www.inegi.org.mx/rnm/index.php/catalog/685/data-dictionary
*The previous link is the source for ALL of the following data cleanup.

/*Wages*/
replace clave_group = "wages" if inlist(clave, "P001", "P002", "P011", "P018", "P019", "P067")

/*Non Wage Labor Income*/
replace clave_group = "non_wage_income" if inlist(clave, "P003", "P004", "P005", "P006", "P007", "P009")
replace clave_group = "non_wage_income" if inlist(clave, "P008","P014","P015","P016")

/*Government Transfers*/
replace clave_group = "gov_transfers" if inlist(clave, "P032","P033","P038","P040","P042","P043","P044","P045") 
replace clave_group = "gov_transfers" if inlist(clave, "P048","P101","P046","P047","P102","P103")
replace clave_group = "gov_transfers" if inlist(clave, "P104","P105","P106","P107","P108")

/*Other*/
replace clave_group = "other" if inlist(clave, "P012","P013","P020","P021","P022","P034","P035","P036","P037")
replace clave_group = "other" if inlist(clave, "P041","P049","P051","P057","P058","P039")
	
/*Rents*/
replace clave_group = "rentas" if inlist(clave, "P023","P024","P025")

/*Financial Capital*/
replace clave_group = "fin_capital" if inlist(clave, "P026","P027","P028","P029","P030","P031","P050")
replace clave_group = "fin_capital" if inlist(clave, "P065","P066","P052","P053","P064")

/*Business Income*/
replace clave_group = "negocio" if inlist(clave, "P068","P069","P070","P071","P072","P073","P074")  
replace clave_group = "negocio" if inlist(clave, "P078","P079","P080","P081","P075","P076","P077")

/*Sales*/
replace clave_group = "ventas" if inlist(clave, "P054","P055","P056","P059","P060","P061","P062","P063")

*Collapse income by our new six income classes
collapse (sum) ing_1 ing_2 ing_3 ing_4 ing_5 ing_6 (first) mes_1 mes_2 mes_3 mes_4 mes_5 mes_6, by(new_id clave_group)

* Replace spaces with the value from the other row with the same new_id
foreach mes in mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 {
    replace `mes' = `mes'[_n-1] if `mes' == " " & new_id == new_id[_n-1]
    replace `mes' = `mes'[_n+1] if `mes' == " " & new_id == new_id[_n+1]
}

*Create a panel dataset
reshape long ing_, i(new_id clave_group) j(month)
gen mes = ""

* Loop through the months and assign the value from mes_1, mes_2, mes_3...
foreach m of numlist 1/6 {
    replace mes = mes_`m' if month == `m'
}

drop month mes_1 mes_2 mes_3 mes_4 mes_5

gen ing_wages = ing_ if clave_group == "wages"
gen ing_non_wage_income = ing_ if clave_group == "non_wage_income"
gen ing_fin_capital = ing_ if clave_group == "fin_capital"
gen ing_gov_transfers = ing_ if clave_group == "gov_transfers"
gen ing_negocio = ing_ if clave_group == "negocio"
gen ing_other = ing_ if clave_group == "other"
gen ing_rentas = ing_ if clave_group == "rentas"
gen ing_ventas = ing_ if clave_group == "ventas"
gen ing_other_income = ing_ if clave_group == "other_income"

collapse (max) ing_wages ing_non_wage_income ing_fin_capital ing_gov_transfers ing_negocio ing_other ing_rentas ing_ventas ing_other_income, by(new_id mes mes_6)

*drop mes_6 (was our sanity check)
drop mes_6

************ Merge with previous datasets *************

merge m:1 new_id using `pop_data_2016'
drop _merge
gen folioviv = substr(new_id, 1, length(new_id) - 3)

merge m:1 folioviv using `hog_data_2016', keepusing(ubica_geo)
drop _merge
