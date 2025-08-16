*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: enighdata
*** STATA VERSION: 18/SE
*** AUTHOR: Victor Alfonso Ortega Le Hénanff
*** EMAIL: vincictor33@gmail.com
*** DATE: 2025-02-16

*** The data dictionary for ENIGH is the following:
*** https://www.inegi.org.mx/rnm/index.php/catalog/685/data-dictionary

*************************************************
************** INFLATION DATASET ***************
*************************************************

tempfile deflators
import delimited using "../input/inpc.csv", clear varnames(1)
gen year = 2016 + int((_n-1)/12)
gen month = mod(_n-1, 12) + 1
gen mes = string(month)
replace mes = "0" + mes if month < 10
gen time = date(string(year) + "-" + mes, "YM")
format time %td
drop year month mes
save `deflators'

*************************************************
******* LOOP FOR ALL ENIGH DATASETS ************
*************************************************

local years "2016 2018 2020 2022 2024"
local cumulative_ids = 0
local tempfiles ""

foreach year of local years {
    display "Processing ENIGH `year' dataset..."
    
    *************************************************
    ******** Poblacion and concentradohogar *********
    *************************************************
    
    use "../input/poblacion`year'.dta", clear
    egen new_id = concat(folioviv foliohog numren)
    keep new_id hablaind lenguaind comprenind etnia sexo edad madre_hog padre_hog asis_esc nivelaprob gradoaprob residencia hor_1 trabajo_mp 
    tempfile pop_data_`year'
    save `pop_data_`year''
    
    use "../input/concentradohogar`year'.dta", clear
    collapse (first) ubica_geo, by(folioviv)
    tempfile hog_data_`year'
    save `hog_data_`year''
	
	*************************************************
    ************* Trabajos ENIGH dataset ************
    *************************************************
	
	use "../input/trabajos`year'.dta", clear
	egen new_id = concat(folioviv foliohog numren)
	collapse (sum) htrab, by(new_id scian subor indep personal pago contrato ///
                         tipocontr pres_1 pres_2 pres_3 pres_4 pres_5 pres_6 ///
						 pres_7 pres_8 pres_9 pres_10 pres_11 pres_12 pres_13 ///
                         pres_14 pres_15 pres_16 pres_17 pres_18 pres_19 pres_20)
	bys new_id: egen max_htrab = max(htrab)
	bys new_id (htrab scian): gen jobnum = 1 if htrab == max_htrab & _n == _N
	replace jobnum = 2 if missing(jobnum)
	drop max_htrab
	reshape wide scian htrab subor indep personal pago contrato tipocontr ///
                pres_1 pres_2 pres_3 pres_4 pres_5 pres_6 pres_7 pres_8 ///
                pres_9 pres_10 pres_11 pres_12 pres_13 pres_14 pres_15 ///
                pres_16 pres_17 pres_18 pres_19 pres_20, i(new_id) j(jobnum)
	tempfile trab_data_`year'
    save `trab_data_`year''
    
    *************************************************
    ************ Ingresos ENIGH dataset *************
    *************************************************
    
    use "../input/ingresos`year'.dta", clear
    egen new_id = concat(folioviv foliohog numren)
    
    gen clave_group = ""
    
    /*Income Classification - consistent across all years*/
    replace clave_group = "wages" if inlist(clave, "P001", "P002", "P011", "P018", "P019", "P067")
    
    replace clave_group = "non_wage_income" if inlist(clave, "P003", "P004", "P005", "P006", "P007", "P009")
    replace clave_group = "non_wage_income" if inlist(clave, "P008","P014","P015","P016")
    
    replace clave_group = "gov_transfers" if inlist(clave, "P032","P033","P038","P040","P042","P043","P044","P045") 
    replace clave_group = "gov_transfers" if inlist(clave, "P048","P101","P046","P047","P102","P103")
    replace clave_group = "gov_transfers" if inlist(clave, "P104","P105","P106","P107","P108")
    
    replace clave_group = "other" if inlist(clave, "P012","P013","P020","P021","P022","P034","P035","P036","P037")
    replace clave_group = "other" if inlist(clave, "P041","P049","P051","P057","P058","P039")
        
    replace clave_group = "rentas" if inlist(clave, "P023","P024","P025")
    
    replace clave_group = "fin_capital" if inlist(clave, "P026","P027","P028","P029","P030","P031","P050")
    replace clave_group = "fin_capital" if inlist(clave, "P065","P066","P052","P053","P064")
    
    replace clave_group = "negocio" if inlist(clave, "P068","P069","P070","P071","P072","P073","P074")  
    replace clave_group = "negocio" if inlist(clave, "P078","P079","P080","P081","P075","P076","P077")
    
    replace clave_group = "ventas" if inlist(clave, "P054","P055","P056","P059","P060","P061","P062","P063")
    
    *Collapse income by income classes
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
    
    collapse (max) ing_wages ing_non_wage_income ing_fin_capital ing_gov_transfers ing_negocio ing_other ing_rentas ing_ventas, by(new_id mes mes_6)
    
    drop mes_6
    
    *************************************************
    ************ Merge with previous datasets *******
    *************************************************
    
    merge m:1 new_id using `pop_data_`year''
    drop _merge
    gen folioviv = substr(new_id, 1, length(new_id) - 3)
	
	merge m:1 new_id using `trab_data_`year''
    drop _merge
    
    merge m:1 folioviv using `hog_data_`year'', keepusing(ubica_geo)
    drop _merge
	
    *************************************************
    ************ Data cleanup for estimation ********
    *************************************************
    
    sort new_id mes
    
    *generate date
    gen time = date("`year'-" + mes, "YM")
    format time %td
    order time, after(mes)
    gen year = `year'
    order year, after(time)
    label variable time "month year date"
    drop mes
    
    *generate gender
    gen gender = .
    replace gender = 1 if sexo == "1"
    replace gender = 0 if sexo == "2"
    label define genderlbl 1 "Male" 0 "Female"
    label values gender genderlbl
    order gender, after(sexo)
    label variable gender "gender"
    label variable edad "age"
    order gender edad, after(time)
    drop sexo
    
    *mother and father home
    gen motherhome = .
    replace motherhome = 1 if madre_hog == "1"
    replace motherhome = 0 if madre_hog == "2"
    label define motherlbl 1 "Yes" 0 "No"
    label values motherhome motherlbl
    order motherhome, after(madre_hog)
    label variable motherhome "lives with mother"
    drop madre_hog
    
    gen fatherhome = .
    replace fatherhome = 1 if padre_hog == "1"
    replace fatherhome = 0 if padre_hog == "2"
    label define fatherlbl 1 "Yes" 0 "No"
    label values fatherhome fatherlbl
    order fatherhome, before(motherhome)
    label variable fatherhome "lives with father"
    drop padre_hog
    
    *indigenous language speaker
    gen indspeaker = .
    replace indspeaker = 1 if hablaind == "1"
    replace indspeaker = 0 if hablaind == "2"
    label define indspeakerlbl 1 "Yes" 0 "No"
    label values indspeaker indspeakerlbl
    order indspeaker, before(hablaind)
    label variable indspeaker "speaks indigenous language"
    drop hablaind
    
    *indigenous language 
    rename lenguaind indlang
    label variable indlang "indigenous language spoken"
    destring indlang, replace
    
    * Define indigenous language labels (complete list)
    label define indlanglbl ///
        111 "Paipai" ///
        112 "Kiliwa" ///
        113 "Cucapá" ///
        114 "Cochimí" ///
        115 "Kumiai" ///
        121 "Seri" ///
        131 "Chontal de Oaxaca" ///
        200 "Chinanteco" ///
        211 "Chinanteco de Ojitlán" ///
        212 "Chinanteco de Usila" ///
        221 "Chinanteco de Quiotepec" ///
        222 "Chinanteco de Yolox" ///
        223 "Chinanteco de Sochiapan" ///
        231 "Chinanteco de Palantla" ///
        232 "Chinanteco de Valle Nacional" ///
        241 "Chinanteco de Lalana" ///
        243 "Unknown" ///
        311 "Pame" ///
        321 "Chichimeca Jonaz" ///
        331 "Otomí" ///
        332 "Mazahua" ///
        341 "Matlatzinca" ///
        342 "Ocuilteco" ///
        400 "Zapoteco" ///
        411 "Zapoteco de Ixtlán" ///
        412 "Zapoteco Vijano" ///
        413 "Zapoteco del Rincón" ///
        421 "Zapoteco Vallista" ///
        422 "Zapoteco del Istmo" ///
        431 "Zapoteco de Cuixtla" ///
        432 "Solteco" ///
        433 "Zapoteco Sureño" ///
        441 "Chatino" ///
        450 "Mixteco" ///
        451 "Mixteco Zona Costa" ///
        452 "Mixteco Zona Alta" ///
        453 "Mixteco Zona Baja" ///
        454 "Mixteco Zona Mazateca" ///
        455 "Mixteco de Puebla" ///
        456 "Tacuate" ///
        461 "Cuicateco" ///
        471 "Triqui" ///
        481 "Amuzgo" ///
        482 "Amuzgo de Guerrero" ///
        483 "Amuzgo de Oaxaca" ///
        491 "Mazateco" ///
        492 "Chocholteco" ///
        493 "Ixcateco" ///
        494 "Popoloca" ///
        511 "Huave" ///
        611 "Tlapaneco" ///
        711 "Totonaca" ///
        712 "Tepehua" ///
        800 "Popoluca" ///
        811 "Mixe" ///
        812 "Popoluca de Oluta" ///
        821 "Popoluca de la Sierra" ///
        822 "Popoluca de Texistepec" ///
        823 "Zoque" ///
        824 "Ayapaneco" ///
        911 "Huasteco" ///
        921 "Lacandón" ///
        922 "Maya" ///
        931 "Ch'ol" ///
        932 "Chontal de Tabasco" ///
        933 "Tzeltal" ///
        934 "Tzotzil" ///
        935 "Tojolabal" ///
        936 "Chuj" ///
        941 "Mame" ///
        942 "Ixil" ///
        943 "Aguacateco" ///
        951 "Motocintleco" ///
        961 "Kanjobal" ///
        962 "Jacalteco" ///
        971 "Quiché" ///
        972 "Cakchiquel" ///
        981 "Kekchi" ///
        1011 "Pima" ///
        1012 "Pápago" ///
        1013 "Tepehuano" ///
        1014 "Tepehuano del Norte" ///
        1015 "Tepehuano del Sur" ///
        1021 "Tarahumara" ///
        1022 "Mayo" ///
        1023 "Yaqui" ///
        1024 "Guarijío" ///
        1031 "Cora" ///
        1032 "Huichol" ///
        1041 "Náhuatl" ///
        1111 "Purépecha" ///
        1211 "Kikapú" ///
        1311 "Chontal" ///
        1999 "Non Specified" ///
        9999 "Non Specified" ///
        5023 "Chapaneco" ///
        5024 "Chiapaneco" ///
        5025 "Chicomucelteco" ///
        5050 "Guachichil" ///
        5053 "Guaycura" ///
        5063 "Huzco" ///
        5098 "Teco" ///
        5103 "Tehueco" ///
        5501 "Añu" ///
        5504 "Akatako" ///
        5513 "Aymara" ///
        5540 "Guajiro" ///
        5544 "Guaymi" ///
        5551 "Quechua" ///
        5556 "Mayu" ///
        5571 "Pipil" ///
        5603 "Other American Languages" ///
        5595 "Wayuu"
    label values indlang indlanglbl
    
    *understands indigenous language 
    gen indund = .
    replace indund = 1 if comprenind == "1"
    replace indund = 0 if comprenind == "2"
    label define indundlbl 0 "No" 1 "Yes"
    label values indund indundlbl
    order indund, before(comprenind)
    label variable indund "understands indigenous language"
    drop comprenind
    
    *indigenous self-identification 
    gen indigenous = .
    replace indigenous = 1 if etnia == "1"
    replace indigenous = 0 if etnia == "2"
    label define indigenouslbl 0 "No" 1 "Yes"
    label values indigenous indigenouslbl
    order indigenous, before(etnia)
    label variable indigenous "indigenous self-identification"
    drop etnia
    
    *school attendance
    gen school_attendance = .
    replace school_attendance = 1 if asis_esc == "1"
    replace school_attendance = 0 if asis_esc == "2"
    label define school_attendlbl 0 "No" 1 "Yes"
    label values school_attendance school_attendlbl
    order school_attendance, before(asis_esc)
    label variable school_attendance "school attendance"
    drop asis_esc
    
    *years of school achieved
    gen years_of_study = .
    replace years_of_study = 0 if nivelaprob == "0"
    replace years_of_study = 0 if nivelaprob == "1"
    replace years_of_study = 1 if nivelaprob == "2" & gradoaprob == "1"
    replace years_of_study = 2 if nivelaprob == "2" & gradoaprob == "2"
    replace years_of_study = 3 if nivelaprob == "2" & gradoaprob == "3"
    replace years_of_study = 4 if nivelaprob == "2" & gradoaprob == "4"
    replace years_of_study = 5 if nivelaprob == "2" & gradoaprob == "5"
    replace years_of_study = 6 if nivelaprob == "2" & gradoaprob == "6"
    replace years_of_study = 7 if nivelaprob == "3" & gradoaprob == "1"
    replace years_of_study = 8 if nivelaprob == "3" & gradoaprob == "2"
    replace years_of_study = 9 if nivelaprob == "3" & gradoaprob == "3"
    replace years_of_study = 10 if nivelaprob == "4" & gradoaprob == "1"
    replace years_of_study = 11 if nivelaprob == "4" & gradoaprob == "2"
    replace years_of_study = 12 if nivelaprob == "4" & gradoaprob == "3"
    replace years_of_study = 13 if nivelaprob == "5" & gradoaprob == "1"
    replace years_of_study = 14 if nivelaprob == "5" & gradoaprob == "2"
    replace years_of_study = 15 if nivelaprob == "5" & gradoaprob == "3"
    replace years_of_study = 16 if nivelaprob == "5" & gradoaprob == "4"
    replace years_of_study = 17 if nivelaprob == "5" & gradoaprob == "5"
    replace years_of_study = 13 if nivelaprob == "6" & gradoaprob == "1"
    replace years_of_study = 14 if nivelaprob == "6" & gradoaprob == "2"
    replace years_of_study = 15 if nivelaprob == "6" & gradoaprob == "3"
    replace years_of_study = 16 if nivelaprob == "6" & gradoaprob == "4"
    replace years_of_study = 17 if nivelaprob == "6" & gradoaprob == "5"
    replace years_of_study = 13 if nivelaprob == "7" & gradoaprob == "1"
    replace years_of_study = 14 if nivelaprob == "7" & gradoaprob == "2"
    replace years_of_study = 15 if nivelaprob == "7" & gradoaprob == "3"
    replace years_of_study = 16 if nivelaprob == "7" & gradoaprob == "4"
    replace years_of_study = 17 if nivelaprob == "7" & gradoaprob == "5"
    replace years_of_study = 18 if nivelaprob == "8" & gradoaprob == "1"
    replace years_of_study = 19 if nivelaprob == "8" & gradoaprob == "2"
    replace years_of_study = 20 if nivelaprob == "8" & gradoaprob == "3"
    replace years_of_study = 21 if nivelaprob == "9" & gradoaprob == "1"
    replace years_of_study = 22 if nivelaprob == "9" & gradoaprob == "2"
    replace years_of_study = 23 if nivelaprob == "9" & gradoaprob == "3"
    replace years_of_study = 24 if nivelaprob == "9" & gradoaprob == "4"
    replace years_of_study = 25 if nivelaprob == "9" & gradoaprob == "5"
    label variable years_of_study "years of study"
    order years_of_study, after(school_attendance)
    drop nivelaprob gradoaprob
    
    *generate state
    drop residencia
    gen state = substr(ubica_geo, 1, 2)
    destring state, replace
    label define states ///
        1 "Aguascalientes" ///
        2 "Baja California" ///
        3 "Baja California Sur" ///
        4 "Campeche" ///
        5 "Coahuila" ///
        6 "Colima" ///
        7 "Chiapas" ///
        8 "Chihuahua" ///
        9 "Ciudad de México" ///
        10 "Durango" ///
        11 "Guanajuato" ///
        12 "Guerrero" ///
        13 "Hidalgo" ///
        14 "Jalisco" ///
        15 "México" ///
        16 "Michoacán" ///
        17 "Morelos" ///
        18 "Nayarit" ///
        19 "Nuevo León" ///
        20 "Oaxaca" ///
        21 "Puebla" ///
        22 "Querétaro" ///
        23 "Quintana Roo" ///
        24 "San Luis Potosí" ///
        25 "Sinaloa" ///
        26 "Sonora" ///
        27 "Tabasco" ///
        28 "Tamaulipas" ///
        29 "Tlaxcala" ///
        30 "Veracruz" ///
        31 "Yucatán" ///
        32 "Zacatecas"

    label values state states
    label variable state "state"
    label variable ubica_geo "municipality"
    label variable new_id "id"
    order ubica_geo state, after(new_id)
    
    *work hours and employment dummy
    rename hor_1 hoursworked
    label variable hoursworked "hours worked per week (personas)"
	rename htrab1 hours1 
	label variable hours1 "hours worked per week on first job (trabajos)"
	rename htrab2 hours2 
	label variable hours2 "hours worked per week on second job (trabajos)"
    gen employed = .
    replace employed = 1 if trabajo_mp == "1"
    replace employed = 0 if trabajo_mp == "2"
    label define employedlbl 0 "No" 1 "Yes"
    label values employed employedlbl
    drop trabajo_mp
    label variable employed "employed previous month"
    drop folioviv
    
    *income labels
    label variable ing_wages "wage income"
    label variable ing_non_wage_income "non wage labor income"
    label variable ing_gov_transfers "government transfer income"
    label variable ing_other "unclassified income"
    label variable ing_rentas "rental income"
    label variable ing_fin_capital "financial capital income"
    label variable ing_negocio "business income"
    label variable ing_ventas "sales income"
    
    *clean id - generate unique IDs across years
    egen dirty_id = group(new_id)
    gen id = dirty_id + `cumulative_ids'
    order id, before(new_id)
    drop dirty_id
    
    *clean ubica_geo
    replace ubica_geo = substr(ubica_geo, 1, length(ubica_geo) - 4)
    
    *count unique individuals for next iteration
    quietly: egen temp_max = max(id)
    local year_max = temp_max[1]
    drop temp_max
    local cumulative_ids = `year_max'
    
    *tempfile for append later
    tempfile enighdata`year'
    save `enighdata`year''
    
    local tempfiles "`tempfiles' `enighdata`year''"
    
    display "Finished processing `year'. Total individuals so far: `cumulative_ids'"
}

*************************************************
********* Append all datasets *******************
*************************************************

display "Appending all datasets..."

* Load first dataset
local first_year : word 1 of `years'
use `enighdata`first_year'', clear

* Append remaining datasets in loop
local remaining_years : list years - first_year
foreach year of local remaining_years {
    append using `enighdata`year''
}

sort id time
merge m:1 time using `deflators'
drop if _merge == 2
drop _merge

*************************************************
**************** New Variables ******************
*************************************************

*destring ubica_geo
destring ubica_geo, replace

*gen income variable
egen ingreso = rowtotal(ing_wages ing_non_wage_income ing_fin_capital ing_gov_transfers ing_negocio ing_other ing_rentas ing_ventas)
order ingreso, before(ing_wages)

*generate treatment
gen zona_a = 0
replace zona_a = 1 if inlist(ubica_geo, 02001, 02002, 02003, 02004, 02005, 26055, 26048, 26070, 26017, 26004) ///
    | inlist(ubica_geo, 26060, 26043, 26059, 26039, 26002, 08035, 08005, 08037, 08053, 08028) ///
    | inlist(ubica_geo, 08015, 08052, 08042, 05023, 05002, 05038, 05014, 05022, 05025, 05012) ///
    | inlist(ubica_geo, 05013, 19005, 28027, 28014, 28024, 28025, 28015, 28007, 28022, 28032) ///
    | inlist(ubica_geo, 28033, 28040)

*generate REAL income variables
gen ingreso_real = (ingreso / inpc) * 100
gen ing_wages_real = (ing_wages / inpc) * 100
gen ing_non_wage_income_real = (ing_non_wage_income / inpc) * 100
gen ing_fin_capital_real = (ing_fin_capital / inpc) * 100
gen ing_gov_transfers_real = (ing_gov_transfers / inpc) * 100
gen ing_negocio_real = (ing_negocio / inpc) * 100
gen ing_other_real = (ing_other / inpc) * 100
gen ing_rentas_real = (ing_rentas / inpc) * 100
gen ing_ventas_real = (ing_ventas / inpc) * 100

*generate log of income sources
gen lni = ln(ingreso_real) if ingreso_real > 0
gen lnw = ln(ing_wages_real) if ing_wages_real > 0
gen lnnwi = ln(ing_non_wage_income_real) if ing_non_wage_income_real > 0
gen lnfc = ln(ing_fin_capital_real) if ing_fin_capital_real > 0
gen lngt = ln(ing_gov_transfers_real) if ing_gov_transfers_real > 0
gen lnn = ln(ing_negocio_real) if ing_negocio_real > 0
gen lno = ln(ing_other_real) if ing_other_real > 0
gen lnr = ln(ing_rentas_real) if ing_rentas_real > 0
gen lnv = ln(ing_ventas_real) if ing_ventas_real > 0

*************************************************
**************** Sample Select ******************
*************************************************

drop if time == .
drop if edad <= 12
gen edadsq = edad*edad
sort id time
order year, after(time)

*save dataset
save "../output/enighdata.dta", replace
*save "$output/enighdata.dta", replace
