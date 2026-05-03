*************************************************
*************** Shared Helpers ******************
*************************************************

*** REPLICATION FILE: _helpers.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-02

*** Sourced via `include _helpers.do` from every build script.
*** Provides:
***   - tempfile $deflators        — INPC monthly deflators (base Aug 2024)
***   - global zlfn_munis          — list of 40 ZLFN municipality codes
***   - program apply_zlfn         — set zlfn = 1 if ubica_geo in zlfn_munis
***   - program apply_state_region — generate state, ent_name, region, macro
***   - program clean_ubica_geo    — 2016 9-digit -> 5-digit truncation
***   - program build_years_of_study — derive years_of_study from nivelaprob/gradoaprob
***   - program classify_clave     — generate clave_group from clave (P001-P108)

*************************************************
**************** INPC Deflators ******************
*************************************************

cap drop deflators_path
global deflators_path "../../data/clean/inpc/inpc.csv"

*** Helper to load deflators into a tempfile.
*** Sets $deflators_tf to the tempfile path. Caller must declare the tempfile
*** in main scope so it persists beyond program execution.
*** Usage:
***     tempfile defl_tf
***     load_deflators_to "`defl_tf'"
cap program drop load_deflators_to
program define load_deflators_to
    args target
    preserve
    import delimited using "$deflators_path", clear varnames(1)
    cap rename año_int year
    cap rename mes month
    drop fecha
    drop inpc
    gen mes_str = string(month)
    replace mes_str = "0" + mes_str if month < 10
    gen time = date(string(year) + "-" + mes_str, "YM")
    format time %td
    keep year month time deflator
    save "`target'", replace
    restore
end

*************************************************
************** ZLFN Municipalities ***************
*************************************************

*** 40 municipalities in the Zona Libre de la Frontera Norte (ZLFN)
*** as defined by the Decreto del Estímulo Fiscal de la Frontera Norte (2018).
*** Codes are 5-digit (state + municipality).
global zlfn_munis ///
    "02001 02002 02003 02004 02005 " ///
    "05002 05012 05013 05014 05022 05023 05025 05038 " ///
    "08005 08015 08028 08035 08037 08042 08052 08053 " ///
    "19005 " ///
    "26002 26004 26017 26019 26039 26043 26048 26055 26059 26060 26070 " ///
    "28007 28014 28015 28022 28024 28025 28027 28032 28033 28040"

cap program drop apply_zlfn
program define apply_zlfn
    *** Generates zlfn (1/0) from ubica_geo (string, 5-digit).
    *** Assumes ubica_geo has been cleaned to 5-digit form.
    syntax , [Ugeo(string)]
    if "`ugeo'" == "" local ugeo "ubica_geo"
    cap drop zlfn
    gen byte zlfn = 0
    label variable zlfn "ZLFN treatment municipality (1=yes)"
    foreach m of global zlfn_munis {
        replace zlfn = 1 if `ugeo' == "`m'"
    }
end

*************************************************
**************** ubica_geo Clean *****************
*************************************************

cap program drop clean_ubica_geo
program define clean_ubica_geo
    *** Cleans ubica_geo to consistent 5-digit string (state+muni).
    *** 2016 ENIGH stores 9-digit codes (state+muni+locality);
    *** later years already store 5-digit. Truncate if needed.
    args yr
    cap confirm string variable ubica_geo
    if _rc {
        tostring ubica_geo, replace
    }
    *** Truncate any code longer than 5 digits to first 5 chars.
    replace ubica_geo = substr(ubica_geo, 1, 5) if length(ubica_geo) > 5
    *** Pad shorter codes with leading zeros (defensive).
    replace ubica_geo = "0" + ubica_geo if length(ubica_geo) == 4
end

*************************************************
*************** State / Region *******************
*************************************************

cap program drop apply_state_region
program define apply_state_region
    *** Generates state, ent_name, reg_num, reg_name, macro_num, macro_name
    *** from ubica_geo (string, 5-digit, first 2 chars = state).
    cap drop state
    gen state = real(substr(ubica_geo, 1, 2))
    label variable state "state code (2-digit)"

    label define _states ///
        1 "Aguascalientes" 2 "Baja California" 3 "Baja California Sur" ///
        4 "Campeche" 5 "Coahuila" 6 "Colima" 7 "Chiapas" 8 "Chihuahua" ///
        9 "Ciudad de México" 10 "Durango" 11 "Guanajuato" 12 "Guerrero" ///
        13 "Hidalgo" 14 "Jalisco" 15 "México" 16 "Michoacán" 17 "Morelos" ///
        18 "Nayarit" 19 "Nuevo León" 20 "Oaxaca" 21 "Puebla" 22 "Querétaro" ///
        23 "Quintana Roo" 24 "San Luis Potosí" 25 "Sinaloa" 26 "Sonora" ///
        27 "Tabasco" 28 "Tamaulipas" 29 "Tlaxcala" 30 "Veracruz" ///
        31 "Yucatán" 32 "Zacatecas", replace
    label values state _states

    *** Persist as a string column too (so the variable list matches R parity)
    cap drop ent_name
    decode state, gen(ent_name)

    cap drop reg_num
    gen byte reg_num = .
    replace reg_num = 1 if inlist(state, 26, 25, 2, 3, 18)
    replace reg_num = 2 if inlist(state, 5, 8, 10, 32, 24)
    replace reg_num = 3 if inlist(state, 28, 19)
    replace reg_num = 4 if inlist(state, 1, 14, 11, 6, 16)
    replace reg_num = 5 if inlist(state, 22, 15, 9, 13, 17, 29, 21)
    replace reg_num = 6 if inlist(state, 12, 20, 7)
    replace reg_num = 7 if inlist(state, 27, 30)
    replace reg_num = 8 if inlist(state, 4, 23, 31)
    label variable reg_num "region (1-8)"

    label define _regs ///
        1 "Northwest" 2 "North" 3 "Northeast" 4 "Center-West" ///
        5 "Center-East" 6 "South" 7 "East" 8 "Peninsula", replace
    label values reg_num _regs
    cap drop reg_name
    decode reg_num, gen(reg_name)

    cap drop macro_num
    gen byte macro_num = .
    replace macro_num = 1 if inlist(reg_num, 1, 2, 3)
    replace macro_num = 2 if inlist(reg_num, 4, 5)
    replace macro_num = 3 if inlist(reg_num, 6)
    replace macro_num = 4 if inlist(reg_num, 7, 8)
    label variable macro_num "macro region (1-4)"

    label define _macros ///
        1 "Northern" 2 "Central" 3 "South" 4 "Eastern", replace
    label values macro_num _macros
    cap drop macro_name
    decode macro_num, gen(macro_name)
end

*************************************************
************* Years of Study *********************
*************************************************

cap program drop build_years_of_study
program define build_years_of_study
    *** Derives years_of_study (0-25) from nivelaprob and gradoaprob.
    *** Mexico education levels:
    ***   niv 0,1 = none/preschool       -> 0
    ***   niv 2   = primary (1-6)        -> grad
    ***   niv 3   = secondary (1-3)      -> 6+grad
    ***   niv 4   = upper secondary (1-3)-> 9+grad
    ***   niv 5,6 = post-secondary tech  -> 12+grad capped 5
    ***   niv 7   = bachelor's            -> 12+grad capped 5
    ***   niv 8   = master's              -> 17+grad
    ***   niv 9   = doctorate             -> 20+grad
    cap destring nivelaprob, replace
    cap destring gradoaprob, replace
    cap drop years_of_study
    gen years_of_study = .
    replace years_of_study = 0                if inlist(nivelaprob, 0, 1)
    replace years_of_study = min(gradoaprob, 6)         if nivelaprob == 2
    replace years_of_study = 6  + min(gradoaprob, 3)    if nivelaprob == 3
    replace years_of_study = 9  + min(gradoaprob, 3)    if nivelaprob == 4
    replace years_of_study = 12 + min(gradoaprob, 5)    if inlist(nivelaprob, 5, 6, 7)
    replace years_of_study = 17 + min(gradoaprob, 3)    if nivelaprob == 8
    replace years_of_study = 20 + min(gradoaprob, 5)    if nivelaprob == 9
    label variable years_of_study "years of completed schooling"
end

*************************************************
************ Income Classification ***************
*************************************************

cap program drop classify_clave
program define classify_clave
    *** Generates clave_group from clave (P001-P108) following the project
    *** classification used in enigh-year.R. Categories:
    ***   wages, non_wage_income, gov_transfers, rentas, fin_capital,
    ***   negocio, ventas, other.
    cap drop clave_group
    gen str20 clave_group = ""
    replace clave_group = "wages"             if inlist(clave, "P001","P002","P011","P018","P019","P067")
    replace clave_group = "non_wage_income"   if inlist(clave, "P003","P004","P005","P006","P007","P008")
    replace clave_group = "non_wage_income"   if inlist(clave, "P009","P014","P015","P016")
    replace clave_group = "gov_transfers"     if inlist(clave, "P032","P033","P038","P040","P042","P043","P044","P045")
    replace clave_group = "gov_transfers"     if inlist(clave, "P046","P047","P048","P101","P102","P103","P104","P105")
    replace clave_group = "gov_transfers"     if inlist(clave, "P106","P107","P108")
    replace clave_group = "rentas"            if inlist(clave, "P023","P024","P025")
    replace clave_group = "fin_capital"       if inlist(clave, "P026","P027","P028","P029","P030","P031","P050","P052")
    replace clave_group = "fin_capital"       if inlist(clave, "P053","P064","P065","P066")
    replace clave_group = "negocio"           if inlist(clave, "P068","P069","P070","P071","P072","P073","P074","P075")
    replace clave_group = "negocio"           if inlist(clave, "P076","P077","P078","P079","P080","P081")
    replace clave_group = "ventas"            if inlist(clave, "P054","P055","P056","P059","P060","P061","P062","P063")
    replace clave_group = "other"             if inlist(clave, "P012","P013","P020","P021","P022","P034","P035","P036")
    replace clave_group = "other"             if inlist(clave, "P037","P039","P041","P049","P051","P057","P058")
end

*************************************************
*************** Benefit Recoding *****************
*************************************************

cap program drop rename_benefits
program define rename_benefits
    *** Renames pres_* benefit indicators to readable names.
    *** 2016: pres_1-6 are medical institution codes; benefits start at pres_7.
    *** 2018+: benefits start at pres_1.
    *** After this step, both years share the same benefit variable names.
    args yr
    if `yr' == 2016 {
        cap drop pres_1 pres_2 pres_3 pres_4 pres_5 pres_6
        rename (pres_7 pres_8 pres_9 pres_10 pres_11 pres_12 pres_13 ///
                pres_14 pres_15 pres_16 pres_17 pres_18 pres_19 pres_20 ///
                pres_21 pres_22 pres_23 pres_24 pres_25 pres_26) ///
               (incapacidad aguinaldo vacaciones utilidades credito_vivienda ///
                guarderias cuidados_parentales sar_afore seguro_vida prestamos ///
                prima_vacacional becas comedor fonacot despensa servicios_publicos ///
                pension_invalidez pension_familia otras_prestaciones sin_prestaciones)
    }
    else {
        rename (pres_1 pres_2 pres_3 pres_4 pres_5 pres_6 pres_7 pres_8 pres_9 ///
                pres_10 pres_11 pres_12 pres_13 pres_14 pres_15 pres_16 pres_17 ///
                pres_18 pres_19 pres_20) ///
               (incapacidad aguinaldo vacaciones utilidades credito_vivienda ///
                guarderias cuidados_parentales sar_afore seguro_vida prestamos ///
                prima_vacacional becas comedor fonacot despensa servicios_publicos ///
                pension_invalidez pension_familia otras_prestaciones sin_prestaciones)
    }
end

cap program drop recode_benefit_indicators
program define recode_benefit_indicators
    *** Recodes the wide-form benefit indicators (suffix 1 = main job, 2 = secondary)
    *** from raw response codes to 0/1 binaries.
    ***
    *** ENIGH benefit response codes — same across years after rename_benefits:
    ***   1 = incapacidad         7  = cuidados_parentales
    ***   2 = aguinaldo           8  = sar_afore
    ***   3 = vacaciones          9  = seguro_vida
    ***   4 = utilidades         10  = prestamos
    ***   5 = credito_vivienda   11  = prima_vacacional
    ***   6 = guarderias         12  = becas        ... etc.
    ***
    *** Each cell carries the response code if the worker received that benefit
    *** (so cell `incapacidad1` == "1" means worker 1 has incapacidad). Convert to 1/0.
    foreach v in incapacidad aguinaldo vacaciones utilidades credito_vivienda ///
                 guarderias cuidados_parentales sar_afore seguro_vida prestamos ///
                 prima_vacacional becas comedor fonacot despensa servicios_publicos ///
                 pension_invalidez pension_familia otras_prestaciones {
        foreach j in 1 2 {
            cap confirm variable `v'`j'
            if _rc == 0 {
                cap destring `v'`j', replace
                replace `v'`j' = 1 if `v'`j' >  0 & `v'`j' < .
                replace `v'`j' = 0 if `v'`j' == 0 | missing(`v'`j')
            }
        }
    }
    *** sin_prestaciones is the "no benefits" flag — 1 means no benefits.
    foreach j in 1 2 {
        cap confirm variable sin_prestaciones`j'
        if _rc == 0 {
            cap destring sin_prestaciones`j', replace
            replace sin_prestaciones`j' = 1 if sin_prestaciones`j' >  0 & sin_prestaciones`j' < .
            replace sin_prestaciones`j' = 0 if sin_prestaciones`j' == 0 | missing(sin_prestaciones`j')
        }
    }
end

*************************************************
************* SCIAN Industry Labels **************
*************************************************

cap program drop label_scian
program define label_scian
    *** Applies SCIAN 2-digit industry labels to ind1, ind2.
    label define indlbl ///
        9 "Working abroad" ///
        10 "Household chores, beggars, inactive" ///
        11 "Agriculture, forestry, fishing, hunting" ///
        21 "Mining" ///
        22 "Electricity, water, gas" ///
        23 "Construction" ///
        31 "Manufacturing" 32 "Manufacturing" 33 "Manufacturing" ///
        43 "Wholesale trade" ///
        46 "Retail trade" ///
        48 "Transportation and storage" ///
        49 "Postal activities" ///
        51 "Mass media information" ///
        52 "Financial and insurance" ///
        53 "Real estate and rental of intangible goods" ///
        54 "Professional, scientific and technical" ///
        55 "Management of corporate groups" ///
        56 "Business support, waste management" ///
        61 "Education" ///
        62 "Health and social" ///
        71 "Cultural, sports and recreational" ///
        72 "Accommodation and food" ///
        81 "Other services except government" ///
        93 "Legislative, governmental, judicial" ///
        97 "Other workers" ///
        99 "Non-specified", replace
    cap label values ind1 indlbl
    cap label values ind2 indlbl
end
