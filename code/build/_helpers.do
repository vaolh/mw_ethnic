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
    *** classification used in enigh-indlevel-year.R. Categories:
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

*************************************************
*********** Comprehensive Value Labels ***********
*************************************************

*** apply_all_labels: defines and applies value labels for every categorical
*** variable across the four cleaned datasets. Idempotent — safe to call more
*** than once. Wraps each `label values` in `cap` so missing variables are
*** silently skipped. Run AFTER all destring + recoding is done, just before
*** save.

cap program drop apply_all_labels
program define apply_all_labels

    *** Yes/No (1/0) base label set, applied to a known list of binary flags.
    label define yesno_lbl 0 "No" 1 "Yes", replace
    foreach v in employed indspeaker indund etnia school_attendance ///
                 motherhome fatherhome zlfn post treat_post informal ///
                 gov_sp public profesional hablaesp alfabetism segsoc ///
                 segpop atemed prob_sal aten_sal diabetes pres_alta peso ///
                 hijos_sob pareja_hog has_wage has_lab has_negocio ///
                 head_etnia head_indspeaker head_employed {
        cap label values `v' yesno_lbl
    }
    *** Apply Yes/No to all benefit *1 / *2 columns.
    foreach b in incapacidad aguinaldo vacaciones utilidades credito_vivienda ///
                 guarderias cuidados_parentales sar_afore seguro_vida prestamos ///
                 prima_vacacional becas comedor fonacot despensa servicios_publicos ///
                 pension_invalidez pension_familia otras_prestaciones sin_prestaciones ///
                 subor indep personal pago contrato {
        foreach j in 1 2 {
            cap label values `b'`j' yesno_lbl
        }
    }
    *** Disability dummies disc2-disc7 (disc1 has its own kind label below).
    foreach j in 2 3 4 5 6 7 {
        cap label values disc`j' yesno_lbl
    }
    *** servmed_*, inscr_*, inst_* health-presence dummies, others.
    forvalues i = 1/11 {
        cap label values servmed_`i' yesno_lbl
        cap label values inscr_`i'   yesno_lbl
    }
    cap label values cocina      yesno_lbl
    cap label values cocina_dor  yesno_lbl
    cap label values excusado    yesno_lbl
    cap label values uso_compar  yesno_lbl
    cap label values biodigest   yesno_lbl
    cap label values lavadero    yesno_lbl
    cap label values fregadero   yesno_lbl
    cap label values regadera    yesno_lbl
    cap label values tinaco_azo  yesno_lbl
    cap label values cisterna    yesno_lbl
    cap label values pileta      yesno_lbl
    cap label values calent_sol  yesno_lbl
    cap label values calent_gas  yesno_lbl
    cap label values medidor_luz yesno_lbl
    cap label values bomba_agua  yesno_lbl
    cap label values tanque_gas  yesno_lbl
    cap label values aire_acond  yesno_lbl
    cap label values calefacc    yesno_lbl
    cap label values estufa_chi  yesno_lbl
    cap label values pago_mesp   yesno_lbl
    cap label values viv_usada   yesno_lbl
    cap label values antigua_ne  yesno_lbl
    cap label values tiene_b     yesno_lbl
    cap label values tiene_c     yesno_lbl

    *** Gender / sex (1=Male, 0=Female).
    label define gender_lbl 0 "Female" 1 "Male", replace
    cap label values gender   gender_lbl
    cap label values head_sex gender_lbl

    *** Female (1=Female).
    label define female_lbl 0 "Male" 1 "Female", replace
    cap label values female female_lbl

    *** Treatment / time.
    label define zlfn_lbl   0 "Non-ZLFN" 1 "ZLFN", replace
    cap label values zlfn       zlfn_lbl
    label define post_lbl   0 "Pre (≤2018)" 1 "Post (>2018)", replace
    cap label values post       post_lbl

    *** Parentesco (relationship to head).
    label define parentesco_lbl ///
        101 "Jefe(a) del hogar" ///
        201 "Esposo(a) o compañero(a)" ///
        301 "Hijo(a)" 302 "Hijastro(a)" ///
        401 "Nuero o nuera" ///
        501 "Nieto(a)" ///
        601 "Padre o madre" 602 "Suegro(a)" ///
        701 "Hermano(a)" 702 "Cuñado(a)" ///
        801 "Otro parentesco" ///
        901 "No parentesco" ///
        999 "No especificado", replace
    cap label values parentesco parentesco_lbl

    *** Marital status (edo_conyug).
    label define edo_conyug_lbl ///
        1 "Unión libre" 2 "Casado(a)" 3 "Separado(a)" ///
        4 "Divorciado(a)" 5 "Viudo(a)" 6 "Soltero(a)", replace
    cap label values edo_conyug edo_conyug_lbl

    *** Education currently attending (nivel) and grade (grado).
    label define nivel_lbl ///
        1 "Preescolar o kinder" 2 "Primaria" 3 "Secundaria" ///
        4 "Preparatoria o bachillerato" 5 "Normal básica" ///
        6 "Carrera técnica o comercial" 7 "Profesional" ///
        8 "Maestría" 9 "Doctorado", replace
    cap label values nivel nivel_lbl
    label define grado_lbl ///
        1 "Primer año" 2 "Segundo año" 3 "Tercer año" ///
        4 "Cuarto año" 5 "Quinto año" 6 "Sexto año", replace
    cap label values grado grado_lbl

    *** Highest completed education (antec_esc).
    label define antec_esc_lbl ///
        1 "Primaria" 2 "Secundaria" 3 "Preparatoria/Bachillerato" ///
        4 "Licenciatura" 5 "Maestría", replace
    cap label values antec_esc antec_esc_lbl

    *** Education level achieved (nivelaprob).
    label define nivelaprob_lbl ///
        0 "Ninguno" 1 "Preescolar" 2 "Primaria" 3 "Secundaria" ///
        4 "Preparatoria/Bachillerato" 5 "Normal básica" ///
        6 "Carrera técnica/comercial" 7 "Profesional" ///
        8 "Maestría" 9 "Doctorado", replace
    cap label values nivelaprob nivelaprob_lbl

    *** Type of school (tipoesc).
    label define tipoesc_lbl 1 "Pública" 2 "Privada" 3 "Otro", replace
    cap label values tipoesc tipoesc_lbl

    *** Reason for work absence (motivo_aus).
    label define motivo_aus_lbl ///
        1 "Huelga o paro laboral" 2 "Paro técnico" 3 "Suspensión temporal" ///
        4 "Capacitación" 5 "Vacaciones" 6 "Permiso o enfermedad" ///
        7 "Falta de vehículo" 8 "Falta de materias primas" 9 "Mal tiempo" ///
        10 "Fin de temporada" 11 "Comenzará nuevo trabajo" ///
        12 "Otra razón" 13 "Ninguna", replace
    cap label values motivo_aus motivo_aus_lbl

    *** PNEA primary activity.
    label define act_pnea1_lbl ///
        1 "No trabajó y no buscó trabajo" 2 "Es pensionado o jubilado" ///
        3 "Quehaceres del hogar" 4 "Estudiar" ///
        5 "Limitación física o mental" 6 "Otros", replace
    cap label values act_pnea1 act_pnea1_lbl
    label define act_pnea2_lbl ///
        2 "Es pensionado o jubilado" 3 "Quehaceres del hogar" ///
        4 "Estudiar" 5 "Limitación física o mental", replace
    cap label values act_pnea2 act_pnea2_lbl

    *** Number of jobs.
    label define num_trabaj_lbl 1 "Solo 1" 2 "Dos o más", replace
    cap label values num_trabaj num_trabaj_lbl

    *** Type of dwelling (tipo_viv).
    label define tipo_viv_lbl ///
        1 "Casa independiente" 2 "Departamento en edificio" ///
        3 "Vivienda en vecindad" 4 "Vivienda en cuarto de azotea" ///
        5 "Local no construido para habitación", replace
    cap label values tipo_viv tipo_viv_lbl

    *** Wall material (mat_pared).
    label define mat_pared_lbl ///
        1 "Material de desecho" 2 "Lámina de cartón" ///
        3 "Lámina de asbesto o metálica" 4 "Carrizo, bambú o palma" ///
        5 "Embarro o bajareque" 6 "Madera" 7 "Adobe" ///
        8 "Tabique, ladrillo, block, piedra, cantera, cemento o concreto", replace
    cap label values mat_pared mat_pared_lbl

    *** Roof material (mat_techos).
    label define mat_techos_lbl ///
        1 "Material de desecho" 2 "Lámina de cartón" ///
        3 "Lámina de asbesto o metálica" 4 "Palma, tejamanil o madera" ///
        5 "Teja" 6 "Losa de concreto / viguetas con bovedilla" ///
        7 "Madera o tejamanil" 8 "Terrado con viguería" ///
        9 "Otro material" 10 "No especificado", replace
    cap label values mat_techos mat_techos_lbl

    *** Floor material (mat_pisos).
    label define mat_pisos_lbl ///
        1 "Tierra" 2 "Cemento o firme" ///
        3 "Madera, mosaico u otro recubrimiento", replace
    cap label values mat_pisos mat_pisos_lbl

    *** Water supply (disp_agua).
    label define disp_agua_lbl ///
        1 "Entubada dentro de la vivienda" ///
        2 "Entubada fuera de la vivienda pero dentro del terreno" ///
        3 "Llave pública o hidrante" 4 "Captadores de agua de lluvia" ///
        5 "Acarreada de otra vivienda" 6 "De pipa" ///
        7 "De pozo, río, lago, arroyo u otro", replace
    cap label values disp_agua disp_agua_lbl

    *** Water supply frequency (dotac_agua).
    label define dotac_agua_lbl ///
        1 "Diario" 2 "Cada tercer día" 3 "Dos veces por semana" ///
        4 "Una vez por semana" 5 "De vez en cuando", replace
    cap label values dotac_agua dotac_agua_lbl

    *** Drainage (drenaje).
    label define drenaje_lbl ///
        1 "A la red pública" 2 "A una fosa séptica" ///
        3 "A tubería a barranca o grieta" ///
        4 "A tubería a río, lago o mar" 5 "No tiene drenaje", replace
    cap label values drenaje drenaje_lbl

    *** Electricity source (disp_elect).
    label define disp_elect_lbl ///
        1 "Del servicio público" 2 "De una planta particular" ///
        3 "De panel solar" 4 "De otra fuente" ///
        5 "No tiene luz eléctrica", replace
    cap label values disp_elect disp_elect_lbl

    *** Cooking fuel (combustible).
    label define combustible_lbl ///
        1 "Leña" 2 "Carbón" 3 "Gas de tanque" ///
        4 "Gas natural o de tubería" 5 "Electricidad" ///
        6 "Otro combustible", replace
    cap label values combustible combustible_lbl

    *** Waste disposal (eli_basura).
    label define eli_basura_lbl ///
        1 "La recoge un camión o carrito de basura" ///
        2 "La llevan a un basurero público" ///
        3 "La depositan en un contenedor o depósito" ///
        4 "La queman" 5 "La entierran" ///
        6 "La tiran en un terreno baldío o calle" ///
        7 "La tiran en una barranca o grieta" ///
        8 "La tiran al río, lago o mar", replace
    cap label values eli_basura eli_basura_lbl

    *** Tenure (tenencia).
    label define tenencia_lbl ///
        1 "Rentada" 2 "Prestada" 3 "Propia, en pago" ///
        4 "Propia" 5 "Intestada o en litigio" 6 "Otra situación", replace
    cap label values tenencia tenencia_lbl

    *** Locality size (tam_loc).
    label define tam_loc_lbl ///
        1 "100,000+ habitantes" 2 "15,000–99,999" ///
        3 "2,500–14,999" 4 "<2,500", replace
    cap label values tam_loc tam_loc_lbl

    *** Socioeconomic stratum (est_socio).
    label define est_socio_lbl ///
        1 "Bajo" 2 "Medio bajo" 3 "Medio alto" 4 "Alto", replace
    cap label values est_socio est_socio_lbl

    *** Hogares: clase_hog (HH type).
    label define clase_hog_lbl ///
        1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" ///
        4 "Compuesto" 5 "Corresidente", replace
    cap label values clase_hog clase_hog_lbl

    *** CONEVAL: pea (labor force status).
    label define pea_lbl ///
        1 "Ocupado" 2 "Desocupado" 3 "No PEA", replace
    cap label values pea pea_lbl

    *** CONEVAL: hli (food poverty).
    label define hli_lbl 0 "No food-poor" 1 "Food-poor" 2 "No info", replace
    cap label values hli hli_lbl

    *** Employment class (clas_emp).
    label define clas_emp_lbl ///
        1 "Empleado público" 2 "Empleado privado" ///
        3 "Trabajador independiente" 4 "Trabajador familiar sin pago" ///
        5 "Patrón" 6 "Otro", replace
    cap label values clas_emp clas_emp_lbl

    *** Type of contract (tipocontr).
    label define tipocontr_lbl 0 "Permanente" 1 "Temporal", replace
    cap label values tipocontr1 tipocontr_lbl
    cap label values tipocontr2 tipocontr_lbl

    *** Indigenous language (lenguaind / indlang).
    label define indlang_lbl ///
        111 "Paipai" 112 "Kiliwa" 113 "Cucapá" 114 "Cochimí" 115 "Kumiai" ///
        121 "Seri" 131 "Chontal de Oaxaca" ///
        200 "Chinanteco" 211 "Chinanteco de Ojitlán" 212 "Chinanteco de Usila" ///
        221 "Chinanteco de Quiotepec" 222 "Chinanteco de Yolox" ///
        223 "Chinanteco de Sochiapan" 231 "Chinanteco de Palantla" ///
        232 "Chinanteco de Valle Nacional" 241 "Chinanteco de Lalana" 243 "Chinanteco (s.e.)" ///
        311 "Pame" 321 "Chichimeca Jonaz" ///
        331 "Otomí" 332 "Mazahua" 341 "Matlatzinca" 342 "Ocuilteco" ///
        400 "Zapoteco" 411 "Zapoteco de Ixtlán" 412 "Zapoteco Vijano" ///
        413 "Zapoteco del Rincón" 421 "Zapoteco Vallista" 422 "Zapoteco del Istmo" ///
        431 "Zapoteco de Cuixtla" 432 "Solteco" 433 "Zapoteco Sureño" ///
        441 "Chatino" ///
        450 "Mixteco" 451 "Mixteco Zona Costa" 452 "Mixteco Zona Alta" ///
        453 "Mixteco Zona Baja" 454 "Mixteco Zona Mazateca" 455 "Mixteco de Puebla" ///
        456 "Tacuate" 461 "Cuicateco" 471 "Triqui" ///
        481 "Amuzgo" 482 "Amuzgo de Guerrero" 483 "Amuzgo de Oaxaca" ///
        491 "Mazateco" 492 "Chocholteco" 493 "Ixcateco" 494 "Popoloca" ///
        511 "Huave" 611 "Tlapaneco" 711 "Totonaca" 712 "Tepehua" ///
        800 "Popoluca" 811 "Mixe" 812 "Popoluca de Oluta" ///
        821 "Popoluca de la Sierra" 822 "Popoluca de Texistepec" ///
        823 "Zoque" 824 "Ayapaneco" ///
        911 "Huasteco" 921 "Lacandón" 922 "Maya" 931 "Ch'ol" ///
        932 "Chontal de Tabasco" 933 "Tzeltal" 934 "Tzotzil" ///
        935 "Tojolabal" 936 "Chuj" ///
        941 "Mame" 942 "Ixil" 943 "Aguacateco" 951 "Motocintleco" ///
        961 "Kanjobal" 962 "Jacalteco" 971 "Quiché" 972 "Cakchiquel" ///
        981 "Kekchi" ///
        1011 "Pima" 1012 "Pápago" 1013 "Tepehuano" ///
        1014 "Tepehuano del Norte" 1015 "Tepehuano del Sur" ///
        1021 "Tarahumara" 1022 "Mayo" 1023 "Yaqui" 1024 "Guarijío" ///
        1031 "Cora" 1032 "Huichol" 1041 "Náhuatl" ///
        1111 "Purépecha" 1211 "Kikapú" 1311 "Chontal" ///
        1999 "No especificado" 9999 "No especificado", replace
    cap label values indlang indlang_lbl
    cap label values lenguaind indlang_lbl

    *** Disability — first type (disc1).
    label define disc1_lbl ///
        1 "Caminar, moverse, subir o bajar" ///
        2 "Ver, aun usando lentes" ///
        3 "Hablar, comunicarse" ///
        4 "Oír, aun usando aparato auditivo" ///
        5 "Vestirse, bañarse o comer" ///
        6 "Poner atención o aprender" ///
        7 "Limitación mental" ///
        8 "No tiene dificultad", replace
    cap label values disc1 disc1_lbl

    *** Disability cause (causa_*).
    label define causa_lbl ///
        1 "Nació así" 2 "Por enfermedad" 3 "Por accidente" ///
        4 "Por edad avanzada" 5 "Otra causa", replace
    forvalues j = 1/7 {
        cap label values causa`j' causa_lbl
    }

    *** Health institution (inst_*).
    label define inst_lbl ///
        1 "IMSS" 2 "ISSSTE" ///
        3 "ISSSTE estatal/PEMEX/Defensa/Marina" ///
        4 "Seguro Popular / IMSS Bienestar" ///
        5 "IMSS-PROSPERA" 6 "Otro", replace
    forvalues i = 1/8 {
        cap label values inst_`i' inst_lbl
    }

    *** GASTOSHOGAR tipo_gasto (string codes).
    *** Stata's value labels are integer-only, so we do not bind these
    *** to the string variable. The variable label below documents the codes.
    cap label variable tipo_gasto "G1=monetario propio, G2=monetario otro hogar, G3=autoconsumo, G5=regalo, G6=transf institucional, G7=alquiler imputado"

    *** GASTOSHOGAR frecuencia.
    label define frecuencia_lbl ///
        0 "No aplica" 1 "Diario" 2 "Cada semana" 3 "Una vez al mes" ///
        4 "Cada año" 5 "Una sola vez" 6 "Otros", replace
    cap label values frecuencia frecuencia_lbl

    *** Capítulo (gastos T-aggregate codes T901..T916). Same caveat — string.
    cap label variable capitulo "INEGI capítulo: alimentos, transporte, limpieza, cuidados, educacion, comunicacion, vivienda, último recibo, vestido, cristalería, salud, enseres, esparcimiento, transporte (gasto), gastos diversos, financieras"

    *** Custom expenditure groups (string).
    cap label variable gasto_group "Custom group: food, durables, services, housing, transport, health, education, leisure, financial, other"

    *** Industry / sector. Already labeled by label_scian — but ensure ind1/ind2 carry it.
    cap label values ind1 indlbl
    cap label values ind2 indlbl

end

*************************************************
*********** Gastos Capitulo Mapping **************
*************************************************

*** ENIGH expenditure clave codes are 6-digit strings. The first 2 digits
*** identify the capítulo (COICOP-style), e.g. "01" = food, "11" = restaurants.
*** T-codes (T901..T916) are pre-aggregated — drop them to avoid double-counting.
***
*** Capitulo groups (first 2 digits of clave):
***   01 = Alimentos y bebidas no alcohólicas
***   02 = Bebidas alcohólicas y tabaco
***   03 = Prendas de vestir y calzado
***   04 = Vivienda, agua, electricidad, gas y otros combustibles
***   05 = Mobiliario, equipo y mantenimiento de la casa
***   06 = Salud
***   07 = Transporte
***   08 = Comunicaciones
***   09 = Recreación y cultura
***   10 = Educación
***   11 = Restaurantes y servicios de alojamiento
***   12 = Cuidados personales y otros bienes y servicios
***   13 = Servicios diversos
***   17 = Erogaciones financieras y de capital (extra: not consumption)
***   18 = Otros productos / vacíos (catch-all in 2024 catalog)

cap program drop classify_gasto_clave
program define classify_gasto_clave
    *** Adds two columns to the gastos data in memory:
    ***   capitulo     — 2-digit string capítulo code
    ***   gasto_group  — coarse custom group (food / durables / services / ...)
    cap drop capitulo
    cap drop gasto_group
    *** Drop pre-aggregated T-codes so we don't double-count.
    drop if substr(clave, 1, 1) == "T"
    gen str2 capitulo = substr(clave, 1, 2)
    gen str20 gasto_group = ""
    replace gasto_group = "food"          if capitulo == "01"
    replace gasto_group = "alcohol_tobac" if capitulo == "02"
    replace gasto_group = "clothing"      if capitulo == "03"
    replace gasto_group = "housing"       if capitulo == "04"
    replace gasto_group = "durables"      if capitulo == "05"
    replace gasto_group = "health"        if capitulo == "06"
    replace gasto_group = "transport"     if capitulo == "07"
    replace gasto_group = "comms"         if capitulo == "08"
    replace gasto_group = "recreation"    if capitulo == "09"
    replace gasto_group = "education"     if capitulo == "10"
    replace gasto_group = "restaurants"   if capitulo == "11"
    replace gasto_group = "personal"      if capitulo == "12"
    replace gasto_group = "services"      if capitulo == "13"
    replace gasto_group = "financial"     if capitulo == "17"
    replace gasto_group = "other"         if gasto_group == ""
end

*************************************************
********* HH × year aggregation of gastos ********
*************************************************

*** Loads gastoshogar`yr'.dta and returns a tempfile path (in the global
*** $hh_gastos_tf) holding one row per (folioviv, foliohog) for survey year
*** `yr', with columns:
***   year                      — the survey wave
***   gas_total_nom             — sum of gasto_tri (nominal trimestral pesos)
***   gas_total_nm_nom          — sum of gas_nm_tri (non-monetary trimestral)
***   gas_<grp>_nom             — sum by custom group (food, durables, ...)
***   gas_cap<NN>_nom           — sum by capítulo NN ∈ {01,..,18}
***   <plus _real counterparts deflated by Aug-yr INPC>

cap program drop aggregate_gastos_hh_year
program define aggregate_gastos_hh_year
    args yr defl_tf target
    preserve
    use "../../data/source/enigh/gastoshogar`yr'.dta", clear
    cap destring gasto_tri,    replace
    cap destring gas_nm_tri,   replace
    classify_gasto_clave
    *** Stash the raw clave-level rows so we can build sub-aggregates.
    tempfile _raw _grp _cap
    save `_raw', replace
    *** (HH × gasto_group) wide table.
    use `_raw', clear
    collapse (sum) gasto_tri gas_nm_tri, ///
        by(folioviv foliohog gasto_group)
    reshape wide gasto_tri gas_nm_tri, ///
        i(folioviv foliohog) j(gasto_group) string
    save `_grp', replace
    *** (HH × capitulo) wide table.
    use `_raw', clear
    collapse (sum) gasto_tri gas_nm_tri, ///
        by(folioviv foliohog capitulo)
    reshape wide gasto_tri gas_nm_tri, ///
        i(folioviv foliohog) j(capitulo) string
    save `_cap', replace
    *** HH total.
    use `_raw', clear
    collapse (sum) gas_total_nom = gasto_tri ///
                   gas_total_nm_nom = gas_nm_tri, ///
        by(folioviv foliohog)
    *** Merge group + capitulo aggregates.
    merge 1:1 folioviv foliohog using `_grp'
    drop _merge
    merge 1:1 folioviv foliohog using `_cap'
    drop _merge
    *** Standardize column names: gasto_tri<grp> -> gas_<grp>_nom etc.
    foreach grp in food alcohol_tobac clothing housing durables health ///
                   transport comms recreation education restaurants ///
                   personal services financial other {
        cap rename gasto_tri`grp'  gas_`grp'_nom
        cap rename gas_nm_tri`grp' gas_`grp'_nm_nom
        cap confirm variable gas_`grp'_nom
        if _rc gen double gas_`grp'_nom = 0
        cap confirm variable gas_`grp'_nm_nom
        if _rc gen double gas_`grp'_nm_nom = 0
        replace gas_`grp'_nom    = 0 if missing(gas_`grp'_nom)
        replace gas_`grp'_nm_nom = 0 if missing(gas_`grp'_nm_nom)
        label variable gas_`grp'_nom    "nominal HH expenditure: `grp' (trimestral pesos)"
        label variable gas_`grp'_nm_nom "nominal HH non-monetary expenditure: `grp' (trimestral pesos)"
    }
    foreach cap_n in 01 02 03 04 05 06 07 08 09 10 11 12 13 17 18 {
        cap rename gasto_tri`cap_n'  gas_cap`cap_n'_nom
        cap rename gas_nm_tri`cap_n' gas_cap`cap_n'_nm_nom
        cap confirm variable gas_cap`cap_n'_nom
        if _rc gen double gas_cap`cap_n'_nom = 0
        cap confirm variable gas_cap`cap_n'_nm_nom
        if _rc gen double gas_cap`cap_n'_nm_nom = 0
        replace gas_cap`cap_n'_nom    = 0 if missing(gas_cap`cap_n'_nom)
        replace gas_cap`cap_n'_nm_nom = 0 if missing(gas_cap`cap_n'_nm_nom)
        label variable gas_cap`cap_n'_nom    "nominal HH expenditure: capítulo `cap_n' (trimestral pesos)"
        label variable gas_cap`cap_n'_nm_nom "nominal HH non-monetary expenditure: capítulo `cap_n' (trimestral pesos)"
    }
    *** Deflate by August INPC of `yr'. Save current (HH-aggregated) data,
    *** read the deflator file to extract the scalar, then reload the data.
    tempfile _hh_pre_defl
    save `_hh_pre_defl', replace
    use "`defl_tf'", clear
    keep if year == `yr' & month == 8
    quietly: summarize deflator
    local ago_def = r(mean)
    use `_hh_pre_defl', clear
    if `ago_def' == 0 | missing(`ago_def') local ago_def = 1
    foreach v of varlist gas_*_nom {
        local r = subinstr("`v'", "_nom", "_real", 1)
        gen double `r' = `v' / `ago_def'
        local lab : variable label `v'
        label variable `r' "real (Aug-2024) `lab'"
    }
    gen year = `yr'
    save "`target'", replace
    restore
end
