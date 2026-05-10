*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off
set linesize 250
set varabbrev off

*** REPLICATION FILE: enigh-indlevel-inc-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-02

*** Builds the individual × month ENIGH panel for years 2016–2024.
*** Canonical variable names — same contract as enigh-year.dta and the
*** household-level files (modulo unit of observation and time index).
***
*** Output: ../../data/clean/enigh/enigh-indlevel-inc-month.dta
*** Companion R script: enigh-indlevel-inc-month.R (must produce the same variables / N).

cap mkdir log
log using "log/enigh-indlevel-inc-month.log", replace text

include _helpers.do

*************************************************
**************** INPC Deflators ******************
*************************************************

tempfile deflators_tf
load_deflators_to "`deflators_tf'"

*************************************************
*********** Build per-year tempfiles *************
*************************************************

local years 2016 2018 2020 2022 2024
local tempfiles ""

foreach year of local years {

    display _n "Processing ENIGH `year' …"

    *** Poblacion (individual demographics — keep ALL available columns).
    *** new_id is preserved as folioviv+foliohog+numren so downstream tables
    *** can be merged in via this key. Variable names drift between waves
    *** (especially the disability block in 2024 which uses disc_ver/disc_oir
    *** etc. instead of disc1-disc7) — we destring whatever is present and
    *** rely on apply_all_labels to attach value labels at the end.
    use "../../data/source/enigh/poblacion`year'.dta", clear

    *** Harmonize 2024 disability aliases back to canonical disc1-disc7 if
    *** the wave used the new naming. Map (best-effort): caminar → disc1,
    *** ver → disc2, hablar → disc3, oir → disc4, vestirse → disc5,
    *** aprender → disc6, mental → disc7. Older waves with disc1-disc7
    *** keep their values.
    cap rename disc_camin   disc1
    cap rename disc_ver     disc2
    cap rename disc_habla   disc3
    cap rename disc_oir     disc4
    cap rename disc_vest    disc5
    cap rename disc_apren   disc6
    cap rename disc_acti    disc7

    *** Selectively destring the helper numerics needed by the canonical
    *** recodes below. The remaining string columns are bulk-destringed
    *** AFTER the recodes (which read raw "1"/"2" strings).
    cap destring sexo edad parentesco nivelaprob gradoaprob hor_1, replace

    *** Canonical recodes
    gen byte gender             = .
    replace gender              = 1 if sexo == 1
    replace gender              = 0 if sexo == 2
    label variable gender       "1=Male, 0=Female"

    gen byte etnia_bin          = .
    replace etnia_bin           = 1 if etnia == "1"
    replace etnia_bin           = 0 if etnia == "2"
    drop etnia
    rename etnia_bin etnia
    label variable etnia        "indigenous self-identification (1=yes)"

    gen byte indspeaker         = .
    replace indspeaker          = 1 if hablaind == "1"
    replace indspeaker          = 0 if hablaind == "2"
    label variable indspeaker   "speaks indigenous language (HLI, 1=yes)"

    gen byte indund             = .
    replace indund              = 1 if comprenind == "1"
    replace indund              = 0 if comprenind == "2"
    label variable indund       "understands indigenous language (1=yes)"

    gen byte school_attendance  = .
    replace school_attendance   = 1 if asis_esc == "1"
    replace school_attendance   = 0 if asis_esc == "2"
    label variable school_attendance "currently attending school (1=yes)"

    gen byte motherhome         = .
    replace motherhome          = 1 if madre_hog == "1"
    replace motherhome          = 0 if madre_hog == "2"
    label variable motherhome   "mother lives in HH (1=yes)"

    gen byte fatherhome         = .
    replace fatherhome          = 1 if padre_hog == "1"
    replace fatherhome          = 0 if padre_hog == "2"
    label variable fatherhome   "father lives in HH (1=yes)"

    gen byte employed           = .
    replace employed            = 1 if trabajo_mp == "1"
    replace employed            = 0 if trabajo_mp == "2"
    label variable employed     "employed in reference month (1=yes)"

    rename hor_1 hoursworked
    label variable hoursworked  "weekly hours worked (poblacion)"

    build_years_of_study

    *** Now bulk-destring all remaining string columns (they were kept as
    *** strings until the canonical recodes finished reading "1"/"2" codes).
    quietly: ds, has(type string)
    foreach v in `r(varlist)' {
        if !inlist("`v'", "folioviv", "foliohog", "numren") {
            cap destring `v', replace
        }
    }

    *** Keep all the raw poblacion columns (nivel/grado/health/time-use/
    *** social-network/fertility/marital/disability/etc.) — they are preserved
    *** alongside the canonical recodes (gender, etnia, indspeaker, indund,
    *** school_attendance, motherhome, fatherhome, employed, hoursworked,
    *** years_of_study). apply_all_labels will attach value labels at save.
    tempfile pop_`year'
    save `pop_`year''

    *** Concentradohogar (HH expansion weight, ubica_geo, smg)
    use "../../data/source/enigh/concentradohogar`year'.dta", clear
    cap confirm string variable ubica_geo
    if _rc {
        tostring ubica_geo, replace
    }
    keep folioviv ubica_geo factor smg
    duplicates drop folioviv, force
    clean_ubica_geo `year'
    tempfile hog_`year'
    save `hog_`year''

    *** Viviendas (housing + survey design)
    *** Variable names drift across years (e.g., 2024 renames disp_agua →
    *** agua_ent, combustible → combus, etc.). Keep only columns that exist
    *** in this year's file, then harmonize aliases below.
    use "../../data/source/enigh/viviendas`year'.dta", clear
    *** Harmonize 2024+ aliases back to canonical names.
    cap rename agua_ent     disp_agua
    cap rename combus       combustible
    cap rename medid_luz    medidor_luz
    cap rename focos        focos_inca
    local viv_all folioviv tipo_viv mat_pared mat_techos mat_pisos antiguedad ///
                  cuart_dorm num_cuarto disp_agua dotac_agua excusado disp_elect ///
                  combustible eli_basura tenencia renta estim_pago pago_viv ///
                  tot_resid tot_hom tot_muj tot_hog tam_loc est_socio est_dis upm
    local viv_keep ""
    foreach v of local viv_all {
        cap confirm variable `v'
        if _rc == 0 local viv_keep "`viv_keep' `v'"
    }
    keep `viv_keep'
    duplicates drop folioviv, force
    foreach v of local viv_keep {
        if "`v'" != "folioviv" cap destring `v', replace
    }
    *** Add any missing canonical columns as . so the panel stacks cleanly.
    foreach v of local viv_all {
        cap confirm variable `v'
        if _rc gen double `v' = .
    }
    tempfile viv_`year'
    save `viv_`year''

    *** Trabajos (jobs and benefits)
    use "../../data/source/enigh/trabajos`year'.dta", clear
    rename_benefits `year'
    *** Some years have tipo_trab/ocupa, others do not — drop if present
    cap drop tipo_trab ocupa
    *** Collapse so each (HH × person × job) has one row.
    collapse (sum) htrab, by(folioviv foliohog numren id_trabajo ///
        scian sinco subor indep personal pago contrato tipocontr ///
        incapacidad aguinaldo vacaciones utilidades credito_vivienda ///
        guarderias cuidados_parentales sar_afore seguro_vida prestamos ///
        prima_vacacional becas comedor fonacot despensa servicios_publicos ///
        pension_invalidez pension_familia otras_prestaciones sin_prestaciones)
    *** Reshape wide so we have *1 (main job) and *2 (secondary job) columns.
    reshape wide htrab scian sinco subor indep personal pago contrato tipocontr ///
        incapacidad aguinaldo vacaciones utilidades credito_vivienda ///
        guarderias cuidados_parentales sar_afore seguro_vida prestamos ///
        prima_vacacional becas comedor fonacot despensa servicios_publicos ///
        pension_invalidez pension_familia otras_prestaciones sin_prestaciones, ///
        i(folioviv foliohog numren) j(id_trabajo) string
    *** Industry codes from SCIAN
    gen ind1 = real(substr(scian1, 1, 2))
    gen ind2 = real(substr(scian2, 1, 2))
    label_scian
    rename htrab1 hours1
    rename htrab2 hours2
    label variable hours1 "weekly hours – main job"
    label variable hours2 "weekly hours – secondary job"
    *** Recode benefits to 0/1 binaries (single source of truth in helper)
    recode_benefit_indicators
    *** Subordinate / independent / personnel / paid / contract → 0/1
    foreach base in subor indep personal pago contrato tipocontr {
        foreach j in 1 2 {
            cap destring `base'`j', replace
            cap recode `base'`j' (1=1)(2=0)(3=0)(else=0)
        }
    }
    tempfile trab_`year'
    save `trab_`year''

    *** Gastospersona (personal expenditure aggregates)
    use "../../data/source/enigh/gastospersona`year'.dta", clear
    collapse (sum) gasto_tri gas_nm_tri, by(folioviv foliohog numren)
    rename gasto_tri  gaspers_tri
    rename gas_nm_tri gaspers_ntri
    label variable gaspers_tri  "trimestral monetary expenditure (person)"
    label variable gaspers_ntri "trimestral non-monetary expenditure (person)"
    tempfile gpers_`year'
    save `gpers_`year''

    *** Ingresos (income panel) — reshape to person × month
    use "../../data/source/enigh/ingresos`year'.dta", clear
    classify_clave
    drop if missing(clave_group) | clave_group == ""
    *** ENIGH ingresos has 6 (slot, amount, month) tuples per record:
    *** ing_1…ing_6 are amounts; mes_1…mes_6 are calendar months ("01"–"12").
    *** Reshape so each row is one (person × clave × slot).
    reshape long ing_ mes_, i(folioviv foliohog numren clave) j(slot)
    *** Normalize the month string and drop empty slots.
    cap confirm string variable mes_
    if _rc {
        tostring mes_, replace force
    }
    replace mes_ = trim(mes_)
    drop if missing(mes_) | mes_ == ""
    gen byte month = real(mes_)
    drop if missing(month) | month < 1 | month > 12
    *** Aggregate to (person × clave_group × calendar-month).
    collapse (sum) ing_nom = ing_, ///
             by(folioviv foliohog numren clave_group month)
    *** Pivot wide so each clave_group becomes its own nominal column.
    reshape wide ing_nom, ///
             i(folioviv foliohog numren month) j(clave_group) string
    *** Standardize column names: ing_nom<group> → ing_<group>_nom.
    foreach grp in wages non_wage_income gov_transfers rentas fin_capital ///
                   negocio ventas other {
        cap rename ing_nom`grp' ing_`grp'_nom
        cap confirm variable ing_`grp'_nom
        if _rc {
            gen double ing_`grp'_nom = .
        }
        replace ing_`grp'_nom = 0 if missing(ing_`grp'_nom)
        label variable ing_`grp'_nom "nominal monthly `grp' income (pesos)"
    }
    tempfile inc_`year'
    save `inc_`year''

    *** Merge all per-year pieces
    use `inc_`year'', clear
    merge m:1 folioviv foliohog numren using `pop_`year''
    drop _merge
    merge m:1 folioviv foliohog numren using `trab_`year''
    drop _merge
    merge m:1 folioviv foliohog numren using `gpers_`year''
    drop _merge
    merge m:1 folioviv using `viv_`year''
    drop _merge
    merge m:1 folioviv using `hog_`year''
    drop _merge

    gen year = `year'
    order folioviv foliohog numren year month

    tempfile yr_`year'
    save `yr_`year''
    local tempfiles "`tempfiles' `yr_`year''"
}

*************************************************
**************** Stack all years *****************
*************************************************

clear
local first : word 1 of `years'
use `yr_`first'', clear
foreach yr of local years {
    if `yr' != `first' {
        append using `yr_`yr'', force
    }
}

*************************************************
*************** Build calendar time **************
*************************************************

gen mes_str   = string(month, "%02.0f")
gen time      = date(string(year) + "-" + mes_str, "YM")
format time %td
drop mes_str
order folioviv foliohog numren year month time

*************************************************
**************** Merge deflators *****************
*************************************************

merge m:1 time using "`deflators_tf'"
drop if _merge == 2
drop _merge

*************************************************
************* Real income & logs ****************
*************************************************

*** Total ingreso aggregates (real & nominal)
egen ing_lab_nom = rowtotal(ing_wages_nom ing_non_wage_income_nom)
egen ing_mon_nom = rowtotal(ing_wages_nom ing_non_wage_income_nom ///
                            ing_gov_transfers_nom ing_rentas_nom ///
                            ing_fin_capital_nom ing_negocio_nom ///
                            ing_ventas_nom ing_other_nom)
label variable ing_lab_nom "nominal monthly labor income (pesos)"
label variable ing_mon_nom "nominal monthly monetary income (pesos)"

*** Real (Aug 2024) = nominal / deflator
foreach base in wages non_wage_income gov_transfers rentas fin_capital ///
                negocio ventas other lab mon {
    gen double ing_`base' = ing_`base'_nom / deflator
    label variable ing_`base' "real monthly `base' income (Aug-2024 pesos)"
}

*** Logs (real, positive only)
gen double lnw   = ln(ing_wages)            if ing_wages > 0
gen double lnnwi = ln(ing_non_wage_income)  if ing_non_wage_income > 0
gen double lngt  = ln(ing_gov_transfers)    if ing_gov_transfers > 0
gen double lnr   = ln(ing_rentas)           if ing_rentas > 0
gen double lnfc  = ln(ing_fin_capital)      if ing_fin_capital > 0
gen double lnn   = ln(ing_negocio)          if ing_negocio > 0
gen double lnv   = ln(ing_ventas)           if ing_ventas > 0
gen double lno   = ln(ing_other)            if ing_other > 0
gen double lni   = ln(ing_lab)              if ing_lab > 0
gen double lnmon = ln(ing_mon)              if ing_mon > 0

label variable lnw   "Log Real Wages"
label variable lnnwi "Log Real Non-Wage Income"
label variable lngt  "Log Real Government Transfers"
label variable lnr   "Log Real Rental Income"
label variable lnfc  "Log Real Financial Capital Income"
label variable lnn   "Log Real Business Income"
label variable lnv   "Log Real Sales Income"
label variable lno   "Log Real Other Income"
label variable lni   "Log Real Labor Income"
label variable lnmon "Log Real Monetary Income"

*************************************************
************ Treatment & Geography ***************
*************************************************

apply_state_region
apply_zlfn

gen byte post = (year > 2018)
label variable post      "Post-Treatment (year > 2018)"
gen byte treat_post = zlfn * post
label variable treat_post "ZLFN × Post"

*************************************************
************* Derived demographics **************
*************************************************

gen edad_pob = edad
label variable edad_pob "age (alias for edad)"
gen edadsq    = edad * edad
label variable edadsq    "age squared"

gen byte female = 1 - gender
label variable female "1=Female, 0=Male"

label variable factor   "household sampling weight"
label variable upm      "primary sampling unit"
label variable est_dis  "sampling stratum"
label variable smg      "general minimum wage (nominal)"
label variable deflator "INPC deflator (1.0 = Aug 2024)"

*************************************************
**************** Sample Select *******************
*************************************************

drop if missing(time)
drop if edad <= 12 | missing(edad)

*************************************************
*************** Order & Save *********************
*************************************************

order folioviv foliohog numren year month time ///
      ubica_geo state ent_name reg_num reg_name macro_num macro_name ///
      tam_loc factor upm est_dis ///
      zlfn post treat_post ///
      gender female edad edad_pob edadsq years_of_study hoursworked ///
      employed school_attendance motherhome fatherhome ///
      etnia indspeaker indund ///
      ing_wages_nom ing_wages ing_non_wage_income_nom ing_non_wage_income ///
      ing_gov_transfers_nom ing_gov_transfers ing_rentas_nom ing_rentas ///
      ing_fin_capital_nom ing_fin_capital ing_negocio_nom ing_negocio ///
      ing_ventas_nom ing_ventas ing_other_nom ing_other ///
      ing_lab_nom ing_lab ing_mon_nom ing_mon ///
      lnw lnnwi lngt lnr lnfc lnn lnv lno lni lnmon ///
      deflator

sort folioviv foliohog numren year month

*** Generate new_id (a string concat of HH + person identifiers) so analysis
*** scripts can join in additional poblacion / trabajos rows by a single key.
cap drop new_id
egen new_id = concat(folioviv foliohog numren)
label variable new_id "individual ID = folioviv+foliohog+numren"
order new_id, after(numren)

*** Apply value labels to every categorical column (Yes/No, sex, parentesco,
*** education, marital, languages, dwelling, materials, water, electricity,
*** combustible, drainage, tenure, locality size, est_socio, clase_hog, pea,
*** clas_emp, tipocontr, indlang, disc1, causa_*, inst_*, etc.).
apply_all_labels

compress
save "../../data/clean/enigh/enigh-indlevel-inc-month.dta", replace

display _n "Saved enigh-indlevel-inc-month.dta with " _N " observations and " c(k) " variables."

cap log close
