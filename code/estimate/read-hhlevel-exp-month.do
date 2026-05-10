*************************************************
*********** Read HH × Month Expenditure *********
*************************************************

*** REPLICATION FILE: read-hhlevel-exp-month.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-07

*** Loads the long expenditure panel (one row per HH × clave × month) from
*** code/build/enigh-hhlevel-exp-month.do, then collapses across all claves
*** to total household expenditure per month. Merges in HH composition and
*** ethnicity proxies from enigh-hhlevel-inc-month for use as covariates and
*** heterogeneity strata.
***
*** WHY TOTAL (not capítulo-level): The build's clave→capitulo mapping is
*** wave-specific. Numeric capítulos ("01"…"18") only appear in 2024 data;
*** alpha codes ("A0", "A1", "A2", "B0") span 2016/2018 only. After
*** subsetting to a single capítulo, post is constant and the DiD treatment
*** effect is unidentifiable. Aggregating over capítulos yields a clean
*** monthly HH-expenditure outcome with full pre/post and treat/control
*** variation. A capítulo-level breakdown awaits a wave-harmonized
*** classification in the build script (see code/build/_helpers.do
*** classify_gasto_clave).
***
*** Output dataset in memory has, per row:
***   folioviv foliohog year month time             (key)
***   gas_total_real lngas_total_real               (outcomes)
***   zlfn post treat_post ubica_geo factor smg     (HH treatment / weights)
***   hli_hh indig_hh                                (HH ethnicity indicators)
***   hh_size n_kids n_adults n_workers              (composition controls)
***   mean_age mean_educ share_female
***
*** Sets the global $controls.

*************************************************
****************** Load Data ********************
*************************************************

use "../../data/clean/enigh/enigh-hhlevel-exp-month.dta", clear

*************************************************
**** Collapse claves -> total HH × month *********
*************************************************

*** Sum real and nominal expenditure across ALL claves within (HH, year, month).
collapse (sum)     gas_total_real    = gas_real        ///
                   gas_total_nm_real = gas_nm_real     ///
                   gas_total_nom     = gas_nom         ///
                   gas_total_nm_nom  = gas_nm_nom      ///
         (firstnm) zlfn post treat_post ubica_geo factor smg upm est_dis ///
                   state ent_name reg_num reg_name macro_num macro_name time, ///
         by(folioviv foliohog year month)

label variable gas_total_real    "Total real HH expenditure (Aug-2024 pesos)"
label variable gas_total_nm_real "Total real non-monetary HH expenditure"

*** Logged outcome (log+1 to handle zero-spending months in unusual cells).
gen double lngas_total_real = ln(gas_total_real + 1)
label variable lngas_total_real "log(total real HH expenditure + 1)"

*************************************************
**** Merge HH composition + ethnicity ************
*************************************************

merge 1:1 folioviv foliohog year month using ///
    "../../data/clean/enigh/enigh-hhlevel-inc-month.dta", ///
    keepusing(hh_size n_kids n_adults n_workers n_employed n_informal ///
              n_indigenous n_hli mean_age mean_educ share_female) ///
    keep(match) nogen

cap drop hli_hh
cap drop indig_hh
gen byte hli_hh   = (n_hli         > 0) if !missing(n_hli)
gen byte indig_hh = (n_indigenous  > 0) if !missing(n_indigenous)
label variable hli_hh   "Household has at least one HLI member"
label variable indig_hh "Household has at least one self-identified indigenous member"

*************************************************
***************** Controls **********************
*************************************************

global controls hh_size n_kids n_adults n_workers mean_age mean_educ share_female
