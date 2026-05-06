*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: sum-descriptive
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-20

log using "log/sum-descriptive.log", replace text

*************************************************
**************** Load + Globals *****************
*************************************************

do read-indlevel-year.do

*************************************************
************* Descriptive Statistics ************
*************************************************

eststo allunits: quietly estpost summarize ///
    edad_pob female school_attendance employed hoursworked years_of_study ///
    indspeaker indund etnia ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc ///
    if ing_lab > 0

eststo treat: quietly estpost summarize ///
    edad_pob female school_attendance employed hoursworked years_of_study ///
    indspeaker indund etnia ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc ///
    if ing_lab > 0 & zlfn == 1

eststo control: quietly estpost summarize ///
    edad_pob female school_attendance employed hoursworked years_of_study ///
    indspeaker indund etnia ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc ///
    if ing_lab > 0 & zlfn == 0

eststo diff: quietly estpost ttest ///
    edad_pob female school_attendance employed hoursworked years_of_study ///
    indspeaker indund etnia ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc ///
    if ing_lab > 0, by(zlfn) unequal

*************************************************
*************** Export TeX Table ****************
*************************************************

esttab allunits treat control diff using "../../paper/tables/sum-descriptive.tex", ///
    style(tex) ///
    cells("mean(pattern(1 1 1 0) fmt(2)) b(star pattern(0 0 0 1) fmt(2)) t(pattern(0 0 0 1) par fmt(2))") ///
    label replace ///
    varlabels(edad_pob "Age" ///
              female "Female" ///
              school_attendance "In School" ///
              employed "Employed" ///
              hoursworked "Hours Worked" ///
              years_of_study "Years of Schooling" ///
              indspeaker "Indigenous Speaker" ///
              indund "Understands Indigenous" ///
              etnia "Self-Identified Indigenous" ///
              lnw "Log Wages" ///
              lni "Log Labor Income" ///
              lnr "Log Rent Income" ///
              lnv "Log Sales Income" ///
              lno "Log Other Income" ///
              lnn "Log Business Income" ///
              lnnwi "Log Non-Wage Income" ///
              lngt "Log Government Transfers" ///
              lnfc "Log Financial Capital Income") ///
    collabels(none) ///
    mlabels("All Units" "Treated" "Control" "Difference")

cap log close
