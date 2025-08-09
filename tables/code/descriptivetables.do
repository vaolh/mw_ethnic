*************************************************
***************** Clean Memory ******************
*************************************************

* que pedo lince

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: descriptivetables
*** STATA VERSION: 18/SE
*** AUTHOR: Victor Alfonso Ortega Le Hénanff
*** EMAIL: vincictor33@gmail.com
*** DATE: 2025-04-26

*************************************************
**************** Load + Globals *****************
*************************************************

global project="/Users/victorortega/Dropbox/mw_ethnic/enighdata/output"
global output="/Users/victorortega/Dropbox/mw_ethnic/tables/output"

use "../input/enighdata.dta", clear
*use "$project/enighdata.dta", clear

*************************************************
******************* Table 1 *********************
*************************************************

eststo allunits: quietly estpost summarize ///
    edad gender school_attendance employed hoursworked years_of_study indspeaker indund indigenous ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc
eststo treat: quietly estpost summarize ///
    edad gender school_attendance employed hoursworked years_of_study indspeaker indund indigenous ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc if zona_a == 1
eststo control: quietly estpost summarize ///
    edad gender school_attendance employed hoursworked years_of_study indspeaker indund indigenous ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc if zona_a == 0
eststo diff: quietly estpost ttest ///
    edad gender school_attendance employed hoursworked years_of_study indspeaker indund indigenous ///
    lnw lni lnr lnv lno lnn lnnwi lngt lnfc, by(zona_a) unequal

esttab allunits treat control diff using "../output/descriptivetables.tex", ///
    style(tex) ///
    cells("mean(pattern(1 1 1 0) fmt(2)) b(star pattern(0 0 0 1) fmt(2)) t(pattern(0 0 0 1) par fmt(2))") ///
	label replace ///
	varlabels(edad "Age" ///
		  gender "Female" ///
          school_attendance "In School" ///
          employed "Employed" ///
          hoursworked "Hours Worked" ///
          years_of_study "Years of Schooling" ///
          indspeaker "Indigenous Speaker" ///
          indund "Understands Indigenous" ///
          indigenous "Self-Identified Indigenous" ///
          lnw "Log Wages" ///
          lni "Log Income" ///
          lnr "Log Rent Income" ///
          lnv "Log Sales Income" ///
          lno "Log Other Income" ///
          lnn "Log Business Income" ///
          lnnwi "Log Non-Wage Income" ///
          lngt "Log Government Transfers" ///
          lnfc "Log Financial Capital Income") ///
    collabels(none) ///
    mlabels("All Units" "Treated" "Control" "Difference")
