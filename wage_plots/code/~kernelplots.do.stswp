*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: kernelplots
*** STATA VERSION: 18.5/SE
*** AUTHOR: Victor Alfonso Ortega Le Hénanff
*** EMAIL: vincictor33@gmail.com
*** DATE: 2025-05-05

*************************************************
**************** Load + Globals *****************
*************************************************

global project="/Users/victorortega/Dropbox/mw_ethnic/enighdata/output"
global output="/Users/victorortega/Dropbox/mw_ethnic/plots/output"

use "../input/enighdata.dta", clear
*use "$project/enighdata.dta", clear

*************************************************
******************* Graph 1 *********************
*************************************************

collapse (mean) gender edad ingreso_real ing_wages_real ing_non_wage_income_real ing_fin_capital_real ing_gov_transfers_real ing_negocio_real ing_other_real ing_rentas_real ing_ventas_real fatherhome motherhome indspeaker indund indigenous school_attendance years_of_study hoursworked employed zona_a, by(id year)

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

*income distribution for the four surveys
twoway (kdensity lni if year == 2016, bw(0.4) color(blue) lwidth(vthin) lcolor(blue) ///
             legend(label(1 "ENIGH 2016"))) ///
       (kdensity lni if year == 2018, bw(0.4) color(red) lwidth(vthin) lcolor(red) ///
             legend(label(2 "ENIGH 2018"))) ///
       (kdensity lni if year == 2020, bw(0.4) color(green) lwidth(vthin) lcolor(green) ///
             legend(label(3 "ENIGH 2020"))) ///
       (kdensity lni if year == 2022, bw(0.4) color(orange) lwidth(vthin) lcolor(magenta) ///
             legend(label(4 "ENIGH 2022"))), ///
	   xlabel(-2(2.5)16.5, grid) ///
       ylabel(, grid) ///
       xtitle("") ///
       ytitle("log average monthly income") ///
       legend(order(1 "ENIGH 2016" 2 "ENIGH 2018" 3 "ENIGH 2020" 4 "ENIGH 2022") ///
              position(1) ring(0) colfirst)
graph export "../output/graph1.png", replace width(4000) height(3000)

*************************************************
******************* Graph 2 *********************
*************************************************


* 2016
twoway (kdensity lni if year == 2016 & indspeaker == 1, bw(0.4) color(blue) lpattern(dash) lwidth(vthin) ///
   legend(label(1 "Indigenous (2016)"))) ///
  (kdensity lni if year == 2016 & indspeaker == 0, bw(0.4) color(blue) lpattern(solid) lwidth(vthin) ///
   legend(label(2 "Non-Indigenous (2016)"))), ///
   xlabel(-2(2.5)16.5, grid) ///
   ylabel(, grid) ///
   xtitle("") ///
   ytitle("log average monthly income") ///
   legend(order(1 "Indigenous (2016)" 2 "Non-Indigenous (2016)") position(1) ring(0) colfirst)
graph export "../output/graph2a.png", replace width(4000) height(3000)

* 2018
twoway (kdensity lni if year == 2018 & indspeaker == 1, bw(0.4) color(blue) lpattern(dash) lwidth(vthin) ///
   legend(label(1 "Indigenous (2018)"))) ///
  (kdensity lni if year == 2018 & indspeaker == 0, bw(0.4) color(blue) lpattern(solid) lwidth(vthin) ///
   legend(label(2 "Non-Indigenous (2018)"))), ///
   xlabel(-2(2.5)16.5, grid) ///
   ylabel(, grid) ///
   xtitle("") ///
   ytitle("log average monthly income") ///
   legend(order(1 "Indigenous (2018)" 2 "Non-Indigenous (2018)") position(1) ring(0) colfirst)
graph export "../output/graph2b.png", replace width(4000) height(3000)

* 2020
twoway (kdensity lni if year == 2020 & indspeaker == 1, bw(0.4) color(blue) lpattern(dash) lwidth(vthin) ///
   legend(label(1 "Indigenous (2020)"))) ///
  (kdensity lni if year == 2020 & indspeaker == 0, bw(0.4) color(blue) lpattern(solid) lwidth(vthin) ///
   legend(label(2 "Non-Indigenous (2020)"))), ///
   xlabel(-2(2.5)16.5, grid) ///
   ylabel(, grid) ///
   xtitle("") ///
   ytitle("log average monthly income") ///
   legend(order(1 "Indigenous (2020)" 2 "Non-Indigenous (2020)") position(1) ring(0) colfirst)
graph export "../output/graph2c.png", replace width(4000) height(3000)

* 2022
twoway (kdensity lni if year == 2022 & indspeaker == 1, bw(0.4) color(blue) lpattern(dash) lwidth(vthin) ///
   legend(label(1 "Indigenous (2022)"))) ///
  (kdensity lni if year == 2022 & indspeaker == 0, bw(0.4) color(blue) lpattern(solid) lwidth(vthin) ///
   legend(label(2 "Non-Indigenous (2022)"))), ///
   xlabel(-2(2.5)16.5, grid) ///
   ylabel(, grid) ///
   xtitle("") ///
   ytitle("log average monthly income") ///
   legend(order(1 "Indigenous (2022)" 2 "Non-Indigenous (2022)") position(11) ring(0) colfirst)
graph export "../output/graph2d.png", replace width(4000) height(3000)


