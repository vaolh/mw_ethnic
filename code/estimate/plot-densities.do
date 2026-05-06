*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: plot-densities
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-20

log using "log/plot-densities.log", replace text

*************************************************
**************** Load + Globals *****************
*************************************************

do read-indlevel-year.do
cap mkdir "../../paper/figures"

*************************************************
*********** Graph: All Years Combined ***********
*************************************************

twoway (kdensity lni if year == 2016, bw(0.4) lcolor(blue) lwidth(vthin) ///
             legend(label(1 "ENIGH 2016"))) ///
       (kdensity lni if year == 2018, bw(0.4) lcolor(red) lwidth(vthin) ///
             legend(label(2 "ENIGH 2018"))) ///
       (kdensity lni if year == 2020, bw(0.4) lcolor(green) lwidth(vthin) ///
             legend(label(3 "ENIGH 2020"))) ///
       (kdensity lni if year == 2022, bw(0.4) lcolor(magenta) lwidth(vthin) ///
             legend(label(4 "ENIGH 2022"))) ///
       (kdensity lni if year == 2024, bw(0.4) lcolor(orange) lwidth(vthin) ///
             legend(label(5 "ENIGH 2024"))), ///
       xlabel(-2(2.5)16.5, grid) ///
       ylabel(, grid) ///
       xtitle("") ///
       ytitle("Log Average Labor Income") ///
       legend(order(1 "2016" 2 "2018" 3 "2020" 4 "2022" 5 "2024") ///
              position(1) ring(0) colfirst)
graph export "../../paper/figures/plot-kernel-all.png", replace width(4000) height(3000)

*************************************************
***** Graphs: By Indigenous Speaker by Year *****
*************************************************

* 2016
twoway (kdensity lni if year == 2016 & indspeaker == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
       (kdensity lni if year == 2016 & indspeaker == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
       xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
       ytitle("Log Average Labor Income") ///
       legend(order(1 "Indigenous (2016)" 2 "Non-Indigenous (2016)") position(1) ring(0) colfirst)
graph export "../../paper/figures/plot-kernel-2016.png", replace width(4000) height(3000)

* 2018
twoway (kdensity lni if year == 2018 & indspeaker == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
       (kdensity lni if year == 2018 & indspeaker == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
       xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
       ytitle("Log Average Labor Income") ///
       legend(order(1 "Indigenous (2018)" 2 "Non-Indigenous (2018)") position(1) ring(0) colfirst)
graph export "../../paper/figures/plot-kernel-2018.png", replace width(4000) height(3000)

* 2020
twoway (kdensity lni if year == 2020 & indspeaker == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
       (kdensity lni if year == 2020 & indspeaker == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
       xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
       ytitle("Log Average Labor Income") ///
       legend(order(1 "Indigenous (2020)" 2 "Non-Indigenous (2020)") position(1) ring(0) colfirst)
graph export "../../paper/figures/plot-kernel-2020.png", replace width(4000) height(3000)

* 2022
twoway (kdensity lni if year == 2022 & indspeaker == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
       (kdensity lni if year == 2022 & indspeaker == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
       xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
       ytitle("Log Average Labor Income") ///
       legend(order(1 "Indigenous (2022)" 2 "Non-Indigenous (2022)") position(11) ring(0) colfirst)
graph export "../../paper/figures/plot-kernel-2022.png", replace width(4000) height(3000)

* 2024
twoway (kdensity lni if year == 2024 & indspeaker == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
       (kdensity lni if year == 2024 & indspeaker == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
       xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
       ytitle("Log Average Labor Income") ///
       legend(order(1 "Indigenous (2024)" 2 "Non-Indigenous (2024)") position(11) ring(0) colfirst)
graph export "../../paper/figures/plot-kernel-2024.png", replace width(4000) height(3000)

*************************************************
********** Graphs: ZLFN Zone by Year ************
*************************************************

foreach yr in 2016 2018 2020 2022 2024 {
    twoway (kdensity lni if year == `yr' & zlfn == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
           (kdensity lni if year == `yr' & zlfn == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
           xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
           ytitle("Log Labor Income") ///
           legend(order(1 "ZLFN (`yr')" 2 "Rest of Country (`yr')") position(1) ring(0) colfirst)
    graph export "../../paper/figures/plot-density-zone-`yr'.png", replace width(4000) height(3000)
}

*************************************************
******** Graphs: Ethnicity by Year **************
*************************************************

foreach yr in 2016 2018 2020 2022 2024 {
    twoway (kdensity lni if year == `yr' & etnia == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
           (kdensity lni if year == `yr' & etnia == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
           xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
           ytitle("Log Labor Income") ///
           legend(order(1 "Indigenous (`yr')" 2 "Non-Indigenous (`yr')") position(1) ring(0) colfirst)
    graph export "../../paper/figures/plot-density-ethnic-`yr'.png", replace width(4000) height(3000)
}

*************************************************
******* Graphs: Informality by Year *************
*************************************************

foreach yr in 2016 2018 2020 2022 2024 {
    twoway (kdensity lni if year == `yr' & informal == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)) ///
           (kdensity lni if year == `yr' & informal == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)), ///
           xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
           ytitle("Log Labor Income") ///
           legend(order(1 "Informal (`yr')" 2 "Formal (`yr')") position(1) ring(0) colfirst)
    graph export "../../paper/figures/plot-density-informality-`yr'.png", replace width(4000) height(3000)
}

*************************************************
********** Graphs: Gender by Year ***************
*************************************************

foreach yr in 2016 2018 2020 2022 2024 {
    twoway (kdensity lni if year == `yr' & sexo == 1, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)) ///
           (kdensity lni if year == `yr' & sexo == 0, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)), ///
           xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
           ytitle("Log Labor Income") ///
           legend(order(1 "Men (`yr')" 2 "Women (`yr')") position(1) ring(0) colfirst)
    graph export "../../paper/figures/plot-density-gender-`yr'.png", replace width(4000) height(3000)
}

*************************************************
******* Graphs: Rural-Urban by Year *************
*************************************************

foreach yr in 2016 2018 2020 2022 2024 {
    twoway (kdensity lni if year == `yr' & rururb == 0, bw(0.4) lcolor(blue) lpattern(solid) lwidth(vthin)) ///
           (kdensity lni if year == `yr' & rururb == 1, bw(0.4) lcolor(blue) lpattern(dash) lwidth(vthin)), ///
           xlabel(-2(2.5)16.5, grid) ylabel(, grid) xtitle("") ///
           ytitle("Log Labor Income") ///
           legend(order(1 "Urban (`yr')" 2 "Rural (`yr')") position(1) ring(0) colfirst)
    graph export "../../paper/figures/plot-density-rural-urban-`yr'.png", replace width(4000) height(3000)
}

*************************************************
******* Graph: Per Capita Income by Year ********
*************************************************

twoway (kdensity ictpc if year == 2016, bw(0.4) lcolor(blue) lwidth(vthin)) ///
       (kdensity ictpc if year == 2018, bw(0.4) lcolor(red) lwidth(vthin)) ///
       (kdensity ictpc if year == 2020, bw(0.4) lcolor(green) lwidth(vthin)) ///
       (kdensity ictpc if year == 2022, bw(0.4) lcolor(magenta) lwidth(vthin)) ///
       (kdensity ictpc if year == 2024, bw(0.4) lcolor(orange) lwidth(vthin)) ///
       if ictpc > 0, ///
       xlabel(4(2)12, grid) ylabel(, grid) xtitle("") ///
       ytitle("Per Capita Household Income (log)") ///
       legend(order(1 "2016" 2 "2018" 3 "2020" 4 "2022" 5 "2024") ///
              position(1) ring(0) colfirst)
graph export "../../paper/figures/plot-density-income-all.png", replace width(4000) height(3000)

*************************************************
** Wage Density: Indigenous vs Non-Indigenous ****
** Pre-treatment (2016–2018) with ZLFN MW line **
*************************************************

*** ZLFN daily MW (2019) × 30 days, log scale
*** Adjust this value if wages are deflated to a different base
local mw_zlfn = ln(176.72 * 30)

twoway (kdensity lnw if post == 0 & indspeaker == 0, bw(0.3) lcolor(blue) lpattern(solid) lwidth(thin)) ///
       (kdensity lnw if post == 0 & indspeaker == 1, bw(0.3) lcolor(red)  lpattern(dash)  lwidth(thin)), ///
       xline(`mw_zlfn', lcolor(black) lpattern(shortdash) lwidth(medthin)) ///
       xlabel(4(1)12, grid) ylabel(, grid) xtitle("Log Monthly Wages") ///
       ytitle("Density") ///
       legend(order(1 "Non-Indigenous" 2 "Indigenous") ///
              position(1) ring(0) colfirst) ///
       note("Vertical line = ln(ZLFN daily MW {&times} 30)")
graph export "../../paper/figures/plot-density-wages-ethnic-pretreat.png", replace width(4000) height(3000)

cap log close