*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: plot-event-exp-ethnic.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-09

*** Event-study plots of ZLFN×Year coefficients on log total HH expenditure,
*** by ethnicity stratum. Year branch uses yearcoefs to get all 5 markers
*** (2016, 2018=0 baseline, 2020, 2022, 2024) with the 2018 cell at exactly 0.
*** Month branch uses the 45-month reference grid identical to plot-event-wage.

cap mkdir log
log using "log/plot-event-exp-ethnic.log", replace text

include _helpers.do

*************************************************
**** Event Study: Total HH Exp by HLI / Indig ****
*************************************************

foreach ds in year month {

    do read-hhlevel-exp-month.do
    estimates clear
    cap mkdir "../../paper/figures/event"

    if "`ds'" == "year" {

        local year_xlabel 1 "2016" 2 "2018" 3 "2020" 4 "2022" 5 "2024"

        reghdfe lngas_total_real i.zlfn##ib2018.year $controls if hli_hh == 0, ///
            absorb(ubica_geo year) vce(cluster ubica_geo)
        eststo graph1
        yearcoefs, eq(graph1)

        coefplot matrix(__year_b), se(__year_se) vertical ///
            recast(connected) lcolor("31 119 180") mcolor("31 119 180") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("31 119 180%30") lwidth(none)) ///
            xline(2.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Year") ///
            xlabel(`year_xlabel') ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-hli-nonind-year.png", replace width(4000) height(3000)

        reghdfe lngas_total_real i.zlfn##ib2018.year $controls if hli_hh == 1, ///
            absorb(ubica_geo year) vce(cluster ubica_geo)
        eststo graph2
        yearcoefs, eq(graph2)

        coefplot matrix(__year_b), se(__year_se) vertical ///
            recast(connected) lcolor("214 39 40") mcolor("214 39 40") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("214 39 40%30") lwidth(none)) ///
            xline(2.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Year") ///
            xlabel(`year_xlabel') ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-hli-ind-year.png", replace width(4000) height(3000)

        estimates clear

        reghdfe lngas_total_real i.zlfn##ib2018.year $controls if indig_hh == 0, ///
            absorb(ubica_geo year) vce(cluster ubica_geo)
        eststo graph3
        yearcoefs, eq(graph3)

        coefplot matrix(__year_b), se(__year_se) vertical ///
            recast(connected) lcolor("31 119 180") mcolor("31 119 180") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("31 119 180%30") lwidth(none)) ///
            xline(2.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Year") ///
            xlabel(`year_xlabel') ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-indig-nonind-year.png", replace width(4000) height(3000)

        reghdfe lngas_total_real i.zlfn##ib2018.year $controls if indig_hh == 1, ///
            absorb(ubica_geo year) vce(cluster ubica_geo)
        eststo graph4
        yearcoefs, eq(graph4)

        coefplot matrix(__year_b), se(__year_se) vertical ///
            recast(connected) lcolor("214 39 40") mcolor("214 39 40") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("214 39 40%30") lwidth(none)) ///
            xline(2.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Year") ///
            xlabel(`year_xlabel') ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-indig-ind-year.png", replace width(4000) height(3000)
    }

    if "`ds'" == "month" {

        *** Month branch: ##ib<base_month>.time event study. ENIGH samples
        *** Aug/Sep/Oct each wave, giving 15 time categories across 2016-2024.
        *** Baseline = aug2018 (time=21397, position 4 in monthcoefs matrix).
        ***
        *** The monthcoefs helper builds __month_b and __month_se as 1×15
        *** matrices indexed by the actual interview months, with aug2018
        *** as the omitted baseline (= 0 in the matrix). This avoids the
        *** coefplot positional-mismatch bug that occurs with a keep() list
        *** longer than the model's coefficient set, which silently drops
        *** all post-treatment points into the pre-treatment x-axis range.

        local month_xlabel ///
            1 "01aug2016" 2 "01sep2016" 3 "01oct2016" ///
            4 "01aug2018" 5 "01sep2018" 6 "01oct2018" ///
            7 "01aug2020" 8 "01sep2020" 9 "01oct2020" ///
            10 "01aug2022" 11 "01sep2022" 12 "01oct2022" ///
            13 "01aug2024" 14 "01sep2024" 15 "01oct2024"

        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if hli_hh == 0, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph1
        monthcoefs, eq(graph1)

        coefplot matrix(__month_b), se(__month_se) vertical ///
            recast(connected) lcolor("31 119 180") mcolor("31 119 180") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("31 119 180%30") lwidth(none)) ///
            xline(6.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Month") ///
            xlabel(`month_xlabel', angle(90) labsize(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-hli-nonind-month.png", replace width(4000) height(3000)

        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if hli_hh == 1, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph2
        monthcoefs, eq(graph2)

        coefplot matrix(__month_b), se(__month_se) vertical ///
            recast(connected) lcolor("214 39 40") mcolor("214 39 40") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("214 39 40%30") lwidth(none)) ///
            xline(6.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Month") ///
            xlabel(`month_xlabel', angle(90) labsize(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-hli-ind-month.png", replace width(4000) height(3000)

        estimates clear

        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if indig_hh == 0, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph3
        monthcoefs, eq(graph3)

        coefplot matrix(__month_b), se(__month_se) vertical ///
            recast(connected) lcolor("31 119 180") mcolor("31 119 180") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("31 119 180%30") lwidth(none)) ///
            xline(6.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Month") ///
            xlabel(`month_xlabel', angle(90) labsize(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-indig-nonind-month.png", replace width(4000) height(3000)

        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if indig_hh == 1, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph4
        monthcoefs, eq(graph4)

        coefplot matrix(__month_b), se(__month_se) vertical ///
            recast(connected) lcolor("214 39 40") mcolor("214 39 40") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("214 39 40%30") lwidth(none)) ///
            xline(6.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Month") ///
            xlabel(`month_xlabel', angle(90) labsize(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-indig-ind-month.png", replace width(4000) height(3000)
    }

}

cap log close
