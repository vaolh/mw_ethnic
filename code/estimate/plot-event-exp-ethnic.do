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

        *** Month branch: use ##ib<base_month>.time as in the wage/income variants.
        *** ENIGH 2018 reference month for ZLFN baseline = 21397 (representing 1aug2018).
        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if hli_hh == 0, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph1

        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if hli_hh == 1, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph2

        local month_keep ///
            "1.zlfn#20485.time" "1.zlfn#20514.time" "1.zlfn#20545.time" ///
            "1.zlfn#20575.time" "1.zlfn#20606.time" "1.zlfn#20636.time" ///
            "1.zlfn#20667.time" "1.zlfn#20698.time" "1.zlfn#20728.time" ///
            "1.zlfn#21216.time" "1.zlfn#21244.time" "1.zlfn#21275.time" ///
            "1.zlfn#21305.time" "1.zlfn#21336.time" "1.zlfn#21366.time" ///
            "1.zlfn#21397.time" "1.zlfn#21428.time" "1.zlfn#21458.time" ///
            "1.zlfn#21946.time" "1.zlfn#21975.time" "1.zlfn#22006.time" ///
            "1.zlfn#22036.time" "1.zlfn#22067.time" "1.zlfn#22097.time" ///
            "1.zlfn#22128.time" "1.zlfn#22159.time" "1.zlfn#22189.time" ///
            "1.zlfn#22677.time" "1.zlfn#22705.time" "1.zlfn#22736.time" ///
            "1.zlfn#22766.time" "1.zlfn#22797.time" "1.zlfn#22827.time" ///
            "1.zlfn#22858.time" "1.zlfn#22889.time" "1.zlfn#22919.time" ///
            "1.zlfn#23407.time" "1.zlfn#23436.time" "1.zlfn#23467.time" ///
            "1.zlfn#23497.time" "1.zlfn#23528.time" "1.zlfn#23558.time" ///
            "1.zlfn#23589.time" "1.zlfn#23620.time" "1.zlfn#23650.time"

        local month_xlabel ///
            1 "01feb2016" 2 "01mar2016" 3 "01apr2016" ///
            4 "01may2016" 5 "01jun2016" 6 "01jul2016" ///
            7 "01aug2016" 8 "01sep2016" 9 "01oct2016" ///
            10 "01feb2018" 11 "01mar2018" 12 "01apr2018" ///
            13 "01may2018" 14 "01jun2018" 15 "01jul2018" ///
            16 "01aug2018" 17 "01sep2018" 18 "01oct2018" ///
            19 "01feb2020" 20 "01mar2020" 21 "01apr2020" ///
            22 "01may2020" 23 "01jun2020" 24 "01jul2020" ///
            25 "01aug2020" 26 "01sep2020" 27 "01oct2020" ///
            28 "01feb2022" 29 "01mar2022" 30 "01apr2022" ///
            31 "01may2022" 32 "01jun2022" 33 "01jul2022" ///
            34 "01aug2022" 35 "01sep2022" 36 "01oct2022" ///
            37 "01feb2024" 38 "01mar2024" 39 "01apr2024" ///
            40 "01may2024" 41 "01jun2024" 42 "01jul2024" ///
            43 "01aug2024" 44 "01sep2024" 45 "01oct2024"

        coefplot graph1 ///
            , keep(`month_keep') vertical ///
            recast(connected) lcolor("31 119 180") mcolor("31 119 180") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("31 119 180%30") lwidth(none)) ///
            xline(18.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Month") ///
            xlabel(`month_xlabel', angle(90) labsize(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-hli-nonind-month.png", replace width(4000) height(3000)

        coefplot graph2 ///
            , keep(`month_keep') vertical ///
            recast(connected) lcolor("214 39 40") mcolor("214 39 40") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("214 39 40%30") lwidth(none)) ///
            xline(18.5, lcolor(gs10) lpattern(dash)) ///
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

        reghdfe lngas_total_real i.zlfn##ib21397.time $controls if indig_hh == 1, ///
            absorb(ubica_geo time) vce(cluster ubica_geo)
        eststo graph4

        coefplot graph3 ///
            , keep(`month_keep') vertical ///
            recast(connected) lcolor("31 119 180") mcolor("31 119 180") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("31 119 180%30") lwidth(none)) ///
            xline(18.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient Estimate on Log HH Expenditure") ///
            xtitle("Month") ///
            xlabel(`month_xlabel', angle(90) labsize(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-indig-nonind-month.png", replace width(4000) height(3000)

        coefplot graph4 ///
            , keep(`month_keep') vertical ///
            recast(connected) lcolor("214 39 40") mcolor("214 39 40") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("214 39 40%30") lwidth(none)) ///
            xline(18.5, lcolor(gs10) lpattern(dash)) ///
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
