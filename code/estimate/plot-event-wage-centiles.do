*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: plot-event-wage-centiles
*** STATA VERSION: StataNow 19.5
*** AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
*** DATE: 2026-03-23

log using "log/plot-event-wage-centiles.log", replace text

*************************************************
******** Centile Coefficient Plot: Wages ********
*************************************************

foreach ds in year month {

do read-indlevel-`ds'.do
estimates clear
cap mkdir "../../paper/figures"

xtile centile = lni, nq(100)

forvalues c = 1/100 {
    quietly reghdfe lnw i.zlfn##i.post $controls if centile == `c', ///
        absorb(ubica_geo year) vce(cluster ubica_geo)
    eststo cent`c'
}

coefplot ///
    cent1 cent2 cent3 cent4 cent5 cent6 cent7 cent8 cent9 cent10 ///
    cent11 cent12 cent13 cent14 cent15 cent16 cent17 cent18 cent19 cent20 ///
    cent21 cent22 cent23 cent24 cent25 cent26 cent27 cent28 cent29 cent30 ///
    cent31 cent32 cent33 cent34 cent35 cent36 cent37 cent38 cent39 cent40 ///
    cent41 cent42 cent43 cent44 cent45 cent46 cent47 cent48 cent49 cent50 ///
    cent51 cent52 cent53 cent54 cent55 cent56 cent57 cent58 cent59 cent60 ///
    cent61 cent62 cent63 cent64 cent65 cent66 cent67 cent68 cent69 cent70 ///
    cent71 cent72 cent73 cent74 cent75 cent76 cent77 cent78 cent79 cent80 ///
    cent81 cent82 cent83 cent84 cent85 cent86 cent87 cent88 cent89 cent90 ///
    cent91 cent92 cent93 cent94 cent95 cent96 cent97 cent98 cent99 cent100 ///
    , keep(1.zlfn#1.post) vertical noci bycoefs ///
    recast(connected) lcolor("31 119 180") mcolor("31 119 180") ///
    msymbol(circle) lw(medthin) msize(vsmall) ///
    xline(10.5 20.5 30.5 40.5 50.5 60.5 70.5 80.5 90.5, ///
        lcolor(gs14) lpattern(dash) lw(thin)) ///
    yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
    ytitle("ZLFN x Post Coefficient on Log Wages") ///
    xtitle("Centile of Income Distribution") ///
    xlabel(1 "1" 10 "10" 20 "20" 30 "30" 40 "40" 50 "50" ///
           60 "60" 70 "70" 80 "80" 90 "90" 100 "100", labsize(small)) ///
    graphregion(color(white)) bgcolor(white) ///
    grid(glcolor(gs14) glwidth(thin))

graph export "../../paper/figures/plot-did-wage-centiles-`ds'.png", replace width(4000) height(3000)

}

cap log close
