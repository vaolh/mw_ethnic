*************************************************
***************** Clean Memory ******************
*************************************************

clear
cap clear
cap log close
set more off

*** REPLICATION FILE: plot-event-exp-subgroup-year.do
*** STATA VERSION:    StataNow 19.5
*** AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
*** DATE:             2026-05-11

*** Year-cadence event-study plots for a curated set of "marquee"
*** subgroups that illustrate the willingness-to-pay-for-quality story.
*** Plots SHARE outcomes (composition, not levels) split by ethnicity
*** stratum (hli==1 vs hli==0, indig==1 vs indig==0).
***
*** Output:
***   paper/figures/event/plot-event-exp-<sub>-<stratum>-year.png
***
*** Specification: same i.zlfn##ib2018.year as plot-event-wage-ethnic.do
***                and uses the yearcoefs helper to build the matrix.

cap mkdir log
log using "log/plot-event-exp-subgroup-year.log", replace text

include _helpers.do
do read-hhlevel-exp-subgroup-year.do

cap mkdir "../../paper/figures/event"

*** Marquee subgroups (curated for the quality narrative).
local MARQUEE ///
    food_meat_beef food_meat_pork food_meat_poultry ///
    food_outside bev_coffee_tea_cocoa ///
    clothing_men clothing_women clothing_kids ///
    durables_appliances durables_furniture ///
    rec_tourism rec_culture_media ///
    transport_intercity transport_local ///
    health_consultations health_pharmacy

local year_xlabel 1 "2016" 2 "2018" 3 "2020" 4 "2022" 5 "2024"

foreach s of local MARQUEE {
    cap confirm variable share_`s'
    if _rc {
        di as text "marquee `s' not in memory — skipping"
        continue
    }

    foreach stratum in nhli hli nind indig {
        local cond ""
        local color "31 119 180"
        local label "Non-HLI"
        if "`stratum'" == "nhli" {
            local cond  "if hli_hh == 0"
            local color "31 119 180"
            local label "Non-HLI"
        }
        if "`stratum'" == "hli" {
            local cond  "if hli_hh == 1"
            local color "214 39 40"
            local label "HLI"
        }
        if "`stratum'" == "nind" {
            local cond  "if indig_hh == 0"
            local color "31 119 180"
            local label "Non-Indig"
        }
        if "`stratum'" == "indig" {
            local cond  "if indig_hh == 1"
            local color "214 39 40"
            local label "Indigenous"
        }

        cap reghdfe share_`s' i.zlfn##ib2018.year $controls `cond', ///
            absorb(ubica_geo year) vce(cluster ubica_geo)
        if _rc {
            di as text "regression failed for `s' × `stratum' — skipping"
            continue
        }
        eststo `s'_`stratum'
        yearcoefs, eq(`s'_`stratum')

        coefplot matrix(__year_b), se(__year_se) vertical ///
            recast(connected) lcolor("`color'") mcolor("`color'") msymbol(circle) lw(medthin) msize(small) ///
            ciopts(recast(rarea) fcolor("`color'%30") lwidth(none)) ///
            xline(2.5, lcolor(gs10) lpattern(dash)) ///
            yline(0, lw(thin) lpattern(solid) lcolor(black)) ///
            ytitle("Coefficient on share of HH expenditure") ///
            xtitle("Year") ///
            xlabel(`year_xlabel') ///
            title("`s' - `label'", size(small)) ///
            graphregion(color(white)) bgcolor(white) ///
            grid(glcolor(gs14) glwidth(thin))
        graph export "../../paper/figures/event/plot-event-exp-`s'-`stratum'-year.png", ///
            replace width(4000) height(3000)
    }
}

cap log close
