gl controls i.gender edad edadsq years_of_study hoursworked i.employed

gen post = year > 2018

reg lni i.gender edad edadsq years_of_study hoursworked i.employed indspeaker

reghdfe lni i.zona_a##i.post $controls , absorb(ubica_geo time) vce(cluster ubica_geo)

reghdfe lni i.zona_a##i.time $controls if indigenous==0, absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo graph1

reghdfe lni i.zona_a##i.time $controls if indigenous==1, absorb(ubica_geo time) vce(cluster ubica_geo)
	 eststo graph2
	 
coefplot ///
(graph1, label("Non Ind") connect(direct) lcolor(blue) lw(thin) msize(small) mfcolor(blue) mlcolor(blue) mlw(thin) ///
 keep("1.zona_a#20514.time" "1.zona_a#20545.time" "1.zona_a#20575.time" "1.zona_a#20606.time" "1.zona_a#20636.time" "1.zona_a#20667.time" "1.zona_a#20698.time" "1.zona_a#20728.time" "1.zona_a#21216.time" "1.zona_a#21244.time" "1.zona_a#21275.time" "1.zona_a#21305.time" "1.zona_a#21336.time" "1.zona_a#21366.time" "1.zona_a#21397.time" "1.zona_a#21428.time" "1.zona_a#21458.time" "1.zona_a#21946.time" "1.zona_a#21975.time" "1.zona_a#22006.time" "1.zona_a#22036.time" "1.zona_a#22067.time" "1.zona_a#22097.time" "1.zona_a#22128.time" "1.zona_a#22159.time" "1.zona_a#22189.time" "1.zona_a#22677.time" "1.zona_a#22705.time" "1.zona_a#22736.time" "1.zona_a#22766.time" "1.zona_a#22797.time" "1.zona_a#22827.time" "1.zona_a#22858.time" "1.zona_a#22889.time" "1.zona_a#22919.time")) ///
(graph2, label("Ind. Speaker") connect(direct) lcolor(red) lw(thin) msize(small) mfcolor(red) mlcolor(red) mlw(thin) ///
 keep("1.zona_a#20514.time" "1.zona_a#20545.time" "1.zona_a#20575.time" "1.zona_a#20606.time" "1.zona_a#20636.time" "1.zona_a#20667.time" "1.zona_a#20698.time" "1.zona_a#20728.time" "1.zona_a#21216.time" "1.zona_a#21244.time" "1.zona_a#21275.time" "1.zona_a#21305.time" "1.zona_a#21336.time" "1.zona_a#21366.time" "1.zona_a#21397.time" "1.zona_a#21428.time" "1.zona_a#21458.time" "1.zona_a#21946.time" "1.zona_a#21975.time" "1.zona_a#22006.time" "1.zona_a#22036.time" "1.zona_a#22067.time" "1.zona_a#22097.time" "1.zona_a#22128.time" "1.zona_a#22159.time" "1.zona_a#22189.time" "1.zona_a#22677.time" "1.zona_a#22705.time" "1.zona_a#22736.time" "1.zona_a#22766.time" "1.zona_a#22797.time" "1.zona_a#22827.time" "1.zona_a#22858.time" "1.zona_a#22889.time" "1.zona_a#22919.time")) ///
, vertical noci ///
xline(18.5, lcolor(red)) ///
yline(0, lw(thin) lpattern(solid)) ///
ytitle("Coefficient Estimate on log Earnings") ///
legend(order(1 "Non Ind" 2 "Indigenous") position(11) ring(0) col(1)) ///
xlabel(1 "01feb2016" 2 "01mar2016" 3 "01apr2016" 4 "01may2016" 5 "01jun2016" 6 "01jul2016" 7 "01aug2016" 8 "01sep2016" 9 "01oct2016" ///
      10 "01feb2018" 11 "01mar2018" 12 "01apr2018" 13 "01may2018" 14 "01jun2018" 15 "01jul2018" 16 "01aug2018" 17 "01sep2018" 18 "01oct2018" ///
      19 "01feb2020" 20 "01mar2020" 21 "01apr2020" 22 "01may2020" 23 "01jun2020" 24 "01jul2020" 25 "01aug2020" 26 "01sep2020" 27 "01oct2020" ///
      28 "01feb2022" 29 "01mar2022" 30 "01apr2022" 31 "01may2022" 32 "01jun2022" 33 "01jul2022" 34 "01aug2022" 35 "01sep2022" 36 "01oct2022", labsize(medsmall) angle(90))

