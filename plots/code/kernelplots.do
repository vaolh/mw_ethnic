collapse (mean) lni, by(id year)

*************************************************
******************* Graph 1 *********************
*************************************************

twoway (kdensity lni if year == 2016, bw(0.4) color(blue) lwidth(thin) lcolor(blue) ///
             legend(label(1 "ENIGH 2016"))) ///
       (kdensity lni if year == 2018, bw(0.4) color(red) lwidth(thin) lcolor(red) ///
             legend(label(2 "ENIGH 2018"))) ///
       (kdensity lni if year == 2020, bw(0.4) color(green) lwidth(thin) lcolor(green) ///
             legend(label(3 "ENIGH 2020"))) ///
       (kdensity lni if year == 2022, bw(0.4) color(orange) lwidth(thin) lcolor(orange) ///
             legend(label(4 "ENIGH 2022"))), ///
       xlabel(, grid) ///
       ylabel(, grid) ///
       xtitle("Log of Income") ///
       legend(order(1 "ENIGH 2016" 2 "ENIGH 2018" 3 "ENIGH 2020" 4 "ENIGH 2022") ///
              position(1) ring(0) colfirst) ///
       title("Log Real Income Density by Year")

