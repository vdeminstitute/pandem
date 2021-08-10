
********************************************************************************
*** REPLICATION CODE FOR:
*** PANDEMIC BACKSLIDING: VIOLATIONS OF DEMOCRATIC STANDARDS DURING COVID-19
*** PUBLISHED IN SOCIAL SCIENCE AND MEDICINE
*** https://doi.org/10.1016/j.socscimed.2021.114244
*** BY AMANDA B. EDGELL, JEAN LACHAPELLE, ANNA LUHRMANN, and SERAPHINE F. MAERZ

*** for questions - contact abedgell@ua.edu
********************************************************************************

* import dataset
use ssm_pandem.dta, clear

********************************************************************************
*** EMPIRICAL OVERVIEW SECTION
********************************************************************************



** summary measure (describing the map)
preserve
keep country_name summary_pandem 
duplicates drop
sum summary_pandem, detail
tab summary_pandem
restore



* Figure 3: Trends in the Pandemic Violations of Democratic Standards 

gen pandem_nc = F.pandem - pandem 
tab pandem_nc
gen chty = 0 if pandem_nc == 0 & F.pandem_nc==0 & F2.pandem_nc ==0 & qtime==2
replace chty = 1 if  qtime==2 & ((pandem_nc>0 & pandem_nc<.) | (F.pandem_nc>0 & F.pandem_nc<.)) 
replace chty = 2 if qtime==2 & (pandem_nc <0 | F.pandem_nc <0) 
replace chty = 3 if qtime==2 & pandem_nc>0 & pandem_nc<. & F.pandem_nc<0
replace chty = 4 if qtime==2 & pandem_nc<0 & F.pandem_nc>0 & F.pandem_nc<.


bys cid: ereplace chty = max(chty)

lab def chty 0 "No change (n=63)" 1 "Increasing (n=14)" 2 "Decreasing (n=55)" ///
				3 "Increase then decrease (n=4)" 4 "Decrease then increase (n=5)" 
lab val chty chty

				
twoway (line pandem qtime if qtime>1, connect(L) color(gs8)) (lfit pandem qtime if qtime>1, color(black) lwidth(thick) lpatt(solid)), by(chty, note("") iyaxes ixaxes legend(off) graphregion(color(white)))  ///
	xlab(2(1)4,valuelabel angle(45) nogrid) ylab(,nogrid) xti("") xsize(12) ysize(6) ///
	legend(off)  subtitle(, fcolor(white) lcolor(black)) yti("PanDem Index") graphregion(color(white))

graph export "figure3.pdf", replace

*** more on types of violations
foreach var of varlist summary_ty* {
	tab `var' if qtime==3
}

* which types of violations have no change?
lab def nc 0 "No change" 1 "Some change"
foreach var of varlist type1-type7 {
	gen `var'_nc = abs(F.`var' - `var')
	bys cid: ereplace `var'_nc = max(`var'_nc)
	replace `var'_nc = 1 if `var'_nc>1 & `var'_nc<.
	lab val `var'_nc nc
	tab `var'_nc if qtime==2
}

replace pandem_nc = abs(pandem_nc)
bys cid: ereplace pandem_nc = max(pandem_nc)
replace pandem_nc = 1 if pandem_nc>0 & pandem_nc <.
lab val pandem_nc nc

/// start -->
preserve
qui reg deaths_cpm_qrt_log ///
	pandem L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid)  
bys cid: egen sam = max(e(sample))	
keep if qtime==1 & sam==1
keep cid type*_nc pandem_nc
rename type1_nc type1
rename type2_nc type2
rename type3_nc type3
rename type4_nc type4 
rename type5_nc type5
rename type6_nc type6
rename type7_nc type7 
rename pandem_nc type8
reshape long type, i(cid) j(value)
rename type change
rename value type
lab def type 1 "Discriminatory measures" 2 "Derogations from non-derogable rights" ///
	3 "Abusive enforcement" 4 "No time limit" 5 "Limitations on legislature" ///
	6 "Official disinformation campaigns" 7 "Media limitations" 8 "PanDem Index" 
lab val type type
tab type change, row
restore
/// <- end


** Figure 4: Pandemic Violations of Democratic Standards and the V-Dem Liberal Democracy Index in 2019.
eststo clear
eststo libpandemq: reg pandem c.v2x_libdem##c.v2x_libdem, cluster(cid)
eststo libpandem: reg pandem v2x_libdem, cluster(cid)	
est stats _all

est restore libpandemq
margins, at(v2x_libdem = (0.05(0.05)0.85)) post
	** check when significant dy/dx obtained
margins, coeflegend
test _b[1bn._at] == _b[2._at]
	* not significantly different, when moving from 0.05 to 0.10
test _b[2._at] == _b[3._at]	
	* not significantly different, when moving from 0.10 to 0.15
test _b[3._at] == _b[4._at]		
	* not significantly different, when moving from 0.15 to 0.20
test _b[4._at] == _b[5._at]	
	* almost significantly different, when moving from 0.20 to 0.25
test _b[5._at] == _b[6._at]	
	* significantly different, when moving from 0.25 to 0.30
test _b[1bn._at] == _b[17._at]
	* significant difference between 0.05 and 0.85 on liberal democracy
	
twoway (scatter pandem v2x_libdem if qtime==2, ///
			m(o) mcolor("68 1 84%60")) ///
		(scatter pandem v2x_libdem if qtime==3, ///
			m(s) mcolor("42 120 142%60")) ///	
		(scatter pandem v2x_libdem if qtime==4, ///
			m(t) mcolor("122 209 81%60")) ///
	(qfitci pandem v2x_libdem, estopts(vce(cluster cid)) ///
	clpatt(solid) clcolor(black) acolor(black%40) level(90)), ///
	xlab(0(0.1)0.9, nogrid) ylab(, nogrid) ///
	xline(0.25, lcolor(black%50) lpatt(longdash)) graphregion(color(white)) /// 
	legend(order(1 "Q2 2020" 2 "Q3 2020" 3 "Q4 2020" 5 "Predicted" 4 "90% CI") ///
		rows(1) pos(6)) yti("Pandem index") xsize(7) ysize(5)
	graph export "figure4.pdf", replace
	

** checking for the rank-ordered correlation
spearman pandem v2x_libdem 


** Establishing patterns in variance based on LDI above 0.35 on PanDem
sum v2x_libdem if pandem>0.35 & pandem !=.
egen pctile = mean((v2x_libdem<`r(max)')/(v2x_libdem<.))
sum pctile 
drop pctile




** Figure 5: Percentage of countries experiencing each type of violation, March-December 2020

/// start ->
preserve
keep if qtime==3
keep cid summary_t*
duplicates drop
reshape long summary_type, j(violation) i(cid)
tab violation
lab def violation 1 "Discriminatory measures" ///
	2 "Derogations from non-derogable rights" ///
	3 "Abusive enforcement" ///
	4 "No time limit" ///
	5 "Limits on legislature" ///
	6 "Official disinformation campaigns" ///
	7 "Restrictions on media freedom" 
lab val violation violation
gen typ = 3-summary_type
lab def typ 0 "Major" 1 "Moderate" 2 "Minor" 3 "None"
lab val typ typ
graph hbar, over(typ) over(violation, relabel(1 "Discriminatory measures" ///
	2 "Derogations from non-derogable rights" ///
	3 "Abusive enforcement" ///
	4 "No time limit" ///
	5 "Limits on legislature" ///
	6 "Official disinformation campaigns" ///
	7 "Restrictions on media freedom") gap(10)) asyvars percentages stack ///
	bar(1, bcolor("68 1 84")) bar(2, bcolor("57 86 140")) ///
	bar(3, bcolor("31 152 139")) bar(4, bcolor(gs13)) ///
	graphregion(color(white)) legend(rows(1)) xsize(9) ysize(5) ///
	yti("") ylab(,nogrid) 
	graph export "figure5.pdf", replace
restore	
/// <- end







********************************************************************************
*** How do the violations of democratic standards relate to public health outcomes?
********************************************************************************


** Table 2: Logged Covid-19 deaths per million and Violations of Democratic Standards
eststo clear
eststo: reg deaths_cpm_qrt_log ///
	pandem L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid)  
eststo:	xtreg deaths_cpm_qrt_log ///
	pandem i.qtime, cluster(cid) fe dfadj
eststo: reg deaths_cpm_qrt_log  ///
	type1 type2 type3 type7 L.deaths_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid) 
eststo: reg deaths_cpm_qrt_log ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime, cluster(cid) 	
eststo: reg deaths_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime, cluster(cid) 	
	

	
** export to csv
esttab _all using "table2.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Covid-19 deaths per million (logged) and PanDem, lagged response model") ///
	nomtitles	///
	order(pandem type1 type2 type3 type4 type5 type6 type7 ///
	v2x_libdem *.v2x_libdem  ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log) drop(*.qtime)
	

** Figure 6: Comparing results for different outcome measures
	
qui eststo est1: reg deaths_cpm_qrt_log ///
	pandem L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid)  
qui eststo est2: reg deaths_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime, cluster(cid) 

	
qui eststo est3: reg excess_deaths_per_million_log  ///
	pandem L.excess_deaths_per_million_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime if owid_pscore !=., cluster(cid)  
qui eststo est4: reg excess_deaths_per_million_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.excess_deaths_per_million_log i.qtime if owid_pscore !=., cluster(cid) 	
	
	
qui eststo est5: reg cases_cpm_qrt_log  ///
	pandem L.cases_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime , cluster(cid)  	
qui eststo est6: reg cases_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.cases_cpm_qrt_log i.qtime, cluster(cid) 	
	
coefplot (est1 est2, m(O) mcolor(black) ciopt(lcolor(black)) label("Logged deaths per million")) ///
	(est3 est4, m(D) mcolor(black) ciopt(lcolor(black)) label("Logged excess deaths per million")) ///
	(est5 est6, m(T) mcolor(black) ciopt(lcolor(black)) label("Logged cases per million")), ///
	keep(pandem type1 type2 type3 type4 type5 type6 type7) ///
	legend(pos(6) rows(1) size(medsmall)) xlab(,nogrid labsize(medsmall)) ylab(,labsize(medsmall)) level(90) ///
	xline(0, lcolor(black) lpatt(vshortdash)) xsize(6) ysize(3) ///
	graphregion(color(white))
graph export "figure6.pdf", replace
	

		
		
		

********************************************************************************
*** APPENDIX
********************************************************************************
	
	* Table A1
	corr type1-type7
	pca type1-type7
	estat kmo
	
	* Figure A1
/// start ->
	tempvar dem
gen `dem' = v2x_regime>=2 & v2x_regime!=. 
replace `dem' = . if v2x_regime==.
lab def dem 0 "Autocracy" 1 "Democracy"
lab val `dem' dem
vioplot pandem, over(`dem') yti("PanDem Index") ///
	density(fcolor(none)) xsize(5) ysize(5)
lab drop dem	
graph export "figureA1.pdf", replace
/// <- end

	* Figure A2: LDI and reported deaths per million logged
est restore est1
sum v2x_libdem if e(sample)
margins, at(v2x_libdem=(0.05(0.1)0.85)) post
test _b[1._at] == _b[9._at]
test _b[1._at] == _b[5._at]

marginsplot, ///
	xlab(0(0.1)1) ciopts(recast(rarea) acolor(gs8%60)) plotopts(recast(line)) ///
	yti("Deaths per million (logged)") xti("Liberal democracy index") level(90) ///
	ti("") ///
	legend(order(2 "Predicted" 1 "90% CI") rows(1) pos(6)) ///	
		addplot(scatter deaths_cpm_qrt_log v2x_libdem if qtime==2, ///
			m(o) mcolor("68 1 84%60") || ///
		scatter deaths_cpm_qrt_log v2x_libdem if qtime==3, ///
			m(s) mcolor("42 120 142%60") || ///	
		scatter deaths_cpm_qrt_log v2x_libdem if qtime==4, ///
			m(t) mcolor("122 209 81%60") ||, below ///
			legend(order(3 "Q2" 4 "Q3" 5 "Q4" 2 "Predicted" 1 "90%CI") rows(1) pos(6))) ///
			graphregion(color(white)) xsize(7) ysize(5)
			
graph export "figureA2.pdf", replace			
			
	


** Table A2: Results with liberal democracy excluded
eststo clear
eststo: reg deaths_cpm_qrt_log  ///
	pandem L.deaths_cpm_qrt_log   ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid)  
eststo: reg deaths_cpm_qrt_log  ///
	type1 type2 type3 type7 L.deaths_cpm_qrt_log   ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid) 
eststo: reg deaths_cpm_qrt_log ///
	type4 type5 type6 type7  ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime, cluster(cid) 	
eststo: reg deaths_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime, cluster(cid) 	
	
** export to latex
esttab _all using "tableA2.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Results with liberal democracy excluded") ///
	nomtitles	///
	order(pandem type1 type2 type3 type4 type5 type6 type7 ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log) drop(*.qtime)
	
	

	
** Figure A3: Results for Covid-19 deaths predicting PanDem (i.e. reverse causality)

eststo clear
eststo: reg pandem L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type1 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type2 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type3 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type4 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
eststo: reg type5 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
eststo: reg type6 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
eststo: reg type7 L.deaths_cpm_qrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)		

eststo: reg pandem deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type1 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type2 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type3 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)
eststo: reg type4 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
eststo: reg type5 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
eststo: reg type6 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
eststo: reg type7 deaths_cpm_pqrt_log  ///
	c.v2x_libdem##c.v2x_libdem i.qtime, cluster(cid)	
	


coefplot (est1, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est9, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Pandem index", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6))  xlab(-0.015(0.005)0.015,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(pandem, replace) swap yscale(off) level(90) xsize(6) ysize(6) graphregion(color(white))

	
coefplot (est2, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est10, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Discriminatory measures", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6))  xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type1, replace)	swap yscale(off) level(90) graphregion(color(white))
	
coefplot (est3, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est11, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Derogations from non-derogable rights", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6))  xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type2, replace)		swap yscale(off) level(90) graphregion(color(white))
	
coefplot (est4, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est12, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Abusive enforcement", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6)) xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type3, replace)	swap yscale(off) level(90) graphregion(color(white))
	
coefplot (est5, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est13, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("No time limit", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6))  xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type4, replace)	swap yscale(off) level(90) graphregion(color(white))

coefplot (est6, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est14, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Limitations on legislature", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6)) xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type5, replace)	swap yscale(off) level(90) graphregion(color(white))
	 
coefplot (est7, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est15, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Official disinformation", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6)) xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type6, replace)	swap yscale(off) level(90) graphregion(color(white))
	 
coefplot (est8, m(Oh) mcolor(black) ciopt(lcolor(black)) ///
				label("Lagged deaths per million (logged)")) ///
	(est16, m(Sh) mcolor(black) ciopt(lcolor(black)) ///
		label("Cumulative deaths per million (logged)")), ///
	keep(L.deaths_cpm_qrt_log deaths_cpm_pqrt_log) ///
	ti("Media limitations", box fcolor(white) bexpand) ///
	coeflabels(L.deaths_cpm_qrt_log = "Lagged deaths per million (logged)") ///
	legend(pos(6)) xlab(-.2(0.1)0.2,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type7, replace)	swap yscale(off) level(90) graphregion(color(white))
	
grc1leg pandem type1 type2 type3 type4 type5 type6 type7, rows(2) graphregion(color(white))
graph di, xsize(16) ysize(7)
graph export "figureA3.pdf", replace


** Figure A4: Heterogeneity across quarters	
eststo clear
eststo: reg deaths_cpm_qrt_log  ///
	c.pandem##i.qtime L.deaths_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index, cluster(cid)  
eststo: reg deaths_cpm_qrt_log  ///
	c.type1##i.qtime c.type2##i.qtime c.type3##i.qtime c.type7##i.qtime ///
	L.deaths_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index, cluster(cid) 
eststo: reg deaths_cpm_qrt_log ///
	c.type4##i.qtime c.type5##i.qtime c.type6##i.qtime c.type7##i.qtime  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log, cluster(cid) 	
eststo: reg deaths_cpm_qrt_log c.type1##i.qtime c.type2##i.qtime c.type3##i.qtime ///
	c.type4##i.qtime c.type5##i.qtime c.type6##i.qtime c.type7##i.qtime  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log, cluster(cid) 	
	
est restore est1 
margins qtime, dydx(pandem) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("PanDem index", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(,nogrid) xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(pandem, replace) level(90) graphregion(color(white)) ///
	ciopts(lcolor(black))

	

est restore est4
margins qtime, dydx(type1) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("Discriminatory measures", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type1, replace) level(90) graphregion(color(white)) ///
	 ciopts(lcolor(black))

est restore est4
margins qtime, dydx(type2)  
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("Derogations from non-derogable rights", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type2, replace)	level(90) graphregion(color(white)) ///
	ciopts(lcolor(black))

	
est restore est4
margins qtime, dydx(type3) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("Abusive enforcement", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type3, replace)	level(90) graphregion(color(white)) ///
	 ciopts(lcolor(black))

	
est restore est4
margins qtime, dydx(type4) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("No time limit", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type4, replace)	level(90) graphregion(color(white)) ///
	ciopts(lcolor(black))

	 
	
est restore est4
margins qtime, dydx(type5) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("Limitations on legislature", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type5, replace)	level(90) graphregion(color(white)) ///
	ciopts(lcolor(black))

	
est restore est4
margins qtime, dydx(type6) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("Official disinformation campaigns", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type6, replace)	level(90) graphregion(color(white)) ///
	ciopts(lcolor(black))

	
	
est restore est4
margins qtime, dydx(type7) 
marginsplot, horizontal plotopts(connect(i) mcolor(black) msymbol(oh)) recastci(rspike) ///
	ti("Media limitations", box fcolor(white) bexpand) yti("") ///
	xti("") xlab(-1(0.5)1,nogrid) ///
	xline(0, lcolor(black) lpatt(vshortdash)) ///
	name(type7, replace)	level(90) graphregion(color(white)) ///
	 ciopts(lcolor(black))

	
graph combine  pandem type1 type2 type3 type4 type5 type6 type7, rows(2) ///
		ycommon  xsize(15) ysize(7) graphregion(color(white))

graph export "figureA4.pdf", replace	



** Table A3: Simple cross-sectional pooled OLS 

eststo clear
eststo: reg deaths_cum_per_million_log  ///
	summary_pandem    ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index if date_td == mdy(12,28, 2020), robust
eststo: reg deaths_cum_per_million_log  ///
	summary_type1 summary_type2 summary_type3 summary_type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index if date_td == mdy(12,28, 2020), robust
eststo: reg deaths_cum_per_million_log ///
	summary_type4 summary_type5 summary_type6 summary_type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index if date_td == mdy(12,28, 2020), robust
eststo: reg deaths_cum_per_million_log summary_type1 summary_type2 summary_type3 ///
	summary_type4 summary_type5 summary_type6 summary_type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index if date_td == mdy(12,28, 2020), robust
	
** export to latex
esttab _all using "tableA3.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Simple cross-sectional pooled OLS for total deaths as of 14 December 2020") ///
	nomtitles	///
	order(summary_pandem summary_type1 summary_type2 summary_type3 summary_type4 summary_type5 summary_type6 summary_type7 ///
	v2x_libdem *.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index)

	
	
** Table A4: Results with logged excess deaths per million as outcome


* using the excess deaths per million (logged)
eststo clear
sort cid qtime
eststo: reg excess_deaths_per_million_log  ///
	pandem L.excess_deaths_per_million_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime if owid_pscore !=., cluster(cid)  
eststo: reg excess_deaths_per_million_log  ///
	type1 type2 type3 type7 L.excess_deaths_per_million_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime if owid_pscore !=., cluster(cid) 
eststo: reg excess_deaths_per_million_log ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.excess_deaths_per_million_log i.qtime if owid_pscore !=., cluster(cid) 	
eststo: reg excess_deaths_per_million_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.excess_deaths_per_million_log i.qtime if owid_pscore !=., cluster(cid) 	
** export to latex
esttab _all using "tableA4.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Using Excess Deaths Per Million as the Outcome") ///
	nomtitles	///
	order(pandem type1 type2 type3 type4 type5 type6 type7 ///
	v2x_libdem *.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.excess_deaths_per_million_log) drop(*.qtime)
	

	
* Figure A7: Quadratic relationship between liberal democracy in 2019 and excess deaths within the quarter
est restore est1
sum v2x_libdem if e(sample)
margins, at(v2x_libdem=(0.05(0.1)0.85)) post
test _b[1._at] == _b[9._at]
test _b[1._at] == _b[5._at]

marginsplot, ///
	xlab(0(0.1)1) ciopts(recast(rarea) acolor(gs8%60)) plotopts(recast(line)) ///
	yti("Logged excess deaths per million") xti("Liberal democracy index") level(90) ///
	ti("") xsize(7) ysize(5)	///
	legend(order(2 "Predicted" 1 "90% CI") rows(1) pos(6)) ///	
		addplot(scatter excess_deaths_per_million_log v2x_libdem if qtime==2, ///
			m(o) mcolor("68 1 84%60") || ///
		scatter excess_deaths_per_million_log v2x_libdem if qtime==3, ///
			m(s) mcolor("42 120 142%60") || ///	
		scatter excess_deaths_per_million_log v2x_libdem if qtime==4, ///
			m(t) mcolor("122 209 81%60") ||, below ///
			legend(order(3 "Q2" 4 "Q3" 5 "Q4" 2 "Predicted" 1 "90%CI") rows(1) pos(6))) ///
			graphregion(color(white))
					
graph export "figureA7.pdf", replace		
			
	
* Table A5: Results with p-scores as the outcome
eststo clear
eststo: reg owid_pscore_log  ///
	pandem L.owid_pscore_log  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid)  
eststo: reg owid_pscore_log  ///
	type1 type2 type3 type7 L.owid_pscore_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime, cluster(cid) 
eststo: reg owid_pscore_log ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.owid_pscore_log i.qtime, cluster(cid) 	
eststo: reg owid_pscore_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.owid_pscore_log i.qtime , cluster(cid) 	
** export to latex
esttab _all using "tableA5.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Using P-scores as the Outcome") ///
	nomtitles	///
	order(pandem type1 type2 type3 type4 type5 type6 type7 ///
	v2x_libdem *.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.owid_pscore_log) drop(*.qtime)
	
		
* Figure A8: Quadratic relationship between liberal democracy in 2019 and p-score within the quarter
	
est restore est1
sum v2x_libdem if e(sample)
margins, at(v2x_libdem=(0.05(0.1)0.85)) post
test _b[1._at] == _b[9._at]
test _b[1._at] == _b[5._at]

marginsplot, ///
	xlab(0(0.1)1) ciopts(recast(rarea) acolor(gs8%60)) plotopts(recast(line)) ///
	yti("Logged p-score)") xti("Liberal democracy index") level(90) ///
	ti("") xsize(7) ysize(5)	///
	legend(order(2 "Predicted" 1 "90% CI") rows(1) pos(6)) ///	
		addplot(scatter owid_pscore_log v2x_libdem if qtime==2, ///
			m(o) mcolor("68 1 84%60") || ///
		scatter owid_pscore_log v2x_libdem if qtime==3, ///
			m(s) mcolor("42 120 142%60") || ///	
		scatter owid_pscore_log  v2x_libdem if qtime==4, ///
			m(t) mcolor("122 209 81%60") ||, below ///
			legend(order(3 "Q2" 4 "Q3" 5 "Q4" 2 "Predicted" 1 "90%CI") rows(1) pos(6))) ///
			graphregion(color(white))

						
graph export "figureA8.pdf", replace			
	
* Table A6: Replication of main results with sample constrained to Tables A4 and A5 
eststo clear
eststo: reg deaths_cpm_qrt_log  ///
	pandem L.deaths_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime if owid_pscore !=., cluster(cid)  
eststo: reg deaths_cpm_qrt_log  ///
	type1 type2 type3 type7 L.deaths_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime if owid_pscore !=., cluster(cid) 
eststo: reg deaths_cpm_qrt_log ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime if owid_pscore !=., cluster(cid) 	
eststo: reg deaths_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime if owid_pscore !=., cluster(cid) 
** export to latex
esttab _all using "tableA6.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Re-estimation of Main Results with Constrained Sample from Excess Deaths and P-Scores") ///
	nomtitles	///
order(pandem type1 type2 type3 type4 type5 type6 type7 ///
	v2x_libdem *.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log) drop(*.qtime) 
	
	

	
** Figure A5: Regional Differences in samples for deaths vs. excess deaths
sort cid qtime
mark main_sample
markout main_sample pandem v2x_libdem share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index deaths_cpm_qrt_log  L.deaths_cpm_qrt_log 
mark owid_sample
markout owid_sample owid_pscore v2x_libdem share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index deaths_cpm_qrt_log L.deaths_cpm_qrt_log 

	* run these lines together to obtain a bar graph showing regional distribution 
/// start -> 
preserve 
contract e_regionpol_6C main_sample, freq(freq) 
keep if main_sample==1
gen pc = freq/414*100
format pc %9.2f
list
tempfile main
save `main'
restore
preserve
contract e_regionpol_6C owid_sample, freq(freq)
keep if owid_sample==1
gen pc = freq/188*100
format pc %9.2f
list
tempfile owid
save `owid'
restore
preserve
append using `owid'
append using `main'
replace e_regionpol_6C = e_regionpol_6C+0.4 if pc !=. & owid_sample==1


twoway (bar pc e_regionpol_6C if main_sample==1, bcolor(gs3) barw(0.5)) ///
	(bar pc e_regionpol_6C if owid_sample==1, bcolor(gs8) barw(0.5)), ///
	xlab(1.2 "EECA" 2.2 "LAC" 3.2 "MENA" 4.2 "SSA" 5.2 "WENA" 6.2 "AP", nogrid) ///
	xmtick(0.5 6.7, notick) yti("Percent of sample") ylab(0(10)40,nogrid) ///
	legend(order(1 "Main sample (n=414, 138 countries)" 2 "OWID sample (n=205, 71 countries)") ///
		position(6) rows(2)) xti("") xsize(6) ysize(4) graphregion(color(white))
		graph export "figureA5.pdf", replace
restore	
/// <- end

* Table A7: Differences of means tests for observations excluded and included in reduced sample for Tables A4 and A5
eststo clear
replace owid_sample = . if main_sample==0
estpost ttest deaths_cpm_qrt_log cases_cpm_qrt_log pandem  ///
						v2x_libdem share_older resp_disease_prev life_exp_2017 ///
						health_exp_pc detect_index, by(owid_sample) unequal 
esttab using "tableA7.html", replace ///
	noobs cells("mu_1(fmt(2)) mu_2(fmt(2)) b(star fmt(2)) se(fmt(2)) count(fmt(0))") ///
	star(* 0.1 ** .05 *** 0.01) collabels("Mean Excluded" "Mean Included" "Diff." "Std. Error" "Obs.") label
	
* Figure A6: Evidence of potentially influential outliers in excess deaths data
extremes excess_deaths_per_million, iqr
graph box excess_deaths_per_million, over(qtime) ///
	marker(1,mlab(cid) mlabsize(vsmall) m(oh) mlabcolor(black) mcolor(black)) medline(lcolor(black)) intensity(0) ///
	xsize(6) ysize(3) graphregion(color(white)) box(1, lcolor(black))
	graph export "figureA6.pdf", replace

** Table A8: Using cases per million logged as the outcome
eststo clear
eststo: reg cases_cpm_qrt_log  ///
	pandem L.cases_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime , cluster(cid)  
eststo: reg cases_cpm_qrt_log  ///
	type1 type2 type3 type7 L.cases_cpm_qrt_log   ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index i.qtime , cluster(cid) 
eststo: reg cases_cpm_qrt_log ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.cases_cpm_qrt_log i.qtime , cluster(cid) 	
eststo: reg cases_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.cases_cpm_qrt_log i.qtime, cluster(cid) 
	

** export to latex
esttab _all using "tableA8.html", replace ///
	label star(* 0.1 ** 0.05 *** 0.01) nobase ///
	alignment(D{.}{.}{-1}) se(2) ar2(2) aic bic sfmt(2) b(2) nodepvars ///
	title("Using Logged Cases Per Million as the Outcome") ///
	nomtitles	///
	order(pandem type1 type2 type3 type4 type5 type6 type7 ///
	v2x_libdem *.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.cases_cpm_qrt_log)	drop(*.qtime)

** Figure A9: Quadratic relationship between liberal democracy in 2019 and logged cases per million within the quarter
	
est restore est1
sum v2x_libdem if e(sample)
margins, at(v2x_libdem=(0.05(0.1)0.85)) post
test _b[1._at] == _b[9._at]
test _b[1._at] == _b[5._at]

marginsplot, ///
	xlab(0(0.1)1) ciopts(recast(rarea) acolor(gs8%60)) plotopts(recast(line)) ///
	yti("Cases per million (logged)") xti("Liberal democracy index") level(90) ///
	ti("") xsize(7) ysize(5)	///
	legend(order(2 "Predicted" 1 "90% CI") rows(1) pos(6)) ///	
		addplot(scatter cases_cpm_qrt_log v2x_libdem if qtime==2, ///
			m(o) mcolor("68 1 84%60") || ///
		scatter cases_cpm_qrt_log v2x_libdem if qtime==3, ///
			m(s) mcolor("42 120 142%60") || ///	
		scatter cases_cpm_qrt_log  v2x_libdem if qtime==4, ///
			m(t) mcolor("122 209 81%60") ||, below ///
			legend(order(3 "Q2" 4 "Q3" 5 "Q4" 2 "Predicted" 1 "90%CI") rows(1) pos(6))) ///
			graphregion(color(white))
						
graph export "figureA9.pdf", replace	
			

	
	
** Figure A10: Results when removing one moderate or major discrimination violation at a time
levelsof cid if type1>1 & e(sample), local(levels)
eststo clear
foreach l of local levels 	{
	eststo: reg deaths_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime if cid!=`l', cluster(cid) 				
}		
eststo: reg deaths_cpm_qrt_log type1 type2 type3 ///
	type4 type5 type6 type7  ///
	c.v2x_libdem##c.v2x_libdem ///
	share_older resp_disease_prev life_exp_2017 health_exp_pc ///
	detect_index L.deaths_cpm_qrt_log i.qtime, cluster(cid) 

coefplot est*, keep(type1) asequation vertical swapnames eqrename(est1 = –BGR ///
	est2 = –BIH est3= –GRC est4 = –LBN est5= –LKA est6= –MYS est7= –PAN est8= –PER est9= –QAT ///
	est10= –SAU est11= –SGP est12= –SRB est13= –SVK est14=–UGA est15=Full) label level(90) ///
	legend(off) mcolor(black) ciopts(lcolor(black)) graphregion(color(white)) yti("Coefficient") ///
	xsize(8) ysize(5)
	graph export "figureA10.pdf", replace
			


