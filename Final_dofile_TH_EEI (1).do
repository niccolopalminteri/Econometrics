/********************
INDUSTRIAL ECONOMETRICS
*********************

authors: Niccolo Palminteri, Kashish Bhatia, Lou-Anne Leroux-Orphant, Lorenzo Perazzo, Tommaso Musetti
*********************/

*** first created: 26/03/2024
*** last updated:  08/05/2024


/*----------------------
Initial script configuration
-----------------------*/

	//Please replace your path in the command below
	cd "C:\Users\Test2\Downloads"
	
	// Preparing the necessary dataset
	clear all
	use EEI_TH_2024.dta
	
	// Installing packages (remove comment to install)
	*cap ssc install shp2dta 
	*cap ssc install spmap
	
	
/*----------------------
Part 1
-----------------------*/
**** Problem 1 ****
	 ** 1.a **
quietly {
	/// Data preparation ///
quietly {
	// Cleaning negative values
	foreach var in K M L W sales {
        drop if  `var'<=0
        }
		
	// Creating log transformed variables
	foreach var in real_sales real_M real_K L real_VA {
		gen ln_`var'=ln(`var')
		}
}
	/// OLS Regression Procedure ///
quietly {
	// Industry 13 //
	// Running the regression
	xi: reg ln_real_VA ln_L ln_real_K i.year i.country if sector==13
	
	// Predicting residuals and transforming exponentially transforming them
	predict ln_TFP_OLS_13, residuals
	gen TFP_OLS_13= exp(ln_TFP_OLS_13)
	
	// Plotting the density and summarizing the obtained values
	kdensity TFP_OLS_13
	sum TFP_OLS_13, d
	
	// Industry 29 //
	// Running the regression
	xi: reg ln_real_VA ln_L ln_real_K i.year i.country if sector==29
	
	// Predicting residuals and transforming exponentially transforming them
	predict ln_TFP_OLS_29, residuals
	gen TFP_OLS_29= exp(ln_TFP_OLS_29)
	
	// Plotting the density and summarizing the obtained values
	kdensity TFP_OLS_29
	sum TFP_OLS_29, d

	* Note: in both industries we find the majority of firms with low productivity and a few champions.
}	
	/// Levinsohn-Petrin Procedure ///
quietly {
	// Industry 13 //
	// Running the levpet command
	xi: levpet ln_real_VA if sector==13, free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
	
	// Predicting the desired values, describing them
	predict TFP_LP_13, omega 
	sum TFP_LP_13, d

	// Generating the log transformation of the previously generated variable
	gen ln_TFP_LP_13= ln(TFP_LP_13)
	
	// Plotting the kdensity
	kdensity ln_TFP_LP_13

	// Industry 29 //
	// Running the levpet command
	xi: levpet ln_real_VA if sector==29, free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
	estimates store LP_29


	// Predicting the desired values, describing them
	predict TFP_LP_29, omega 
	sum TFP_LP_29, d
	
	// Generating the log transformation of the previously generated variable
	gen ln_TFP_LP_29= ln(TFP_LP_29)
	
	// Plotting the kdensity
	kdensity ln_TFP_LP_29
}
	/// Wooldridge Procedure ///
quietly {
	// Industry 13 //
	// Running the prodest command
	xi: prodest ln_real_VA if sector==13, met(wrdg) free(ln_L) proxy(ln_real_M) state(ln_real_K) va
	estimates store WRDG_13

	
	// Predicting the desired values
	predict ln_TFP_WRDG_13, resid

	// Plotting the kdensity
	kdensity ln_TFP_WRDG_13
	
	// Industry 29 //
	// Running the prodest command
	xi: prodest ln_real_VA if sector==29, met(wrdg) free(ln_L) proxy(ln_real_M) state(ln_real_K) va
	estimates store WRDG_29

	
	// Predicting the desired values
	predict ln_TFP_WRDG_29, resid 
	
	// Plotting the kdensity
	kdensity ln_TFP_WRDG_29
}
}
**** Problem 3 ****
	 ** 3.a **
quietly {
	// Sector 13, Levinsohn-Petrin: cleaning data of extreme values of TFP
	egen p1_LP_13 = pctile(TFP_LP_13), p(1)
	egen p99_LP_13 = pctile(TFP_LP_13), p(99)
	replace TFP_LP_13 = . if TFP_LP_13 < p1_LP_13 | TFP_LP_13 > p99_LP_13

	// Sector 13, Wooldridge: cleaning data of extreme values of TFP
	egen p1_WRDG_13 = pctile(ln_TFP_WRDG_13), p(1)
	egen p99_WRDG_13 = pctile(ln_TFP_WRDG_13), p(99)
	replace ln_TFP_WRDG_13 = . if ln_TFP_WRDG_13 < p1_WRDG_13 | ln_TFP_WRDG_13 > p99_WRDG_13

	// Sector 29, Levinsohn-Petrin: cleaning data of extreme values of TFP
	egen p1_LP_29 = pctile(TFP_LP_29), p(1)
	egen p99_LP_29 = pctile(TFP_LP_29), p(99)
	replace TFP_LP_29 = . if TFP_LP_29 < p1_LP_29 | TFP_LP_29 > p99_LP_29

	// Sector 29, Wooldridge: cleaning data of extreme values of TFP
	egen p1_WRDG_29 = pctile(ln_TFP_WRDG_29), p(1)
	egen p99_WRDG_29 = pctile(ln_TFP_WRDG_29), p(99)
	replace ln_TFP_WRDG_29 = . if ln_TFP_WRDG_29 < p1_WRDG_29 | ln_TFP_WRDG_29 > p99_WRDG_29
	
	// Saving cleaned sample
	save EEI_TH_2024_cleaned.dta, replace
	
	// Levinsohn-Petrin: plotting the kdensity of TFP in the two industries, 
	graph twoway (kdensity TFP_LP_13, legend(label(1 "Sector 13"))) ///
              (kdensity TFP_LP_29, legend(label(2 "Sector 29"))) ///
              , title("Comparison of TFP LP per Sector")
	graph save TFP_LP_3_a, replace
	
	// Levinsohn-Petrin: plotting the kdensity of log transformed TFP in the two industries
	graph twoway (kdensity ln_TFP_LP_13, legend(label(1 "Sector 13"))) ///
              (kdensity ln_TFP_LP_29, legend(label(2 "Sector 29"))) ///
              , title("Comparison of Log-Transformed TFP LP per Sector")
	graph save ln_TFP_LP_3_a, replace

	// Wooldridge: plotting the kdensity of log transformed TFP in the two industries
	graph twoway (kdensity ln_TFP_WRDG_13, legend(label(1 "Sector 13"))) ///
              (kdensity ln_TFP_WRDG_29, legend(label(2 "Sector 29"))) ///
              , title("Comparison of Log-Transformed TFP Wooldridge per Sector")
	graph save ln_TFP_WRDG_3_a, replace
}	
	 ** 3.b **
quietly {
	// Levinsohn-Petrin - Industry 13, Plotting the TFP distribution for each country
	twoway (kdensity ln_TFP_LP_13 if country=="Italy" & sector==13, lw(medthin) lcolor(green)) ///
	|| (kdensity ln_TFP_LP_13 if country=="Spain" & sector==13,lw(medthin) lcolor(sienna)) ///
	|| (kdensity ln_TFP_LP_13 if country=="France" & sector==13,lw(medthin) lcolor(blue)), ///
	title("Productivity distributions by country, LP industry 13") legend(label(1 "Italy") label(2 "Spain") label(3 "France"))
	graph save LP_13_3_b, replace

	// Levinsohn-Petrin - Industry 29, Plotting the TFP distribution for each country
	twoway (kdensity ln_TFP_LP_29 if country=="Italy" & sector==29, lw(medthin) lcolor(green)) ///
	|| (kdensity ln_TFP_LP_29 if country=="Spain" & sector==29,lw(medthin) lcolor(sienna)) ///
	|| (kdensity ln_TFP_LP_29 if country=="France" & sector==29,lw(medthin) lcolor(blue)), ///
	title("Productivity distributions by country, LP industry 29") legend(label(1 "Italy") label(2 "Spain") label(3 "France"))
	graph save LP_29_3_b, replace


	// Wooldridge - Industry 13, Plotting the TFP distribution for each country
	twoway (kdensity ln_TFP_WRDG_13 if country=="Italy" & sector==13, lw(medthin) lcolor(green)) ///
	|| (kdensity ln_TFP_WRDG_13 if country=="Spain" & sector==13,lw(medthin) lcolor(sienna)) ///
	|| (kdensity ln_TFP_WRDG_13 if country=="France" & sector==13,lw(medthin) lcolor(blue)), ///
	title("Productivity distributions by country, WRDG industry 13") legend(label(1 "Italy") label(2 "Spain") label(3 "France"))
	graph save WRDG_13_3_b, replace


	// Wooldridge - Industry 29, Plotting the TFP distribution for each country
	twoway (kdensity ln_TFP_WRDG_29 if country=="Italy" & sector==29, lw(medthin) lcolor(green)) ///
	|| (kdensity ln_TFP_WRDG_29 if country=="Spain" & sector==29,lw(medthin) lcolor(sienna)) ///
	|| (kdensity ln_TFP_WRDG_29 if country=="France" & sector==29,lw(medthin) lcolor(blue)), ///
	title("Productivity distributions by country, WRDG industry 29") legend(label(1 "Italy") label(2 "Spain") label(3 "France"))
	graph save WRDG_29_3_b, replace

}	
	 ** 3.c **
quietly {
	// Levinsohn-Petrin Italy-Spain 2005 comparison	
	twoway (kdensity ln_TFP_LP_13 if country=="Italy" & year==2005 & sector==13, lw(medthick) lcolor(green) lp(dash)) ///
	|| (kdensity ln_TFP_LP_13 if country=="Spain" & year==2005 & sector==13,lw(medthin) lcolor(red)lp(dash)), ///
	title("Textile LP TFP 2005") legend(label(1 "Italy") label(2 "Spain"))
	graph save LP_2005_3_c, replace

	// Levinsohn-Petrin Italy-Spain 2010 comparison	
	twoway(kdensity ln_TFP_LP_13 if country=="Italy" & year==2010 & sector==13,lw(medthin) lcolor(green)) ///
	|| (kdensity ln_TFP_LP_13 if country=="Spain" & year==2010 & sector==13,lw(medthin) lcolor(red)), ///
	title("Textile LP TFP 2010") legend(label(1 "Italy") label(2 "Spain"))
	graph save LP_2010_3_c, replace


	// Wooldridge Italy-Spain 2005 comparison	
	twoway (kdensity ln_TFP_WRDG_13 if country=="Italy" & year==2005 & sector==13, lw(medthick) lcolor(green) lp(dash)) ///
	|| (kdensity ln_TFP_WRDG_13 if country=="Spain" & year==2005 & sector==13,lw(medthin) lcolor(red)lp(dash)), ///
	title("Textile WRDG TFP 2005") legend(label(1 "Italy") label(2 "Spain"))
	graph save WRDG_2005_3_c, replace

	// Wooldridge Italy-Spain 2010 Comparison
	twoway(kdensity ln_TFP_WRDG_13 if country=="Italy" & year==2010 & sector==13,lw(medthin) lcolor(green)) ///
	|| (kdensity ln_TFP_WRDG_13 if country=="Spain" & year==2010 & sector==13,lw(medthin) lcolor(red)), ///
	title("Textile WRDG TFP 2010") legend(label(1 "Italy") label(2 "Spain"))
	graph save WRDG_2010_3_c, replace

}
	 ** 3.d **
quietly {
	// Obtaining skewness statistic of the TFP distribution for both years for Italy, LP
	sum ln_TFP_LP_13 if country=="Italy" & year==2005 & sector==13, d
	sum ln_TFP_LP_13 if country=="Italy" & year==2010 & sector==13, d

	// Obtaining skewness statistic of the TFP distribution for both years for Spain, LP
	sum ln_TFP_LP_13 if country=="Spain" & year==2005 & sector==13, d
	sum ln_TFP_LP_13 if country=="Spain" & year==2010 & sector==13, d
	
	// Obtaining skewness statistic of the TFP distribution for both years for Italy, WRDG
	sum ln_TFP_WRDG_13 if country=="Italy" & year==2005 & sector==13, d
	sum ln_TFP_WRDG_13 if country=="Italy" & year==2010 & sector==13, d
	
	// Obtaining skewness statistic of the TFP distribution for both years for Spain, WRDG
	sum ln_TFP_WRDG_13 if country=="Spain" & year==2005 & sector==13, d
	sum ln_TFP_WRDG_13 if country=="Spain" & year==2010 & sector==13, d

	// Saving the cleaned dataset 
	save EEI_TH_2024_cleaned2.dta, replace
}
**** Problem 4 ****
	 ** 4.a **
quietly {
	// Constructing a firm-level measure of markups
	gen PCM = (sales-W-M)/sales
	sum PCM, de
	
	// Clearing the markup estimates from extreme values
	replace PCM=. if !inrange(PCM,r(p1),r(p99))
}	
	 ** 4.b **
quietly {
	// Providing descriptive statistics in 2006 for Italy for both industries
	twoway histogram PCM if country == "Italy" & nuts2=="ITI1" & year==2006 & sector==29, name(Histo29) xtitle("Industry 29 PCM") 
twoway histogram PCM if country == "Italy" & nuts2=="ITI1" & year==2006 & sector==13,  name(Histo13) xtitle("Industry 13 PCM") 
graph combine Histo13 Histo29
	sum PCM if sector == 13 & year == 2006 & nuts2 == "ITI1", d
	sum PCM if sector == 29 & year == 2006 & nuts2 == "ITI1"
	ttest PCM if year == 2006 & nuts2 == "ITI1", by(sector)
}

	sum if country == "Italy" & nuts2=="ITI1" & year==2006 & sector==29
	 ** 4.d **
quietly {
	// Saving the dataset before the collapsing
	save "EEI_TH_2024_V2.dta", replace
	
	// Providing descriptive statistics in 2006 for Italy for both industries
	preserve
	collapse PCM, by(country year)
	twoway line PCM year if country == "Italy" & year>2004 || line PCM year if country == "Spain" & year>2004  || line PCM year if country == "France"& year>2004 , title("Average Markup Levels Over Time") legend(label(1 "Italy")label(2 "Spain")label(3 "France"))
	graph export "PCMbycountry.pdf", replace
	restore
}
	 ** 4.e **
quietly {
	// Setting the panel variable
	xtset id_n year 
	
	// Ascertaining which industry has the highest mean PCM
	sum PCM if sector==13
	sum PCM if sector==29
	* Note: sector 13 has the highest average markup.
	
	// Generating country dummies
	tabulate country, gen(country_dummy)

	
	// Running the requested regression
	xtreg ln_TFP_WRDG_13 PCM i.year country_dummy* if sector==13
}		
/*----------------------
Part 2
-----------------------*/
**** Problem 5 ****
	** 5.a **
quietly {
	// Preparing the master dataset
	use Employment_Shares_Take_Home.dta, clear
	
	// Merging datasets
	merge m:1 year country nace using Imports_China_Take_Home
	drop _merge
	merge m:1 nace year using Imports_US_China_Take_Home
	drop _merge

	// Generating the China Shock
	sort country nace nuts2 year
	bys country nace nuts2: gen diff_imports= (real_imports_china[_n] - real_imports_china[_n-5])
	gen China_shock= (empl/tot_empl_nuts2)*(diff_imports/tot_empl_country_nace)
	
	// Generating the US China Shock for the Instrumental Variable
	bys country nace nuts2: gen diff_imports_US_China = (real_USimports_china[_n] - real_USimports_china[_n-5])
	gen US_China_shock= (empl/tot_empl_nuts2)*(diff_imports_US_China/tot_empl_country_nace)
	
	// Saving the dataset
	save Complete.dta
}	
	** 5.b **
quietly {
	// Finding the regional average shock
	use Complete.dta, clear
	collapse (sum) China_shock, by(nuts2 nuts2_name year)
	drop if China_shock == 0
	collapse China_shock, by(nuts2)
	save nuts_shock.dta
	
	// Finding the US regional average shock
	use Complete.dta, clear
	collapse (sum) US_China_shock, by(nuts2 nuts2_name year)
	drop if US_China_shock == 0
	collapse US_China_shock, by(nuts2)
	save China_US_nuts_shock.dta
}	
	** 5.c **
quietly {
	// Datasets preparation, second map
	shp2dta using ENUTS2.shp, ///
      database(regions) coordinates(regioncoord) genid(id) replace
	use regions.dta,clear
	gen nuts2 = substr(ADMIN_NAME,1,4)
	save regions.dta, replace
	use nuts_shock.dta, clear
	merge 1:m nuts2 using "regions.dta"
	drop if _merge != 3

	// Generating the map for the China Shock
	spmap China_shock using regioncoord, id(id)           ///
	fcolor(Blues2) clnumber(6) title("China Shock by nuts2 region", size(*0.8))         ///
        legstyle(2) legend(region(lcolor(black))) ///
		legend(pos(6) col(1) rowgap(0.3) size(*0.9) margin(l = 0.6 b = -0.2) title("China Shock scale", size(*0.5)) symysize(*0.9) ///
    label(7 "0.21 to 0.31")    ///
    label(6 "0.17 to 0.21")    ///
    label(5 "0.14 to 0.17")   ///
    label(4 "0.10 to 0.14")   ///
    label(3 "0.06 to 0.10")   ///
    label(2 "0.01 to 0.06"))
	graph export "Map_China_Shock.pdf", replace
	
	// Datasets preparation, second map
	use Employment_Shares_Take_Home.dta, clear
	sort nace country year
	gen manufacturing_share = (empl/tot_empl_nuts2)
	collapse manufacturing_share, by(nuts2)
	format manufacturing_share %4.3f
	save manufacturing_share.dta 
	use manufacturing_share.dta, clear
	merge 1:m nuts2 using "regions.dta"
	drop if _merge != 3
	
	// Generating the map for the share of manufacturing compared to overall employment 
	spmap manufacturing_share using regioncoord, id(id)           ///
	fcolor(Blues2) clnumber(6) title("Share of employment in manufacturing", size(*0.8))         ///
        legstyle(2) legend(region(lcolor(black))) ///
		legend(pos(6) col(1) rowgap(0.3) size(*0.9) margin(l = 0.6 b = -0.2) title("Share of employment", size(*0.5))  symysize(*0.9))
	graph export "Map_Manu_Empl.pdf", replace
}				
**** Problem 6 ****
	** 6.a **
quietly {
	// Preparing the dataset
	use EEI_TH_P6_2024.dta, clear
	*we decide to generate lags first so that we can have more information, this way we will have lags for all years when computing the regressions so the coefficients will be more accurate

	// Generating lags
	sort nuts_code year 
	egen unique_combination = group(nuts_code year)
	duplicates report unique_combination
	quietly by nuts_code year:  gen dup = cond(_N==1,0,_n)
	drop if dup !=1
	gen lag_share_tert_educ = share_tert_educ[_n-3] if year > 2002 
	gen lag_lnpop = lnpop[_n-3] if year > 2002 
	gen lag_control_gdp = control_gdp[_n-3] if year > 2002    //the GDP does not vary across years, so the lag will retrieve the same result
	keep year nuts_code lag_share_tert_educ lag_lnpop lag_control_gdp 

	// Saving the dataset
	save Nuts2_6.dta, replace

	// Merging datasets
	use EEI_TH_P6_2024.dta, clear
	merge m:1 nuts_code year using Nuts2_6
	drop _merge
	rename nuts_code nuts2
	merge m:1 nuts2 using nuts_shock.dta
	drop _merge
	merge m:1 nuts2 using China_US_nuts_shock.dta
	drop _merge

	// Creating Post-Crisis means
	keep if year >= 2014 & year <= 2017
	bysort nuts2: egen av_wage = mean(mean_uwage)
	bysort nuts2: egen av_tfp = mean(tfp) 
	
	// Regressing the post-crisis average of tfp against the region-level China shock previously constructed, controlling for the 3-year lags of population, education and gdp.
	reg av_tfp China_shock lag_lnpop lag_share_tert_educ lag_control_gdp 
}	
	** 6.b **
quietly {
	// Running an IV regression utilizing the instrumental variable built before
	ivregress 2sls av_tfp lag_lnpop lag_share_tert_educ lag_control_gdp (China_shock = US_China_shock), robust
}	
	** 6.c **
quietly {
	// Running the requested OLS regression
	reg av_wage China_shock lag_lnpop lag_share_tert_educ lag_control_gdp 

	// Running the requested IV regression
	ivregress 2sls av_wage lag_lnpop lag_share_tert_educ lag_control_gdp (China_shock = US_China_shock), robust
	
	// Running the previous OLS regression controlling for average TFP and lags of relevant variables 
	reg av_wage China_shock av_tfp lag_lnpop lag_share_tert_educ lag_control_gdp 
	
	// Running the previous IV regression controlling for average TFP and lags of relevant variables 
	ivregress 2sls av_wage av_tfp lag_lnpop lag_share_tert_educ lag_control_gdp (China_shock = US_China_shock), robust
}				
**** Problem 7 ****
	** 7.a **
quietly {
	// Preparing first dataset for merge
	use China_US_nuts_shock.dta, replace
	rename nuts2 region
	keep region US_China_shock
	gen reg2 = substr(region, 1, length(region) - 2)
	drop if reg2 != "IT"
	keep region US_China_shock
	save US_region_shock.dta, replace
	
	// Preparing second dataset for merge
	use nuts_shock.dta, clear
	rename nuts2 region
	gen reg2 = substr(region, 1, length(region) - 2)
	drop if reg2 != "IT"
	save region_shock.dta, replace
	
	// Preparing the augmented ESS dataset
	use ESS8e02_3.dta, clear
	keep if cntry=="IT"
	keep gndr agea pspwght edlvdit region prtvtbit
	gen env = .
	replace env = 25.806 if prtvtbit == 4
	replace env = 0 if prtvtbit == 1
	replace env = 13.903 if prtvtbit == 2
	replace env = 7.005 if prtvtbit == 3
	replace env = 1.671 if prtvtbit == 6
	replace env = 4.167 if prtvtbit == 5
	replace env = 6.19 if prtvtbit == 9
	replace env = 7.592 if prtvtbit == 10
	replace env = 6.19 if prtvtbit == 8	

}	
	** 7.b **
quietly {
	// Merging the first dataset from before
	merge m:1 region using region_shock.dta
	drop _merge
	
	// Merging the second dataset from before
	merge m:1 region using US_region_shock.dta
	drop _merge
	
	// Focusing on italian observations
	drop if reg2 != "IT"
	drop if edlvdit==.
}
	** 7.c **
quietly {
	// Creating education level dummy
	tabulate edlvdit, gen(edu)
	local educ edu1-edu19
	
	// Creating NUTS level 1 dummies
	gen reg1 = substr(region, 1, length(region) - 1)
	tabulate reg1, gen(region1_)
	local NUTS1 region1_1-region1_5
	
	// Running the requested regression
	regress env China_shock gndr agea `educ' `NUTS1' [pweight=pspwght], vce(cluster region)
}
	** 7.d **
quietly {
	// Running the required IV regression, second stage
	ivregress 2sls env gndr agea `educ' `NUTS1' (China_shock = US_China_shock)[pweight=pspwght], vce(cluster region)
	
	// Running the required IV regression, first stage
	ivregress 2sls env gndr agea `educ' `NUTS1' (China_shock = US_China_shock)[pweight=pspwght], vce(cluster region) first
}



