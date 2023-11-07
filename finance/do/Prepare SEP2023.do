
global path "F:\Consulting\TME finance code\"
// set latest year of data reported. This is the budget for the year data were reported.
scalar latest_year=2023
// set base year for constant dollar calculation (default is year of the report)
scalar base_year=latest_year-1
set type double
	
cd "$path"

// to start with reported view or "financing" which seems to not include all past fixes.
use "$path\financing.dta", clear
	merge m:1 iso2 using country_info, update replace force nogen // For instances where country info changes eg in Jul updates
	merge 1:1 iso3 year using outcomes, update replace force nogen 
	merge 1:1 iso3 year using estimates, update replace force nogen 
	merge 1:1 iso3 year using estimates_mdr, update replace force nogen 
	merge 1:1 iso3 year using population, update replace force nogen 
	merge 1:1 iso3 year using notifications, update replace force nogen
	merge 1:1 iso3 year using mdr_among_notified, update replace force nogen 
	merge 1:1 iso3 year using wb.dta, update replace force nogen 
drop if country==""	
keep if year >=2013

	** FILLING IN MISSING NOTIFICATIONS
	//After speaking with PG we decided to not attempt to scale up notifications.
	// Instead we will use the previous year's notification
	sort iso3 year
	by iso3: replace c_notified=c_notified[_n-1] if c_notified==.
	
// Percentage of TB burden for base year
egen percent_of_cases=pc(c_notified) if year==base_year
gsort country - year
by country: replace percent_of_cases = percent_of_cases[_n-1] if percent_of_cases==.
	
do "$path/2023clean.do"  /* This includes manual fixing of country data in that year*/

/*************************************************************************************************************************
Fill in missing received funding. First with expenditures, then with that year's committed, 
then with previous year's spending. Also backwards fill for early years. Assume reported 0 totals are missing.
*************************************************************************************************************************/

replace rcvd_tot=. if rcvd_tot==0
replace exp_tot=. if exp_tot==0
replace cf_tot=. if cf_tot==0

//Manually setting countries where not enough data was available to accurately fill in rcvd_tot. Code should be developed to automatically detect these in future.
// These countries are Dominica, Grenada, and Uzbekistan who have last reported data from 2014, 2016, and 2018, respectively.
gen do_not_fill=0
replace do_not_fill=1 if inlist(iso3, "DMA", "GRD", "UZB")


gen rcvd_imputation="Reported data" if rcvd_tot !=. & year !=latest_year
replace rcvd_imputation = "Expenditure" if rcvd_tot==. & exp_tot!=. & year !=latest_year & do_not_fill!=1
replace rcvd_tot=exp_tot if rcvd_tot==. & exp_tot!=. & year !=latest_year  & year !=latest_year & do_not_fill!=1

bysort country (year): replace rcvd_imputation = "Committed funding" if rcvd_tot==. & cf_tot!=.  & year !=latest_year & do_not_fill!=1
bysort country (year): replace rcvd_tot = cf_tot if rcvd_tot==. & cf_tot!=. & year !=latest_year & do_not_fill!=1

bysort country (year): replace rcvd_imputation = "Lagged received" if rcvd_tot==. & rcvd_tot[_n-1]!=. & year !=latest_year & do_not_fill!=1
bysort country (year): replace rcvd_tot = rcvd_tot[_n-1] if rcvd_tot==. & rcvd_tot[_n-1]!=. & year !=latest_year & do_not_fill!=1

// fill holes in earlier years making all blanks equal to next year with rcvd_tot
gsort country - year
by country: replace rcvd_imputation = "Future year's received" if rcvd_tot==. & rcvd_tot[_n-1]!=. & year !=latest_year & do_not_fill!=1
by country: replace rcvd_tot = rcvd_tot[_n-1] if rcvd_tot==. & rcvd_tot[_n-1]!=. & year !=latest_year & do_not_fill!=1

replace rcvd_imputation="No data" if rcvd_imputation==""
sort country year

	//FILLIN MISSING COUNTRY NAMES, ISO2 CODES, ISO_NUMERIC VALUES, GROUPINGS
	foreach var of varlist iso3 iso_numeric country g_whoregion g_hb_tb g_hb_tbhiv g_hb_mdr g_income {
	gsort iso2 - `var'
	by iso2: carryforward (`var'), replace
	}
	
	sort country year
	destring e_pop_num, replace

	capture drop _merge
	merge 1:1 iso3 year using weoreptc, force update replace 
	drop if _merge==2
	drop _merge

	** PN 2021: deflator_us variable isn't updated for 22 countries (those who're not in WEO database). Malually update these (use code from WEO section above)
	tab iso3 year if deflator_us == . & year >= base_year
	bysort year: egen deflator_us_replacer =mode(deflator_us) // Use most common value per year, since some few countries may have old data still which is not updated 
	replace deflator_us = deflator_us_replacer // PN: This ensures uniform deflation for USD values across all countries. Keep it this way.
	drop deflator_us_replacer

	// ADJUSTMENTS TO WB AND IMF DATA
	// PN Jul 2023: 8 countries had their WB income group changed in Jul 2023. Confirmed that these are updated on country_info.dta
	// https://blogs.worldbank.org/opendata/new-world-bank-group-country-classifications-income-level-fy24
	// PN Jul 2022: 6 countries had their WB income group changed in Jul 2022. Confirmed that these are updated on country_info.dta
	// https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2022-2023
	
	replace imf_gdp_pc_cur_usd = ny_gdp_pcap_cd if inlist(iso3, "PSE", "ASM", "SYR", "CUB")
	
	// 1B. GDP Per capita, in International PPP adjusted dollars.
	// This is primarily obtained from the WB dataset and is used as the principal input for choice model,
	// but a few countries don't have values.

	misstable summ ny_gdp_pcap_pp_cd imf_gdp_pc_cur_int if year > 2010 & iso3== "YEM" // "SYR", "YEM", "CUB","ERI","PRK"
	replace ny_gdp_pcap_pp_cd = imf_gdp_pc_cur_int if inlist(iso3, "SYR", "YEM", "CUB", "ERI","PRK")

	// 2. OFFICIAL EXCHANGE RATES 
	* PN 2023: Compared exchange rates from GHED but realise that these are drawn from WB dataset, and are 1 year behind 
	* Decision is to use WB's exchange rates.
	
	/* IN 2022, there were missing exchange rate values for the following non HICs
	
	misstable summ pa_nus_fcrf if year == base_year & g_income != "HIC"
	
	                                                              Obs<.
                                                +------------------------------
               |                                | Unique
      Variable |     Obs=.     Obs>.     Obs<.  | values        Min         Max
  -------------+--------------------------------+------------------------------
   pa_nus_fcrf |        29                 107  |     84        .71    23271.21
  -----------------------------------------------------------------------------

	*/    
	rename pa_nus_fcrf off_exch_rate
	merge 1:1 iso3 year using treasury, keepus(un_pa_exch_rate) // Include the un exchange rate variable called un_pa_exch_rate
	drop if _merge==2
	drop _merge
	// Discrepancies (missing values) in WB exchange rates 
	li iso3 year off_exch_rate un_pa_exch_rate if off_exch_rate == . & year > 2012 & year < latest_year & g_income != "HIC"
	replace off_exch_rate=un_pa_exch_rate if off_exch_rate==. & inlist(iso3, "CUB","PRK", "SYR", "SOM", "TKM")
	
	// Missing 2021 and 2022 exchange rates, that can be updated. Apart from  IRN is not updared
	li iso3 year off_exch_rate un_pa_exch_rate if inlist(iso3, "AFG","COD", "ETH", "GIN", "IRQ", "MMR") & year > 2012 & year < latest_year & g_income != "HIC"
	replace off_exch_rate=un_pa_exch_rate if off_exch_rate==. & inlist(iso3, "AFG","COD", "ETH", "GIN","IRQ", "MMR")
	li iso3 year off_exch_rate un_pa_exch_rate if inlist(iso3, "GUY","KGZ", "LBR", "LKA", "MNG", "MRT","MWI") & year > 2012 & year < latest_year & g_income != "HIC"
	replace off_exch_rate=un_pa_exch_rate if off_exch_rate==. & inlist(iso3, "GUY","KGZ", "LBR", "LKA", "MNG", "MRT","MWI")
	 li iso3 year off_exch_rate un_pa_exch_rate if inlist(iso3, "NGA","PNG","SLB","SSD","STP","TUR","TZA") & year > 2002 & year < latest_year & g_income != "HIC"
	 replace off_exch_rate=un_pa_exch_rate if off_exch_rate==. & inlist(iso3, "NGA","PNG","SLB","SSD","STP","TUR","TZA")

	// Further discrepancies - we carry previous year's value forward
	li iso3 year off_exch_rate un_pa_exch_rate if inlist(iso3, "IRN", "VEN","ZWE") & year > 2012 & year < latest_year & g_income != "HIC"
	replace off_exch_rate=1 if iso3 =="ZWE" & year >= 2007 & year <= 2022 //ZWE adopted USD as curr in 2008, and reverted to ZWE bonds in 2018
	replace off_exch_rate= 9.975 if iso3 == "VEN" & year >= 2018 & year < latest_year //Venezuela's hyperinflatoin of 2016 makes period average exchange rates invalid
	replace off_exch_rate = off_exch_rate[_n-1] if off_exch_rate == . & iso3 == "IRN" & year > 2018 & year < latest_year 	//PN 2022: Iran's exchange rate is currently carried forward from last year
	
	
	// PN 2022: Cuba's exchange rate is govt controlled at 1 to the dollar for state agencies. Fill backwards
	replace off_exch_rate = 1 if iso3 == "CUB" & off_exch_rate == . // updates years 2010 to 2014
	*replace off_exch_rate = 1 if iso3 == "CUB" & (year == 2021 | year == 2022) //To check again in 2023
	replace off_exch_rate = off_exch_rate[_n-1] if iso3 == "IRN" &  year == 2022
	
	// PN 2023: PSE uses the Israeli Shekhel as currency (but there are parallel currencies eg JOR dinar, and USD) https://excellencenter.org/currency-and-money/
	gen temp_var1 = off_exch_rate if iso3 == "ISR"
	bys year: egen temp_var2 = min(temp_var1)
	replace off_exch_rate = temp_var2 if iso3 == "PSE"
	drop temp_var1 temp_var2
	sort country year
	
	// 3. PPP in WB's dataset (pa_nus_ppp) compared to IMF.
	** PN 2022: From 2020 we use WB's variable for the unit cost computation (instead of the IMF predicted values). 
	** However WB's data has more missing values - see list below	 
	* We may use imf_ppp_conv for some specific country changes (having checked that these specific changes are reasonable)
	li year iso3 pa_nus_ppp imf_ppp_conv if pa_nus_ppp == . & imf_ppp_conv != . & year < latest_year & g_income != "HIC"
	replace pa_nus_ppp = imf_ppp_conv if imf_ppp_conv != . & inlist(iso3, "AND", "CUB", "DJI", "ERI", "PRK" , "SSD" ,"SYR" ,"YEM", "VEN") 
	replace pa_nus_ppp = imf_ppp_conv if pa_nus_ppp == . & iso3 == "SOM" & year <= 2010
	* We leave VEN and ZWE  with the 2017 value since the exchange rate is also carried forward from 2017 for now 
	replace pa_nus_ppp = 2.6809826 if iso3 == "VEN" & year >= 2018 & year <= latest_year
	replace pa_nus_ppp = 1.032 if iso3 == "ZWE" & year >= 2018 & year <= latest_year
	
		* Sierra leone. WB provides exchange rate in rebased currency (1 SLL = 1000 previous SLL
	* convert pa_nus_ppp which hasn't yet been rebased to new currency.
	replace pa_nus_ppp = pa_nus_ppp / 1000 if iso3 == "SLE" & pa_nus_ppp > 10 // previous range was 600 - 3600. Desired range is below 10 for now.

	
	// 4. DEFLATOR (IMF) - imf_deflator (from WEO NGDP_D) versus (WB) - ny_gdp_defl_zs
	** PN 2023: Decision to use IMF deflator based on guidance from the Discussion paper on macroeconomic data sources for GHED
	** "IMF WEO is suggested for its completeness, stability, abundant metadata and the consistency that it brings to the indicators involving GDP"
	** Source "Indikadahena CK, Brindley C, Xu K, Roubal T. Sources of macro-economic data for global health expenditure indicators. Geneva: World Health Organization; 2018."

	li iso3 year ny_gdp_defl_zs imf_deflator deflator if deflator==. & year > 2012 & year < 2023 & g_income != "HIC"
	// In the 2023 run, there are 5 countries whose IMF deflator (the principal variable to use) is missing for 2022 and therefore couldn't compute rebasing ratio 
	// AFG, ASM, CUB, LBN, PRK, PSE and SYR
	replace imf_deflator = ny_gdp_defl_zs if inlist(iso3, "PSE", "ASM", "SYR", "CUB")

	** In the unit cost script, this is dealt with by projecting the log transformed variable
	
	* PN 2023: replace ZWE  and VEN latest deflator values (for 2019 and 2020) with missing, 
	* to aid UC script to interpolate (challenge with hyperinflation 2018 -20)
	* (2019 value was 992%, 2020 was 6537%)
	 replace ny_gdp_defl_zs = . if iso3 == "ZWE" & (year >= 2019 & year <= latest_year) 
	 replace imf_deflator = . if iso3 == "ZWE" & (year >= 2019 & year <= latest_year) 
	
	 replace ny_gdp_defl_zs = . if iso3 == "VEN" & (year >= 2017 & year <= latest_year) 
	 replace imf_deflator = . if iso3 == "VEN" & (year >= 2017 & year <= latest_year) 
	
	
	** PN 2022: As opposed to last year when we used IMF deflator, this time we revert to WB even though it is not yet rebased to 2018 - it's using 2014 base)
	//replace deflator_wb = deflator if iso3 == "BLR"
	* PN 2022: SSD data in WB reports only till 2015. IMF projects from 2019 onwards but results in outlier values (14295% in 2022)
	* As done in 2021, replace South Sudan values with IMF reported values (WB values missing) but only until 2018
	//replace ny_gdp_defl_zs = imf_deflator if iso3 == "SSD" & year <= 2018 & ny_gdp_defl_zs == . // from 2019, IMF values are projected and not real data points

	* PN 2022: Sudan's deflator reported is high at 20225% in 2020 (due to the political situation). Change to missing this year
	//replace ny_gdp_defl_zs = . if iso3 == "SDN" & (year == 2020 | year == 2021) 
	
	* PN Jul 16 2022 Other countries (AFG, LKA, LBY, ARG, VEN) look consistent based on a comparison with last year.
	
	// RENAME COUNTRIES FOR WHICH COMMAS CREATE PROBLEMS WHEN WE EXPORT TO CSV FILES
	replace country="China (Hong Kong SAR)" if country=="China, Hong Kong SAR"
	replace country="China (Macao SAR)" if country=="China, Macao SAR"
	replace country="Bonaire Saint Eustatius and Saba" if country=="Bonaire, Saint Eustatius and Saba"

	// CORRECTIONS TO AUTO-CALCULATION OF FIELDS IN THE ONLINE FORM
	// FOR THE TOTALS AND ROW SUBTOTALS, WE DO NOT ALLOW ZEROES
	foreach source in gap exp budget cf rcvd{
	 replace `source'_tot=. if `source'_tot==0 
	}
	
	// REPLACE NEGATIVE GAPS WITH MISSING WHEN TOTAL REQUIRED IS NOT REPORTED
	foreach var in prog fld lab staff sld mdrmgt tpt tbhiv patsup orsrvy oth tot{ 
	replace gap_`var'=. if gap_`var' < 0 
	}

	save finance_clean.dta, replace
	
	
/******************************************************************************************************************
 Calculate GHS costs with unit costs.
********************************************************************************************************************/

	// UTILIZATION OF GENERAL HEALTH-CARE SERVICES (GHS) IGB revised 26.06.13 I hid ZAF, BRA sent to AP check if 2012 needs adj!!
	//Importing IER unit costs from Christopher's 2012. Source: finance_pred_cint_dta 7/25/2012. Since I have not run PART III, I will use financingadj1

	use finance_clean.dta, clear
	do "$path/unit_costs_2023-08-22.do" //PN: Updated do file
	keep iso3 year ny_gdp_pcap_pp_cd off_exch_rate pa_nus_ppp uc_* deflator
	drop if iso3==""
	duplicates list iso3 year

	save unit_cost.dta, replace

	use finance_clean, clear
	merge 1:1 iso3 year using unit_cost.dta, keepusing(uc* deflator) update replace nogen
		
	/****************************** 
		Clean utilization data
	******************************/		
	// Missing outpatient utilization data set to previous year.  Also MDR patients on treatment
	foreach var in hcfvisit_mdr  hospd_mdr_prct hospd_mdr_dur hcfvisit_dstb hospd_dstb_prct hospd_dstb_dur mdr_tx {
	sort iso3 year
	quietly by iso3: replace `var'=`var'[_n-1]  if `var'==.
	gsort iso3 - year
	quietly by iso3: replace `var'=`var'[_n-1]  if `var'==. & year <= latest_year
	sort iso3 year
	}
	
	gen neg_mdr_tx = -mdr_tx
	// rowtotal ensures that missing values in one variable are treated as a zero
	egen c_notif_less_mdr = rowtotal(c_notified neg_mdr_tx), missing 
	drop neg_mdr_tx 
	// This replacement will only affect records before 2008, where some HICs eg LVA were reporting separate DS and DR numbers.
	replace c_notif_less_mdr = c_notified if c_notif_less_mdr < 0 
	
	//Calculate # of bed days (Aug 2023: using c_notif_less_mdr for ds calculation)
	replace beddays_nmdr=(c_notif_less_mdr)*hospd_dstb_prct/100*hospd_dstb_dur if year >= 2015
	replace beddays_mdr=mdr_tx*hospd_mdr_prct/100*hospd_mdr_dur if year >= 2015

	//Calculate # of bed days - South Sudan exception . SP/SN utilization blank
	replace hcfvisit_dstb=23 if iso3=="SSD" & year <2015
	replace beddays_nmdr=(c_notif_less_mdr)*hospd_dstb_prct/100*hospd_dstb_dur if iso3=="SSD"
	replace beddays_mdr=mdr_tx*hospd_mdr_prct/100*hospd_mdr_dur if iso3=="SSD"

	// Fill in bed days with previous years. 
	foreach var in beddays_nmdr beddays_mdr {
		quietly by iso3 (year), sort: replace `var'=`var'[_n-1]  if `var'==.
	}
	// Fill in MDR visits with previous ones
	quietly by iso3 (year), sort: replace mdr_tx = mdr_tx[_n-1] if mdr_tx==.

	// Make remaining missing utilization equal to 0
	foreach var in beddays_nmdr beddays_mdr hcfvisit_dstb hcfvisit_mdr {
	replace `var'=0 if `var'==. 
	}

	/* Replace number of visits to 1 per patient if currently missing
	replace hcfvisit_dstb =1 if hcfvisit_dstb==. & c_notified!=. & c_notified>=1
	replace hcfvisit_mdr=1 if hcfvisit_mdr==. & mdr_tx!=. & mdr_tx>=1
	// Replace hospital LOS to 1 day if percent of hospitalized is reported but LOS is missing.
	replace hospd_dstb_dur=1 if hospd_dstb_dur==. & hospd_dstb_prct!=0 & hospd_dstb_prct!=.  & c_notified!=. & c_notified>=1
	replace hospd_mdr_dur=1 if hospd_mdr_dur==. & hospd_mdr_prct!=0 & hospd_mdr_prct!=.  & mdr_tx!=. & mdr_tx>=1
	// This set of changes only adjusted HICs as of Aug 2023. Exclude until we can find a better proxy than 1.
	*/
	
	// Calculate GHS
	//NMDR for 2014 onwards (plus SSD's exceptional circumstance)
	replace c_clinic_nmdr=(c_notif_less_mdr)*hcfvisit_dstb*uc_visit_cur_usd if (year >= 2014 | iso3=="SSD")
	replace c_hospital_nmdr=beddays_nmdr*uc_bedday_nmdr_cur_usd if (year >= 2014 | iso3=="SSD")
	
	// MDR-TB
	replace c_clinic_mdr=mdr_tx*hcfvisit_mdr*uc_visit_cur_usd if (year >= 2014 | iso3=="SSD")
	replace c_hospital_mdr= beddays_mdr*uc_bedday_mdr_cur_usd if (year >= 2014 | iso3=="SSD")

	/*****************************
		  GHS SUBTOTALS 
	****************************/
	///GHS Non-MDR and MDR split
	capture drop c_ghs_nmdr c_ghs_mdr c_ghs_inpatient c_ghs_outpatient c_ghs
	
	egen c_ghs_nmdr= rowtotal(c_clinic_nmdr c_hospital_nmdr), missing 
	label variable c_ghs_nmdr "cost of GHS: non-mdr"
	egen c_ghs_mdr= rowtotal(c_clinic_mdr c_hospital_mdr), missing
	label variable c_ghs_mdr "cost of GHS: mdr"

	//GHS Inpatient and Outpatient Split
	egen c_ghs_inpatient = rowtotal(c_hospital_nmdr c_hospital_mdr), missing 
	egen c_ghs_outpatient = rowtotal(c_clinic_nmdr c_clinic_mdr), missing
	label var c_ghs_inpatient  "Total Inpatient (DS-TB and MDR-TB) Cost"
	label var c_ghs_outpatient  "Total Outpatient (DS-TB and MDR-TB) Cost"
	
	//TOTAL GHS
	egen c_ghs= rowtotal(c_ghs_nmdr c_ghs_mdr), missing
	label variable c_ghs "Cost of GHS: total"
	
 	 /* FIX  MDR BREAKDOWN FOR COUNTRIES WHERE GHS IS THOUGH TO BE INCLUDED IN NTP REPORTING 
     33% of beddays were for MDR in 2013, 2014 around 10% in 2008-2012. The proportion has increased to 71% 2018 and 83.2% in 2020 . Now 93% 
	 Andrew and Taghreed discussed and felt it better to split by estimated GHS costs ratio for MDR and Non-MDR as this includes visit data
	 as well as hopsital level/staffing mix. It was also felt this hsould be done for those ocuntries with GHS thought to be included in NTP reported
	 spending. This was only done for countries where we had some information on which category the GHS costs were being reported as (from Lela).
	 It is unclear which categories for China and Turkmenistan contain GHS so not adjustment for them. */
     
     /* Note also that latest data on service utilisation is linked to the expenditure year, not the budget year, i.e. latest_year-1, and then
	carry that forward to latest_year to do the budget adjustments */

	foreach country in RUS CHN KAZ TKM ARM AZE BLR KGZ TJK UKR GEO{
 	gen float mdr_pct_`country' = c_ghs_mdr/(c_ghs_nmdr+c_ghs_mdr) if iso3=="`country'"
    replace mdr_pct_`country'=mdr_pct_`country'[_n-1] if year==latest_year
	}

// Countries where GHS is believed to be in Staff and Other: Russia, KAZ
   foreach country in RUS KAZ{
   foreach type in rcvd exp{
     gen long `type'_staff_temp=(`type'_staff * mdr_pct_`country') if iso3=="`country'"
     gen long `type'_oth_temp=(`type'_oth * mdr_pct_`country') if  iso3=="`country'"
	 
     egen `type'_mdrmgt_temp=rowtotal(`type'_mdrmgt `type'_staff_temp `type'_oth_temp ), missing
     replace `type'_mdrmgt=`type'_mdrmgt_temp if iso3=="`country'"
     replace `type'_staff= round(`type'_staff * (1-mdr_pct_`country'), 1) if iso3=="`country'"
     replace `type'_oth= round(`type'_oth * (1-mdr_pct_`country'), 1) if iso3=="`country'"
	 }

     /* Adjust budget tables*/
    foreach type in budget cf gap{
     gen long `type'_staff_temp=(`type'_staff * mdr_pct_`country') if iso3=="`country'"  
	 gen long `type'_oth_temp=(`type'_oth * mdr_pct_`country') if iso3=="`country'"     
     
     egen `type'_mdrmgt_temp=rowtotal(`type'_mdrmgt `type'_staff_temp `type'_oth_temp ), missing

     replace `type'_mdrmgt=`type'_mdrmgt_temp if iso3=="`country'"
     replace `type'_staff= round(`type'_staff * (1-mdr_pct_`country'), 1) if iso3=="`country'"
     replace `type'_oth= round(`type'_oth * (1-mdr_pct_`country'), 1) if iso3=="`country'"  
	 drop *_temp
	}
	}

	// Countries where GHS is believed to be in Other only starting from 2019 spending: ARM, KGZ, TJK, UKR
	// add in GEO for all years
	foreach country in ARM KGZ TJK UKR GEO{
	foreach type in rcvd exp{
     gen long `type'_oth_temp=(`type'_oth * mdr_pct_`country') if  iso3=="`country'"
	 
     egen `type'_mdrmgt_temp=rowtotal(`type'_mdrmgt `type'_oth_temp ), missing
     replace `type'_mdrmgt=`type'_mdrmgt_temp if (iso3=="`country'" & year >=2019) | iso3=="GEO"
     replace `type'_oth= round(`type'_oth * (1-mdr_pct_`country'), 1) if (iso3=="`country'" & year >=2019) | iso3=="GEO"
	 drop *_temp
	 }

     /* Adjust budget tables */
    foreach type in budget cf gap{
	 gen long `type'_oth_temp=(`type'_oth * mdr_pct_`country') if iso3=="`country'"     
     egen `type'_mdrmgt_temp=rowtotal(`type'_mdrmgt `type'_oth_temp ), missing
     replace `type'_mdrmgt=`type'_mdrmgt_temp if (iso3=="`country'" & year >=2020) | iso3=="GEO"
     replace `type'_oth= round(`type'_oth * (1-mdr_pct_`country'), 1) if (iso3=="`country'" & year >=2020) | iso3=="GEO" 
	 drop *_temp
	}
	}
	
	// AZERBIJAN & BELARUS STAFF AND PROGRAMME MANGEMENT CATEGORIES ALL BELIEVED TO INCLUDE GHS
	foreach country in AZE BLR{
	foreach type in rcvd exp{
     gen long `type'_staff_temp=(`type'_staff * mdr_pct_`country') if iso3=="`country'"
     gen long `type'_prog_temp=(`type'_prog * mdr_pct_`country') if  iso3=="`country'"
	 
     egen `type'_mdrmgt_temp=rowtotal(`type'_mdrmgt `type'_staff_temp `type'_prog_temp ), missing
     replace `type'_mdrmgt=`type'_mdrmgt_temp if iso3=="`country'"
     replace `type'_staff= round(`type'_staff * (1-mdr_pct_`country'), 1) if iso3=="`country'"
	 replace `type'_prog= round(`type'_prog * (1-mdr_pct_`country'), 1) if iso3=="`country'"
	 drop *_temp
	  }

     /* Adjust budget tables*/
    foreach type in budget cf gap{
     gen long `type'_staff_temp=(`type'_staff * mdr_pct_`country') if iso3=="`country'"  
 	 gen long `type'_prog_temp=(`type'_prog * mdr_pct_`country') if iso3=="`country'"  
	 
     egen `type'_mdrmgt_temp=rowtotal(`type'_mdrmgt `type'_staff_temp `type'_prog_temp ), missing

     replace `type'_mdrmgt=`type'_mdrmgt_temp if iso3=="`country'"
     replace `type'_staff= round(`type'_staff * (1-mdr_pct_`country'), 1) if iso3=="`country'"
	 replace `type'_prog= round(`type'_prog * (1-mdr_pct_`country'), 1) if iso3=="`country'"  
	 drop *_temp
	}
	}

	//SET GHS to 0 for countries where it is believed to be already reported from Lela's work 
	foreach var of varlist c_clinic* c_hospital* c_ghs* {
	replace `var' = 0 if (iso3=="RUS" | iso3=="CHN" | iso3=="KAZ" | iso3 == "TKM" | ///
	iso3=="ARM" | iso3=="AZE" | iso3=="BLR" | iso3=="KGZ" |iso3=="TJK" | iso3=="UKR"| iso3=="GEO")
	format `var' %20.0g
	}
 
	/********************************
		CALCULATE SHARES
	*********************************/
	// INT, EXT
	foreach var in varlist rcvd_int rcvd_int_sh rcvd_ext rcvd_ext_sh cf_int cf_int_sh cf_ext cf_ext_sh{
	capture drop `var'
	}

	/*CF*/
	egen cf_int=rowtotal(cf_tot_gov cf_tot_loan) if cf_tot !=. & cf_tot !=0 & year <=2014, missing
	replace cf_int=cf_tot_domestic if year >=2015 
	gen cf_int_sh=cf_int/cf_tot
	replace cf_int_sh=1 if cf_int_sh>1 & cf_int_sh!=.
	replace cf_int_sh=0 if cf_int_sh<0
	
	//if cf_int_sh = missing set it to previous year
	sort iso3 year
	bysort iso3:replace cf_int_sh=cf_int_sh[_n-1] if cf_int_sh==.
		
	// ASSUME ALL HIGH-INCOME ARE 100% INTERNALLY FINANCED (IF SHARE IS MISSING)
	replace cf_int_sh=1 if cf_int_sh==. & g_income=="HIC" 
	//Use Share if type_int is missing
	replace cf_int_sh=1 if cf_int_sh==. & iso3=="RUS" 
	// same for Albania which was a HIC and was moved to UMC in 2020, but still uses short form 
	replace cf_int_sh=1 if cf_int_sh==.& iso3 == "ALB" 
	//Replace CF_INT using share if missing
	replace cf_int=cf_int_sh*cf_tot if cf_int==.
	gen cf_ext_sh=1-cf_int_sh

	//if cf_ext_sh = missing set it to previous year
	bysort iso3 year:replace cf_ext_sh=cf_ext_sh[_n-1] if cf_ext_sh==.
	gen cf_ext=cf_ext_sh*cf_tot 
	
		/* RCVD*/
	egen rcvd_int= rowtotal(rcvd_tot_gov rcvd_tot_loan) if rcvd_tot !=. & rcvd_tot !=0 & year <=2013, missing
	replace rcvd_int= rcvd_tot_domestic if year >=2014
	gen rcvd_int_sh=rcvd_int/rcvd_tot
	replace rcvd_int_sh=1 if rcvd_int_sh>1 & rcvd_int_sh!=.
	replace rcvd_int_sh=0 if rcvd_int_sh<0 
	
	// ASSUME ALL HIGH-INCOME ARE 100% INTERNALLY FINANCED (IF SHARE IS MISSING)
	replace rcvd_int_sh=1 if rcvd_int_sh==. & g_income=="HIC"
	// PN Jul 2020: Even when some countries not classified as HIC, we replace missing share as 100% domestic. 6 islans added by AS in AUG 2023.
	replace rcvd_int_sh=1 if rcvd_int_sh==. & inlist(iso3, "RUS", "ALB", "COM", "CUB", "DMA", "GRD", "LCA", "WSM")
	// if no INT but some EXT: 0% rcvd_int share
	replace rcvd_int_sh = 0 if rcvd_int_sh == . & (rcvd_tot_gf != . | rcvd_tot_usaid != . | rcvd_tot_grnt != .) & rcvd_tot_sources != 0 & year >= 2014	 	
	//PN May2020: if rcvd_int_sh is still missing set it to previous year
	by iso3 (year), sort: replace rcvd_int_sh=rcvd_int_sh[_n-1] if rcvd_int_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace rcvd_int_sh=cf_int_sh if cf_int_sh!=. & rcvd_int_sh==.
	//Replace RCVD_INT using share if missing
	replace rcvd_int=rcvd_int_sh*rcvd_tot if rcvd_int==.
	
	//GEN RCVD_EXT and set to 1 minus rcvd_int
	gen rcvd_ext_sh=1-rcvd_int_sh 
	//if rcvd_ext_sh = missing set it to previous year
	bysort iso3 (year):replace rcvd_ext_sh=rcvd_ext_sh[_n-1] if rcvd_ext_sh==.
	gen rcvd_ext=rcvd_ext_sh*rcvd_tot 
		
	// EXT_GF, EXT_NGF
	foreach var in varlist rcvd_ext_gf rcvd_ext_gf_sh rcvd_ext_ngf rcvd_ext_ngf_sh cf_ext_gf cf_ext_gf_sh cf_ext_ngf cf_ext_ngf_sh{
	capture drop `var'
	}

	//CF
	egen cf_ext_gf=rowtotal(cf_tot_gf) if cf_tot!=. & cf_tot!=0 , missing
	gen cf_ext_gf_sh=cf_ext_gf/cf_ext
	replace cf_ext_gf_sh=. if cf_ext_gf_sh>1 & cf_ext_gf_sh!=.
	replace cf_ext_gf_sh=. if cf_ext_gf_sh<0
	sort iso3 year
    bysort iso3 : replace cf_ext_gf_sh=cf_ext_gf_sh[_n-1] if cf_ext_gf_sh==.
	bysort iso3 : replace cf_ext_gf_sh=cf_ext_gf_sh[_n+1] if cf_ext_gf_sh==.
	replace cf_ext_gf = cf_ext_gf_sh*cf_ext if cf_ext_gf!=.
	gen cf_ext_ngf_sh=1-cf_ext_gf_sh
	gen cf_ext_ngf=cf_ext_ngf_sh*cf_ext
	
	//RCVD
	egen rcvd_ext_gf=rowtotal(rcvd_tot_gf) if rcvd_tot!=. & rcvd_tot!=0 , missing
	gen rcvd_ext_gf_sh=rcvd_ext_gf/rcvd_ext
	replace rcvd_ext_gf_sh=. if rcvd_ext_gf_sh>1 & rcvd_ext_gf_sh!=.
	replace rcvd_ext_gf_sh=. if rcvd_ext_gf_sh<0
    sort iso3 year
	bysort iso3: replace rcvd_ext_gf_sh=rcvd_ext_gf_sh[_n-1] if rcvd_ext_gf_sh==.
	bysort iso3: replace rcvd_ext_gf_sh=rcvd_ext_gf_sh[_n+1] if rcvd_ext_gf_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace rcvd_ext_gf_sh=cf_ext_gf_sh if cf_ext_gf_sh!=. & rcvd_ext_gf_sh==.
	replace rcvd_ext_gf=rcvd_ext_gf_sh*rcvd_ext if rcvd_ext_gf==.
	gen rcvd_ext_ngf_sh=1-rcvd_ext_gf_sh
	gen rcvd_ext_ngf=rcvd_ext_ngf_sh*rcvd_ext
	
	
	foreach type in exp {
	capture drop `type'_all_lines
	egen `type'_all_lines=rowtotal(`type'_fld `type'_staff `type'_prog `type'_lab `type'_tbhiv `type'_sld `type'_mdrmgt `type'_patsup `type'_orsrvy `type'_tpt `type'_oth) 
	}
	foreach type in budget {
	capture drop `type'_all_lines
	egen `type'_all_lines=rowtotal(`type'_fld `type'_staff `type'_prog `type'_lab `type'_tbhiv `type'_sld `type'_mdrmgt `type'_patsup `type'_orsrvy `type'_tpt `type'_oth) 
	}

	/*****************************************************************
	* To more fully understand the mapping between line items and our
	* reported intervention areas created below see the PPTX file on:
	* J:\t-TME\UnitData\Financing 2015.
	*****************************************************************/

	// MDR, NMDR
	foreach var in varlist exp_mdr exp_mdr_sh exp_nmdr exp_nmdr_sh budget_mdr budget_mdr_sh budget_nmdr budget_nmdr_sh{
	capture drop `var'
	}

	// making correction for those countries in which the sum of the inner matrix does not come to the sum of the 1st column and/or last row
	// we are effectively assuming that if the inner matrix total differs from the outer matrix total, we use the outer matrix total but determine shares using the inner matrix

	foreach var in varlist rcvd_mdr rcvd_mdr_sh rcvd_nmdr rcvd_nmdr_sh cf_mdr cf_mdr_sh cf_nmdr cf_nmdr_sh{
	capture drop `var'
	}
	
	foreach type in  cf rcvd {
	egen `type'_mdr=rowtotal(`type'_sld `type'_mdrmgt) if `type'_tot !=0 & `type'_tot !=., missing
	//do not allow piece to be greater than parent.
	replace `type'_mdr = `type'_tot if `type'_mdr!=. & `type'_tot!=. & `type'_mdr > `type'_tot
	gen `type'_mdr_sh=`type'_mdr/`type'_tot
	replace `type'_mdr_sh=1 if `type'_mdr_sh>1 & `type'_mdr_sh!=.
	replace `type'_mdr_sh=0 if `type'_mdr_sh<0
    sort iso3 year
    bysort iso3 : replace `type'_mdr_sh=`type'_mdr_sh[_n-1] if `type'_mdr_sh==.
	bysort iso3 : replace `type'_mdr_sh=`type'_mdr_sh[_n+1] if `type'_mdr_sh==.	
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace `type'_mdr_sh=cf_mdr_sh if cf_mdr_sh!=. & `type'_mdr_sh==.
	replace `type'_mdr=`type'_mdr_sh*`type'_tot if `type'_mdr==.
	gen `type'_nmdr_sh=1-`type'_mdr_sh
	gen `type'_nmdr=`type'_nmdr_sh*`type'_tot
	}
	
	foreach type in exp budget {
	egen `type'_mdr=rowtotal(`type'_sld `type'_mdrmgt) if `type'_tot!=. & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_mdr = `type'_tot if `type'_mdr!=. & `type'_tot!=. & `type'_mdr > `type'_tot
	gen `type'_mdr_sh=`type'_mdr/`type'_tot
	replace `type'_mdr_sh=1 if `type'_mdr_sh>1 & `type'_mdr_sh!=.
	replace `type'_mdr_sh=0 if `type'_mdr_sh<0
    sort iso3 year
    bysort iso3 : replace `type'_mdr_sh=`type'_mdr_sh[_n-1] if `type'_mdr_sh==.
	bysort iso3 : replace `type'_mdr_sh=`type'_mdr_sh[_n+1] if `type'_mdr_sh==.
	replace `type'_mdr=`type'_mdr_sh * `type'_tot if `type'_mdr==.
	gen `type'_nmdr_sh=1-`type'_mdr_sh
	gen `type'_nmdr=`type'_nmdr_sh*`type'_tot
	}

	// MDR_SLD, MDR_NSLD
	foreach var in varlist exp_mdr_sld exp_mdr_sld_sh exp_mdr_nsld exp_mdr_nsld_sh budget_mdr_sld budget_mdr_sld_sh budget_mdr_nsld budget_mdr_nsld_sh {
	capture drop `var'
	}

	foreach type in exp budget {
	egen `type'_mdr_sld=rowtotal(`type'_sld) if `type'_tot!=. & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_mdr_sld = `type'_mdr if `type'_mdr_sld!=. & `type'_mdr!=. & `type'_mdr_sld > `type'_mdr
	gen `type'_mdr_sld_sh=`type'_mdr_sld/`type'_mdr
	replace `type'_mdr_sld_sh=1 if `type'_mdr_sld_sh>1 & `type'_mdr_sld_sh!=.
	replace `type'_mdr_sld_sh=0 if `type'_mdr_sld_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_mdr_sld_sh=`type'_mdr_sld_sh[_n-1] if `type'_mdr_sld_sh==.
	bysort iso3 : replace `type'_mdr_sld_sh=`type'_mdr_sld_sh[_n+1] if `type'_mdr_sld_sh==.
	replace `type'_mdr_sld=`type'_mdr_sld_sh*`type'_mdr if `type'_mdr_sld==.
	gen `type'_mdr_nsld_sh=1-`type'_mdr_sld_sh
	gen `type'_mdr_nsld=`type'_mdr_nsld_sh*`type'_mdr
	}

	foreach var in varlist rcvd_mdr_sld rcvd_mdr_sld_sh rcvd_mdr_nsld rcvd_mdr_nsld_sh cf_mdr_sld cf_mdr_sld_sh cf_mdr_nsld cf_mdr_nsld_sh {
	capture drop `var'
	}

	foreach type in  cf rcvd {
	egen `type'_mdr_sld=rowtotal(`type'_sld) , missing
	//do not allow piece to be greater than parent.
	replace `type'_mdr_sld = `type'_mdr if `type'_mdr_sld!=. & `type'_mdr!=. & `type'_mdr_sld > `type'_mdr
	gen `type'_mdr_sld_sh=`type'_mdr_sld/`type'_mdr
	replace `type'_mdr_sld_sh=1 if `type'_mdr_sld_sh>1 & `type'_mdr_sld_sh!=.
	replace `type'_mdr_sld_sh=0 if `type'_mdr_sld_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_mdr_sld_sh=`type'_mdr_sld_sh[_n-1] if `type'_mdr_sld_sh==.
    bysort iso3 : replace `type'_mdr_sld_sh=`type'_mdr_sld_sh[_n+1] if `type'_mdr_sld_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace `type'_mdr_sld_sh=cf_mdr_sld_sh if cf_mdr_sld_sh!=. & `type'_mdr_sld_sh==.
	replace `type'_mdr_sld = `type'_mdr_sld_sh * `type'_mdr if `type'_mdr_sld==.
	gen `type'_mdr_nsld_sh=1-`type'_mdr_sld_sh
	gen `type'_mdr_nsld=`type'_mdr_nsld_sh*`type'_mdr
	}

	// NMDR_DOT, NMDR_NDOT
	// By DOT we mean FLD, staff, programme management, labs and (pre-2005) variables for buildings and "other". 

	foreach var in varlist exp_nmdr_dot exp_nmdr_dot_sh exp_nmdr_ndot exp_nmdr_ndot_sh ///
	budget_nmdr_dot budget_nmdr_dot_sh budget_nmdr_ndot budget_nmdr_ndot_sh {
	capture drop `var'
	}

	foreach type in exp budget{
	egen `type'_nmdr_dot= rowtotal(`type'_fld `type'_staff  `type'_prog `type'_lab ) if `type'_tot!=.  ///
	& `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_dot = `type'_nmdr if `type'_nmdr_dot!=. & `type'_nmdr!=. & `type'_nmdr_dot > `type'_nmdr
	gen `type'_nmdr_dot_sh=`type'_nmdr_dot/`type'_nmdr
	replace `type'_nmdr_dot_sh=1 if `type'_nmdr_dot_sh>1 & `type'_nmdr_dot_sh!=.
	replace `type'_nmdr_dot_sh=0 if `type'_nmdr_dot_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_dot_sh=`type'_nmdr_dot_sh[_n-1] if `type'_nmdr_dot_sh==.
	bysort iso3 : replace `type'_nmdr_dot_sh=`type'_nmdr_dot_sh[_n+1] if `type'_nmdr_dot_sh==.
	replace `type'_nmdr_dot= `type'_nmdr_dot_sh * `type'_nmdr if `type'_nmdr_dot==.
	gen `type'_nmdr_ndot_sh=1-`type'_nmdr_dot_sh
	gen `type'_nmdr_ndot= `type'_nmdr_ndot_sh*`type'_nmdr
	}

	foreach var in varlist rcvd_nmdr_dot rcvd_nmdr_dot_sh rcvd_nmdr_ndot rcvd_nmdr_ndot_sh ///
	cf_nmdr_dot cf_nmdr_dot_sh cf_nmdr_ndot cf_nmdr_ndot_sh {
	capture drop `var'
	}

	foreach type in cf rcvd{
	egen `type'_nmdr_dot= rowtotal(`type'_fld `type'_staff `type'_prog `type'_lab), missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_dot = `type'_nmdr if `type'_nmdr_dot!=. & `type'_nmdr!=. & `type'_nmdr_dot > `type'_nmdr
	gen `type'_nmdr_dot_sh=`type'_nmdr_dot/`type'_nmdr
	replace `type'_nmdr_dot_sh=1 if `type'_nmdr_dot_sh>1 & `type'_nmdr_dot_sh!=.
	replace `type'_nmdr_dot_sh=0 if `type'_nmdr_dot_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_dot_sh=`type'_nmdr_dot_sh[_n-1] if `type'_nmdr_dot_sh==.
	bysort iso3 : replace `type'_nmdr_dot_sh=`type'_nmdr_dot_sh[_n+1] if `type'_nmdr_dot_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace `type'_nmdr_dot_sh=cf_nmdr_dot_sh if cf_nmdr_dot_sh!=. & `type'_nmdr_dot_sh==.
	replace `type'_nmdr_dot=`type'_nmdr_dot_sh *`type'_nmdr if `type'_nmdr_dot==.
	gen `type'_nmdr_ndot_sh=1-`type'_nmdr_dot_sh
	gen `type'_nmdr_ndot= `type'_nmdr_ndot_sh*`type'_nmdr
	}

	// NMDR_DOT_FLD, NMDR_DOT_NFLD
	foreach var in varlist exp_nmdr_dot_fld exp_nmdr_dot_fld_sh exp_nmdr_dot_nfld exp_nmdr_dot_nfld_sh ///
	budget_nmdr_dot_fld budget_nmdr_dot_fld_sh budget_nmdr_dot_nfld budget_nmdr_dot_nfld_sh{
	capture drop `var'
	}

	foreach type in exp budget {
	egen `type'_nmdr_dot_fld=rowtotal(`type'_fld) if `type'_tot!=.  & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_dot_fld = `type'_nmdr_dot if `type'_nmdr_dot_fld!=. & `type'_nmdr_dot!=. & `type'_nmdr_dot_fld > `type'_nmdr_dot
	gen `type'_nmdr_dot_fld_sh=`type'_nmdr_dot_fld/`type'_nmdr_dot
	replace `type'_nmdr_dot_fld_sh=1 if `type'_nmdr_dot_fld_sh>1 & `type'_nmdr_dot_fld_sh!=.
	replace `type'_nmdr_dot_fld_sh=0 if `type'_nmdr_dot_fld_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_dot_fld_sh=`type'_nmdr_dot_fld_sh[_n-1] if `type'_nmdr_dot_fld_sh==.
	bysort iso3 : replace `type'_nmdr_dot_fld_sh=`type'_nmdr_dot_fld_sh[_n+1] if `type'_nmdr_dot_fld_sh==.
	replace `type'_nmdr_dot_fld= `type'_nmdr_dot_fld_sh*`type'_nmdr_dot if `type'_nmdr_dot_fld==.
	gen `type'_nmdr_dot_nfld_sh=1-`type'_nmdr_dot_fld_sh
	gen `type'_nmdr_dot_nfld=`type'_nmdr_dot_nfld_sh*`type'_nmdr_dot
	}

	foreach var in varlist rcvd_nmdr_dot_fld rcvd_nmdr_dot_fld_sh rcvd_nmdr_dot_nfld rcvd_nmdr_dot_nfld_sh ///
	cf_nmdr_dot_fld cf_nmdr_dot_fld_sh cf_nmdr_dot_nfld cf_nmdr_dot_nfld_sh{
	capture drop `var'
	}

	foreach type in cf rcvd{ 
	egen `type'_nmdr_dot_fld=rowtotal(`type'_fld), missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_dot_fld = `type'_nmdr_dot if `type'_nmdr_dot_fld!=. & `type'_nmdr_dot!=. & `type'_nmdr_dot_fld > `type'_nmdr_dot
	gen `type'_nmdr_dot_fld_sh=`type'_nmdr_dot_fld/`type'_nmdr_dot
	replace `type'_nmdr_dot_fld_sh=1 if `type'_nmdr_dot_fld_sh>1 & `type'_nmdr_dot_fld_sh!=.
	replace `type'_nmdr_dot_fld_sh=0 if `type'_nmdr_dot_fld_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_dot_fld_sh=`type'_nmdr_dot_fld_sh[_n-1] if `type'_nmdr_dot_fld_sh==.
	bysort iso3 : replace `type'_nmdr_dot_fld_sh=`type'_nmdr_dot_fld_sh[_n+1] if `type'_nmdr_dot_fld_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace `type'_nmdr_dot_fld_sh=cf_nmdr_dot_fld_sh if cf_nmdr_dot_fld_sh!=. & `type'_nmdr_dot_fld_sh==.
	replace `type'_nmdr_dot=`type'_nmdr_dot_sh *`type'_nmdr if `type'_nmdr_dot==.
	replace `type'_nmdr_dot_fld = `type'_nmdr_dot_fld_sh * `type'_nmdr_dot if `type'_nmdr_dot_fld==.
	gen `type'_nmdr_dot_nfld_sh=1-`type'_nmdr_dot_fld_sh
	gen `type'_nmdr_dot_nfld=`type'_nmdr_dot_nfld_sh*`type'_nmdr_dot
	}

	// NMDR_DOT_NFLD_LAB, NMDR_DOT_NFLD_NLAB
	foreach var in varlist exp_nmdr_dot_nfld_lab exp_nmdr_dot_nfld_lab_sh exp_nmdr_dot_nfld_nlab exp_nmdr_dot_nfld_nlab_sh ///
	budget_nmdr_dot_nfld_lab budget_nmdr_dot_nfld_lab_sh budget_nmdr_dot_nfld_nlab budget_nmdr_dot_nfld_nlab_sh {
	capture drop `var'
	}

	foreach type in exp budget {
	egen `type'_nmdr_dot_nfld_lab=rowtotal(`type'_lab) if `type'_tot!=. & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_dot_nfld_lab = `type'_nmdr_dot_nfld if `type'_nmdr_dot_nfld_lab!=. & `type'_nmdr_dot!=. & `type'_nmdr_dot_nfld_lab > `type'_nmdr_dot_nfld
	gen `type'_nmdr_dot_nfld_lab_sh=`type'_nmdr_dot_nfld_lab/`type'_nmdr_dot_nfld
	replace `type'_nmdr_dot_nfld_lab_sh=1 if `type'_nmdr_dot_nfld_lab_sh>1 & `type'_nmdr_dot_nfld_lab_sh!=.
	replace `type'_nmdr_dot_nfld_lab_sh=0 if `type'_nmdr_dot_nfld_lab_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_dot_nfld_lab_sh=`type'_nmdr_dot_nfld_lab_sh[_n-1] if `type'_nmdr_dot_nfld_lab_sh==.
	bysort iso3 : replace `type'_nmdr_dot_nfld_lab_sh=`type'_nmdr_dot_nfld_lab_sh[_n+1] if `type'_nmdr_dot_nfld_lab_sh==.
	replace `type'_nmdr_dot_nfld_lab=`type'_nmdr_dot_nfld_lab_sh*`type'_nmdr_dot_nfld if `type'_nmdr_dot_nfld_lab==.
	gen `type'_nmdr_dot_nfld_nlab_sh=1-`type'_nmdr_dot_nfld_lab_sh
	gen `type'_nmdr_dot_nfld_nlab=`type'_nmdr_dot_nfld_nlab_sh*`type'_nmdr_dot_nfld
	}

	foreach var in varlist rcvd_nmdr_dot_nfld_lab rcvd_nmdr_dot_nfld_lab_sh rcvd_nmdr_dot_nfld_nlab rcvd_nmdr_dot_nfld_nlab_sh ///
	cf_nmdr_dot_nfld_lab cf_nmdr_dot_nfld_lab_sh cf_nmdr_dot_nfld_nlab cf_nmdr_dot_nfld_nlab_sh {
	capture drop `var'
	}

	foreach type in cf rcvd{
	egen `type'_nmdr_dot_nfld_lab=rowtotal(`type'_lab) , missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_dot_nfld_lab = `type'_nmdr_dot_nfld if `type'_nmdr_dot_nfld_lab!=. & `type'_nmdr_dot!=. & `type'_nmdr_dot_nfld_lab > `type'_nmdr_dot_nfld
	gen `type'_nmdr_dot_nfld_lab_sh=`type'_nmdr_dot_nfld_lab/`type'_nmdr_dot_nfld
	replace `type'_nmdr_dot_nfld_lab_sh=1 if `type'_nmdr_dot_nfld_lab_sh>1 & `type'_nmdr_dot_nfld_lab_sh!=.
	replace `type'_nmdr_dot_nfld_lab_sh=0 if `type'_nmdr_dot_nfld_lab_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_dot_nfld_lab_sh=`type'_nmdr_dot_nfld_lab_sh[_n-1] if `type'_nmdr_dot_nfld_lab_sh==.
	bysort iso3 : replace `type'_nmdr_dot_nfld_lab_sh=`type'_nmdr_dot_nfld_lab_sh[_n+1] if `type'_nmdr_dot_nfld_lab_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace `type'_nmdr_dot_nfld_lab_sh=cf_nmdr_dot_nfld_lab_sh if cf_nmdr_dot_nfld_lab_sh!=. & `type'_nmdr_dot_nfld_lab_sh==.
	replace `type'_nmdr_dot_nfld_lab= `type'_nmdr_dot_nfld_lab_sh * `type'_nmdr_dot_nfld if `type'_nmdr_dot_nfld_lab==.
	gen `type'_nmdr_dot_nfld_nlab_sh=1-`type'_nmdr_dot_nfld_lab_sh
	gen `type'_nmdr_dot_nfld_nlab=`type'_nmdr_dot_nfld_nlab_sh*`type'_nmdr_dot_nfld
	}

	**PN Jul 2020: Include TPT category. Previous years' values are zeroed. This creates a 'residual' category of other costs where
	** TBHIV and TPT costs have been subtracted. The name of this category is retained as NMDR_NDOT_NHIV
	// NMDR_NDOT_HIV, NMDR_NDOT_TPT & NMDR_NDOT_NHIV
	foreach var in varlist exp_nmdr_ndot_hiv exp_nmdr_ndot_hiv_sh exp_nmdr_ndot_tpt exp_nmdr_ndot_tpt_sh exp_nmdr_ndot_nhiv exp_nmdr_ndot_nhiv_sh ///
	budget_nmdr_ndot_hiv budget_nmdr_ndot_hiv_sh budget_nmdr_ndot_tpt budget_nmdr_ndot_tpt_sh budget_nmdr_ndot_nhiv budget_nmdr_ndot_nhiv_sh {
	capture drop `var'
	}

	foreach type in exp budget {
	// HIV
	egen `type'_nmdr_ndot_hiv=rowtotal(`type'_tbhiv) if `type'_tot!=. & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_ndot_hiv= `type'_nmdr_ndot if `type'_nmdr_ndot_hiv!=. & `type'_nmdr_ndot!=. & `type'_nmdr_ndot_hiv > `type'_nmdr_ndot
	gen `type'_nmdr_ndot_hiv_sh=`type'_nmdr_ndot_hiv/`type'_nmdr_ndot
	replace `type'_nmdr_ndot_hiv_sh=1 if `type'_nmdr_ndot_hiv_sh>1 & `type'_nmdr_ndot_hiv_sh!=.
	replace `type'_nmdr_ndot_hiv_sh=0 if `type'_nmdr_ndot_hiv_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_ndot_hiv_sh=`type'_nmdr_ndot_hiv_sh[_n-1] if `type'_nmdr_ndot_hiv_sh==.
	bysort iso3 : replace `type'_nmdr_ndot_hiv_sh=`type'_nmdr_ndot_hiv_sh[_n+1] if `type'_nmdr_ndot_hiv_sh==.
	replace `type'_nmdr_ndot_hiv=`type'_nmdr_ndot_hiv_sh*`type'_nmdr_ndot if `type'_nmdr_ndot_hiv==.
	
	// TPT
	egen `type'_nmdr_ndot_tpt=rowtotal(`type'_tpt) if `type'_tot!=. & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_ndot_tpt= `type'_nmdr_ndot if `type'_nmdr_ndot_tpt!=. & `type'_nmdr_ndot!=. & `type'_nmdr_ndot_tpt> `type'_nmdr_ndot
	gen `type'_nmdr_ndot_tpt_sh=`type'_nmdr_ndot_tpt/`type'_nmdr_ndot
	replace `type'_nmdr_ndot_tpt_sh=1 if `type'_nmdr_ndot_tpt_sh>1 & `type'_nmdr_ndot_tpt_sh!=.
	replace `type'_nmdr_ndot_tpt_sh=0 if `type'_nmdr_ndot_tpt_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_ndot_tpt_sh=`type'_nmdr_ndot_tpt_sh[_n-1] if `type'_nmdr_ndot_tpt_sh==.
	bysort iso3 : replace `type'_nmdr_ndot_tpt_sh=`type'_nmdr_ndot_tpt_sh[_n+1] if `type'_nmdr_ndot_tpt_sh==.
	replace `type'_nmdr_ndot_tpt= `type'_nmdr_ndot_tpt_sh* `type'_nmdr_ndot  if `type'_nmdr_ndot_tpt==.

	//NHIV (and also subtracting TPT)
	gen `type'_nmdr_ndot_nhiv_sh=1-(`type'_nmdr_ndot_hiv_sh + `type'_nmdr_ndot_tpt_sh) //PN here we suptract both HIV share and TPT share from 100% of NMDR_NDOT
	gen `type'_nmdr_ndot_nhiv=`type'_nmdr_ndot_nhiv_sh*`type'_nmdr_ndot
	}

	foreach var in varlist rcvd_nmdr_ndot_hiv rcvd_nmdr_ndot_hiv_sh varlist rcvd_nmdr_ndot_tpt rcvd_nmdr_ndot_tpt_sh rcvd_nmdr_ndot_nhiv rcvd_nmdr_ndot_nhiv_sh ///
	cf_nmdr_ndot_hiv cf_nmdr_ndot_hiv_sh cf_nmdr_ndot_tpt cf_nmdr_ndot_tpt_sh cf_nmdr_ndot_nhiv cf_nmdr_ndot_nhiv_sh {
	capture drop `var'
	}

	foreach type in cf rcvd{
	// HIV
	egen `type'_nmdr_ndot_hiv=rowtotal(`type'_tbhiv) , missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_ndot_hiv= `type'_nmdr_ndot if `type'_nmdr_ndot_hiv!=. & `type'_nmdr_ndot!=. & `type'_nmdr_ndot_hiv > `type'_nmdr_ndot
	gen `type'_nmdr_ndot_hiv_sh=`type'_nmdr_ndot_hiv/`type'_nmdr_ndot
	replace `type'_nmdr_ndot_hiv_sh=1 if `type'_nmdr_ndot_hiv_sh>1 & `type'_nmdr_ndot_hiv_sh!=.
	replace `type'_nmdr_ndot_hiv_sh=0 if `type'_nmdr_ndot_hiv_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_ndot_hiv_sh=`type'_nmdr_ndot_hiv_sh[_n-1] if `type'_nmdr_ndot_hiv_sh==.
	bysort iso3 : replace `type'_nmdr_ndot_hiv_sh=`type'_nmdr_ndot_hiv_sh[_n+1] if `type'_nmdr_ndot_hiv_sh==.
	// If still missing (e.g. ZAF use committed funds)
	by iso3 (year), sort: replace `type'_nmdr_ndot_hiv_sh=cf_nmdr_ndot_hiv_sh if cf_nmdr_ndot_hiv_sh!=. & `type'_nmdr_ndot_hiv_sh==.
	replace `type'_nmdr_ndot_hiv = `type'_nmdr_ndot_hiv_sh * `type'_nmdr_ndot if `type'_nmdr_ndot_hiv==.
	
	// TPT
	egen `type'_nmdr_ndot_tpt=rowtotal(`type'_tpt) , missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_ndot_tpt= `type'_nmdr_ndot if `type'_nmdr_ndot_tpt!=. & `type'_nmdr_ndot!=. & `type'_nmdr_ndot_tpt > `type'_nmdr_ndot
	gen `type'_nmdr_ndot_tpt_sh=`type'_nmdr_ndot_tpt/`type'_nmdr_ndot
	replace `type'_nmdr_ndot_tpt_sh=1 if `type'_nmdr_ndot_tpt_sh>1 & `type'_nmdr_ndot_tpt_sh!=.
	replace `type'_nmdr_ndot_tpt_sh=0 if `type'_nmdr_ndot_tpt_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_ndot_tpt_sh=`type'_nmdr_ndot_tpt_sh[_n-1] if `type'_nmdr_ndot_tpt_sh==.
	bysort iso3 : replace `type'_nmdr_ndot_tpt_sh=`type'_nmdr_ndot_tpt_sh[_n+1] if `type'_nmdr_ndot_tpt_sh==.
	// If still missing (e.g. ZAF) use committed funds
	by iso3 (year), sort: replace `type'_nmdr_ndot_tpt_sh=cf_nmdr_ndot_tpt_sh if cf_nmdr_ndot_tpt_sh!=. & `type'_nmdr_ndot_tpt_sh==.
	replace `type'_nmdr_ndot_tpt= `type'_nmdr_ndot_tpt_sh * `type'_nmdr_ndot if `type'_nmdr_ndot_tpt==.

	// NHIV (and also subtracting TPT)
	gen `type'_nmdr_ndot_nhiv_sh=1- (`type'_nmdr_ndot_hiv_sh + `type'_nmdr_ndot_tpt_sh) //PN here we suptract both HIV share and TPT share from 100% of NMDR_NDOT
	gen `type'_nmdr_ndot_nhiv=`type'_nmdr_ndot_nhiv_sh*`type'_nmdr_ndot
	}

	// NMDR_NDOT_NHIV_OTH, NMDR_NDOT_NHIV_NOTH 
	// Correcting for the fact that "Other" is treated differently pre and post 2006
	// PN Jul 2020: Also correcting to remove tpt from nhiv_noth

	foreach var in varlist exp_nmdr_ndot_nhiv_oth exp_nmdr_ndot_nhiv_oth_sh exp_nmdr_ndot_nhiv_noth exp_nmdr_ndot_nhiv_noth_sh ///
	budget_nmdr_ndot_nhiv_oth budget_nmdr_ndot_nhiv_oth_sh budget_nmdr_ndot_nhiv_noth budget_nmdr_ndot_nhiv_noth_sh {
	capture drop `var'
	}

	foreach type in exp budget{
	egen `type'_nmdr_ndot_nhiv_oth=rowtotal(`type'_oth) if `type'_tot!=. & `type'_tot!=0 & `type'_all_lines!=. & `type'_all_lines!=0, missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_ndot_nhiv_oth= `type'_nmdr_ndot_nhiv if `type'_nmdr_ndot_nhiv_oth!=. & `type'_nmdr_ndot_nhiv!=. & `type'_nmdr_ndot_nhiv_oth > `type'_nmdr_ndot_nhiv
	replace `type'_nmdr_ndot_nhiv_oth=0 if year<2006
	gen `type'_nmdr_ndot_nhiv_oth_sh=`type'_nmdr_ndot_nhiv_oth/`type'_nmdr_ndot_nhiv
	replace `type'_nmdr_ndot_nhiv_oth_sh=1 if `type'_nmdr_ndot_nhiv_oth_sh>1 & `type'_nmdr_ndot_nhiv_oth_sh!=.
	replace `type'_nmdr_ndot_nhiv_oth_sh=0 if `type'_nmdr_ndot_nhiv_oth_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_ndot_nhiv_oth_sh=`type'_nmdr_ndot_nhiv_oth_sh[_n-1] if `type'_nmdr_ndot_nhiv_oth_sh==.
	bysort iso3 : replace `type'_nmdr_ndot_nhiv_oth_sh=`type'_nmdr_ndot_nhiv_oth_sh[_n+1] if `type'_nmdr_ndot_nhiv_oth_sh==.
	replace `type'_nmdr_ndot_nhiv_oth = `type'_nmdr_ndot_nhiv_oth_sh * `type'_nmdr_ndot_nhiv if `type'_nmdr_ndot_nhiv_oth==.
	gen `type'_nmdr_ndot_nhiv_noth_sh=1-`type'_nmdr_ndot_nhiv_oth_sh
	gen `type'_nmdr_ndot_nhiv_noth=`type'_nmdr_ndot_nhiv_noth_sh*`type'_nmdr_ndot_nhiv
	}

	foreach var in varlist rcvd_nmdr_ndot_nhiv_oth rcvd_nmdr_ndot_nhiv_oth_sh rcvd_nmdr_ndot_nhiv_noth rcvd_nmdr_ndot_nhiv_noth_sh ///
	cf_nmdr_ndot_nhiv_oth cf_nmdr_ndot_nhiv_oth_sh cf_nmdr_ndot_nhiv_noth cf_nmdr_ndot_nhiv_noth_sh {
	capture drop `var'
	}

	foreach type in cf rcvd{
	egen `type'_nmdr_ndot_nhiv_oth=rowtotal(`type'_oth) , missing
	//do not allow piece to be greater than parent.
	replace `type'_nmdr_ndot_nhiv_oth= `type'_nmdr_ndot_nhiv if `type'_nmdr_ndot_nhiv_oth!=. & `type'_nmdr_ndot_nhiv!=. & `type'_nmdr_ndot_nhiv_oth > `type'_nmdr_ndot_nhiv
	replace `type'_nmdr_ndot_nhiv_oth=0 if year<2006
	gen `type'_nmdr_ndot_nhiv_oth_sh=`type'_nmdr_ndot_nhiv_oth/`type'_nmdr_ndot_nhiv
	replace `type'_nmdr_ndot_nhiv_oth_sh=1 if `type'_nmdr_ndot_nhiv_oth_sh>1 & `type'_nmdr_ndot_nhiv_oth_sh!=.
	replace `type'_nmdr_ndot_nhiv_oth_sh=0 if `type'_nmdr_ndot_nhiv_oth_sh<0
	sort iso3 year
    bysort iso3 : replace `type'_nmdr_ndot_nhiv_oth_sh=`type'_nmdr_ndot_nhiv_oth_sh[_n-1] if `type'_nmdr_ndot_nhiv_oth_sh==.
	bysort iso3 : replace `type'_nmdr_ndot_nhiv_oth_sh=`type'_nmdr_ndot_nhiv_oth_sh[_n+1] if `type'_nmdr_ndot_nhiv_oth_sh==.
	// If still missing (e.g. ZAF) use committed funds
	by iso3 (year), sort: replace `type'_nmdr_ndot_nhiv_oth_sh=cf_nmdr_ndot_nhiv_oth_sh if cf_nmdr_ndot_nhiv_oth_sh!=. & `type'_nmdr_ndot_nhiv_oth_sh==.
	replace `type'_nmdr_ndot_nhiv_oth= `type'_nmdr_ndot_nhiv_oth_sh * `type'_nmdr_ndot_nhiv if `type'_nmdr_ndot_nhiv_oth==.
	gen `type'_nmdr_ndot_nhiv_noth_sh=1-`type'_nmdr_ndot_nhiv_oth_sh
	gen `type'_nmdr_ndot_nhiv_noth=`type'_nmdr_ndot_nhiv_noth_sh*`type'_nmdr_ndot_nhiv
	}

	save finance_clean.dta, replace
		/*******************************************************************
	SAVE DATASETS IN NOMINAL USD, CONSTANT USD AND CONSTANT I$ VERSIONS
	********************************************************************/
	// NOMINAL
	gen include=0
	replace include=1 if inlist(g_income, "LIC", "LMC", "UMC")
	
	//foreach country in DZA CRI {
	// PN 2019: In 2018 the list for exclusion was ALB AZE CRI DMA DZA EGY GNQ GRD IRN LBY LCA MUS PSE SYR TKM VCT WSM
	// PN 2020: In 2019 the list for exclusion was ALB BDI CRI DMA DZA ECU EGY FSM GMB IRN JAM LBY MUS WSM MKD TUR TKM UZB
	// PN 2021: In 2020 the list was ALB CRI DZA GMB GRD  LBY   PSE  VCT   DMA  EGY JAM MUS TKM UZB
	// AS 2023: The inlusion rule was discussed and set for any country with either 2022 GHS cost or 2022 rcvd_total after data filling. 
	// This include all countries but for 3 they will only have GHS costs: Costa Rica, Mauritius, and St. Vincent.
	//replace include=0 if iso3=="`country'"
	//}
	
	label var include "Included in 2023 GTBR"
	drop e_pop*   source*

	foreach var of varlist iso2 iso_numeric country g_whoregion g_hb_tb g_hb_tbhiv g_hb_mdr g_income{
	gsort iso3 - `var'
	by iso3: carryforward (`var'), replace
	}

	foreach var of varlist include include gf_eligible  usaid_amount  gf_funding_need gf_domestic  gf_gf_funding{
	replace `var'=0 if `var'==.
	}
	
	cap drop _fillin
	fillin iso_numeric year
	drop _fillin 
	
	** PN / IGB 2021: update g_hb_tb to include the three countries in the global watchlist in 2021 - 30
	replace g_hb_tb = 1 if iso3 == "RUS"| iso3 == "KHM" | iso3 == "ZWE"
	
	/* Check what countries are being dropped here!!! It should just be non-countries like regions from WB */
	//br if country == ""
	drop if country==""
	sort country year
	save finance_imputable_nomusd.dta, replace
	
	preserve
	keep if year==latest_year
	gen var=""
	replace var="Included" if include==1
	replace var="Excluded" if include==0
	keep iso3 var
	sort iso3
	
	//export delimited using "$path/include_map.csv", replace
	restore
	
	keep if include==1
	
	label var off_exch_rate "LCU per USD, official else DEC, prd average else end of prd"
	save finance_imputable_nomusd_balanced.dta, replace

/**********************************/
// CONSTANT DOLLARS NOT BALANCED
/**********************************/
use finance_imputable_nomusd_balanced.dta, clear

/*global dollarvars budget_cpp_dstb budget_cpp_mdr budget_cpp_xdr ///
budget_prog gap_prog budget_fld gap_fld budget_staff gap_staff budget_lab gap_lab budget_tbhiv gap_tbhiv budget_sld gap_sld budget_mdrmgt gap_mdrmgt ///
budget_or gap_or budget_orsrvy gap_orsrvy budget_patsup gap_patsup budget_oth gap_oth budget_tot gap_tot ///
cf_tot_gov cf_tot_loan cf_tot_gf cf_tot_grnt cf_tot_usaid cf_tot cf_tot_sources cf_tot_domestic ///
exp_cpp_dstb exp_cpp_mdr exp_cpp_xdr ///
exp_prog exp_fld exp_staff exp_lab exp_tbhiv exp_sld exp_mdrmgt exp_or exp_orsrvy exp_patsup exp_oth exp_tot ///
rcvd_tot_gov rcvd_tot_loan rcvd_tot_gf rcvd_tot_grnt rcvd_tot_usaid rcvd_tot rcvd_tot_domestic rcvd_tot_sources ///
cf_prog cf_fld cf_staff cf_lab cf_tbhiv cf_sld cf_mdrmgt cf_or cf_orsrvy  cf_patsup  cf_oth ///
rcvd_prog rcvd_fld rcvd_staff rcvd_lab rcvd_tbhiv rcvd_sld rcvd_mdrmgt rcvd_or rcvd_orsrvy rcvd_patsup rcvd_oth ///
c_clinic_sp c_hospital_sp c_clinic_sn c_hospital_sn c_hospital_nmdr c_clinic_nmdr ///
c_clinic_mdr c_hospital_mdr ///
c_ghs_sp c_ghs_sn c_ghs_nmdr c_ghs_mdr c_ghs_inpatient c_ghs_outpatient c_ghs ///
exp_mdr exp_nmdr budget_mdr budget_nmdr rcvd_mdr rcvd_nmdr cf_mdr cf_nmdr exp_mdr_sld exp_mdr_nsld ///
budget_mdr_sld budget_mdr_nsld rcvd_mdr_sld rcvd_mdr_nsld cf_mdr_sld cf_mdr_nsld exp_nmdr_dot ///
exp_nmdr_ndot budget_nmdr_dot budget_nmdr_ndot rcvd_nmdr_dot rcvd_nmdr_ndot cf_nmdr_dot /// 
cf_nmdr_ndot exp_nmdr_dot_fld exp_nmdr_dot_nfld budget_nmdr_dot_fld budget_nmdr_dot_nfld ///
rcvd_nmdr_dot_fld rcvd_nmdr_dot_nfld cf_nmdr_dot_fld cf_nmdr_dot_nfld exp_nmdr_dot_nfld_lab ///
exp_nmdr_dot_nfld_nlab budget_nmdr_dot_nfld_lab budget_nmdr_dot_nfld_nlab rcvd_nmdr_dot_nfld_lab ///
rcvd_nmdr_dot_nfld_nlab cf_nmdr_dot_nfld_lab cf_nmdr_dot_nfld_nlab exp_nmdr_ndot_hiv exp_nmdr_ndot_nhiv ///
budget_nmdr_ndot_hiv budget_nmdr_ndot_nhiv rcvd_nmdr_ndot_hiv rcvd_nmdr_ndot_nhiv cf_nmdr_ndot_hiv /// 
cf_nmdr_ndot_nhiv exp_nmdr_ndot_nhiv_oth exp_nmdr_ndot_nhiv_noth exp_nmdr_ndot_nhiv_noth ///
budget_nmdr_ndot_nhiv_oth budget_nmdr_ndot_nhiv_noth rcvd_nmdr_ndot_nhiv_oth rcvd_nmdr_ndot_nhiv_noth ///
cf_nmdr_ndot_nhiv_oth cf_nmdr_ndot_nhiv_noth gf_rcvd_ext_gf ///
rcvd_int rcvd_ext rcvd_ext_ngf rcvd_ext_gf ///
cf_int cf_ext cf_ext_ngf cf_ext_gf //uc_*cur*
*/
foreach var of varlist $dollarvars{
replace `var'=(`var')/ deflator_us
replace `var'=round(`var',.000001)
label variable `var' "`var' constant US$"
}
gen imf_gdp_pc_con_usd= imf_gdp_pc_cur_usd/deflator_us
sort iso3 year
save finance_pred_cus.dta, replace
	
/********************************/
// CONSTANT DOLLARS BALANCED
/******************************/
use finance_imputable_nomusd_balanced.dta, clear

sort iso3 year
by iso3: replace deflator_us=deflator_us[_n-1] if deflator_us==.

global dollarvars budget_cpp_dstb budget_cpp_mdr budget_cpp_xdr budget_cpp_tpt ///
budget_prog gap_prog budget_fld gap_fld budget_staff gap_staff budget_lab gap_lab budget_tbhiv gap_tbhiv budget_sld gap_sld budget_mdrmgt gap_mdrmgt ///
budget_tpt gap_tpt budget_or gap_or budget_orsrvy gap_orsrvy budget_patsup gap_patsup budget_oth gap_oth budget_tot gap_tot ///
cf_tot_gov cf_tot_loan cf_tot_gf cf_tot_grnt cf_tot_usaid cf_tot cf_tot_sources cf_tot_domestic ///
exp_cpp_dstb exp_cpp_mdr exp_cpp_xdr exp_cpp_tpt ///
exp_prog exp_fld exp_staff exp_lab exp_tbhiv exp_sld exp_mdrmgt exp_tpt exp_or exp_orsrvy exp_patsup exp_oth exp_tot ///
rcvd_tot_gov rcvd_tot_loan rcvd_tot_gf rcvd_tot_grnt rcvd_tot_usaid rcvd_tot rcvd_tot_domestic rcvd_tot_sources ///
cf_prog cf_fld cf_staff cf_lab cf_tbhiv cf_sld cf_mdrmgt cf_tpt cf_or cf_orsrvy  cf_patsup  cf_oth ///
rcvd_prog rcvd_fld rcvd_staff rcvd_lab rcvd_tbhiv rcvd_sld rcvd_mdrmgt rcvd_tpt rcvd_or rcvd_orsrvy rcvd_patsup rcvd_oth ///
c_clinic_sp c_hospital_sp c_clinic_sn c_hospital_sn c_hospital_nmdr c_clinic_nmdr ///
c_clinic_mdr c_hospital_mdr ///
c_ghs_sp c_ghs_sn c_ghs_nmdr c_ghs_mdr c_ghs_inpatient c_ghs_outpatient c_ghs ///
exp_mdr exp_nmdr budget_mdr budget_nmdr rcvd_mdr rcvd_nmdr cf_mdr cf_nmdr exp_mdr_sld exp_mdr_nsld ///
budget_mdr_sld budget_mdr_nsld rcvd_mdr_sld rcvd_mdr_nsld cf_mdr_sld cf_mdr_nsld exp_nmdr_dot ///
exp_nmdr_ndot budget_nmdr_dot budget_nmdr_ndot rcvd_nmdr_dot rcvd_nmdr_ndot cf_nmdr_dot /// 
cf_nmdr_ndot exp_nmdr_dot_fld exp_nmdr_dot_nfld budget_nmdr_dot_fld budget_nmdr_dot_nfld ///
rcvd_nmdr_dot_fld rcvd_nmdr_dot_nfld cf_nmdr_dot_fld cf_nmdr_dot_nfld exp_nmdr_dot_nfld_lab ///
exp_nmdr_dot_nfld_nlab budget_nmdr_dot_nfld_lab budget_nmdr_dot_nfld_nlab rcvd_nmdr_dot_nfld_lab ///
rcvd_nmdr_dot_nfld_nlab cf_nmdr_dot_nfld_lab cf_nmdr_dot_nfld_nlab exp_nmdr_ndot_hiv exp_nmdr_ndot_nhiv ///
budget_nmdr_ndot_hiv budget_nmdr_ndot_nhiv rcvd_nmdr_ndot_hiv rcvd_nmdr_ndot_nhiv cf_nmdr_ndot_hiv ///
exp_nmdr_ndot_tpt budget_nmdr_ndot_tpt rcvd_nmdr_ndot_tpt cf_nmdr_ndot_tpt /// 
cf_nmdr_ndot_nhiv exp_nmdr_ndot_nhiv_oth exp_nmdr_ndot_nhiv_noth exp_nmdr_ndot_nhiv_noth ///
budget_nmdr_ndot_nhiv_oth budget_nmdr_ndot_nhiv_noth rcvd_nmdr_ndot_nhiv_oth rcvd_nmdr_ndot_nhiv_noth ///
cf_nmdr_ndot_nhiv_oth cf_nmdr_ndot_nhiv_noth gf_rcvd_ext_gf ///
rcvd_int rcvd_ext rcvd_ext_ngf rcvd_ext_gf ///
cf_int cf_ext cf_ext_ngf cf_ext_gf //uc_*cur* PN 2023 we dont need this since we now have the *const* versions

foreach var of varlist $dollarvars{
replace `var'=(`var')/ deflator_us
replace `var'=round(`var',.000001)
label variable `var' "`var' constant US$"
}

gen imf_gdp_pc_con_usd= imf_gdp_pc_cur_usd/deflator_us 

capture drop include
gen include=0
	replace include=1 if inlist(g_income, "LIC", "LMC", "UMC")
	//replace this list with countries to exclude from figures
	/* PN: In 2020 we find that 
	//CRI DZA GMB GRD  LBY   PSE  VCT   DMA  EGY JAM MUS TKM UZB
	li iso3 country rcvd_tot exp_tot  imputed if (rcvd_tot == . | exp_tot == .) & year == 2019 & include == 1
	li iso3 country budget_tot cf_tot  imputed if (budget_tot == . | cf_tot == .) & year == 2020 & include == 1

	foreach country in GRD CRI CUB MUS VCT {
*2018	foreach country in ALB AZE CRI DMA DZA EGY GNQ GRD IRN  LBY LCA MUS PSE SYR TKM VCT WSM{
	replace include=0 if iso3=="`country'"
	}
	*/	
	// PN/ IGB 19 Jul 2021: We use the rule that if there is utilisation data, we allow for GHS costs to be estimated.
	// Include mainly in trend data as expenditures. 
	
	label var include "Included in 2023 GTBR"
	keep if include==1
sort iso3 year
save finance_pred_cus_balanced.dta, replace

