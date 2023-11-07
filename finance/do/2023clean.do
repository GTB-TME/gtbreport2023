/* PN / TY: 2023 changes and cleaning */

/*
label var c_notified "Notifications (DS)"
label var mdr_tx "Notifications (MDR-TB)"
label var uc_visit_cur_usd "Unit cost per visit (CHOICE), US$"
label var uc_bedday_nmdr_cur_usd "Unit cost per bedday for DS-TB (CHOICE), US$"
label var uc_bedday_mdr_cur_usd "Unit cost per bedday for MDR-TB (CHOICE), US$"
label var hcfvisit_mdr "Typical number of visits to a health facility after diagnosis for MDR-TB treatment"
label var hospd_mdr_prct "Estimated average duration of stay for MDR cases if they are  hospitalised"
label var hospd_mdr_dur "Estimated percentage of DS-TB cases that are hospitalised"
label var hcfvisit_dstb "Typical number of visits to a health facility after diagnosis for DS-TB treatment"
label var hospd_dstb_prct "Estimated percentage of DS-TB cases that are hospitalised"
label var hospd_dstb_dur "Estimated average duration of stay for DS-TB cases if they are  hospitalised"
label var c_clinic_nmdr "TME calculated outpatient visit for DS-TB (# visits * uc*notifications)"
label var c_hospital_nmdr "TME calculated inpatient visit for DS-TB (# days hospitalized* duration* uc*notifications)"
label var c_clinic_mdr "TME calculated outpatient visit for MDR-TB (# visits * uc*notifications)"
label var c_hospital_mdr "TME calculated inpatient visit for MDR (# days hospitalized* duration* uc*notifications)"
label var c_ghs_inpatient "TME calculated total costs for hospitalisation (MDR+DS-TB)"
label var c_ghs_outpatient "TME calculated outpatient visit for MDR-TB + DS-TB (# visits * uc*notifications)"
label var c_ghs "TME calculated total costs for hospitalisation + outpatient care (MDR+DS-TB)"
*/

// PN: some countries have no income group label

tab country if g_income == ""
replace g_income="HIC" if inlist(iso3, "AIA", "COK","MSR","NIU", "TKL", "WLF") & g_income == ""
//Also missing is Venezuela (since July 2021) but for consistency keep it as prev year.
replace g_income = "UMC" if iso3 == "VEN" & g_income == ""

gsort iso3 year

//create a local variable to store the specific suffixes of the financial data's disaggregates
global suffix lab staff fld prog sld mdrmgt tpt tbhiv patsup orsrvy oth tot
 
/*
// Updating the totals fields - template code
egen budget_tot_temp = rowtotal(budget_fld budget_staff budget_lab budget_tpt budget_tbhiv budget_sld budget_mdrmgt budget_oth budget_prog budget_orsrvy budget_patsup) if iso3 == "ARM" & year == 2021
replace budget_tot = budget_tot_temp if iso3 == "ARM" & year == 2021
egen cf_tot_temp = rowtotal(cf_fld cf_staff cf_lab cf_tpt cf_tbhiv cf_sld cf_mdrmgt cf_oth cf_prog cf_orsrvy cf_patsup) if iso3 == "ARM" & year == 2021
replace cf_tot = cf_tot_temp if iso3 == "ARM" & year == 2021
egen gap_tot_temp = rowtotal(gap_fld gap_staff gap_lab gap_tpt gap_tbhiv gap_sld gap_mdrmgt gap_oth gap_prog gap_orsrvy gap_patsup) if iso3 == "ARM" & year == 2021
replace gap_tot = gap_tot_temp if iso3 == "ARM" & year == 2021
drop budget_tot_temp cf_tot_temp gap_tot_temp

*/


* Bangladesh
* Reported GF component (and Lab component)  received in 2022 - 130 million was a Covid RF grant for lab commodities. Remove from the lab and the GF source components
replace rcvd_lab = rcvd_lab - 130000000 if rcvd_lab == 181076092 & iso3 == "BGD" & year == 2022
replace exp_lab = exp_lab - 130000000 if exp_lab == 180483721 & iso3 == "BGD" & year == 2022
* by source - subtract from GF source
replace rcvd_tot_gf = rcvd_tot_gf - 130000000 if rcvd_tot_gf == 199434217 & iso3 == "BGD" & year == 2022
* Totals
egen new_rcvd_tot = rowtotal(rcvd_lab rcvd_staff rcvd_fld rcvd_prog rcvd_sld rcvd_mdrmgt rcvd_tpt rcvd_tbhiv rcvd_patsup rcvd_orsrvy rcvd_oth) if iso3 == "BGD" & year == 2022, missing
replace rcvd_tot = new_rcvd_tot if  iso3 == "BGD" & year == 2022
drop new_rcvd_tot

egen new_exp_tot = rowtotal (exp_lab exp_staff exp_fld exp_prog exp_sld exp_mdrmgt exp_tpt exp_tbhiv exp_patsup exp_orsrvy exp_oth) if iso3 == "BGD" & year == 2022, missing
replace exp_tot = new_exp_tot if  iso3 == "BGD" & year == 2022
drop new_exp_tot

*Belarus. Starting data set had incorrect rcvd total amounts. Reverting to country reported amounts
replace rcvd_tot= 69266597 if iso3=="BLR" & year==2016
replace rcvd_tot= 71159292 if iso3=="BLR" & year==2017
replace rcvd_tot= 70765651 if iso3=="BLR" & year==2018


* Cambodia
li year cf_tot rcvd_tot exp_tot rcvd_sld exp_sld cf_sld if iso3 == "KHM" & year >= 2021
* Let previous report's cf_ values be used as this year's expenditure value
foreach item in $suffix {
	replace rcvd_`item' = cf_`item' if rcvd_tot == . & iso3 == "KHM" & year == 2022
	replace exp_`item' = cf_`item' if exp_tot == . & iso3 == "KHM" & year == 2022
}
replace rcvd_tot_domestic = cf_tot_domestic if rcvd_tot_domestic == . & iso3 == "KHM" & year == 2022
replace rcvd_tot_gf = cf_tot_gf if rcvd_tot_gf == . & iso3 == "KHM" & year == 2022
replace rcvd_tot_usaid = cf_tot_usaid if rcvd_tot_usaid == . & iso3 == "KHM" & year == 2022
replace rcvd_tot_grnt = cf_tot_grnt if rcvd_tot_grnt == . & iso3 == "KHM" & year == 2022


* COG
* temporarily change reported values (rcvd 2022 and budget / cf 2023) to USD using echange rate (XOF?)

foreach item in $suffix {
	replace rcvd_`item' = round(rcvd_`item'/243.509,0) if rcvd_tot == 767510775 & iso3 == "COG" & year == 2022
	replace exp_`item' = round(exp_`item'/243.509,0) if exp_tot == 767510775 & iso3 == "COG" & year == 2022
}

foreach item in tot_gf tot_usaid tot_domestic {
	replace rcvd_`item' = round(rcvd_`item'/243.509,0) if rcvd_tot == 767510775 & iso3 == "COG" & year == 2022
}

* Cuba
li year rcvd_tot rcvd_int rcvd_ext_gf rcvd_ext_ngf if iso3 == "CUB" & year > 2012
* Notice that the rcvd_int variable is empty, and total sources don't add up to rcvd_tot in 2014-19
replace rcvd_int = rcvd_tot if iso3 == "CUB" & rcvd_int == . & rcvd_ext_gf == 0


* DPRK
sort iso3 year
replace rcvd_tot = cf_tot[_n-1] if iso3 == "PRK" & year == 2022 & rcvd_tot == .
replace exp_tot = cf_tot[_n-1] if iso3 == "PRK" & year == 2022 & exp_tot == .
replace rcvd_int = cf_int[_n-1] if iso3 == "PRK" & year == 2022 & rcvd_tot == .
foreach item in $suffix {
	replace rcvd_`item' = cf_`item'[_n-1] if rcvd_`item' == . & iso3 == "PRK" & year == 2022
}

* GAbon
** Move research expenditure reported in 2022 rcvd_oth, from domestic to other grant
list rcvd_tot rcvd_tot_domestic rcvd_patsup rcvd_orsrvy rcvd_oth if iso3 == "GAB" & year >= 2021
replace rcvd_tot_domestic = rcvd_tot_domestic - rcvd_orsrvy if rcvd_orsrvy == 2329986 & rcvd_tot_domestic == 3701009 & iso3 == "GAB" & year == 2022
replace rcvd_tot_grnt = rcvd_tot_grnt + rcvd_orsrvy if rcvd_orsrvy == 2329986 & rcvd_tot_grnt == 73280 & iso3 == "GAB" & year == 2022

* Kryzygstan. Starting data set had incorrect rcvd total amounts. Reverting to country reported amounts

replace rcvd_tot= 15328783 if iso3=="KGZ" & year==2019
replace rcvd_tot= 17273868 if iso3=="KGZ" & year==2020



* Liberia
* Include last year's late submitted report (2021 rcvd and exp, exp_cpp_ and utilisation variables) from the DCF.

list year budget_tot cf_tot cf_tot_gf rcvd_tot rcvd_tot_gf rcvd_lab rcvd_sld rcvd_oth if iso3 == "LBR" & year >= 2021

replace rcvd_lab = 185131 if rcvd_lab == . & year == 2021 & iso3 == "LBR"
replace rcvd_staff = 160229 if rcvd_staff == . & year == 2021 & iso3 == "LBR"
replace rcvd_fld = 147564 if rcvd_fld == . & year == 2021 & iso3 == "LBR"
replace rcvd_prog = 573185 if rcvd_prog == . & year == 2021 & iso3 == "LBR"
replace rcvd_sld = 222085 if rcvd_sld == . & year == 2021 & iso3 == "LBR"
replace rcvd_mdrmgt = . if rcvd_mdrmgt < . & year == 2021 & iso3 == "LBR"
replace rcvd_tpt = . if rcvd_tpt < . & year == 2021 & iso3 == "LBR"
replace rcvd_tbhiv = 123396 if rcvd_tbhiv == . & year == 2021 & iso3 == "LBR"
replace rcvd_patsup = 83631 if rcvd_patsup == . & year == 2021 & iso3 == "LBR"
replace rcvd_orsrvy = . if rcvd_orsrvy < . & year == 2021 & iso3 == "LBR"
replace rcvd_oth = 21037 if rcvd_oth == . & year == 2021 & iso3 == "LBR"
replace rcvd_tot = 1516258  if rcvd_tot == . & year == 2021 & iso3 == "LBR"

replace rcvd_tot_domestic = . if year == 2021 & iso3 == "LBR"
replace rcvd_tot_gf = 1516258 if year == 2021 & iso3 == "LBR"
replace rcvd_tot_usaid = . if year == 2021 & iso3 == "LBR"
replace rcvd_tot_grnt = . if year == 2021 & iso3 == "LBR"

foreach item in $suffix {
	replace exp_`item' = rcvd_`item' if exp_`item' == . & iso3 == "LBR" & year == 2021
}

replace exp_cpp_dstb = 104 if exp_cpp_dstb == . & year == 2021 & iso3 == "LBR"
replace exp_cpp_mdr = 2037 if exp_cpp_mdr == . & year == 2021 & iso3 == "LBR"
replace exp_cpp_tpt = 70 if exp_cpp_tpt == . & year == 2021 & iso3 == "LBR"

replace hospd_dstb_dur = 60 if year == 2021 & iso3 == "LBR"

* Now to correct this reporting year's values (they filled in zeros, but with no confirmation of change in protocols for dstb)
replace hospd_dstb_dur = . if hospd_dstb_dur == 0 & year == 2022 & iso3 == "LBR"
replace hospd_dstb_prct = . if hospd_dstb_prct == 0 & year == 2022 & iso3 == "LBR"


* North Makedonia
* Temporarily Fill in backwards from 2018
gsort iso3 -year
replace exp_tot = exp_tot[_n-1] if exp_tot & year > 2017 & year < 2022 & iso3 == "MKD"
replace budget_tot = budget_tot[_n-1] if budget_tot & year > 2018 & year < 2023 & iso3 == "MKD"

// Namibia: Hadnt reported earlier, needs careful review
//li country iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "NAM"
// li country iso3 year  rcvd_tot_domestic   rcvd_tot_gf rcvd_tot_grnt rcvd_tot_usaid rcvd_tot gap_tot if iso3 == "NAM" // Domestic funding has dipped to 25% of last year.

* Niger (NER)
//update expenditure and received funds 2022, with what's on the dcf. They may have made updates after the snapshot was taken
li year cf_tot rcvd_tot exp_tot rcvd_sld exp_sld cf_sld if iso3 == "NER" & year >= 2021
replace exp_lab = 7632 if exp_lab == . & iso3 == "NER" & year == 2022
replace exp_prog = 101861 if exp_prog == . & iso3 == "NER" & year == 2022
replace exp_fld = 640000 if exp_fld == . & iso3 == "NER" & year == 2022
replace exp_sld = 18542 if exp_sld == . & iso3 == "NER" & year == 2022
replace exp_patsup = 31775 if exp_patsup == . & iso3 == "NER" & year == 2022
replace exp_oth = 221057 if exp_oth == . & iso3 == "NER" & year == 2022
replace exp_tot = 1020867 if exp_tot == . & iso3 == "NER" & year == 2022

replace rcvd_tot_gf = 2072687 if rcvd_tot_gf == . & iso3 == "NER" & year == 2021
replace rcvd_tot = 2072687 if rcvd_tot == . & iso3 == "NER" & year == 2021
replace rcvd_tot_domestic = 800700 if rcvd_tot_domestic == . & iso3 == "NER" & year == 2022
replace rcvd_tot_gf = 356006 if rcvd_tot_gf == . & iso3 == "NER" & year == 2022
replace rcvd_tot_usaid = 123448 if rcvd_tot_usaid == . & iso3 == "NER" & year == 2022
replace rcvd_tot = 1280154 if rcvd_tot == . & iso3 == "NER" & year == 2022

* Nigeria (NGA)- updated budget data from 11 Aug 2023
li year budget_tot cf_tot rcvd_tot exp_tot rcvd_sld exp_sld cf_sld if iso3 == "NGA" & year >= 2021
replace budget_tot = 388005000 if budget_tot == . & iso3 == "NGA" & year == 2023
replace cf_tot = 115895037 if cf_tot == . & iso3 == "NGA" & year == 2023
replace gap_tot = 272109963 if gap_tot == . & iso3 == "NGA" & year == 2023
replace cf_tot_domestic = 23766517 if cf_tot_domestic == . & iso3 == "NGA" & year == 2023
replace cf_tot_gf = 67128520 if cf_tot_gf == . & iso3 == "NGA" & year == 2023
replace cf_tot_usaid = 25000000 if cf_tot_usaid == . & iso3 == "NGA" & year == 2023



 * Russia (RUS)
//Temporarily in 2023, we decided to use what was committed in 2021 as expenditure in 2022
sort iso3 year
li year cf_tot rcvd_tot exp_tot rcvd_sld exp_sld cf_sld if iso3 == "RUS" & year >= 2021
* Let last year's cf_ value be used as this year's expenditure value
foreach item in $suffix {
	replace rcvd_`item' = cf_`item' if rcvd_tot == . & iso3 == "RUS" & year == 2022
	replace exp_`item' = cf_`item' if exp_tot == . & iso3 == "RUS" & year == 2022
} 

* South Africa
// As always, replace rcvd_tot and exp_tot with cf_tot from the previous year (latest_year - 1)
li year cf_tot rcvd_tot exp_tot rcvd_sld exp_sld cf_sld if iso3 == "ZAF" & year >= 2021
* Let last year's cf_ value be used as this year's expenditure value
foreach item in $suffix {
	replace rcvd_`item' = cf_`item' if rcvd_tot == . & iso3 == "ZAF" & year == 2022
	replace exp_`item' = cf_`item' if exp_tot == . & iso3 == "ZAF" & year == 2022
}

* Senegal
// Clear the 1500 value reported as received funds for drugs in 2022
li rcvd_fld rcvd_tot if iso3 == "SEN" & year == 2022
replace rcvd_fld = . if rcvd_fld == 1500 & iso3 == "SEN" & year == 2022
replace rcvd_tot = . if rcvd_tot == 1500 & iso3 == "SEN" & year == 2022
replace rcvd_tot_domestic = . if rcvd_tot_domestic == 1500 & iso3 == "SEN" & year == 2022

replace exp_cpp_dstb = . if exp_cpp_dstb == 1000 & iso3 == "SEN" & year == 2022
replace exp_cpp_mdr = . if exp_cpp_mdr == 100 & iso3 == "SEN" & year == 2022
replace exp_cpp_xdr = . if exp_cpp_xdr == 10 & iso3 == "SEN" & year == 2022
replace exp_cpp_tpt = . if exp_cpp_tpt == 5 & iso3 == "SEN" & year == 2022
// Fill with previous year's value if missing
replace exp_cpp_dstb = exp_cpp_dstb[_n-1] if exp_cpp_dstb == . & iso3 == "SEN" & (year == 2022 | year == 2021)
replace exp_cpp_mdr = exp_cpp_mdr[_n-1] if exp_cpp_mdr == . & iso3 == "SEN" & (year == 2022 | year == 2021)
replace exp_cpp_xdr = exp_cpp_xdr[_n-1] if exp_cpp_xdr == . & iso3 == "SEN" & (year == 2022 | year == 2021)
replace exp_cpp_tpt = exp_cpp_tpt[_n-1] if exp_cpp_tpt == . & iso3 == "SEN" & (year == 2022 | year == 2021)


// Vanuatu - undervalued. Awaiting clarifications from country
li country iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "VUT"
replace budget_tot = . if budget_tot == 614 & iso3 == "VUT" & year == 2022
replace cf_tot = . if cf_tot == 614 & iso3 == "VUT" & year == 2022
replace exp_tot = . if exp_tot == 460 & iso3 == "VUT" & year == 2021
replace rcvd_tot = . if rcvd_tot == 460 & iso3 == "VUT" & year == 2021
replace exp_tot = . if exp_tot == 0 & iso3 == "VUT" & year == 2022
replace rcvd_tot = . if rcvd_tot == 0 & iso3 == "VUT" & year == 2022


* TKM
replace exp_tot = . if iso3 == "TKM" & year == 2021 & exp_tot == 0
replace rcvd_tot = . if iso3 == "TKM" & year == 2021 & rcvd_tot == 0

* TLS
replace budget_cpp_tpt = 12 if budget_cpp_tpt == 120 & year == 2023 & iso3 == "TLS"
replace exp_cpp_tpt = 12 if exp_cpp_tpt == 5672646 & year == 2023 & iso3 == "TLS"
replace exp_cpp_dstb = 24 if exp_cpp_tpt == 1582129 & year == 2023 & iso3 == "TLS"
replace exp_cpp_mdr = 31 if exp_cpp_tpt == 51594 & year == 2023 & iso3 == "TLS"
* TUR
//	foreach item in budget cf rcvd exp {
//	gsort iso3 -year
//	replace `item'_tot = 0 if `item'_tot == . & year > 2012 & iso3 == "TUR"
//	}

// YEMEN
* PN / TA Aug 2023. Yemen never reports in patient utilisation - next followup with country to check why they choose this model of care

* UKRAINE 2019/2020 fix (shown in 2020 clean file) do not seem to be saved in financing.dta. Added with hard numbers. Other included GHS 
//so other was subtracted from total.

replace budget_tot = 147710296  - 109986985 if year == 2020 & iso3 == "UKR"
replace cf_tot = 74982083 - 40857325 if year == 2020 & iso3 == "UKR"
replace budget_oth= 0 if year == 2020 & iso3 == "UKR"
replace cf_oth= 0 if year == 2020 & iso3 == "UKR"

replace exp_tot = 128959449 - 96628827 if year == 2019 & iso3 == "UKR"
replace rcvd_tot = 132703841 - 97684450 if year == 2019 & iso3 == "UKR"
replace exp_oth= 0 if year == 2019 & iso3 == "UKR"
replace rcvd_oth= 0 if year == 2019 & iso3 == "UKR"



























///////////////////////////// Re-run older clean files because I could not see the fixes reflected in starting point

//2020
/* PN: 2020 changes and cleaning */

// PN: 6 countries that had no income group label
replace g_income="HIC" if inlist(iso3, "AIA", "COK","MSR","NIU", "TKL", "WLF") 


//create a local variable to store the specific suffixes of the financial data's disaggregates
global suffix lab staff fld prog sld mdrmgt tpt tbhiv patsup orsrvy oth tot
 
// Armenia (ARM): replace budget 2019 with expenditure 2018 (budget 2019 appears understated).
li exp_lab exp_staff exp_fld exp_prog exp_sld exp_mdrmgt exp_tpt exp_tbhiv exp_patsup exp_orsrvy ///
 exp_oth exp_tot if iso3 == "ARM" & year == 2018
li budget_lab budget_staff budget_fld budget_prog budget_sld budget_mdrmgt budget_tpt budget_tbhiv budget_patsup budget_orsrvy ///
 budget_oth budget_tot if iso3 == "ARM" & year == 2019

gsort iso3 year
foreach item in $suffix {
	replace budget_`item' = exp_`item'[_n-1] if iso3 == "ARM" & year == 2019  
	
}

// Azerbaijan (AZE): For budget and expenditure breakdown, the country to use updated total amounts minus drugs and apply breakdown from 2019 expenditures 
// then add drug amounts reported separately (in 2020 to update historic data)
** PN Jul 2020: Budget data has been provided by country, in long form, no need to modify anything **
// Missing budget_tot will be imputed since there are 5 records so far (PN confirmed)
gsort iso3 year //(sorted ascending with 2020 last (i.e. 19th place with 2002 as first?)
li budget_lab budget_staff budget_fld budget_prog budget_sld budget_mdrmgt budget_tpt budget_tbhiv budget_patsup budget_orsrvy budget_oth budget_tot if iso3 == "AZE"
li exp_lab exp_staff exp_fld exp_prog exp_sld exp_mdrmgt exp_tpt exp_tbhiv exp_patsup exp_orsrvy exp_oth exp_tot if iso3 == "AZE" 

// Algeria (DZA): Have only reported GHS data. 
li iso3 year budget_tot cf_tot exp_tot rcvd_tot c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DZA"
// With 7 values for budget_tot in the past, this may still get imputed (PN confirmed)


// Belarus (BLR):
// PN Make hard coded updates based on audit done in 2020 (ref Leia's excel worksheets) 
** Those figures include expenditures data for all in- and out-patient treatment , which are allocated under lines "NTP Staff", "MDR-TB: Program Costs", "DS-TB: Program Cost"
** historical updates affecting exp and rcvd data 2016 - 2018

li year exp_fld exp_staff exp_lab exp_tbhiv exp_sld exp_mdrmgt exp_oth  exp_prog ///
 exp_orsrvy exp_patsup exp_tot if iso3 == "BLR" & year >= 2015
/** 2016
replace exp_lab = 2021186 if iso3 == "BLR" & year == 2016 
replace exp_staff = 18690711 if iso3 == "BLR" & year == 2016 
replace exp_fld = 154400 if iso3 == "BLR" & year == 2016 
replace exp_prog = 18795904 if iso3 == "BLR" & year == 2016 
replace exp_sld = 12936659 if iso3 == "BLR" & year == 2016 
replace exp_mdrmgt = 15245689 if iso3 == "BLR" & year == 2016 
replace exp_tbhiv = 375297 if iso3 == "BLR" & year == 2016 
replace exp_patsup = 336394 if iso3 == "BLR" & year == 2016 
replace exp_orsrvy = 189473 if iso3 == "BLR" & year == 2016 
replace exp_oth = 520884 if iso3 == "BLR" & year == 2016 
replace exp_tot = 69266597 if iso3 == "BLR" & year == 2016 

** 2017
replace exp_lab = 1273722 if iso3 == "BLR" & year == 2017 
replace exp_staff = 25224550 if iso3 == "BLR" & year == 2017 
replace exp_fld = 127000 if iso3 == "BLR" & year == 2017 
replace exp_prog = 18639507 if iso3 == "BLR" & year == 2017 
replace exp_sld = 8426229 if iso3 == "BLR" & year == 2017 
replace exp_mdrmgt = 15949239 if iso3 == "BLR" & year == 2017 
replace exp_tbhiv = 461486 if iso3 == "BLR" & year == 2017 
replace exp_patsup = 334641 if iso3 == "BLR" & year == 2017 
replace exp_orsrvy = 202802 if iso3 == "BLR" & year == 2017 
replace exp_oth = 520116 if iso3 == "BLR" & year == 2017 
replace exp_tot = 71159292 if iso3 == "BLR" & year == 2017 

** 2018
replace exp_lab = 1841652 if iso3 == "BLR" & year == 2018 
replace exp_staff = 27358864 if iso3 == "BLR" & year == 2018 
replace exp_fld = 106150 if iso3 == "BLR" & year == 2018 
replace exp_prog = 16362125 if iso3 == "BLR" & year == 2018 
replace exp_sld = 7818988 if iso3 == "BLR" & year == 2018 
replace exp_mdrmgt = 14902823 if iso3 == "BLR" & year == 2018 
replace exp_tbhiv = 427908 if iso3 == "BLR" & year == 2018 
replace exp_patsup = 321153 if iso3 == "BLR" & year == 2018 
replace exp_orsrvy = 149230 if iso3 == "BLR" & year == 2018 
replace exp_oth = 1476759 if iso3 == "BLR" & year == 2018 
replace exp_tot = 70765652 if iso3 == "BLR" & year == 2018 

** Note that 2019 expenditure data was correctly reported and is not adjusted in this hard code.

* Update budget and cf fields in prev years (make equal to exp).
sort iso3 year 
foreach var in $suffix {
	replace rcvd_`var' = exp_`var' if iso3 == "BLR" & year >= 2016 & year <=2018 
	replace cf_`var' = exp_`var'[_n - 1] if iso3 == "BLR" & year >= 2017 & year <=2019 
	replace budget_`var' = exp_`var'[_n - 1] if iso3 == "BLR" & year >= 2017 & year <=2019
	replace gap_`var' = 0 if iso3 == "BLR" & year >= 2017 & year <=2019
}

** Sources of funds
replace rcvd_tot_domestic = 64032000 if iso3 == "BLR" & year == 2016 
replace rcvd_tot_gf = 4713714 if iso3 == "BLR" & year == 2016 
replace rcvd_tot_grnt = 520884 if iso3 == "BLR" & year == 2016 
replace rcvd_tot_usaid = 0 if iso3 == "BLR" & year == 2016 
replace rcvd_tot_sources = 69266598 if iso3 == "BLR" & year == 2016 

replace rcvd_tot_domestic = 67056000 if iso3 == "BLR" & year == 2017 
replace rcvd_tot_gf = 3583176 if iso3 == "BLR" & year == 2017 
replace rcvd_tot_grnt = 520116 if iso3 == "BLR" & year == 2017 
replace rcvd_tot_usaid = 0 if iso3 == "BLR" & year == 2017 
replace rcvd_tot_sources = 71159292 if iso3 == "BLR" & year == 2017 
 
replace rcvd_tot_domestic = 65878200 if iso3 == "BLR" & year == 2018 
replace rcvd_tot_gf = 3410692 if iso3 == "BLR" & year == 2018 
replace rcvd_tot_grnt = 1476759 if iso3 == "BLR" & year == 2018 
replace rcvd_tot_usaid = 0 if iso3 == "BLR" & year == 2018 
replace rcvd_tot_sources = 70765651 if iso3 == "BLR" & year == 2018 
 
replace cf_tot_domestic = rcvd_tot_domestic[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
replace cf_tot_gf = rcvd_tot_gf[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
replace cf_tot_grnt = rcvd_tot_grnt[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
replace cf_tot_usaid = rcvd_tot_usaid[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
replace cf_tot_sources = rcvd_tot_sources[_n-1] if iso3 == "BLR" & year >= 2017 & year <=2019
 
 */
 
// Bosjnia BIH: fill in with previous years data: CF_tot2020=Cf_tot2019, Exp_2019=CF_2019
li iso3 year budget_tot cf_tot c_notified tx_dstb if iso3 == "BIH"
gsort iso3 year //sort by country and asc order of year ( 2020 last)
replace cf_tot = cf_tot[_n-1] if year == 2020 & iso3 == "BIH"
replace budget_tot = cf_tot if year == 2020 & iso3 == "BIH"
replace exp_tot = cf_tot if year == 2019 & iso3 == "BIH"
replace rcvd_tot = cf_tot if year == 2019 & iso3 == "BIH"

// Botswana (BwA): Fill in 2020 cf_tot 2020 based on c_notified2019 (PN used tx_dstb instead since there's no c_notified )
// and cf_tot2019. rcvd_*2019 based on rcvd_* 2018 
li iso3 year budget_tot budget_fld cf_tot cf_fld rcvd_tot rcvd_fld c_notified tx_dstb if iso3 == "BWA"
gsort iso3 year //sort by country and asc order of year ( 2020 last)
replace cf_tot = cf_tot[_n-1] * round(tx_dstb / tx_dstb[_n-1]) if cf_tot == . & iso3 == "BWA" & year == 2020
replace budget_tot = budget_tot[_n-1] * round(tx_dstb / tx_dstb[_n-1])  if budget_tot == . & iso3 == "BWA" & year == 2020
foreach item in $suffix {
	replace rcvd_`item' = rcvd_`item'[_n-1] if rcvd_`item' == . & iso3 == "BWA" & year == 2019   
}


// CHad (TCD): Fill in exp with received funding: exp_* = rcvd_* if exp_* is missing for 2019
li iso3 year exp_tot exp_fld rcvd_tot rcvd_fld c_notified tx_dstb if iso3 == "TCD"
foreach item in $suffix {
	replace exp_`item' = rcvd_`item' if iso3 == "TCD" & year == 2019   
	// replaced all items (including exp_tot) to ensure they add up
}

// Chile (CHL): use budget_tot instead of cf_tot, for 2015-2020. Short form report
// PN: CHL being HIC, shouldn't have gap_tot values?
li iso3 year budget_tot cf_tot c_notified tx_dstb if iso3 == "CHL"
replace cf_tot = budget_tot if year >= 2014 & iso3 == "CHL"
replace gap_tot = . if year >= 2014 & iso3 == "CHL"
// Cases notified in 2019 = blank. Shall be carried forward from 2018

// Colombia (COL): correct hc_fvisit_mdr from 2013-2019 to 350.
// MDR years 2013-2018 should be max 350
li iso3 year c_notified hcfvisit_dstb hcfvisit_mdr if iso3 == "COL"
replace hcfvisit_mdr = 350 if year >= 2013 & iso3 == "COL"

// Comoros (COM): Budget_tot 2020 = budget_tot2019, exp_tot2020=exp_tot2019
li iso3 year budget_tot cf_tot exp_tot rcvd_tot if iso3 == "COM"
sort iso3 year
replace budget_tot = budget_tot[_n-1] if iso3 == "COM" & year == 2020 & budget_tot == .
replace cf_tot = budget_tot if iso3 == "COM" & year == 2020 & cf_tot == .
replace exp_tot = exp_tot[_n-1] if iso3 == "COM" & year == 2019 & exp_tot == .
replace rcvd_tot = exp_tot if iso3 == "COM" & year == 2019 & rcvd_tot == .

// CUBa: fill rcvd_tot with exp_tot
replace rcvd_tot = exp_tot if rcvd_tot == . & iso3 == "CUB" & year == 2019
replace cf_tot = budget_tot if cf_tot == . & iso3 == "CUB" & year == 2020

//Djibouti (DJI):fill in budget_oth=cf_oth in 2020. 
replace budget_oth = cf_oth if year == 2020 & iso3 == "DJI" 

//DPRK Jun 2020: PRK's gdp I$ (const 2017) data and US$ (from WB and IMF) is all blank. Replace with data from 
// World CIA Factbook https://www.cia.gov/library/publications/the-world-factbook/geos/kn.html
// as curated by Index Mundi in I$ (const 2015). No deflation index too.
//		Country	1999	2000	2002	2003	2004	2005	2006	2007	2008	2009	2011	2014	2015
//	North Korea	1,000	1,000	1,000	1,300	1,700	1,700	1,800	1,700	1,800	1,800	1,800	1,800	1,700
** Estimating gross national product in North Korea is a difficult task because of a dearth of economic data and 
** the problem of choosing an appropriate rate of exchange for the North Korean won, the nonconvertible North Korean currency.
li iso3 year ny_gdp_pcap_kd ny_gdp_pcap_cd  if iso3 == "PRK" 
replace imf_gdp_pc_cur_int = 1000 if iso3 == "PRK" & year == 2002
replace imf_gdp_pc_cur_int = 1300 if iso3 == "PRK" & year == 2003
replace imf_gdp_pc_cur_int = 1700 if iso3 == "PRK" & year == 2004 | year == 2005 
replace imf_gdp_pc_cur_int = 1800 if iso3 == "PRK" & year == 2006 
replace imf_gdp_pc_cur_int = 1700 if iso3 == "PRK" & year == 2007 | year == 2015
replace imf_gdp_pc_cur_int = 1800 if iso3 == "PRK" & year >= 2008 & year < 2015


// PN 2020: Gambia (GMB): Budget and cf missing for 2020
gsort iso3 -year // 2020 first
replace budget_tot = rcvd_tot[_n-1] if budget_tot == . & year == 2020 & iso3 == "GMB"
replace cf_tot = budget_tot if cf_tot == . & year == 2020 & iso3 == "GMB"
// budget_cpp_mdr is wrong, change to blank
replace budget_cpp_mdr = . if year == 2020 & iso3 == "GMB"

//Guatemala 
li year budget_tot cf_tot gap_tot if iso3 == "GTM"
foreach item in $suffix {
	replace budget_`item' = cf_`item' if iso3 == "GTM" & year >= 2019 & gap_`item' < 0 //if Gap is -ve, replace 
	replace gap_`item' = budget_`item' - cf_`item' if iso3 == "GTM" & year >= 2019
}

// Iran (IRN): Budget not available, use sum of 4.3= dx_*  x  budget_cpp_*.
li year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "IRN"
replace budget_tot = (budget_cpp_dstb * tx_dstb) + (budget_cpp_mdr * tx_mdr) + ///
 (budget_cpp_xdr * tx_xdr) + (budget_cpp_tpt *tx_tpt) if budget_tot == . & year == 2020 & iso3 == "IRN"
replace budget_tot = (budget_cpp_dstb * tx_dstb) + (budget_cpp_mdr * tx_mdr) + ///
 (budget_cpp_xdr * tx_xdr) if budget_tot == . & year == 2019 & iso3 == "IRN"
// PN: This is done only for 2019 & 2020.
 
 
// Lesotho (LSO): Change hospd_mdr_prct= 91% ( in 2018) with hospd_mdr_prct= 30%; also change hospd_mdr_dur =30 in 2018 with hospd_mdr_dur=21 
li year hospd_mdr_prct hospd_mdr_dur if iso3 == "LSO"
replace hospd_mdr_prct = 30 if year == 2018 & iso3 == "LSO"
replace hospd_mdr_dur = 21 if year == 2018 & iso3 == "LSO"

//Libya (LBY):  cf_*=budget_*   for 2020.   
sort iso3 year
foreach item in lab staff fld prog sld mdrmgt tpt tbhiv patsup orsrvy oth tot {
	replace cf_`item' = budget_`item' if cf_`item' == . & iso3 == "LBY" & year == 2020
	replace gap_`item' = 0 if iso3 == "LBY" & year == 2020
	replace exp_`item' = cf_`item'[_n-1] if iso3 == "LBY" & year == 2020
}

//Mauritania: Fill hospd_* fill in with last year data. 
// Fill in budget_* with cf_* if budget_*=. & year==2020
li year budget_tot cf_tot gap_tot if iso3 == "MRT"
foreach item in $suffix {
	replace budget_`item' = cf_`item' if iso3 == "MRT" & year == 2020 & budget_`item' == . 
	replace gap_`item' = budget_`item' - cf_`item' if iso3 == "MRT" & year == 2020
}
li year hcfvisit_dstb hcfvisit_mdr hospd* if iso3 == "MRT"
gsort iso3 year // 2020 last
foreach var of varlist hcfvisit_dstb hcfvisit_mdr hospd_dstb_prct hospd_mdr_prct hospd_dstb_dur hospd_mdr_dur {
	replace `var' = `var'[_n-1] if iso3 == "MRT" & year == 2020
}

// Federal state of micronesia: use previous year's values for utilization
// Note from country: Patients are visited at home for DOT, can have 5 visits to the clinic for sputum collection.
// Non MDR=TB patients can be hospitalized up to 3 weeks
// MDR-TB patients are hospitalized up to 2-3  months
li year hcfvisit_dstb hcfvisit_mdr hospd* if iso3 == "FSM"
gsort iso3 year // 2020 last
foreach var of varlist hcfvisit_dstb hcfvisit_mdr hospd_dstb_prct hospd_mdr_prct hospd_dstb_dur hospd_mdr_dur {
	replace `var' = `var'[_n-1] if iso3 == "FSM" & year == 2020 & `var' == .
}

// Moldova : Use 2019 expenditure data to replace 2020 expected funding lines for everyhting 
//except FLD, SLD, patient support and operational research
gsort iso3 year // 2020 last
foreach var in lab staff prog mdrmgt tbhiv oth {
	replace cf_`var' = exp_`var'[_n-1] if iso3 == "MDA" & year == 2020
}

// Nauru: budget_tot 2020 = budget_tot 2019. Utilization 2019 as utilization 2018
li iso3 year budget_tot cf_tot exp_tot rcvd_tot if iso3 == "NRU"
sort iso3 year // 2020 last
replace budget_tot = budget_tot[_n-1] if iso3 == "NRU" & year == 2020 & budget_tot == .
replace cf_tot = budget_tot if iso3 == "NRU" & year == 2020 & cf_tot == .
replace exp_tot = exp_tot[_n-1] if iso3 == "NRU" & year == 2019 & exp_tot == .
replace rcvd_tot = exp_tot if iso3 == "NRU" & year == 2019 & rcvd_tot == .

li year hcfvisit_dstb hcfvisit_mdr hospd* if iso3 == "NRU"
gsort iso3 year // 2020 last
foreach var of varlist hcfvisit_dstb hcfvisit_mdr hospd_dstb_prct hospd_mdr_prct hospd_dstb_dur hospd_mdr_dur {
	replace `var' = `var'[_n-1] if iso3 == "NRU" & year == 2020 * `var' == .
}

//Panama: Use tx_dstb to scale rcvd_tot 2019 (to estimate av funding 2020)
li year rcvd_tot exp_tot budget_tot cf_tot gap_tot tx_dstb c_notified if iso3 == "PAN"
sort iso3 year   
foreach item in $suffix {
	replace budget_`item' = budget_`item'[_n-1] if iso3 == "PAN" & year == 2020 & cf_tot == .	 //cf_tot more reliable check than budget_tot
	replace cf_`item' = cf_`item'[_n-1] if iso3 == "PAN" & year == 2020 & cf_tot == .	 
	replace gap_`item' = gap_`item'[_n-1] if iso3 == "PAN" & year == 2020 & cf_tot == . 	 
	
	replace exp_`item' = exp_`item'[_n-1] if iso3 == "PAN" & year == 2019 & exp_tot == .	
 	replace rcvd_`item' = rcvd_`item'[_n-1] if iso3 == "PAN" & year == 2019 & rcvd_tot == .	
 	
}


//PNG: use budget data to fill exp.
li year rcvd_tot exp_tot budget_tot cf_tot gap_tot tx_dstb c_notified if iso3 == "PNG"
foreach item in $suffix {
	replace exp_`item' = budget_`item' if iso3 == "PNG" & year == 2020 & exp_`item' == .	
 	
}

//RUS: 
// PN Jul 2020: Fix RUS share of external and internal funding in 2005 and 2006, to match 2007. This is because there was 
// a mistake in share proportions values 
li year rcvd_tot rcvd_int rcvd_ext_gf rcvd_ext_ngf if iso3 == "RUS" & year >= 2002
gsort iso3 -year
replace rcvd_tot_gov = rcvd_ext_ngf if year == 2005 & iso3 == "RUS" //Pre 2006, internal was reported as tot_domestic
replace rcvd_ext_ngf = . if year == 2005 & iso3 == "RUS"
replace rcvd_tot = 647555850 if year == 2006 & iso3 == "RUS"  // Similar to the estimate obtained from imputation in 2018
replace rcvd_tot_gov = rcvd_tot if year == 2006 & iso3 == "RUS" // all gov (domestic) funding

li year rcvd_tot rcvd_mdr rcvd_nmdr rcvd_sld rcvd_fld rcvd_lab if iso3 == "RUS" & year >= 2002
gsort iso3 -year
// To modify the proportion of dr and ds in 2005 (and subsequently in 2006) to be consistent with previously reported values (see GTB 2018)..
// in 2018, the proportional share of total rcvd that was allocated to 2006 DR was 0.100, DS_DOT was .364 and tbhiv was .535
replace rcvd_sld = round(0.10034428 * rcvd_tot) if year == 2005 & iso3 == "RUS" 
replace rcvd_fld = round(0.36424288 * rcvd_tot)  if year == 2005 & iso3 == "RUS" 
replace rcvd_tbhiv = round(0.53541284 * rcvd_tot)  if year == 2005 & iso3 == "RUS" 

replace rcvd_sld = round(0.10034428 * rcvd_tot) if year == 2006 & iso3 == "RUS" 
replace rcvd_fld = round(0.36424288 * rcvd_tot)  if year == 2006 & iso3 == "RUS" 
replace rcvd_tbhiv = round(0.53541284 * rcvd_tot)  if year == 2006 & iso3 == "RUS" 


//Senegal (SEN): hcfvisit_dstb 2019 fill in with 2018. 
li year hcfvisit* if iso3 == "SEN"
sort iso3 year
replace hcfvisit_dstb = hcfvisit_dstb[_n-1] if year == 2019 & iso3 == "SEN"




// Tunisia (TUN):fill budget_*2020 = budget_*2019 adjusted with tx_dstb, same for cf and exp
li iso3 year budget_tot cf_tot exp_tot gap_tot rcvd_tot c_notified tx_dstb if iso3 == "TUN"
gsort iso3 year //sort by country and ascending order of year (2020 last)
foreach item in $suffix {
	replace budget_`item' = budget_`item'[_n-1] if iso3 == "TUN" & year == 2020 & budget_`item' == .
	replace cf_`item' = budget_`item' if iso3 == "TUN" & year == 2020 & cf_`item' == .	
	replace exp_`item' = exp_`item'[_n-1] if iso3 == "TUN" & year == 2019 & exp_`item' == .
	replace rcvd_`item' = exp_`item' if iso3 == "TUN" & year == 2019 & rcvd_`item' == .	
}
	

//Tuvalu not reported, use last year's values
li iso3 year budget_tot cf_tot exp_tot gap_tot rcvd_tot c_notified tx_dstb if iso3 == "TUV"
gsort iso3 year //sort by country and ascending order of year (2020 last)
foreach item in $suffix {
	replace budget_`item' = budget_`item'[_n-1] if iso3 == "TUV" & year == 2020 & budget_`item' == .
	replace cf_`item' = budget_`item' if iso3 == "TUV" & year == 2020 & cf_`item' == .	
	replace exp_`item' = exp_`item'[_n-1] if iso3 == "TUV" & year == 2019 & exp_`item' == .
	replace rcvd_`item' = exp_`item' if iso3 == "TUV" & year == 2019 & rcvd_`item' == .	
	
}

//foreach var of varlist rcvd_* exp_* gap_*{
//replace `var'= `var'/1.2954 if iso3=="TUV" & year==2018
//}

// Ukraine (UKR): Budget_oth cf_oth in 2020 and exp_oth rcvd_oth in 2019 include GHS costs. 
replace budget_tot = budget_tot - budget_oth if year == 2020 & iso3 == "UKR"
replace cf_tot = cf_tot - cf_oth if year == 2020 & iso3 == "UKR"
replace exp_tot = exp_tot - exp_oth if year == 2019 & iso3 == "UKR"
replace rcvd_tot = rcvd_tot - rcvd_oth if year == 2019 & iso3 == "UKR"

//Vanuatu (VUT): fill rcvd_*2019= rcvd_*2018 with c_notified 2019
li iso3 year budget_tot cf_tot exp_tot gap_tot rcvd_tot c_notified tx_dstb if iso3 == "VUT"
gsort iso3 year //sort by country and ascending order of year (2020 last)
foreach item in $suffix {
	replace exp_`item' = exp_`item'[_n-1] * round(c_notified / c_notified[_n-1]) if iso3 == "VUT" & year == 2019 & exp_`item' == .
	replace rcvd_`item' = exp_`item' if iso3 == "TUV" & year == 2019 & rcvd_`item' == .	
}





** PN: 13 Jul 2020: Further cleaning based on findings from individual country GHS data review
** c_ghs is computed from c_clinic and c_hospital variables. for each country reviewed, these 
** and their components are investigated  and the following changes recommended



** Make changes to past coutnry data

//ZAF: change hcfvisit_mdr in 2019 from 9 to prevoius years' value
gsort iso3 year
li year hcfvisit_mdr hospd_mdr_dur hospd_mdr_prct c_notified  if iso3 == "ZAF" & year >= 2016
replace hcfvisit_mdr = hcfvisit_mdr[_n-1] if iso3 == "ZAF" & year == 2019 & hcfvisit_mdr == 9
// IGB/PN 23Jul2020: Values in 2006 are manually fixed (and allocated) because imputation results in
// halving of figures, ncausing the total global value to reduce by 0.1 B USD

replace rcvd_tot =  299932088 if iso3 == "ZAF" & year == 2006 //  299932088.4 in 2006 nominal US$, equivalent to 366944991 in 2018 US$
replace rcvd_sld = round(0.45188398 * rcvd_tot) if year == 2006 & iso3 == "ZAF" 
replace rcvd_fld = round(0.47110833 * rcvd_tot)  if year == 2006 & iso3 == "ZAF"  
replace rcvd_tbhiv = round(0.06382217 * rcvd_tot)  if year == 2006 & iso3 == "ZAF" 
replace rcvd_orsrvy = round(0.01318551 * rcvd_tot)  if year == 2006 & iso3 == "ZAF" 

replace rcvd_tot_gov = round(0.99204674 * rcvd_tot) if year == 2006 & iso3 == "ZAF"
replace rcvd_tot_grnt = round(0.00179391 * rcvd_tot) if year == 2006 & iso3 == "ZAF"
replace rcvd_tot_gf = round(0.00615935 * rcvd_tot) if year == 2006 & iso3 == "ZAF"

// ZWE: c_clinic_nmdr overly high in 2012 - 2013 since comuptation used previous data (hcfvisit_sp & _sn) which were set at 240 visits instead of 4 - 6 visits for DS)
li year c_ghs c_ghs_nmdr c_clinic_nmdr c_hospital_nmdr c_notified hcfvisit_dstb uc_visit_cur_usd if iso3 == "ZWE"
gsort iso3 year
replace hcfvisit_sp = hcfvisit_sp[_n-1] if year > 2010 & year <=2013 & iso3 == "ZWE"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year > 2010 & year <=2013 & iso3 == "ZWE"

gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "ZWE" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if year > 2010 & year <=2013 & iso3 == "ZWE" & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year > 2010 & year <=2013 & iso3 == "ZWE"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year > 2010 & year <=2013 & iso3 == "ZWE"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year > 2010 & year <= 2013 & iso3 == "ZWE" //permanent refix of OP cost aggregate since existing value was too high

// Also change hcfvisit_dstb and hcf_visit_mdr values for 2015 and before with those of 2016
gsort iso3 -year // latest yar first
replace hcfvisit_dstb = hcfvisit_dstb[_n-1] if year == 2016 & iso3 == "ZWE" & hcfvisit_dstb == 64 // From 64 to 24 consistent with next year's data
replace hcfvisit_dstb = hcfvisit_dstb[_n-1] if year <=2015 & iso3 == "ZWE"
replace hcfvisit_mdr = hcfvisit_mdr[_n-1] if year <=2015 & year >=2014 & iso3 == "ZWE"

// AGO: c_hospital_nmdr is exagerrated (33M compared to 2M in neighbor years). Root cause is computational issue in 2012 - 13
li year c_hospital_nmdr hcfvisit_sn hcfvisit_sp hcfvisit_dstb if iso3 == "AGO" & year > 2010
gsort iso3 year //earliest_year first, since we are filling from prev years' data
replace c_clinic_nmdr =  c_clinic_nmdr[_n-1] if year > 2010 & year <= 2013 & iso3 == "AGO" //permanent refix of OP cost aggregate since existing value was too high
replace c_hospital_nmdr = c_hospital_nmdr[_n-1] if year > 2010 & year <= 2013 & iso3 == "AGO"

// BRA: inpatient utilsation for 2014 ought to be replaced since it's great conpared to 2015. Seems more like 2013'svalue but this ignores a UC change in 2014 
li year c_hospital_nmdr c_notified hospd_dstb_dur  hospd_dstb_prct   uc_bedday_nmdr_cur_usd  if iso3 == "BRA"
gsort iso3 -year //latest_year first, since we are filling from next years' data
replace hospd_dstb_dur = hospd_dstb_dur[_n-1] if year == 2014 & iso3 == "BRA"
replace hospd_dstb_prct = hospd_dstb_prct[_n-1] if year == 2014 & iso3 == "BRA"

// COG: backfill 2012 and 2013 hospital values using 2014's 
gsort iso3 year //earliest year first, since we are filling from past years' data
replace hospd_sn_dur = hospd_sn_dur[_n-1] if year > 2011 & year <=2013 & iso3 == "COG"
replace hospd_sp_dur = hospd_sp_dur[_n-1] if year > 2011 & year <=2013 & iso3 == "COG"
replace hospd_sn_prct = hospd_sn_prct[_n-1] if year > 2011 & year <=2013 & iso3 == "COG"
replace hospd_sp_prct = hospd_sp_prct[_n-1] if year > 2011 & year <=2013 & iso3 == "COG"

gen sh_sp = c_hospital_sp / (c_hospital_sp + c_hospital_sn) if iso3 == "COG" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if year > 2011 & year <=2013 & iso3 == "COG" & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_hospital_sp = c_notified * hospd_sp_dur * hospd_sp_prct * uc_visit_cur_usd * sh_sp if year > 2011 & year <=2013 & iso3 == "COG"
replace c_hospital_sn = c_notified * hospd_sn_dur * hospd_sn_prct * uc_visit_cur_usd * (1 - sh_sp) if year > 2011 & year <=2013 & iso3 == "COG"
drop sh_sp

replace c_hospital_nmdr =  c_hospital_sp + c_hospital_sn if year > 2011 & year <= 2013 & iso3 == "COG" //permanent refix of OP cost aggregate since existing value was too high

// IGB/PN 15 Jul 2020: INDia 2014 beddays to be backfilled from 2015
gsort iso3 -year
replace hospd_dstb_prct = hospd_dstb_prct[_n-1] if iso3 == "IND" & year == 2014 & hospd_dstb_prct == .
replace hospd_dstb_dur = hospd_dstb_dur[_n-1] if iso3 == "IND" & year == 2014 & hospd_dstb_dur == .
replace beddays_nmdr=c_notified*hospd_dstb_prct/100*hospd_dstb_dur if iso3 == "IND" & year == 2014
 
 // Comoros (COM): backfill utilisation values from 2017 till 2014 with 2018's report
 gsort iso3 -year
 replace hospd_dstb_dur = hospd_dstb_dur[_n-1] if iso3 == "COM" & year < 2018 & year >= 2014
 replace hospd_dstb_prct = hospd_dstb_prct[_n-1] if iso3 == "COM" & year < 2018 & year >= 2014
 
// CIV: backfill from 2015
 gsort iso3 -year 
 replace c_clinic_nmdr = c_clinic_nmdr[_n-1] if year <2015 & iso3 == "CIV"
 replace c_hospital_nmdr = c_hospital_nmdr[_n-1] if year <2015 & iso3 == "CIV"
 replace c_clinic_mdr = c_clinic_mdr[_n-1] if year <2015 & iso3 == "CIV"
 replace c_hospital_mdr = c_hospital_mdr[_n-1] if year <2015 & iso3 == "CIV"

 // CUba 2012 & 13
li year c_ghs_nmdr c_clinic_nmdr hcfvisit_s* hcfvisit_dstb c_notified uc_visit_cur_usd  if iso3 == "CUB" & year >= 2010
gsort iso3 year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if year > 2011 & year <=2013 & iso3 == "CUB"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year > 2011 & year <=2013 & iso3 == "CUB"

gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "CUB" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if iso3 == "CUB" & (year > 2011 & year <= 2013) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year > 2011 & year <=2013 & iso3 == "CUB"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year > 2011 & year <=2013 & iso3 == "CUB"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year > 2011 & year <= 2013 & iso3 == "CUB" //permanent refix of OP cost aggregate since existing value was too high

  //DJI 2011 and 2012 OP
 li year c_ghs_nmdr c_clinic_nmdr hcfvisit_s* hcfvisit_dstb c_notified uc_visit_cur_usd  if iso3 == "DJI" & year >= 2010
 gsort iso3 -year
 replace  hcfvisit_dstb =  hcfvisit_dstb[_n-1] if iso3 == "DJI" & year < 2019 & year >= 2014
 
 // ECuador 2012
 li year c_ghs_nmdr c_clinic_nmdr hcfvisit_s* hcfvisit_dstb c_notified uc_visit_cur_usd  if iso3 == "ECU" & year >= 2010
 gsort iso3 -year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if year == 2012 & iso3 == "ECU"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year == 2012 & iso3 == "ECU"

gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "ECU" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if iso3 == "ECU" & (year == 2012) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year ==2012 & iso3 == "ECU"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year ==2012 & iso3 == "ECU"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year == 2012 & iso3 == "ECU" //permanent refix of OP cost aggregate since existing value was too high


   // Guatemala 2010
 li year c_ghs_nmdr c_clinic_nmdr hcfvisit_s* hcfvisit_dstb c_notified new_sp_cur new_sp_coh new_snep_coh if iso3 == "GTM" & year >= 2010
 gsort iso3 year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if year == 2010 & iso3 == "GTM"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year == 2010 & iso3 == "GTM"

gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "GTM" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if iso3 == "GTM" & (year == 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year ==2010 & iso3 == "GTM"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year ==2010 & iso3 == "GTM"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year == 2010 & iso3 == "GTM" //permanent refix of OP cost aggregate since existing value was too high

  
  // Honduras: backfill from 2014 since previous estimates are unreliable
 li year c_ghs_nmdr c_clinic_nmdr c_hospital_nmdr c_clinic_mdr c_hospital_mdr if iso3 == "HND" & year >= 2008
 gsort iso3 -year 
 replace c_clinic_nmdr = c_clinic_nmdr[_n-1] if year <2014 & year >= 2008 & iso3 == "HND"
 replace c_hospital_nmdr = c_hospital_nmdr[_n-1] if year <2014 & year >= 2008 & iso3 == "HND"
 replace c_clinic_mdr = c_clinic_mdr[_n-1] if year <2014 & year >= 2008 & iso3 == "HND"
 replace c_hospital_mdr = c_hospital_mdr[_n-1] if year <2014 & year >= 2008 & iso3 == "HND"
 
  // Iran: replace c_clnic and c_hospital costs from 2013 and before (negative hospital costs pre 2014?)
 li year c_ghs_nmdr c_clinic_nmdr c_hospital_nmdr c_clinic_mdr c_hospital_mdr if iso3 == "IRN" & year <=2013
 gsort iso3 -year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if hcfvisit_sp == . & year <=2013 & iso3 == "IRN"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if hcfvisit_sn == . & year <=2013 & iso3 == "IRN"
gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "IRN" & (year >= 2006 & year <= 2013)
 gsort iso3 year 
 replace sh_sp = sh_sp[_n-1] if iso3 == "IRN" & (year == 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year ==2010 & iso3 == "IRN"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year ==2010 & iso3 == "IRN"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year == 2010 & iso3 == "IRN" //permanent refix of OP cost aggregate since existing value was too high

 replace hospd_sp_prct = hospd_dstb_prct if year <= 2013 & iso3 == "IRN" 
 replace hospd_sn_prct = hospd_dstb_prct if year <= 2013 & iso3 == "IRN" 
gen sh_sp = c_hospital_sp / (c_hospital_sp + c_hospital_sn) if iso3 == "IRN" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if (year > 2011 & year <=2013) & iso3 == "IRN" & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_hospital_sp = c_notified * hospd_sp_dur * hospd_sp_prct * uc_visit_cur_usd * sh_sp if year > 2011 & year <=2013 & iso3 == "IRN"
replace c_hospital_sn = c_notified * hospd_sn_dur * hospd_sn_prct * uc_visit_cur_usd * (1 - sh_sp) if year > 2011 & year <=2013 & iso3 == "IRN"
drop sh_sp

replace c_hospital_nmdr =  c_hospital_sp + c_hospital_sn if year <= 2013 & iso3 == "IRN" //permanent refix of OP cost aggregate since existing value was too high
 

 //Liberia (LBR): Replace values before 2014 with 2014 via hard code, since any changes in Prepare are made only for 2014 and above
 **
 
 
 //MDG: 2010 utilixation for OP was faulty. Replace with next year.
 li year c_ghs_nmdr c_clinic_nmdr hcfvisit_s* hcfvisit_dstb c_notified uc_visit_cur_usd  if iso3 == "MDG" & year >= 2010
 gsort iso3 -year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if year == 2010 & iso3 == "MDG"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year == 2010 & iso3 == "MDG"

gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "MDG" & (year >= 2006 & year <= 2013) 
replace sh_sp = sh_sp[_n-1] if iso3 == "MDG" & (year == 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year ==2010 & iso3 == "MDG"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year ==2010 & iso3 == "MDG"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year == 2010 & iso3 == "MDG"

// MDV: 2009 and 2010 IP Proporton backfilled with 2011
 li year c_ghs_nmdr c_clinic_nmdr c_hospital_nmdr c_clinic_mdr c_hospital_mdr if iso3 == "MDV" & year >=2009
 gsort iso3 -year 
gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "MDV" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if iso3 == "MDV" & (year >=2009 & year <= 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year >=2009 & year <= 2010 & iso3 == "MDV"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year >=2009 & year <= 2010 & iso3 == "MDV"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year >=2009 & year <= 2010 & iso3 == "MDV" 

 replace hospd_sp_prct = hospd_sp_prct[_n-1] if year >=2009 & year <= 2010 & iso3 == "MDV" 
 replace hospd_sn_prct = hospd_sn_prct[_n-1] if year >=2009 & year <= 2010 & iso3 == "MDV" 
 
gen sh_sp = c_hospital_sp / (c_hospital_sp + c_hospital_sn) if iso3 == "MDV" & (year >= 2006 & year <= 2013) 
replace sh_sp = sh_sp[_n-1] if year > 2010 & year <=2013 & iso3 == "MDV" & (year >=2009 & year <= 2010) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_hospital_sp = c_notified * hospd_sp_dur * hospd_sp_prct * uc_visit_cur_usd * sh_sp if year >=2009 & year <= 2010 & iso3 == "MDV"
replace c_hospital_sn = c_notified * hospd_sn_dur * hospd_sn_prct * uc_visit_cur_usd * (1 - sh_sp) if year >=2009 & year <= 2010 & year <=2013 & iso3 == "MDV"
drop sh_sp

replace c_hospital_nmdr =  c_hospital_sp + c_hospital_sn if year >=2009 & year <= 2010 & iso3 == "MDV" //permanent refix of OP cost aggregate since existing value was too high

  
//MLI: 2015 and 2014 percent utilisation to be back filled from 2016
li year c_ghs_nmdr c_clinic_nmdr c_hospital_nmdr c_clinic_mdr c_hospital_mdr if iso3 == "MLI" & year >=2009
replace c_hospital_nmdr = c_hospital_nmdr[_n-1] if year >= 2014 & year <=2015 & iso3 == "MLI"
  
// NER change hospd_mdr_dur = 120 if hospd_mdr_dur  == 30
replace hospd_mdr_dur = 120 if hospd_mdr_dur == 30 & year == 2014 & iso3 == "NER"

// PAK: replace 2013 values with 2014
 gsort iso3 year 
 replace c_clinic_nmdr = c_clinic_nmdr[_n+1] if iso3 == "PAK" & year == 2013
 replace c_hospital_nmdr = c_hospital_nmdr[_n+1] if iso3 == "PAK" & year == 2013

 //PER 2015 mdr % 
 gsort iso3 -year 
 replace hospd_mdr_prct = hospd_mdr_prct[_n-1] if  year == 2015 & iso3 == "PER"

 // RWA 2007 - 2012
 gsort iso3 -year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if year >= 2007 & year <= 2012 & iso3 == "RWA"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year >= 2007 & year <= 2012 & iso3 == "RWA"
gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "RWA" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if iso3 == "RWA" & (year >= 2007 & year <= 2012) & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year >= 2007 & year <= 2012 & iso3 == "RWA"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year >= 2007 & year <= 2012 & iso3 == "RWA"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year >= 2007 & year <= 2012 & iso3 == "RWA"


// SOM 2017: utilisation (hcfvisit_dstb) low
replace hcfvisit_dstb = hcfvisit_dstb[_n-1] if year == 2017 & iso3 == "SOM"


// TZA 2012
//twoway line c_clinic_nmdr year if iso3 == "TZA" ||  line c_hospital_nmdr year if iso3 == "TZA" || line c_ghs_nmdr year if iso3 == "TZA"
 gsort iso3 -year 
replace hcfvisit_sp =  hcfvisit_sp[_n-1] if year == 2012 & iso3 == "TZA"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if year == 2012 & iso3 == "TZA"
gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "TZA" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if year == 2012 & iso3 == "TZA" & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year == 2012 & iso3 == "TZA"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year == 2012 & iso3 == "TZA"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year == 2012 & iso3 == "TZA"
replace c_ghs_nmdr = c_clinic_nmdr + c_hospital_nmdr if year == 2012 & iso3 == "TZA" 

 //PRK 2014: Vsst = 0 hosp 100, days = as per 2015
//twoway line c_clinic_nmdr year if iso3 == "PRK" ||  line c_hospital_nmdr year if iso3 == "PRK" || line c_ghs_nmdr year if iso3 == "PRK"
// OP costs nmdr are almost 0 in 20123 - 13
 gsort iso3 -year 
 replace hcfvisit_sp =  hcfvisit_sp[_n-1] if  year >= 2012 & year <= 2013 & iso3 == "PRK"
replace hcfvisit_sn = hcfvisit_sn[_n-1] if  year >= 2012 & year <= 2013 & iso3 == "PRK"
gen sh_sp = c_clinic_sp / (c_clinic_sp + c_clinic_sn) if iso3 == "PRK" & (year >= 2006 & year <= 2013)
replace sh_sp = sh_sp[_n-1] if year >= 2012 & year <= 2013  & iso3 == "PRK" & (sh_sp == . | sh_sp == 0 | sh_sp == 1)

replace c_clinic_sp = c_notified * hcfvisit_sp * uc_visit_cur_usd * sh_sp if year >= 2012 & year <= 2013  & iso3 == "PRK"
replace c_clinic_sn = c_notified * hcfvisit_sn * uc_visit_cur_usd * (1 - sh_sp) if year >= 2012 & year <= 2013  & iso3 == "PRK"
drop sh_sp

replace c_clinic_nmdr =  c_clinic_sp + c_clinic_sn if year >= 2012 & year <= 2013  & iso3 == "PRK"

replace c_ghs_nmdr = c_clinic_nmdr + c_hospital_nmdr if  year >= 2012 & year <= 2013 & iso3 == "PRK" 

// IGB /PN 14 Jul 2020 VEN c_hospital_nmdr prior to 2014 is high and may need valdation in future.


//2021
/* PN: 2020 changes and cleaning */

// PN: 6 countries that had no income group label
replace g_income="HIC" if inlist(iso3, "AIA", "COK","MSR","NIU", "TKL", "WLF") 
//Also missing is Venezuela (2021) but for consistency keep it as prev year.
replace g_income = "UMC" if iso3 == "VEN"

//create a local variable to store the specific suffixes of the financial data's disaggregates
global suffix lab staff fld prog sld mdrmgt tpt tbhiv patsup orsrvy oth tot
 
// Armenia (ARM): replace per patient costs 2020 with budget 2021 if still empty.
li year exp_cpp_* if iso3 == "ARM" & year > 2018
li year budget_cpp_* if iso3 == "ARM" & year > 2018

gsort iso3 -year
foreach item in dstb mdr xdr tpt {
	replace exp_cpp_`item' = budget_cpp_`item'[_n-1] if iso3 == "ARM" & year == 2020 &   exp_cpp_`item' == .
	
}
gsort iso3 year

// Bangladesh (BGD): if hcvisit_mdr is 130 in 2019, replace with 80
li year hcfvisit_mdr if year >= 2018 & iso3 == "BGD"
replace hcfvisit_mdr = 80 if iso3 == "BGD" & year == 2019 & hcfvisit_mdr == 130

// Algeria (DZA): Have only reported GHS data. 
li iso3 year budget_tot cf_tot exp_tot rcvd_tot c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DZA"
li year budget_cpp* budget_sld tx_* if iso3 == "DZA" & year >= 2010
	
// Correct the historical anomalous value of budget_cpp_* in 2015
replace budget_cpp_dstb = budget_cpp_dstb / tx_dstb  if iso3 == "DZA" & year == 2015 & budget_cpp_dstb == 440000 
replace budget_cpp_mdr = budget_cpp_mdr / tx_mdr  if iso3 == "DZA" & year == 2015 & budget_cpp_mdr ==  42000 
replace budget_cpp_xdr = .  if iso3 == "DZA" & year == 2015 & budget_cpp_xdr == 0 
// In 2021 the _cpp values were used to estimate budget and cf subcomponent values, code under the temporary assumptions section of prepare do file
	
// Belarus (BLR): CONFIRM that exp_* values 2020 are larger than before, since they should include ghs costs
li year exp_fld exp_staff exp_lab exp_tbhiv exp_sld exp_mdrmgt exp_oth  exp_prog ///
 exp_orsrvy exp_patsup exp_tot if iso3 == "BLR" & year >= 2015

 
// Bosnia BIH: hcfvisit and hospd data for 2020 are reported and are similar to 2019. 
// Note: budget and exp data are much lower than previous years 
li iso3 year c_notified hcfvisit_dstb hospd_dstb_prct hospd_dstb_dur if iso3 == "BIH" // utilisation for DSTB
li iso3 year tx_mdr hcfvisit_mdr hospd_mdr_prct hospd_mdr_dur if iso3 == "BIH" //Utilization for MDR
li iso3 year budget_tot rcvd_tot cf_tot exp_tot c_notified tx_dstb if iso3 == "BIH"


// Bulgaria BGR: utilization data are reported for 2020
li iso3 year c_notified tx_dstb hcfvisit_dstb hospd_dstb_prct hospd_dstb_dur if iso3 == "BGR" // utilisation for DSTB
li iso3 year tx_mdr hcfvisit_mdr hospd_mdr_prct hospd_mdr_dur if iso3 == "BGR" //Utilization for MDR
//C_Notified will be carried over from 2019

// Cuba (CUB): change 0 to missing to allow imputation (the country could not isolate TB budgets or expenses from overall health data in 2020, so reported 0 instead of missing)
replace budget_tot = . if budget_tot == 0 & iso3 == "CUB" & year == latest_year
replace exp_tot = . if exp_tot == 0 & iso3 == "CUB" & year == latest_year - 1 

// CHad (TCD): Fill in exp with received funding: fill with last year if missing for 2020
li iso3 year exp_tot exp_fld rcvd_tot rcvd_fld c_notified tx_dstb if iso3 == "TCD" 
gsort iso3 year
foreach item in $suffix {
	replace exp_`item' = exp_`item'[_n-1] if iso3 == "TCD" & year == 2020 & exp_tot == .  
	replace rcvd_`item' = rcvd_`item'[_n-1] if iso3 == "TCD" & year == 2020 & rcvd_tot == . 
	
}



// Colombia (COL): correct hc_fvisit_DSTB in 2020 (from 12532 to what was reported previous year).
// MDR years 2013-2018 should be max 350
li iso3 year c_notified hcfvisit_dstb hcfvisit_mdr if iso3 == "COL"
replace hcfvisit_dstb = hcfvisit_dstb[_n-1] if year == 2020 & iso3 == "COL" & hcfvisit_dstb == 12532


// PRK: Replace hcfvisit_mdr in 2019 with 301 (confirmed by country in 2021). Note that exp_sld == 0 in 2020 (no DR drugs were purchased in 2020
li year hcfvisit_* exp_sld if iso3 == "PRK" & year >= 2019
replace hcfvisit_mdr = 301 if iso3 == "PRK" & year == 2019

//Djibouti (DJI):HCFVISIT 2020 are not blank. 
li year hcfvisit_* if iso3 == "DJI" & year >= 2019
replace budget_oth = cf_oth if year == 2020 & iso3 == "DJI" 

// Fiji: HCFvisit_dstb erroneously reported as c_notified, correct to reflect last year's value.
li year c_notified hcfvisit_* hospd_dstb_* hospd_mdr_* if iso3 == "FJI" & year >= 2015
replace hcfvisit_dstb = hcfvisit_dstb[_n-1] if iso3 == "FJI" & year == 2020 & hcfvisit_dstb == c_notified


// Gambia (GMB): Budget and cf missing for 2020
// IGB/PN Jul 2021: Gambia has exp reported in 2019. Fill in 2020 exp with 2019 and use same to fill n budget 2021 and 2020
li year budget_tot cf_tot exp_tot rcvd_tot budget_cpp_* if iso3 == "GMB" & year >= 2019
replace exp_tot = exp_tot[_n-1] if exp_tot == 0 & year == 2020 & iso3 == "GMB"
replace rcvd_tot = rcvd_tot[_n-1] if rcvd_tot == 0 & year == 2020 & iso3 == "GMB"

replace budget_tot = exp_tot[_n-1] if budget_tot == . & (year == 2021 | year == 2020) & iso3 == "GMB"
replace cf_tot = exp_tot[_n-1] if cf_tot == . & (year == 2021 | year == 2020) & iso3 == "GMB"


//Guatemala (GTM): Use expected finding instead of budget to remove negative gaps.
li year budget_tot cf_tot gap_tot if iso3 == "GTM"
foreach item in $suffix {
	replace budget_`item' = cf_`item' if iso3 == "GTM" & year >= 2019 & year <= 2021 & (gap_`item' < 0 | gap_`item' == .) //if Gap is -ve, replace 
	replace gap_`item' = budget_`item' - cf_`item' if iso3 == "GTM" & & year >= 2019 & year <= 2021
}
 
 //Libya (LBY): Fill in expenditure values 2019 and 2020, with budget values from 2020 and 2021 respectively
li iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "LBY" 
gsort iso3 -year
// Estimate expenditures with cf_* data.
replace budget_tot = budget_tot[_n-1] if budget_tot == . & year == 2021 & iso3 == "LBY" 
//replace budget_tot = budget_tot[_n-1] * round(c_notified / c_notified[_n-1]) if budget_tot == . & year < 2019 & year >= 2016  & iso3 == "LBY" //Budget 2020 copied down to 2006
//replace cf_tot = budget_tot if cf_tot == . & year <= 2019 & year >= 2016 & iso3 == "LBY"
foreach item in $suffix {
	replace exp_`item' = budget_`item'[_n-1] if (exp_`item' == . | exp_`item' == 0) & iso3 == "LBY" & year >= 2019 & year <= 2021 
	replace rcvd_`item' = budget_`item'[_n-1] if (rcvd_`item' == . | rcvd_`item' == 0) & iso3 == "LBY" & year >= 2019 & year <= 2021 
} 

//marshall (MHL):  exp_* replaced with exp_* [_n-1] for 2020.   
sort iso3 year
foreach item in $suffix {
	replace exp_`item' = exp_`item'[_n-1] if exp_`item'  == . & iso3 == "MHL" & year == 2020
}

// exp_cpp_dstb is wrongly estimated (200000 usd in 2020 compared to 100 in 2019. Replace with bidget_cpp_dstb for 2021
gsort iso3 -year
replace exp_cpp_dstb = budget_cpp_dstb[_n-1] if exp_cpp_dstb == 200000 & iso3 == "MHL" & year == 2020

// Moldova (MDA): if utilization data in 2020 is blank, carry forward from 2019
li year hcfvisit_dstb hospd_dstb_* hcfvisit_mdr hospd_mdr_* if iso3 == "MDA" & year >= 2019

// West Bank (PSE):  exp_cpp_dstb=budget_cpp_dstb{_n-1] if year==2020.. (the have GHS data also for wo previous years)
li year budget_cpp* exp_cpp_* c_notified if iso3 == "PSE"
gsort iso3 -year //sort by country and descending order of year (to backfill)
foreach item in dstb mdr xdr tpt {
		replace budget_cpp_`item' = budget_cpp_`item'[_n-1] if iso3 == "PSE" & year == 2019 
		replace exp_cpp_`item' = exp_cpp_`item'[_n-1] if iso3 == "PSE" & year == 2020
	}

//RUS: Note that patient costs for mdr and xdr are inseparable and are reported in exp_cpp_mdr
// also, $612 485 in exp_oth in 2020 is for TB vaccines

li year exp_tot exp_mdr exp_oth if iso3 == "RUS" & year == 2020

//Senegal (SEN): move 2020 budget_oth to budget_prog,  
// cf_oth to cf_prog and  gap_oth to gap_prog 
li year budget_oth cf_oth gap_oth budget_prog cf_prog gap_prog if iso3 == "SEN" & year >= 2019
replace cf_prog = cf_prog + cf_oth if iso3 == "SEN" & year == 2020
replace cf_oth = . if iso3 == "SEN" & year == 2020
replace budget_prog = budget_prog + budget_oth if iso3 == "SEN" & year == 2020
replace budget_oth = . if iso3 == "SEN" & year == 2020
replace gap_prog = gap_prog + gap_oth if iso3 == "SEN" & year == 2020
replace gap_oth = . if iso3 == "SEN" & year == 2020
// correct mdr utilization for 2019
li year hcfvisit* if iso3 == "SEN"
sort iso3 year
replace hcfvisit_mdr = hcfvisit_mdr[_n-1] if year == 2019 & iso3 == "SEN"


//Tuvalu IF 2021 not reported, use last year's values
li iso3 year budget_tot cf_tot exp_tot gap_tot rcvd_tot c_notified tx_dstb if iso3 == "TUV"
gsort iso3 year //sort by country and ascending order of year (2020 last)
foreach item in $suffix {
	replace budget_`item' = budget_`item'[_n-1] if iso3 == "TUV" & year == 2021 & budget_`item' == .
	replace cf_`item' = budget_`item' if iso3 == "TUV" & year == 2021 & cf_`item' == .	
	replace exp_`item' = exp_`item'[_n-1] if iso3 == "TUV" & year == 2020 & exp_`item' == .
	replace rcvd_`item' = exp_`item' if iso3 == "TUV" & year == 2020 & rcvd_`item' == .	
	
}

//UZB : Correction to exp_cpp_dstb, mdr and xdr in 2015 values were filled ni wrong place
replace exp_cpp_xdr = exp_cpp_mdr if year == 2015 & iso3 == "UZB"
replace exp_cpp_mdr = exp_cpp_dstb if year == 2015 & iso3 == "UZB"
replace exp_cpp_dstb = . if year == 2015 & iso3 == "UZB"
replace exp_cpp_xdr = exp_cpp_xdr[_n-1] if year >= 2014 & year < 2016 & iso3 == "UZB"
replace exp_cpp_mdr = exp_cpp_mdr[_n-1] if year >= 2014 & year < 2016 & iso3 == "UZB"
replace exp_cpp_dstb = exp_cpp_dstb[_n-1] if year >= 2014 & year < 2016 & iso3 == "UZB"

//ZAF: Note that 2020 TBHIV reported committed funds are 0. No change made 
li year cf_nmdr cf_nmdr_dot cf_nmdr_dot_fld cf_nmdr_dot_nfld_lab cf_nmdr_ndot_hiv cf_nmdr_ndot_tpt cf_nmdr_ndot_nhiv_oth cf_nmdr_ndot_nhiv_noth if iso3 == "ZAF"

//replace total amounts if missing
	replace rcvd_tot = cf_tot if iso3=="ZAF" & year >= 2014 & rcvd_tot == .
	replace rcvd_int = cf_int if iso3=="ZAF" & year >= 2014 & rcvd_int == .
	replace rcvd_ext_gf = cf_ext_gf if iso3=="ZAF" & year >= 2014 & rcvd_ext_gf == .
	replace rcvd_ext_ngf = cf_ext_ngf if iso3=="ZAF" & year >= 2014 & rcvd_ext_ngf == .
	replace exp_tot = cf_tot if iso3=="ZAF" & year >= 2014 & exp_tot == .
	

//2022
/* PN: 2022 changes and cleaning */

// PN: some countries have no income group label

tab country if g_income == ""
replace g_income="HIC" if inlist(iso3, "AIA", "COK","MSR","NIU", "TKL", "WLF") & g_income == ""
//Also missing is Venezuela (since July 2021) but for consistency keep it as prev year.
replace g_income = "UMC" if iso3 == "VEN" & g_income == ""

gsort iso3 year

//create a local variable to store the specific suffixes of the financial data's disaggregates
global suffix lab staff fld prog sld mdrmgt tpt tbhiv patsup orsrvy oth tot
 
/*
// Updating the totals fields - template code
egen budget_tot_temp = rowtotal(budget_fld budget_staff budget_lab budget_tpt budget_tbhiv budget_sld budget_mdrmgt budget_oth budget_prog budget_orsrvy budget_patsup) if iso3 == "ARM" & year == 2021
replace budget_tot = budget_tot_temp if iso3 == "ARM" & year == 2021
egen cf_tot_temp = rowtotal(cf_fld cf_staff cf_lab cf_tpt cf_tbhiv cf_sld cf_mdrmgt cf_oth cf_prog cf_orsrvy cf_patsup) if iso3 == "ARM" & year == 2021
replace cf_tot = cf_tot_temp if iso3 == "ARM" & year == 2021
egen gap_tot_temp = rowtotal(gap_fld gap_staff gap_lab gap_tpt gap_tbhiv gap_sld gap_mdrmgt gap_oth gap_prog gap_orsrvy gap_patsup) if iso3 == "ARM" & year == 2021
replace gap_tot = gap_tot_temp if iso3 == "ARM" & year == 2021
drop budget_tot_temp cf_tot_temp gap_tot_temp

*/
 
// Armenia (ARM)
li year exp_cpp_* if iso3 == "ARM" & year > 2018
li year budget_cpp_* if iso3 == "ARM" & year > 2018

// Note that 2021 budget_fld was about 9 fold of prior years, but expenditure in 2022 reflects expected amounts. In future country should correct this. For now, hard code the change where we move the _fld values to _oth variable
li year budget_fld budget_oth budget_tot if iso3 == "ARM" & year > 2018
replace budget_oth = budget_oth + budget_fld if iso3=="ARM" & year == 2021 & budget_fld == 428693 
replace budget_fld = . if iso3=="ARM" & year == 2021 & budget_fld == 428693 
replace cf_oth = cf_oth + cf_fld if iso3=="ARM" & year == 2021 & cf_fld == 359663
replace cf_fld = . if iso3=="ARM" & year == 2021 & cf_fld == 359663
replace gap_oth = gap_oth + gap_fld if iso3=="ARM" & year == 2021 & gap_fld == 69030
replace gap_fld = . if iso3=="ARM" & year == 2021 & gap_fld == 69030


// Azerbaijan: No report for finance nor utilisation. 
li iso3 year budget_tot cf_tot exp_tot rcvd_tot c_notified tx_dstb if iso3 == "AZE" & year >= 2016 
// PN 2022: Allowing imputation to fill in finance (since there are spending records from 2016 to 2020 and budget 2021)
// for _cpp, carry forward from last report. Utilization will be automatically carried forward in prepare do file
li iso3 year budget_cpp_* if iso3 == "AZE" & year >= 2016
li iso3 year exp_cpp_* if iso3 == "AZE" & year >= 2016
gsort iso3 year
foreach item in dstb mdr xdr tpt {
	replace exp_cpp_`item' = exp_cpp_`item'[_n-1] if iso3 == "AZE" & year >= (latest_year-2) & year < latest_year & exp_cpp_`item' == .  
	replace budget_cpp_`item' = budget_cpp_`item'[_n-1] if iso3 == "AZE" & (year >= latest_year - 1) & budget_cpp_`item' == . 
	
} 

// Botswana: No report in 2022. Allow imputation to fill in - No further action.
// Utilization available. Budget_cpp values are same as last year, and have reduced from previous years (_dstb reduced from 
// 120 to 25, _mdr reduced from 8000 to 900.) Since exp_cpp are not yet reported in 2021, replace with budget_cpp  
li iso3 year budget_cpp_* if iso3 == "BWA" & year > 2016
li iso3 year exp_cpp_* if iso3 == "BWA" & year >= 2016
gsort iso3 -year // latest year first, so that we take latest budget_cpp value and apply to prevoius year's exp_cpp
foreach item in dstb mdr xdr tpt {
	replace exp_cpp_`item' = budget_cpp_`item'[_n-1] if iso3 == "BWA" & year >= (latest_year-2) & year < latest_year & exp_cpp_`item' == .  
	
} 
gsort iso3 year

// Cuba (CUB): change legacy 0s to missing to allow imputation - CUBA was a short form country till 2020.  
// (Note that the country could not isolate TB budgets or expenses from overall health data in 2020, so reported 0 instead of missing)
li year budget_tot cf_tot cf_int cf_ext_gf rcvd_ext_gf exp_tot rcvd_tot if iso3 == "CUB" & year > 2010
replace budget_tot = . if budget_tot == 0 & iso3 == "CUB" & year >= latest_year - 1
replace cf_tot = budget_tot if cf_tot == . & iso3 == "CUB" & year >= 2014 
replace exp_tot = . if exp_tot == 0 & iso3 == "CUB" & year >= latest_year - 1 
replace rcvd_tot = . if rcvd_tot == 0 & iso3 == "CUB" & year >= latest_year - 1 
replace cf_ext_gf = . if cf_ext_gf == 0 & iso3 == "CUB" & year == 2020
replace rcvd_ext_gf = . if rcvd_ext_gf == 0 & iso3 == "CUB" & year >= 2014


// Algeria (DZA): Have only reported utilization data. 
li year budget_tot cf_tot exp_tot rcvd_tot if iso3 == "DZA" & year > 2016
li year c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DZA" & year > 2016
li year budget_cpp* budget_sld tx_* if iso3 == "DZA" & year >= 2010
	
// hospd_mdr_dur in 2021 reported as 60 from prevoius years' 30. change back to 30 for 2021.
replace hospd_mdr_dur = 30 if iso3 == "DZA" & year == 2021 & hospd_mdr_dur == 60
// In 2021 the _cpp values were used to estimate budget and cf subcomponent values, code under the temporary assumptions section of prepare do file


//Djibouti (DJI): No reporting. We are aware that GF disbusrsements are closed as at 2021. 
li year budget_tot cf_tot cf_int cf_ext_gf exp_tot rcvd_tot if iso3 == "DJI" & year > 2016
li year c_notified hcfvisit_mdr hcfvisit_dstb hospd_dstb_dur hospd_mdr_dur  if iso3 == "DJI" & year > 2016
li year budget_cpp* budget_sld tx_* if iso3 == "DJI" & year >= 2010


// Egypt: Temporarily fill in with GF data as done last year. They reported 2022 budgets
li year budget_tot cf_tot cf_int cf_ext_gf exp_tot rcvd_tot if iso3 == "EGY" & year > 2016


// Guinea Bissou: Correct 2022 error
li year budget_prog budget_tot gap_prog gap_tot if iso3 == "GNB" & year == 2022
replace budget_prog = 1640734 if budget_prog == 16407340 & iso3 == "GNB" & year == 2022
replace gap_prog = budget_prog - cf_prog if gap_prog == 15009281 & iso3 == "GNB" & year == 2022

replace budget_tot = budget_lab + budget_staff + budget_fld + budget_prog + budget_sld + budget_mdrmgt + budget_tpt + budget_tbhiv + budget_patsup + budget_orsrvy + budget_oth if iso3 == "GNB" & year == 2022
replace gap_tot = gap_lab + gap_staff + gap_fld + gap_prog + gap_sld + gap_mdrmgt + gap_tpt + gap_tbhiv + gap_patsup + gap_orsrvy + gap_oth if iso3 == "GNB" & year == 2022

* PN 6 Sept 2022: India program staff spending in 2021
* India noted that there was a release of salary of large number of sub national staff directly 
* from the State Health Mission and not from NTEP budgets (40 million). Therefore this is added into their received funding
*
li year rcvd_staff exp_staff gap_staff rcvd_tot exp_tot if iso3 == "IND" & year >= 2020
replace rcvd_staff = rcvd_staff + 40000000 if iso3 == "IND" & year == 2021 & rcvd_staff == 39795347
replace exp_staff = rcvd_staff if iso3 == "IND" & year == 2021 

replace rcvd_tot = rcvd_tot + 40000000 if iso3 == "IND" & year == 2021 & rcvd_tot == 296775986
replace exp_tot = rcvd_tot if iso3 == "IND" & year == 2021 

replace rcvd_tot_domestic = rcvd_tot_domestic + 40000000 if iso3 == "IND" & year == 2021 & rcvd_tot_domestic == 143040058

li year budget_tot cf_tot rcvd_tot exp_tot rcvd_int if iso3 == "IND" & year >= 2020
 

// Jordan
li year budget_oth budget_lab budget_tot if iso3 == "JOR" & year >= 2021
replace budget_lab = budget_lab + 4000000 - budget_oth[_n-1] if budget_oth == 4000000 & iso3 == "JOR" & year == 2022
replace budget_oth = budget_oth[_n-1] if budget_oth == 4000000 & iso3 == "JOR" & year == 2022


 //Libya (LBY): Fill in expenditure values 2019 and 2020, with budget values from 2020 and 2021 respectively
li iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "LBY" 
gsort iso3 -year
// Estimate expenditures with cf_* data.
replace budget_tot = budget_tot[_n-1] if budget_tot == . & year == 2022 & iso3 == "LBY" 
foreach item in $suffix {
	replace exp_`item' = budget_`item'[_n-1] if (exp_`item' == . | exp_`item' == 0) & iso3 == "LBY" & year >= 2020 & year <= 2022 
	replace rcvd_`item' = budget_`item'[_n-1] if (rcvd_`item' == . | rcvd_`item' == 0) & iso3 == "LBY" & year >= 2020 & year <= 2022 
}

//Marshall island MHL: Utilisation (hcfvisit ) reported as 119 visits up from 3 in 2020. Headed back to previous value 180 in 2019. No change to be dones 


// Namibia: Hadnt reported earlier, needs careful review
li country iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "NAM"
 li country iso3 year  rcvd_tot_domestic   rcvd_tot_gf rcvd_tot_grnt rcvd_tot_usaid rcvd_tot gap_tot if iso3 == "NAM" // Domestic funding has dipped to 25% of last year.

// Vanuatu - undervalued. Awaiting clarifications from country
li country iso3 year budget_tot cf_tot rcvd_tot exp_tot gap_tot if iso3 == "VUT"
replace budget_tot = . if budget_tot == 614 & iso3 == "VUT" & year == 2022
replace cf_tot = . if cf_tot == 614 & iso3 == "VUT" & year == 2022
replace exp_tot = . if exp_tot == 460 & iso3 == "VUT" & year == 2021
replace rcvd_tot = . if rcvd_tot == 460 & iso3 == "VUT" & year == 2021


* TRK
replace exp_tot = . if iso3 == "TKM" & year == 2021 & exp_tot == 0
replace rcvd_tot = . if iso3 == "TKM" & year == 2021 & rcvd_tot == 0


	

