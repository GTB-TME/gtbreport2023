# WHO Global Tuberculosis Report 2023

Code and data used to develop the WHO Global Tuberculosis Report for 2023. The report was published on 7 November 2023 at https://www.who.int/teams/global-tuberculosis-programme/tb-reports/global-tuberculosis-report-2023/

# Folders

* **data**: External datasets:

  * **bcg**: BCG coverage indicator from the WHO Global Health Observatory

  * **gtb**: R binary datasets extracted from the WHO gobal TB database:

    * **other**: Country lists, population estimates, reference lists, survey results and external indicators

    * **snapshot_yyyy-mm-dd**: Snapshot of country-reported data

  * **ihme**: VR data from IHME

  * **mortality**: VR data from the WHO Mortality database

  * **unaids**: HIV estimates from UNAIDS

* **disaggregation**: Pete Dodd's R scripts to estimate incidence and mortality disaggregated by age group and sex.

* **doc**: A PDF version of the web-based data collection form used by countries to report data to WHO. The PDF also shows database variable names.

* **drtb**: Pete Dodd's R and Stan scripts to estimate drug-resistant TB burden.

* **dynamic**: Nim Arinaminpathy's Matlab code for dynamic modelling of TB incidence and mortality for the period 2020-2022 in selected countries.

* **finance**: Andrew Siroka and Peter Nguhiu's R and Stata code for analysing TB financing data.

* **import**: R scripts to load saved data files from the WHO gobal TB database and the Global Health Observatory.

* **inc_mort**: Mathieu Bastard and Philippe Glaziou's R scripts to produce estimates of TB incidence and mortality

* **lives_saved**: Takuya Yamanaka and Philippe Glaziou's R scripts to produce estimates of the number of death averted by TB treatment and ART since 2005.

* **report**: R Markdown scripts by Mathieu Bastard/Philippe Glaziou, Irwin Law, Peter Nguhiu, Hazim Timimi and Takuya Yamanaka to generate tables, static figures, interactive Kendo UI charts and text for the 2023 edition of the WHO Global Tuberculosis Report web pages and the report PDF. 

# Global tuberculosis database data sets

The following sections show the data object names chosen in previous years. If you use the `load_gtb()` function you don't need to know if a data object is part of a snapshot or other, not does your code have to use the same object name.

For example, the following line loads the most recently saved snapshot of notifications into a dataframe / data table called `notifs`:
```
notifs <- load_gtb("tb")
```

## Snapshot data

* **agg**: TB/HIV indicators with rules to be used to calculate aggregates from `view_TME_master_TBHIV_for_aggregates`

* **covid**: Impact of COVID on services and response to the UNHLM commitments from `view_TME_master_covid_unhlm`

* **drfq**: DRS records used to estimate fluoroquinolone resistance among RR-TB patients from `view_DRS_for_estimation_sldst`

* **drhnew**: DRS records used to estimate HR-TB among new TB patients from `view_DRS_for_estimation_new_INH`

* **drhret**: DRS records used to estimate HR-TB among previously treated TB patients from `view_DRS_for_estimation_ret_INH`

* **drnew**: DRS records used to estimate RR-TB among new TB patients from `view_DRS_for_estimation_new`

* **drret**: DRS records used to estimate RR-TB among previously treated TB patients from `view_DRS_for_estimation_ret`

* **drroutine**: Routine drug resistance surveillance records from `view_TME_master_dr_surveillance`

* **ltbi**: Estimates of TPT coverage among children (numbers derived from reported data) from `view_TME_estimates_ltbi`

* **monthly**: Provisional monthly or quarterly notifications from `dcf.latest_provisional_c_newinc`

* **sty**: Services, PPM, community engagement, M&E systems from `view_TME_master_strategy`

* **tb**: TB notifications from `view_TME_master_notifications`

* **tpt**: TB preventive treatment from `view_TME_master_contacts_tpt`

* **tx**: Treatment outcomes from `view_TME_master_outcomes`

* **vrgtb**: VR data reported by countries in the European Region from `dcf.latest_vr`


## Other data

### Explanatory 

* **dic**: Data dictionary from `view_TME_data_dictionary`

* **codes**: Meaning of codes used for categorical variables from `view_TME_data_codes`

### Reference: countries, country groups, population and SDG indicators

* **cty**: Country and area names in 4 languages, their codes and WHO region and status from `view_TME_master_report_country`

* **datacoll**: Options set for the data collection form for each country-data collection year combinations from `view_TME_master_data_collection`

* **grptypes**: Themes by which to group countries from `view_country_group_types`

* **grp**: Country groups within each grouping theme (e.g. the 4 income groups of High, Upper Middle, Lower Middle and Low in the World Bank income classification) from `view_country_groups`

* **grpmbr**: Countries belonging to each country group from `view_country_group_membership`

* **pop**: UN Population Division population estimates from `view_TME_estimates_population`

* **sdg**: SDG indicator data and codes relevant to TB incidence from `external_indicators.view_indicator_data`

* **sdgdef**: Full names of SDG indicators and their sources from `"external_indicators.view_indicator_definition`

### Surveys and survey results

* **svy.cc**: Results of catastrophic costs surveys from `survey.view_catastrophic_costs_survey`

* **svy.prev**: Prevalence estimates resulting from prevalence surveys from `survey.view_prevalence_survey_estimates`

* **svy.prevcases**: Numbers of TB cases found in prevalence surveys from `survey.view_prevalence_survey_cases`

* **svy.prevchar**: Characteristics of prevalence surveys from `survey.view_prevalence_survey`

Codes used in survey records:

* **svy.agegr**: Codes for age groups from `survey.age_group`

* **svy.areatype**: Codes for area types from `survey.area_type`

* **svy.casetype**: Codes for case types from `survey.case_type`

* **svy.patientgr**: Codes for patient groups from `survey.patient_group`

* **svy.screen**: Codes for screening methods from `survey.screen_group`

* **svy.sex**: Codes for sex from `survey.sex`
