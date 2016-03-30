//***************Step 1 - Preparing Harvard Patent database citation 75-99 dataset******************************
use "F:\Thesis\Basic Data\Harvard Patent database\citation75_99\citation75_99.dta", clear
unique patent
/*Number of unique values of patent is  2116395
Number of records is  16301992*/
bysort patent : gen seq = _n
drop citseq
label variable seq "CitSeq"
rename seq citseq
by patent : keep if _n==_N
//(14185597 observations deleted)
misstable summarize citseq
//(variables nonmissing or string)
unique patent
/*Number of unique values of patent is  2116395
Number of records is  2116395*/
save "F:\Thesis\Prepared Data\1-Harvard Patent Database 1975-1999\prepared_citation75_99.dta"
//file F:\Thesis\Prepared Data\1-Harvard Patent Database 1975-1999\prepared_citation75_99.dta saved
clear

//***************Step 2 - Preparing Harvard Patent database citation 00-10 dataset******************************
insheet using "F:\Thesis\Basic Data\Harvard Patent database\citation00_10\citation00_10.csv"
//(8 vars, 37225313 obs)
unique patent
/*Number of unique values of patent is  2013990
Number of records is  37225313*/
egen new_citseq = group(patent)
gen seq = 1
bysort new_citseq : replace seq = _n
drop citseq
label variable seq "CitSeq"
rename seq citseq
by new_citseq: keep if _n==_N
//(35211323 observations deleted)
misstable summarize citseq
drop new_citseq
save "F:\Thesis\Prepared Data\2-Harvard Patent Database 2000-2010\prepared_citation00-10.dta"
//file F:\Thesis\Prepared Data\2-Harvard Patent Database 2000-2010\prepared_citation00-10.dta saved


***************Step 3 - Preparing Harvard Patent database citation 75-10 dataset****************************
// Merging both 75-99 (2116395 obs) & 00-10 (2013990 obs) patent citation datasets
use "F:\Thesis\Prepared Data\1-Harvard Patent Database 1975-1999\prepared_citation75_99.dta", clear
// Dropping variables cit_date, cit_name, cit_kind, cit_country, category from both the 75-99 & 00-10 patent citation datasets
drop  cit_date cit_name cit_kind cit_country category
save "F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\prepared_citation75-99_after_dropping_vars.dta"
//file F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\prepared_citation75-99_after_dropping_vars.dta saved
use "F:\Thesis\Prepared Data\2-Harvard Patent Database 2000-2010\prepared_citation00-10.dta", clear
drop  cit_date cit_name cit_kind cit_country category
save "F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\prepared_citation00-10_after_dropping_vars.dta"
//file F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\prepared_citation00-10_after_dropping_vars.dta saved
use "F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\prepared_citation75-99_after_dropping_vars.dta", clear
append using "F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\prepared_citation00-10_after_dropping_vars.dta"
//citation was str8 now str20
drop citation
rename citseq cit_count
save "F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\Final\prepared_citation75-10.dta"
file F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\Final\prepared_citation75-10.dta saved
misstable summarize  cit_count
//(variables nonmissing or string)


****************Step 4 - Preparing Amit Seru Patent database**************************************************
insheet using "F:\Thesis\Basic Data\Amit Seru Patent database\patents\patents.txt"
//(5 vars, 6237596 obs)
gen prefix = 0
egen patent = concat(prefix patnum)
misstable summarize  permno
drop if missing(permno)
//(4310017 observations deleted)
drop patnum pubdate prefix
save "F:\Thesis\Prepared Data\4-Amit Seru Patent database\Amit_Seru_patent_permno.dta"
//file F:\Thesis\Prepared Data\4-Amit Seru Patent database\Amit_Seru_patent_permno.dta saved


****************Step 5 - Merging Harvard Patent database and Amit Seru Patent database***********************
use "F:\Thesis\Prepared Data\3-Harvard Patent Database 1975-2010\Final\prepared_citation75-10.dta"
joinby patent using "F:\Thesis\Prepared Data\4-Amit Seru Patent database\Amit_Seru_patent_permno.dta", unmatched(none)
misstable summarize cit_count
//(variables nonmissing or string)
misstable summarize fdate
/* Obs<.
                                                +------------------------------
               |                                | Unique
      Variable |     Obs=.     Obs>.     Obs<.  | values        Min         Max
  -------------+--------------------------------+------------------------------
         fdate |       822             1408029  |   >500   1.87e+07    9.19e+07
*/
misstable summarize issdate
//(variables nonmissing or string)
misstable summarize permno
//(variables nonmissing or string)
save "F:\Thesis\Prepared Data\5-Harvard Amit Seru Merged Patent Database\Harvard_Amit_Seru_Merged_Patent_Database.dta"
//file F:\Thesis\Prepared Data\5-Harvard Amit Seru Merged Patent Database\Harvard_Amit_Seru_Merged_Patent_Database.dta saved

***************Step 6 - Preparing Thomson One Acquisitions dataset*************************************************
insheet using "F:\Thesis\Prepared Data\6-Thomson One Acquisitions Data\Acquisitions Final dataset.csv", comma
//(54 vars, 230632 obs)
gen edate1 = mdy(m1,d1,y1)
format edate1 %td
gen edate2 = mdy(m2,d2,y2)
//(8 missing values generated)
format edate2 %td
sort edate2
gen edate3 = mdy(m3,d3,y3)
format edate3 %td
drop  dateannounced dateeffective dateeffectiveunconditional d1 y1 m1  d2 m2 y2  d3 m3 y3
order  edate1, first
order edate2, after(edate1)
order edate3, after(edate2)
rename edate1 date_announced
rename edate2 date_effective
rename edate3 date_effective_unconditional
rename targetcusip target_cusip
rename targetprimarysiccode target_primary_sic_code
rename targetnation target_nation
rename acquirorcusip acquiror_cusip
rename acquirorprimarysiccode acquiror_primary_sic_code
rename ofsharesacq of_shares_acq
rename ownedaftertransaction owned_after_transaction
rename  enterprisevalueatannouncementmil enterprise_value_at_announcement
rename  equityvalueatannouncementmil equity_value_at_announcement
rename  pricepershare price_per_share
rename  valueoftransactionmil value_of_transaction
rename v52 offerpricepremium1weekpriortoann
rename v53 offerpricepremium4weekpriortoann
label variable date_announced "Date of announcement"
label variable date_effective "Effective date of transaction"
label variable date_effective_unconditional "Effective/Unconditional date"
save "F:\Thesis\Prepared Data\6-Thomson One Acquisitions Data\Acquisitions Final dataset-1.dta"
//file F:\Thesis\Prepared Data\6-Thomson One Acquisitions Data\Acquisitions Final dataset-1.dta saved
export excel using "F:\Thesis\Prepared Data\6-Thomson One Acquisitions Data\Acquisitions Final dataset-1.xlsx"
drop in 230633/1048575
//(817943 observations deleted)

************************Step 7 - Filtering of Thomson One dataset  
use "F:\Thesis\Prepared Data\New Method\2-Thomson One dataset without updated CUSIP\Acquisitions Ref dataset.dta", clear
duplicates drop
gen owned_before_transaction = owned_after_transaction - of_shares_acq
order owned_before_transaction, before(of_shares_acq)
label variable owned_before_transaction "% of Shares owned before transaction"
keep if owned_before_transaction < 50 & owned_after_transaction > 50
gen year = year(date_announced)
gen month = month(date_announced)
order year, first
order month, after(year)
label variable year "Year"
label variable month "Month"
keep year month date_announced date_effective date_effective_unconditional target_cusip target_primary_sic_code acquiror_cusip acquiror_primary_sic_code value_of_transaction
sort  acquiror_cusip  year  month
rename value_of_transaction value_of_acquisition
gen target_primary_sic_code_3digit = substr(target_primary_sic_code,1,3)
gen acquiror_primary_sic_code_3digit = substr(acquiror_primary_sic_code,1,3)
gen acquisition_same_industry = 0
replace acquisition_same_industry = 1 if acquiror_primary_sic_code_3digit == target_primary_sic_code_3digit
gen value_acquisition_same_industry = acquisition_same_industry * value_of_acquisition
gen acquisition_another_industry = 0
replace acquisition_another_industry = 1 if acquiror_primary_sic_code_3digit != target_primary_sic_code_3digit
gen val_acquisition_another_industry = acquisition_another_industry * value_of_acquisition
drop target_primary_sic_code_3digit acquiror_primary_sic_code_3digit
save "F:\Thesis\Prepared Data\New Method\3-Thomson One dataset filtered\3-Thomson One dataset filtered.dta", replace


**********************Step 8 - Generating patent expiration year
use "F:\Thesis\Prepared Data\New Method\5-Patent database with ncusip\Patent database with ncusip - revised.dta", clear
gen year_of_issuance = year(issdate)
gen year_of_filing = year(fdate)
label variable year_of_issuance "Year of issuance of patent"
label variable year_of_filing "Year of filing of patent"
drop if year_of_filing == .
drop if year_of_filing >  year_of_issuance
gen expiration_year = year_of_filing + 20
gen Uruguay_Round_Agreement_date = 19950608
tostring Uruguay_Round_Agreement_date, replace format(%20.0f)
gen date1 = date(Uruguay_Round_Agreement_date,"YMD")
format date1 %td
drop Uruguay_Round_Agreement_date
rename date1 Uruguay_Round_Agreement_date
replace expiration_year = max(year_of_filing + 20,year_of_issuance + 17) if fdate < Uruguay_Round_Agreement_date
drop Uruguay_Round_Agreement_date
label variable expiration_year "Patent Expiration Year"
save "F:\Thesis\Prepared Data\New Method\6-Patent database with expiration year\patent database with expiration year.dta"


**********************Step 9 - Generating patent value by firm-year
use "F:\Thesis\Prepared Data\New Method\6-Patent database with expiration year\patent database with expiration year.dta", clear
sort patent
expand 20
bysort patent: gen years_left_to_expiration = expiration_year - year_of_issuance - _n 
label variable years_left_to_expiration "Years left for patent expiration"
count if years_left_to_expiration > 20
//0
drop if years_left_to_expiration < 0
//(3367770 observations deleted)
summarize  years_left_to_expiration
count if years_left_to_expiration == .
//0
gen year = year_of_issuance + years_left_to_expiration
count if year >  expiration_year
// 0
sort ncusip year
by ncusip year : gen patent_value = sum(cit_count)
by ncusip year : replace patent_value = patent_value[_N]
keep ncusip year patent_value
duplicates drop
drop if year > 2010
save "F:\Thesis\Prepared Data\New Method\7-Patent value by firm-year\Patent value by firm-year.dta", replace


*********************Step 10 - Merging Acquisitions and Patent Value dataset
use "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", clear
rename  year_announcement year
rename  month_announcement month
save "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", replace
use "F:\Thesis\Prepared Data\New Method\3-Thomson One dataset filtered\3-Thomson One dataset filtered.dta", clear
rename acquiror_cusip cusip6
joinby cusip6 year month using "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", unmatched(master) _merge(_merge)
count if _merge == 1
count if _merge == 3
drop if _merge == 1
drop _merge
rename ncusip acquiror_ncusip
drop cusip6 permno cusip
drop date_effective_unconditional
order acquiror_ncusip, after(date_effective)


drop if year > 2010
rename year year_announcement
rename acquiror_ncusip ncusip
label variable year_announcement "Year of Announcement"
joinby ncusip using "F:\Thesis\Prepared Data\New Method\7-Patent value by firm-year\Patent value by firm-year.dta", unmatched(both) _merge(_merge)
sort ncusip year
drop if missing(ncusip)
order ncusip, first
order year, after(ncusip)
duplicates drop
rename ncusip acquiror_ncusip
rename target_cusip cusip6
rename _merge _merge_acquiror
rename patent_value acquiror_patent_value
drop if year == .
rename month month_announcement
save "F:\Thesis\Prepared Data\New Method\8-Acquisitions dataset for merge with Patent value\Acquisitions dataset for merge with Patent value.dta",replace


******************Step 11
use "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta",clear
rename year year_announcement
rename month month_announcement
save "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", replace

use "F:\Thesis\Prepared Data\New Method\8-Acquisitions dataset for merge with Patent value\Acquisitions dataset for merge with Patent value.dta",clear
joinby cusip6 year_announcement month_announcement using "F:\Thesis\Prepared Data\8-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", unmatched(master) _merge(_merge)
drop if ncusip == acquiror_ncusip 
drop permno cusip _merge cusip6
joinby ncusip year using "F:\Thesis\Prepared Data\New Method\7-Patent value by firm-year\Patent value by firm-year.dta", unmatched(master) _merge(_merge)
rename ncusip target_ncusip
rename _merge _merge_target
rename patent_value target_patent_value
replace target_patent_value = 0 if _merge_target == 1
replace acquiror_patent_value = 0 if _merge_acquiror == 1
gen patent_value =  acquiror_patent_value + target_patent_value
save "F:\Thesis\Prepared Data\New Method\9-Acquisitions-Patent Merged dataset\acquisitions-patent merged dataset.dta",replace


****************************Step 12 - Acquiror becoming target
keep target_ncusip date_effective
gen year_of_acquisition = year(date_effective)
duplicates drop target_ncusip, force
drop if missing(target_ncusip)
drop date_effective
rename target_ncusip acquiror_ncusip
save "F:\Thesis\Prepared Data\New Method\10-Acquiror becoming target\Acquiror becoming target.dta",replace


*************************Step 13 - Updated patent value
use "F:\Thesis\Prepared Data\New Method\9-Acquisitions-Patent Merged dataset\acquisitions-patent merged dataset.dta",clear
joinby acquiror_ncusip using "F:\Thesis\Prepared Data\New Method\10-Acquiror becoming target\Acquiror becoming target.dta", unmatched(master) _merge(_merge)
duplicates drop
sort acquiror_ncusip year
rename year_of_acquisition year_of_getting_acquired
drop _merge
save "F:\Thesis\Prepared Data\New Method\11-Updated patent value using acquiror becoming target\acquisitions patent dataset for merge with acquiror as target.dta",replace

by acquiror_ncusip year : replace target_patent_value = sum(target_patent_value)
by acquiror_ncusip year : replace target_patent_value = target_patent_value[_N]
by acquiror_ncusip year : replace patent_value = acquiror_patent_value + target_patent_value
duplicates drop acquiror_ncusip year, force
keep acquiror_ncusip year patent_value
rename acquiror_ncusip target_ncusip
rename patent_value target_patent_value_1
save "F:\Thesis\Prepared Data\New Method\11-Updated patent value using acquiror becoming target\acquiror as target with patent value.dta",replace

use "F:\Thesis\Prepared Data\New Method\11-Updated patent value using acquiror becoming target\acquisitions patent dataset for merge with acquiror as target.dta",clear
joinby target_ncusip year using "F:\Thesis\Prepared Data\New Method\11-Updated patent value using acquiror becoming target\acquiror as target with patent value.dta", unmatched(master) _merge(_merge)
replace target_patent_value = target_patent_value_1 if target_patent_value_1!=.
replace patent_value = acquiror_patent_value + target_patent_value
drop  _merge target_patent_value_1
drop  value_of_acquisition acquisition_same_industry value_acquisition_same_industry acquisition_another_industry val_acquisition_another_industry

sort acquiror_ncusip year target_ncusip
by acquiror_ncusip year : gen new_target_patent_value_t2 = target_patent_value
by acquiror_ncusip year : replace new_target_patent_value_t2 = 0 if year == year_announcement | year == year_announcement + 1 | year == year_announcement + 2
by acquiror_ncusip year : replace new_target_patent_value_t2 = sum(new_target_patent_value_t2)
by acquiror_ncusip year : replace new_target_patent_value_t2 = new_target_patent_value_t2[_N]
by acquiror_ncusip year : replace target_patent_value = sum(target_patent_value)
by acquiror_ncusip year : replace target_patent_value = target_patent_value[_N]
replace patent_value = acquiror_patent_value + target_patent_value
drop year_announcement month_announcement date_announced date_effective target_primary_sic_code _merge_acquiror target_ncusip _merge_target 
duplicates drop
duplicates drop acquiror_ncusip year, force
gen new_patent_value_t2 = acquiror_patent_value + new_target_patent_value_t2
bysort acquiror_ncusip : gen patent_value_t2 = new_patent_value_t2[_n+2]
drop acquiror_primary_sic_code new_target_patent_value_t2 new_patent_value_t2 year_of_getting_acquired
gen change_in_patents = (patent_value_t2 - patent_value)*100/patent_value
save "F:\Thesis\Prepared Data\New Method\11-Updated patent value using acquiror becoming target\updated patent value.dta",replace



****************************Step 14 - Extracting info from acquisitions dataset
use "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", clear
rename year_announcement year
rename month_announcement month
save "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", replace
use "F:\Thesis\Prepared Data\New Method\3-Thomson One dataset filtered\3-Thomson One dataset filtered.dta",clear
rename acquiror_cusip cusip6
joinby cusip6 year month using "F:\Thesis\Prepared Data\New Method\4-cusip-permno data from CRSP\annual - permno-cusip-Ncusip basic file.dta", unmatched(none)
rename ncusip acquiror_ncusip
order acquiror_ncusip,first
sort acquiror_ncusip year
by acquiror_ncusip year : gen number_acquisitions = _N
replace value_of_acquisition = 0 if value_of_acquisition == .
by acquiror_ncusip year : replace value_of_acquisition = sum(value_of_acquisition)
by acquiror_ncusip year : replace value_of_acquisition = value_of_acquisition[_N]
by acquiror_ncusip year : replace acquisition_same_industry = sum(acquisition_same_industry)
by acquiror_ncusip year : replace acquisition_same_industry = acquisition_same_industry[_N] 
replace value_acquisition_same_industry = 0 if value_acquisition_same_industry == .
by acquiror_ncusip year : replace value_acquisition_same_industry = sum(value_acquisition_same_industry)
by acquiror_ncusip year : replace value_acquisition_same_industry = value_acquisition_same_industry[_N]
by acquiror_ncusip year : replace acquisition_another_industry = sum(acquisition_another_industry)
by acquiror_ncusip year : replace acquisition_another_industry = acquisition_another_industry[_N]
replace val_acquisition_another_industry = 0 if val_acquisition_another_industry == .
by acquiror_ncusip year : replace val_acquisition_another_industry = sum(val_acquisition_another_industry)
by acquiror_ncusip year : replace val_acquisition_another_industry = val_acquisition_another_industry[_N]
drop month date_announced date_effective date_effective_unconditional target_cusip target_primary_sic_code cusip6 acquiror_primary_sic_code permno cusip 
duplicates drop
save "F:\Thesis\Prepared Data\New Method\12-Acquisitions dataset extract\Acquisitions dataset extract.dta",replace


***********************Step 15 - Merging updated patent value and acquisitions dataset extract 
use "F:\Thesis\Prepared Data\New Method\11-Updated patent value using acquiror becoming target\updated patent value.dta", clear
joinby acquiror_ncusip year using "F:\Thesis\Prepared Data\New Method\12-Acquisitions dataset extract\Acquisitions dataset extract.dta", unmatched(master) _merge(_merge)
replace value_of_acquisition = 0 if value_of_acquisition == .
replace acquisition_same_industry = 0 if acquisition_same_industry == .
replace value_acquisition_same_industry = 0 if value_acquisition_same_industry == .
replace acquisition_another_industry = 0 if acquisition_another_industry == .
replace val_acquisition_another_industry = 0 if val_acquisition_another_industry == .
replace number_acquisitions = 0 if number_acquisitions == .
drop _merge
save "F:\Thesis\Prepared Data\New Method\13-Merged updated patent value-acquisitions extract\Merged updated patent value-acquisitions extract.dta",replace


************************Step 16 - Preparing accounting info
use "F:\Thesis\Prepared Data\New Method\14-Compustat accounting info\compustat accounting info-merged with ncusip.dta", clear
duplicates drop
save "F:\Thesis\Prepared Data\New Method\14-Compustat accounting info\compustat accounting info-merged with ncusip.dta", replace
joinby acquiror_ncusip year using "F:\Thesis\Prepared Data\New Method\14-Compustat accounting info\acquiror primary sic code.dta", unmatched(master) _merge(_merge)
drop permno _merge
sort acquiror_ncusip year
by acquiror_ncusip : gen sale_tminus1 = sale[_n-1] if _n>1
by acquiror_ncusip : gen market_to_book_ratio_tminus1 = market_to_book_ratio[_n-1] if _n>1
by acquiror_ncusip : gen xrd_tminus1 = xrd[_n-1] if _n>1
by acquiror_ncusip : gen roa_tminus1 = roa[_n-1] if _n>1
by acquiror_ncusip : gen leverage_tminus1 = leverage[_n-1] if _n>1
by acquiror_ncusip : gen size_tminus1 = size[_n-1] if _n>1
by acquiror_ncusip : gen ch_tminus1 = ch[_n-1] if _n>1
order acquiror_primary_sic_code,after(year)
save "F:\Thesis\Prepared Data\New Method\15-Compustat info with ncusip and sic\Compustat info with ncusip and sic.dta",replace


***********************Step 17 - Merging acquisitions-patent info and accounting data
use "F:\Thesis\Prepared Data\New Method\13-Merged updated patent value-acquisitions extract\Merged updated patent value-acquisitions extract.dta", clear
joinby acquiror_ncusip year using "F:\Thesis\Prepared Data\New Method\15-Compustat info with ncusip and sic\Compustat info with ncusip and sic.dta", unmatched(none)
order acquiror_primary_sic_code,after(year)
egen count = group(acquiror_ncusip)
order count, after(acquiror_ncusip)
winsor change_in_patents, gen (w_change_in_patents) p(0.1) highonly
gen acquiror_primary_sic_code_3digit = int(acquiror_primary_sic_code/10)
order acquiror_primary_sic_code_3digit, after(acquiror_primary_sic_code)
gen hitech = 0
replace hitech = 1 if acquiror_primary_sic_code_3digit == 283 | acquiror_primary_sic_code_3digit == 357 | acquiror_primary_sic_code_3digit == 366 | acquiror_primary_sic_code_3digit == 367 | acquiror_primary_sic_code_3digit == 382 | acquiror_primary_sic_code_3digit == 384 | acquiror_primary_sic_code_3digit == 481 | acquiror_primary_sic_code_3digit == 482 | acquiror_primary_sic_code_3digit == 489 | acquiror_primary_sic_code_3digit == 737 | acquiror_primary_sic_code_3digit == 873
gen hitech_interaction = hitech * w_change_in_patents
gen normalized_value_of_acquisition = ln(value_of_acquisition)
order normalized_value_of_acquisition, after(number_acquisitions)
replace normalized_value_of_acquisition = 0 if normalized_value_of_acquisition < 0 | normalized_value_of_acquisition == .
gen per_acquisition_same_industry = (value_acquisition_same_industry * 100)/value_of_acquisition
order per_acquisition_same_industry,after(normalized_value_of_acquisition)
gen per_acquisition_another_industry = (val_acquisition_another_industry * 100)/value_of_acquisition
order per_acquisition_another_industry,after(per_acquisition_same_industry)
gen numper_acq_same_industry = (acquisition_same_industry*100)/number_acquisitions
gen numper_acq_another_industry = (acquisition_another_industry*100)/number_acquisitions
order numper_acq_same_industry, after(per_acquisition_another_industry)
order numper_acq_another_industry, after(numper_acq_same_industry)
sort acquiror_ncusip year
by acquiror_ncusip : gen n_acquisition_tminus1 = number_acquisitions[_n-1] if _n > 1
by acquiror_ncusip : gen val_acquisition_tminus1 = normalized_value_of_acquisition[_n-1] if _n > 1
sort acquiror_ncusip year
by acquiror_ncusip : gen n_acquisition_tminus2 = number_acquisitions[_n-2] if _n > 2
by acquiror_ncusip : gen val_acquisition_tminus2 = normalized_value_of_acquisition[_n-2] if _n > 2
by acquiror_ncusip : gen n_acquisition_tminus3 = number_acquisitions[_n-3] if _n > 3
by acquiror_ncusip : gen val_acquisition_tminus3 = normalized_value_of_acquisition[_n-3] if _n > 3
save "F:\Thesis\Prepared Data\New Method\16-Accounting info merged with patent value-acquisitions\Accounting info merged with patent value-acquisitions.dta",replace


***************Step 18 - Robustness Checks
sort acquiror_ncusip year
by acquiror_ncusip : gen never_acquired = sum(number_acquisitions)
by acquiror_ncusip : replace never_acquired = never_acquired[_N]
drop if never_acquired == 0


Logit model
bysort acquiror_ncusip year: gen acq_prob = 0
by acquiror_ncusip year: replace acq_prob = 1 if number_acquisitions > 0
by acquiror_ncusip : gen decliningpatent_prob = 0
by acquiror_ncusip : replace decliningpatent_prob = 1 if change_in_patents <= 0


bysort acquiror_ncusip year: gen acq_tminus1_prob = 0 if n_acquisition_tminus1 != .
replace acq_tminus1_prob = 1 if n_acquisition_tminus1 > 0 & n_acquisition_tminus1 != .
by acquiror_ncusip : gen increasing_patent = 0 if _n > 1
by acquiror_ncusip : replace increasing_patent = 1 if acquiror_patent_value[_n] > acquiror_patent_value[_n-1]
