clear
set mem 300m
use data
keep childid age weightbirth msd bp ppvt recg tempI bpiC inv02 inv03 inv24 asvab2 se3 education
foreach var of varlist weightbirth msd bp ppvt recg tempI bpiC inv02 inv03 inv24 asvab2 se3 education {
	replace `var' = . if `var' == -100
}

egen aux = std(weightbirth)
replace weightbirth = aux
drop aux

gen cognitive = . 
replace cognitive = weightbirth if age == 0
replace cognitive =  bp if age == 1
replace cognitive = ppvt if age == 3
replace cognitive = recg if age >= 5

gen noncognitive = . 
replace noncognitive = tempI if age == 0
replace noncognitive = tempI if age == 1
replace noncognitive = bpiC if age >= 3

gen investment = . 
replace investment = inv03 if age == 0
replace investment = inv03 if age == 1
replace investment = inv03 if age == 3
replace investment = inv24 if age >= 5
replace investment = log(investment)
gen momcog = asvab2
gen momncogn = se3

drop weightbirth bp ppvt recg tempI bpiC inv02 inv03 inv24 asvab2 se3 

foreach var of varlist cognitive noncognitive investment education {
	gen d`var' = 0
	replace d`var' = 1 if `var' != . 
	replace `var' = -100 if `var' == .
}

replace dinvestment = 0 if dcognitive == 0
replace dinvestment = 0 if dnoncognitive == 0

replace deducation = 0 if dcognitive == 0
replace deducation = 0 if dnoncognitive == 0

gen dcont = dinvestment + dcognitive + dnoncognitive
bys childid: egen v = sum(dcont)
drop if v < 2
drop v dcont

order childid age cognitive noncognitive investment momcog momncogn dcognitive dnoncognitive dinvestment educ deduc

sort childid age
save simple, replace
outfile using simple.raw, w replace