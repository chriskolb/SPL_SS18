clear
capture log close
cd "C:\Users\Chris\Dropbox\HU Berlin\Statistik\2. Semester\SPL"

*merge data sets
use pequiv.dta
merge 1:1 pid syear using pequiv.dta, nogen
tab h11111
*drop obs that can never be part of a cohort 
*increases chances of getting representative household individual from first obs in hid-syear
drop if d11101<25 | d11101>65
*drop all individuals who are not househols heads (so only household heads will be matched to HIDs later)
drop if d11105 != 1
sort hid pid syear
bysort hid syear: gen count = _n
*drop if count != 1
save pequivpgenmall.dta, replace
clear


use hgen.dta




merge 1:1 hid syear using pequivpgenmall.dta
drop if _merge!=3

*exclude dissolved households
bysort hid: gen firsthead=pid[1]
gen samehead = 0
replace samehead = 1 if pid==firsthead
bysort hid: egen dissolve = min(samehead)
replace dissolve = 1 - dissolve
tab dissolve samehead
*drop if dissolve==1

*generate time index variable (age-25)
capture drop timevar
gen timevar = d11101-25

*exclude households who don't start at 25 in the dataset
bysort hid: egen minage = min(d11101)
*drop if minage>27

*declare panel structure
xtset hid syear

tab hgowner
*-1=keine Angabe, 1=Eigentümer, 2=Hauptmieter, 3=Untermieter, 4=Mieter, 5=Heimbewohner
gen lagowner = L.hgowner
tab hgacquis

*generate change variable
gen change1 = 1 if hgowner==1 & L.hgowner==2
gen change2 = 1 if hgowner==1 & L.hgowner==3
gen change3 = 1 if hgowner==1 & L.hgowner==4
gen change4 = 1 if hgowner==1 & L.hgowner==5
gen changetot = 1 if change1==1 | change2==1 | change3==1 | change4==1
replace changetot = 0 if hgowner >=2 & hgowner<=5 & L.hgowner>=2 & L.hgowner<=5
replace changetot = 0 if hgowner==1 & L.hgowner==1

*gen help = L.hid
*gen firstobs = 1 if help==.
*drop help
*replace changetot = -1 if firstobs==1

replace changetot = -2 if hgowner>=2 & hgowner<=5 & L.hgowner==1

replace changetot = -3 if hgowner==-1 | L.hgowner==-1

/* Erläuterung changetot
-3: keine Angabe in hgowner oder L.hgowner
-2: Wechsel von Eigentümer zu einem Mieterhältnis (2-5)
-1: first wave
0: entweder kein Wechsel, oder Wechsel innerhalb von versch. Mietverhältnissen
1: Wechsel von einem Mietsverhältnis (2-5) zu Eigentum (1)
*/

gen failure = 0 if changetot==0
replace failure = 1 if changetot==1

xtset, clear

*generate  firstyear variable
bysort hid: gen firstyear = syear[1]

*generate numobs variable
bysort hid: gen numobs = _N

*flag households where failure occurs
bysort hid: egen changeflag = max(changetot)

*compare the changetot values of all observations of househols where failure occurs eventually

tab changeflag changetot

*birthyear

gen birthyear = syear-d11101

*generate cohorts

capture: drop cohort*
gen cohort=.

gen cohort11 = 1 if syear==1985 & d11101 >= 24 & d11101<=27
bysort hid: egen cohort1 = max(cohort11)
replace cohort=1 if cohort1==1

gen cohort21 = 1 if syear==1990 & d11101 >=24 & d11101<=27
bysort hid: egen cohort2 = max(cohort21)
replace cohort=2 if cohort2==1


gen cohort31 = 1 if syear==1995 & d11101 >=24 & d11101<=27
bysort hid: egen cohort3 = max(cohort31)
replace cohort=3 if cohort3==1

replace cohort1=0 if cohort>0 & cohort<=3 & cohort1!=1
replace cohort2=0 if cohort>0 & cohort<=3 & cohort2!=1
replace cohort3=0 if cohort>0 & cohort<=3 & cohort3!=1






/*
gen timevar = .
replace timevar=syear-1985 if cohort1==1
replace timevar=syear-1990 if cohort2==1
replace timevar=syear-1995 if cohort3==1
replace timevar=. if timevar<0
*/

save dataprep.dta, replace

********************************

clear
use dataprep.dta

*declare survival data
stset timevar, id(hid) failure(changetot=1)


*survival graphs
sts graph, by(cohort) graphregion(color(white)) bgcolor(white) title("Sexy Survival Curves") ylabel( 0.4(0.15)1, nogrid )
sts graph, by(cohort) haz graphregion(color(white)) bgcolor(white) title("Sexy Smooth Hazard Rates") 
*stcox, stpm2, streg etc.


********************************
*TESTcodeschnipsel
*comparison of survival graphs with east/west analysis (was looked sexier?)
*construct eastwest-cohorts:

*alle, die in 1995 zw 24 und 27 Jahre alt waren
gen cohortregion=.

gen cohortwest1=1 if syear==1995 & l11102==1 & d11101 >= 24 & d11101<=27
bysort hid: egen cohortwest = max(cohortwest1)
replace cohortregion=1 if cohortwest==1

gen cohorteast1=1 if syear==1995 & l11102==2 & d11101 >= 24 & d11101<=27
bysort hid: egen cohorteast = max(cohorteast1)
replace cohortregion=2 if cohorteast==1



capture drop timeregion
gen timeregion =.
replace timeregion = syear-1995 if cohortwest==1 | cohorteast==1
replace timeregion =. if timeregion<0

stset timeregion, id(hid) failure(failure)
sts graph, by (cohortregion)
