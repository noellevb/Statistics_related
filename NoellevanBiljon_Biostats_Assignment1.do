. use "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assgn1.dta", clear

sort case
preserve
set seed 15091995
sample 40

sort case 
save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assignment_1Case.dta" ,replace
restore

merge case using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assignment_1Case.dta" 
tab _merge
rename _merge dataset
label define datasetlbl 1 validation 3 analysis
label values dataset datasetlbl
tab dataset
tab dataset, nolabel
save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assignment_1_Working.dta" ,replace

preserve
keep if dataset==1
save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assignment_1_Validation.dta" , replace
restore


keep if dataset==3
save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assignment_1_Analysis.dta" , replace


//Univariate Analysis
//histogram age
//est store AgeHist

//continuous
summarize age baseef dobef
histogram dobef
histogram age
histogram baseef
graph box age dobef baseef

//categorical
tabulate event
tabulate restwma
tabulate posse


//Bivariate Analysis

//categorical
tabulate event posse ,chi row
tabulate event restwma ,chi row

//continuous and cat
graph box dobef, by(event)
graph box baseef, by(event)
graph box age, by(event)

//Dobef and Baseef
scatter dobef baseef

//Fitting the Model
//Null Model
logistic event
est store Null
//outreg2 using Assign1regoutput, excel stats(coef se tstat pval ci) replace

//Plus Age
logistic event age
est store NullAge
//outreg2 using Assign1regoutput, excel

//Compare Null and Age
lrtest Null NullAge, stats

//Plus baseef
logistic event baseef
est store NullBaseef
//outreg2 using Assign1regoutput, excel
//Compare Null and Baseef
lrtest Null NullBaseef, stats

//Plus Dobef
logistic event dobef
est store NullDobef
//outreg2 using Assign1regoutput, excel
//Compare Null and dobef
lrtest Null NullDobef, stats

//Plus restwma
logistic event restwma
est store NullRestwma
//outreg2 using Assign1regoutput, excel
//Compare Null and Restwma
lrtest Null NullRestwma, stats

//Plus Posse
logistic event posse
est store NullPosse
//outreg2 using Assign1regoutput, excel
//Compare Null and posse
lrtest Null NullPosse, stats

//NullDobef Most Sig
//Add Age
logistic event dobef age
est store NullDobefAge
//outreg2 using Assign1regoutput, excel
//Compare NullDobef and With age
lrtest NullDobef NullDobefAge, stats

//Add Baseef
logistic event dobef baseef
est store NullDobefBaseef
//outreg2 using Assign1regoutput, excel
//Compare NullDobef and with baseef
lrtest NullDobef NullDobefBaseef, stats

//Add Restwma
logistic event dobef restwma
est store NullDobefRestwma
//outreg2 using Assign1regoutput, excel
//Compare NullDobef and with restwma
lrtest NullDobef NullDobefRestwma, stats

//Add Posse
logistic event dobef posse
est store NullDobefPosse
//outreg2 using Assign1regoutput, excel
//Compare NullDobef and with posse
lrtest NullDobef NullDobefPosse, stats

// Posse most Sig
//Add Age
logistic event dobef posse age
est store NullDobefPosseAge
//outreg2 using Assign1regoutput, excel
//Compare NullDobefPosse and with age
lrtest NullDobefPosse NullDobefPosseAge, stats

//Add Baseef
logistic event dobef posse baseef
est store NullDobefPosseBaseef
//outreg2 using Assign1regoutput, excel
//Compare NullDobefPosse and with baseef
lrtest NullDobefPosse NullDobefPosseBaseef, stats

//Add Restwma
logistic event dobef posse restwma
est store NullDobefPosseRestwma
//outreg2 using Assign1regoutput, excel
//Compare NullDobefPosse and with restwma
lrtest NullDobefPosse NullDobefPosseRestwma, stats

//Testing for confounding
logistic event dobef posse, coef
logistic event dobef posse age, coef
spearman age dobef
spearman age posse
spearman event age
mean age if event ==1
mean age if event ==0

// Adding interaction terms:
// PosSE and Dobef
gen possedobef = posse*dobef
logistic event dobef posse possedobef
est store NullDobefPossepossedobef
//outreg2 using Assign1regoutput, excel
//Compare NullDobef and with posse
lrtest NullDobefPosse NullDobefPossepossedobef, stats

// Final Model
// PosSE and Dobef and event
logistic event dobef posse, coef
est store final
outreg2 using Assign1regoutput, excel
estat gof


//Model Checking
predict xb ,xb
predict p ,p
predict h , hat 
predict d, deviance
predict rs, rstandard
gen ds = d/sqrt(1-h)
predict dbeta, dbeta
predict dx2, dx2
predict dd, dd

list case dobef posse event p h dbeta rs ds, nodisplay noobs sep(0)
scatter dx2 p [w=dbeta], title("Fig 7: symbol size proportional to dBeta") mfcolor(none)
gsort -dbeta
list case dobef posse event p h dbeta dx2 dd if _n<5, nodisplay noobs sep(0)
tabstat posse, stats(min p25 p75 max) by(event)
tabstat dobef, stats(min p25 p75 max) by(event)
//scatter case rs
twoway (scatter rs case, msymbol(none) mlabel(case) mlabposition(0))
//scatter case ds
twoway (scatter ds case, msymbol(none) mlabel(case) mlabposition(0))
//scatter ds xb
twoway (scatter ds xb, msymbol(none) mlabel(case) mlabposition(0) yline(0))
// scatter case h
twoway (scatter h case, msymbol(none) mlabel(case) mlabposition(0))
//scatter dbeta case
twoway (scatter dbeta case, msymbol(none) mlabel(case) mlabposition(0))


//Classification:
logistic event dobef posse
//gof
estat gof
estat gof, group(10)
//finding cutoff
lstat
lsens, xlabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0) ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
roctab event p, detail graph
lstat, cutoff(0.105)

graph box p, over(event) yline(0.105)

// PosSE and Dobef and event
logistic event dobef posse, coef
est store final

//Validation
use "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_1\Assignment_1_Validation.dta" , clear

predict eventpredict
br eventpredict

//gen beta0 = 2.589436
//gen betaposse = -0.9395043
//gen betadobef = -0.061569 

//gen logitexpected = beta0 + betaposse*posse + betadobef*dobef
//gen eexpected = exp(logitexpected)
//gen expectedevent = eexpected / (1+eexpected)
//same as predicted event

logistic event posse dobef, coef
est store validation

predict p ,p
lstat
lsens, xlabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0) ylabel(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
roctab event p, detail graph
lstat, cutoff(0.105)

graph box p, over(event) yline(0.105)
