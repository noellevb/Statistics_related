insheet using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign_2.csv", clear

//save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign2_FULL.csv", replace

//LDA

//spagetti plot - xline fvc 

summarize fvc0 fvc3 fvc6 fvc9 fvc12 fvc15 fvc18 fvc21 fvc24 
graph box fvc0 fvc3 fvc6 fvc9 fvc12 fvc15 fvc18 fvc21 fvc24, by(treat)

reshape long fvc, i(pt_id score fvc0 tlc0 treat maxfib surv failure_type ) j(time)

//Explortaion
egen pt_idnum = group(pt_id)
xtset pt_idnum time

egen treat_fvcmeans = mean(fvc),by(treat time)
twoway (line treat_fvcmeans time if treat=="A") (line treat_fvcmeans time if treat=="B"), ///
		yscale(range(50 90)) ylab(50 60 70 80 90) xtitle(Month) ytitle( Average FVC) ylab() ///
		legend(lab(1 "Treatment A") lab(2 "Treatment B"))
//comment of group effect time effect etc.

//xtline treat_fvcmeans ,t(time) i(pt_id) overlay, if treat=="A", yscale(range(0 150)) ylab(0 75 150) xtitle(month) ylab() legend(off), by(treat)
//xtline fvc,t(time) i(pt_id) overlay, if treat=="B", yscale(range(0 150)) ylab(0 75 150) xtitle(month) ylab() 

summarize score tlc0 maxfib surv 

histogram fvc, by(time) normal

//by treatment summarise
summarize fvc  score fvc0 tlc0 maxfib if treat == "A"
summarize fvc  score fvc0 tlc0 maxfib if treat == "B"
graph box score tlc0 fvc0 maxfib fvc, by(treat)

histogram fvc0, by(treat)
histogram score, by(treat)
histogram tlc0, by(treat)
histogram maxfib, by(treat)
histogram fvc, by(treat)

// remove missing - GEE
insheet using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign_2.csv", clear

drop if missing(fvc0)
drop if missing(fvc3)
drop if missing(fvc6)
drop if missing(fvc9)
drop if missing(fvc12)
drop if missing(fvc15)
drop if missing(fvc18)
drop if missing(fvc21)
drop if missing(fvc24)

pwcorr fvc0 fvc3 fvc6 fvc9 fvc12 fvc15 fvc18 fvc21 fvc24

reshape long fvc, i(pt_id score fvc0 tlc0 treat maxfib surv failure_type ) j(time)
//save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign2_FULL_long.csv", replace
//fitting GEE
gen treatcat = (treat =="A")
gen timetreat = time*treatcat

egen pt_idnum = group(pt_id)
xtset pt_idnum time
xtdes

//xtgee score group2 group3 week grpwk2 grpwk3,family(gaussian) link(identity)corr(exchangeable ) i(subject) t(week)
//xtgee fvc treatcat timetreat, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat timetreat, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time) nodisplay

//xtgee fvc treatcat time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//xtgee fvc treatcat maxfib , family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat maxfib , family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//xtgee fvc treatcat tlc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat tlc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//xtgee fvc treatcat score, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat score, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//xtgee fvc treatcat fvc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)


//FCV0 most sig - round 2
qic fvc treatcat fvc0 timetreat, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 tlc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 score, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//maxfib most sig -round 3
qic fvc treatcat fvc0 maxfib timetreat, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib tlc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib score, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//score most sig - round 4
qic fvc treatcat fvc0 maxfib score timetreat, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib score time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib score tlc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//treattime most sig -round 5
qic fvc treatcat fvc0 maxfib score timetreat time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
qic fvc treatcat fvc0 maxfib score timetreat tlc0, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//tlc0 most sig - round 6
qic fvc treatcat fvc0 maxfib score timetreat tlc0 time, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)

//Final Model
xtgee fvc treatcat fvc0 maxfib, family(gaussian) link(identity) corr(ar1) i(pt_idnum) t(time)
estat wcor


//LMM
insheet using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign_2.csv", clear
reshape long fvc, i(pt_id score fvc0 tlc0 treat maxfib surv failure_type ) j(time)

gen treatcat = (treat =="A")
gen timetreat = time*treatcat

egen pt_idnum = group(pt_id)
//random intercept model
xtmixed fvc treatcat fvc0 maxfib, || pt_idnum: 
est store LMMint
vce

//random intercept and slopes model
xtmixed fvc treatcat fvc0 maxfib, || pt_idnum: time 
est store LMMint_slope
vce

lrtest LMMint LMMint_slope,stats

//validation
predict slope intercept, reffects //between patient residuals
predict rs, rstandard // within patient residuals
predict xb, xb // linear component of model
summarize slope
summarize intercept
summarize rs
gen slope_std = slope/0.3675302
gen intercept_std = intercept/2.592626 
gen rs_std = rs/0.9048754

histogram slope_std, normal xtitle() 
histogram intercept_std, normal xtitle()
histogram rs_std, normal xtitle()

twoway scatter slope_std xb, msymbol(none) mlabel(pt_idnum) mlabposition(0) yline(0) 
twoway scatter intercept_std xb, msymbol(none) mlabel(pt_idnum) mlabposition(0) yline(0) 
twoway scatter rs_std xb, msymbol(none) mlabel(pt_idnum) mlabposition(0) yline(0) 

list pt_idnum treatcat surv failure_type maxfib time if slope_std > 2 
list pt_idnum treatcat surv failure_type maxfib time if slope_std < -2

list pt_idnum treatcat surv failure_type maxfib  if intercept_std > 2 
list pt_idnum treatcat surv failure_type maxfib  if intercept_std < -2

list pt_idnum treatcat surv failure_type maxfib time if rs_std > 2 
list pt_idnum treatcat surv failure_type maxfib time if rs_std < -2


// Survival Analysis
insheet using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign_2.csv", clear
reshape long fvc, i(pt_id score tlc0 treat maxfib surv failure_type ) j(time)
gen treatcat = (treat =="A")

//overall analysis
drop if time == 3 
drop if time == 6 
drop if time == 9 
drop if time == 12 
drop if time == 15 
drop if time == 18 
drop if time == 21 
drop if time == 24
//save "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign2_Surv_long.csv", replace

//Kaplan Meier
stset surv, failure(failure_type==1)
sts list, by(treatcat) compare
sts test treatcat, logrank
sts graph, by(treatcat) risktable risktable(0 3 6 9 12 15 18 21 24, failevents) tmax(50) xlabel(0 3 6 9 12 15 18 21 24 27) ylabel(0.8 0.85 0.9 0.95 1.0) 
stsum if treat == "A"
stsum if treat == "B"

stset surv, failure(failure_type==2)
sts list, by(treatcat) compare
sts test treatcat, logrank
sts graph, by(treatcat) risktable risktable(0 3 6 9 12 15 18 21 24, failevents) tmax(50) xlabel(0 3 6 9 12 15 18 21 24) ylabel(0.7 0.75 0.8 0.85 0.9 0.95 1.0) 
stsum if treat == "A"
stsum if treat == "B"



// Cox Proportional Hazards Models
insheet using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign_2.csv", clear
reshape long fvc, i(pt_id score tlc0 treat maxfib surv failure_type ) j(time)

//overall analysis
drop if time == 3 
drop if time == 6 
drop if time == 9 
drop if time == 12 
drop if time == 15 
drop if time == 18 
drop if time == 21 
drop if time == 24

gen treatcat = (treat =="A")

//death - model
stset surv,failure(failure_type == 1)

stcox tlc0,nolog
est store A

stcox score,nolog
est store B

stcox fvc,nolog
est store C

stcox maxfib,nolog
est store D

estimates stats _all

//Maxfib most significant

stcox maxfib tlc0,nolog
est store E

stcox maxfib score,nolog
est store F

stcox maxfib fvc,nolog
est store G

lrtest D E , stats
lrtest D F , stats
lrtest D G , stats

//Maxfib most sig

//add treatcat
//gen treatcat = (treat =="A")

stcox treatcat
est store K
// 44% decrease in RR
stcox maxfib treatcat
est store L
// 36% decrease in RR

sort treatcat
by treatcat: summarize maxfib tlc0 score

//check for interaction
gen maxfibtreat = maxfib*treatcat

stcox maxfib treatcat maxfibtreat
est store M

lrtest L M, stats

// add none.

//check with stepwise
//stepwise, pe(0.10) pr(0.20) forward : stcox tlc0 maxfib fvc score
//stepwise, pe(0.10) pr(0.20) : stcox tlc0 maxfib fvc score


//Interpretation
stset surv,failure(failure_type==1)
stsum 
tabstat maxfib treatcat maxfibtreat,stats(n mean sd median p25 p75 min max) by(treatcat)
stcox maxfib treatcat 
stcox maxfib treatcat ,nohr nolog 

//stcox maxfib treatcat maxfibtreat basesurv(basesurv0) 
//stcurve, surv at1 

// Validation
//CoxSnell
stset surv,failure(failure_type==1)
stcox maxfib treatcat, mgale(mg)

predict cs, csnell

stset cs, failure(failure_type==1)
sts generate km=s
gen H = -ln(km)
line H cs cs, sort ytitle("") clstyle(.refine) legend(nodraw)

//Martingale
stset surv,failure(failure_type==1)
stcox maxfib treatcat, mgale(mg1) nolog
lowess mg1 maxfib, mean noweight title("") note("") m(o)

//deviance residuals
egen pt_idnum = group(pt_id)
drop mg mg1
stcox maxfib treatcat, mgale(mg)
predict xb, xb
predict dev, deviance
twoway scatter dev xb, msymbol(none) mlabel(pt_idnum) mlabposition(0) ylab(-2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3) 

//gsort -dev
list pt_idnum treatcat surv failure_type maxfib if dev > 2
list pt_idnum treatcat surv failure_type maxfib if dev < -2

//Leverage - dfbetas
stcox maxfib treatcat,esr(esr*)
set matsize 158
mkmat esr1 esr2, matrix(esr)
mat V = e(V)
mat Inf = esr*V
svmat Inf, names(s)
label var s1 "dfbeta - maxfib"
label var s2 "dfbeta - treatment"
twoway scatter s1 surv, yline(0) msymbol(none) mlabel(pt_idnum) 
twoway scatter s1 surv, yline(0) msymbol(none) mlabel(maxfib) 

twoway scatter s2 surv, yline(0) msymbol(none) mlabel(pt_idnum) 
twoway scatter s2 surv, yline(0) msymbol(none) mlabel(treat) 

//Assumptions
stcox maxfib treatcat, scaledsch(sca*) schoenfeld(sch*) nolog
stphtest, rank detail
stphplot ,by(treat)



//Dropout Model
insheet using "C:\Users\nvbil\Desktop\University\2017\Biostats\Assignment_2\Assign_2.csv", clear
reshape long fvc, i(pt_id score tlc0 treat maxfib surv failure_type ) j(time)

//overall analysis
drop if time == 3 
drop if time == 6 
drop if time == 9 
drop if time == 12 
drop if time == 15 
drop if time == 18 
drop if time == 21 
drop if time == 24

gen treatcat = (treat =="A")

//dropout - model
stset surv,failure(failure_type == 2)

stcox tlc0,nolog
est store A

stcox score,nolog
est store B

stcox fvc,nolog
est store C

stcox maxfib,nolog
est store D

estimates stats _all

//Maxfib most significant

stcox maxfib tlc0,nolog
est store E

stcox maxfib score,nolog
est store F

stcox maxfib fvc,nolog
est store G

lrtest D E , stats
lrtest D F , stats
lrtest D G , stats

//Maxfib most sig- none improved

//add treatcat
//gen treatcat = (treat =="A")

stcox treatcat
est store H
// 20% increase in RR
stcox maxfib treatcat
est store I
//22% increase in RR

sort treatcat
by treatcat: summarize maxfib 

//check for interaction
gen maxfibtreat = maxfib*treatcat

stcox maxfib treatcat maxfibtreat
est store J

lrtest I J, stats
// add maxfibtreat as sig

//check with stepwise
//stepwise, pe(0.10) pr(0.20) forward : stcox tlc0 maxfib fvc score
//stepwise, pe(0.10) pr(0.20) : stcox tlc0 maxfib fvc score


//Interpretation
stset surv,failure(failure_type==2)
stsum 
tabstat maxfib treatcat maxfibtreat,stats(n mean sd median p25 p75 min max)
stcox maxfib treatcat maxfibtreat
stcox maxfib treatcat maxfibtreat,nohr nolog 


// Validation
//CoxSnell
//gen maxfibtreat = maxfib*treatcat
egen pt_idnum = group(pt_id)

stset surv,failure(failure_type==2)
stcox maxfib treatcat maxfibtreat, mgale(mg)

predict cs, csnell

stset cs, failure(failure_type==2)
sts generate km=s
gen H = -ln(km)
line H cs cs, sort ytitle("") clstyle(.refine) legend(nodraw)

//Martingale
stset surv,failure(failure_type==2)
stcox treatcat maxfibtreat, mgale(mg1) nolog
lowess mg1 maxfib, mean noweight title("") note("") m(o)

drop mg1

stcox maxfib treatcat, mgale(mg1) nolog
lowess mg1 maxfibtreat, mean noweight title("") note("") m(o)

//residuals-deviance
//egen pt_idnum = group(pt_id)
drop mg mg1
stcox maxfib treatcat maxfibtreat, mgale(mg)
predict xb, xb
predict dev, deviance

//drop if failure_type==0
//drop if failure_type==1

twoway scatter dev xb, msymbol(none) mlabel(pt_idnum) mlabposition(0) ylab(-0.5 0 0.5 1 1.5 2 2.5 3) 

list pt_idnum treatcat surv failure_type maxfib if dev > 2
list pt_idnum treatcat surv failure_type maxfib if dev < -2


//dfbetas- leverage
// ADD MAXFIB*TREAT
stcox maxfib treatcat maxfibtreat,esr(esr*)
set matsize 158
mkmat esr1 esr2 esr3, matrix(esr)
mat V = e(V)
mat Inf = esr*V
svmat Inf, names(s)
label var s1 "dfbeta - maxfib"
label var s2 "dfbeta - treatment"
label var s3 "dfbeta - maxfibtreat"

twoway scatter s1 surv, yline(0) msymbol(none) mlabel(pt_idnum) 
twoway scatter s1 surv, yline(0) msymbol(none) mlabel(maxfib) 

twoway scatter s2 surv, yline(0) msymbol(none) mlabel(pt_idnum) 
twoway scatter s2 surv, yline(0) msymbol(none) mlabel(treat) 

twoway scatter s3 surv, yline(0) msymbol(none) mlabel(pt_idnum) 
twoway scatter s3 surv, yline(0) msymbol(none) mlabel(maxfibtreat) 

//testing hazard assumption
stcox maxfib treatcat maxfibtreat, scaledsch(sca*) schoenfeld(sch*) nolog
stphtest, rank detail
stphplot ,by(treat)
