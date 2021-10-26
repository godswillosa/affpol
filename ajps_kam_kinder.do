bplist00—_WebMainResource’	
_WebResourceData_WebResourceMIMEType_WebResourceTextEncodingName^WebResourceURL_WebResourceFrameNameO¶á<html><head></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">***************************************************
*************** syntax for replicating ***********
**Kam, Cindy D. and Donald R. Kinder.  Forthcoming.  
**"Ethnocentrism as a Short-Term Force in the 2008 American Presidential Election"
**AJPS
*****************************************************

/*Using the dataset from ANES, just renaming it*/
use "/Users/godswillosa/Documents/Research/affpol/anes_timeseries_2012.dta", clear
set more off

*Coding racial background
tab V081102
recode V081102 (1=1 "White") (2=2 "Black")(3=3 "Hispanic") (4 5 6 7=5 "Other")(else=.), gen(race08)
replace race08 = 3 if V081103==1
tab race08

/*We analyze only whites*/
tab race08 V085044a
disp 411/413
drop if race08~=1

************MEASURING ETHNOCENTRISM*************
*Generating a measure of E based on the post
tab1 V085174a-V085175d
recode V085174a V085174b V085174c V085174d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=.), gen(wwork08 bwork08 hwork08 awork08)
recode V085175a V085175b V085175c V085175d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=.), gen(wsmt08 bsmt08 hsmt08 asmt08)

lab def work 1"hardworking" 0"lazy"
lab val wwork08 bwork08 hwork08 awork08 work

lab def smt 1"intelligent" 0"unintelligent"
lab val wsmt08 bsmt08 hsmt08 asmt08 smt

gen wworkb = wwork08-bwork08
gen wworkh = wwork08-hwork08
gen wworka = wwork08-awork08
gen wsmtb = wsmt08-bsmt08
gen wsmth = wsmt08-hsmt08
gen wsmta = wsmt08-asmt08

*reliabilities
alpha wworkb wworkh wworka wsmtb wsmth wsmta if race08==1 
/*.8050*/

recode V085174a V085174b V085174c V085174d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=0), gen(wwork08a bwork08a hwork08a awork08a)
recode V085175a V085175b V085175c V085175d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=0), gen(wsmt08a bsmt08a hsmt08a asmt08a)
egen emis = anycount(V085174a V085174b V085174c V085174d V085175a V085175b V085175c V085175d), val(-9 -8 -2)
egen emisw = anycount(V085174a V085175a), val(-9 -8 -2)
egen emisb = anycount(V085174b V085175b), val(-9 -8 -2)
egen emish = anycount(V085174c V085175c), val(-9 -8 -2)
egen emisa = anycount(V085174d V085175d), val(-9 -8 -2)


*WHITES
gen wig08 = (wwork08+wsmt08)/2
gen wogb08 = (bwork08+bsmt08)/2
replace wogb08 = bwork08a+bsmt08a if emisb==1
gen wogh08 = (hwork08+hsmt08)/2
replace wogh08 = hwork08a+hsmt08a if emish==1
gen woga08 = (awork08+asmt08)/2
replace woga08 = awork08a+asmt08a if emisa==1
gen wog08 = (wogb08+wogh08+woga08)/3

gen wecb08 = wig08-wogb08
gen wech08 = wig08-wogh08
gen weca08 = wig08-woga08
gen wec08 = wig08-wog08
sum wig08-wec08

*ETHNOCENTRISM
gen e08 = .
replace e08 = wec08 if race08==1
gen ig08 = .
replace ig08 = wig08 if race08==1
gen og08 = .
replace og08 = wog08 if race08==1

*pre-election
tab1 V083207a-V083208d
recode V083207a V083207b V083207c V083207d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=.), gen(wwork08pre bwork08pre hwork08pre awork08pre)
recode V083208a V083208b V083208c V083208d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=.), gen(wsmt08pre bsmt08pre hsmt08pre asmt08pre)
lab val wwork08pre bwork08pre hwork08pre awork08pre work

lab val wsmt08pre bsmt08pre hsmt08pre asmt08pre smt

gen wworkbpre = wwork08pre-bwork08pre
gen wworkhpre = wwork08pre-hwork08pre
gen wworkapre = wwork08pre-awork08pre
gen wsmtbpre = wsmt08pre-bsmt08pre
gen wsmthpre = wsmt08pre-hsmt08pre
gen wsmtapre = wsmt08pre-asmt08pre

*reliabilities
alpha wworkbpre wworkhpre wworkapre wsmtbpre wsmthpre wsmtapre if race08==1 /*.767*/

recode V083207a V083207b V083207c V083207d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=0), gen(wwork08apre bwork08apre hwork08apre awork08apre)
recode V083208a V083208b V083208c V083208d (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=0), gen(wsmt08apre bsmt08apre hsmt08apre asmt08apre)

egen emispre = anycount(V083207a V083207b V083207c V083207d V083208a V083208b V083208c V083208d), val(-9 -8 -2)
egen emiswpre = anycount(V083207a V083208a), val(-9 -8 -2)
egen emisbpre = anycount(V083207b V083208b), val(-9 -8 -2)
egen emishpre = anycount(V083207c V083208c), val(-9 -8 -2)
egen emisapre = anycount(V083207d V083208d), val(-9 -8 -2)

*WHITES
gen wig08pre = (wwork08pre+wsmt08pre)/2
gen wogb08pre = (bwork08pre+bsmt08pre)/2
replace wogb08pre = bwork08apre+bsmt08apre if emisb==1
gen wogh08pre = (hwork08pre+hsmt08pre)/2
replace wogh08pre = hwork08apre+hsmt08apre if emish==1
gen woga08pre = (awork08pre+asmt08pre)/2
replace woga08pre = awork08apre+asmt08apre if emisa==1
gen wog08pre = (wogb08pre+wogh08pre+woga08pre)/3

gen wecb08pre = wig08pre-wogb08pre
gen wech08pre = wig08pre-wogh08pre
gen weca08pre = wig08pre-woga08pre
gen wec08pre = wig08pre-wog08pre
sum wig08pre-wec08pre

*ETHNOCENTRISM
gen e08pre = .
replace e08pre = wec08pre if race08==1
gen ig08pre = .
replace ig08pre = wig08pre if race08==1
gen og08pre = .
replace og08pre = wog08pre if race08==1

gen e08both = (e08+e08pre)/2
replace e08both = e08pre if V084009==-2
sum e08both if race08==1
gen ig08both = (ig08+ig08pre)/2
replace ig08both = ig08pre if V084009==-2
gen og08both = (og08+og08pre)/2
replace og08both = og08pre if V084009==-2

//NOTE 2
ttest e08=e08pre 
corr e08 e08pre

//FIGURE 1
histogram e08both if race08==1, xtitle(" ") b2t(" ") xlabel(-1 "Favor Outgroups" 0 "Neutral" 1 "Favor Ingroup", tlwidth(medthick)) ytitle(Percent) ylabel(, angle(horizontal) tlwidth(medthick)) percent xoverhangs name(fig_1, replace) scheme(s1mono) fcolor(white)

//NOTE 3
sum e08both if race08==1


***************ETHNOCENTRISM AND THE 2008 VOTE***************
//NOTE 4
*VOTE CHOICE
tab V085044a
recode V085044a (1=0 "Obama")(3=1 "McCain")(-1=2 "Abstained")(else=.), gen(voteobama)

svyset [pweight=V080102a]
svy: tab voteobama if race08==1 &amp; voteobama&lt;2
svy: tab voteobama if race08==1

//CONTROL VARIABLES
*PARTISANSHIP
recode V083097 (1=1)(2=0)(3 4 5=.5)(else=.), gen(pid308)
gen pid708 = .
replace pid708 = 0 if pid308==0 &amp; V083098a==1
replace pid708 = .17 if pid308==0 &amp; V083098a==5
replace pid708 = .33 if V083098b==1
replace pid708 = .5 if V083098b==3
replace pid708 = .67 if V083098b==5
replace pid708 = .83 if pid308==1 &amp; V083098a==5
replace pid708 = 1 if pid308==1 &amp; V083098a==1
gen pid_new = pid708*2-1

*POLICY
/*Spending &amp; services (split sample: V083105; V083108x); 
Defense spending (split sample: V083112; V083115x);
Guaranteed jobs (V083128) - only half sample
Health insurance (V083119) - only half sample*/

//all issues coded with higher values indicating liberal response//
*spending and services
recode V083105 (1=0)(2=.17)(3=.33)(4 -7 -8 =.5)(5=.67)(6=.83)(7=1)(else=.), gen(spdsvc7a)
recode V083108x (7=0)(6=.17)(5=.33)(4 -7 -8=.5)(3=.67)(2=.83)(1=1)(else=.), gen(spdsvc7b)
gen spdsvcnew = .
replace spdsvcnew = 0 if spdsvc7a&gt;=0 &amp; spdsvc7a&lt;=2
replace spdsvcnew = 1 if spdsvc7b&gt;=0 &amp; spdsvc7b&lt;=2
gen spdsvc708 = spdsvc7a
replace spdsvc708 = spdsvc7b if spdsvcnew == 1

*defense spending
tab1 V083112 V083115x
recode V083112 (1=1)(2=.83)(3=.67)(4 -7 -8 =.5)(5=.33)(6=.17)(7=0)(else=.), gen(spddef7a)
recode V083115x (7=1)(6=.83)(5=.67)(4 -7 -8=.5)(3=.33)(2=.17)(1=0)(else=.), gen(spddef7b)
gen spddefnew = .
replace spddefnew = 0 if spddef7a&gt;=0 &amp; spddef7a&lt;=2
replace spddefnew = 1 if spddef7b&gt;=0 &amp; spddef7b&lt;=2
gen spddef708 = spddef7a
replace spddef708 = spddef7b if spddefnew == 1

*guaranteed jobs
recode V083128 (1=1)(2=.83)(3=.67)(4 -7 -8=.5)(5=.33)(6=.17)(7=0)(else=.), gen(jobsol708)

*health insurance
recode V083119 (1=1)(2=.83)(3=.67)(4 -7 -8=.5)(5=.33)(6=.17)(7=0)(else=.), gen(hlthins08)

*RELIABILITY OF POLICY ISSUE SCALE
alpha spdsvc708 spddef708 jobsol708 hlthins08

egen issuemis = rowmiss(spdsvc708 spddef708 jobsol708 hlthins08)
tab issuemis
recode spdsvc708 spddef708 jobsol708 hlthins08 (.=0), gen(spdsvc708a spddef708a jobsol708a hlthins08a)
gen issuescale = (spdsvc708a+spddef708a+jobsol708a+hlthins08a)/(4-issuemis) if issuemis&lt;3
sum issuescale
gen issue_new = issuescale*2-1


*ASSESSMENT OF RETROSPECTIVE NATIONAL ECONOMIC CONDITIONS
recode V083083x (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(ecprog08)
recode V083087x (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(ueprog08)
recode V083089x (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(inprog08)

alpha ecprog08 ueprog08 inprog08
gen ecprog08scale = (ecprog08+ueprog08+inprog08)/3
gen ecprog_new = ecprog08scale*2-1

*POLITICAL INFORMATION
gen ptycon= .
replace ptycon = 1 if V085119a==5
replace ptycon= 0 if V085119a==-8|V085119a==-9|V085119a==1 | V085119==5|V085119==-8|V085119==9
recode V085066 (1=1)(5=0)(-8 -9=0)(else=.), gen(hormaj)
recode V085067 (1=1)(5=0)(-8 -9=0)(else=.), gen(senmaj)
gen infopty08 = (ptycon+hormaj+senmaj)/3
alpha ptycon hormaj senmaj 

*EDUCATION
recode V083217 (-9 -8 =.0), gen(educ08)
tab educ08 
replace educ08 = educ08/17

*FEMALE
gen female08 = V081101-1

*HOUSEHOLD ECONOMIC CONDITIONS
recode V083057x (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(hhecon08)
gen hhecon_new = hhecon08*2-1


**************TABLE 1**************
mlogit voteobama e08both pid_new issue_new ecprog_new educ08 infopty08 female08 hhecon_new if race08==1, base(0)
est store mlogit_orig
est table mlogit_orig, b(%9.2f) se
est table mlogit_orig, b(%9.2f) star(.1 .05 .01)

//TEST OF IIA
mlogit voteobama e08both pid_new hhecon_new ecprog_new issue_new educ08 infopty08 female if race08==1 &amp; voteobama ~="Abstained":voteobama, base(0)
est store mlogit_M
mlogit voteobama e08both pid_new hhecon_new ecprog_new issue_new educ08 infopty08 female if race08==1 &amp; voteobama ~="McCain":voteobama, base(0)
est store mlogit_A
hausman mlogit_M mlogit_orig, alleqs constant
hausman mlogit_A mlogit_orig, alleqs constant
suest mlogit_M mlogit_orig
test [mlogit_M_McCain=mlogit_orig_McCain]
*p~.09
suest mlogit_A mlogit_orig
test [mlogit_A_Abstained=mlogit_orig_Abstained]
*p~.35
*Under conventional p values, IIA is not violated


*****PREDICTED PROBABILITIES
set more off
mlogit voteobama e08both pid_new issue_new ecprog_new educ08 infopty08 female08 hhecon_new if race08==1, base(0)
preserve
collapse e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new if race08==1
sum e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new 
replace female08=1
expand 13
egen x = fill(-.5(.1).7)
replace e08both=x
predict obama, outcome(0)
table e08both, c(mean obama n obama)
restore

//NOTE 6
recode V085063b (-9 -8 -2=.), gen(ftobamapost)
recode V083037a (-9 -8 -6 -2=.), gen(ftobamapre)
gen ftobama = (ftobamapre+ftobamapost)/2
replace ftobama = ftobamapre if ftobamapost==.
replace ftobama = ftobamapost if ftobamapre==.
recode V085063c  (-9 -8 -6 -2=.), gen(ftmccainpost)
recode V083037b (-9 -8 -6 -2=.), gen(ftmccainpre)
gen ftmccain = (ftmccainpre+ftmccainpost)/2
replace ftmccain = ftmccainpre if ftmccainpost==.
replace ftmccain = ftmccainpost if ftmccainpre==.
svyset [pweight=V080101a]
svy: mean ftobama if race08==1
svy: mean ftmccain if race08==1

*Estimating feelings towards Obama 
preserve
keep if race08==1
reg ftobama e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new
est store with_E
reg ftmccain e08both pid_new issue_new ecprog_new  female08 educ08 infopty08 hhecon_new
est store mccain
est table with_E mccain, b(%9.2f) se style(col) stats(F r2_a N)
est table with_E mccain, b(%9.2f) star(.1 .05 .01) style(col) stats(F r2_a N)
restore


//ESTIMATING MODEL SEPARATELY FOR DEM, IND, GOP VOTERS
gen strpid08 = abs((pid708-.5)*2)
tab strpid08
mlogit voteobama e08both strpid08 issue_new ecprog_new female08 educ08 infopty08 hhecon_new if race08==1 &amp; pid308==0, base(0)
est store GOP
mlogit voteobama e08both strpid08 issue_new ecprog_new female08 educ08 infopty08 hhecon_new if race08==1 &amp; pid308==.5, base(0)
est store IND
mlogit voteobama e08both strpid08 issue_new ecprog_new female08 educ08 infopty08 hhecon_new if race08==1 &amp; pid308==1, base(0)
est store DEM
est table GOP IND DEM, b(%9.2f) star(.1 .05 .01) stats(N) eq(1)
est table GOP IND DEM, b(%9.2f) se stats(N) eq(1)

***********FIGURE 2**************
//SUBSTANTIVE EFFECTS
preserve
set more off
collapse e08both strpid08 issue_new ecprog_new female08 educ08 infopty08 hhecon_new 
replace female08=1
expand 13
egen x = fill(-.5(.1).7)
replace e08both=x
estimates restore GOP
predict obama_GOP, outcome(0)
estimates restore IND
predict obama_IND, outcome(0)
estimates restore DEM
predict obama_DEM, outcome(0)
table e08both, c(mean obama_GOP mean obama_IND mean obama_DEM)
twoway (connected obama_GOP e08both, msymbol(i) lpattern(solid)) (connected obama_IND e08both, msymbol(i) lpattern(dot)) (connected obama_DEM e08both, msymbol(i) lpattern(dash)), xtitle("Ethnocentrism") xlabel(-.5(.2).7) legend(col(3) lab(1 "Republicans") lab(2 "Independents") lab(3 "Democrats")) ytitle("Pr(Obama)") scheme(s1mono) ylabel(0(.2)1)
restore


/////ROBUSTNESS CHECKS
*FT DV, coded above
*egalitarianism
tab1 V085162-V085167
recode V085162 V085164 V085167 (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(V085162r V085164r V085167r)
recode V085163 V085165 V085166 (1=0)(2=.25)(3=.5)(4=.75)(5=1)(else=.), gen(V085163r V085165r V085166r)
alpha V085162r V085163r V085164r V085165r V085166r V085167r
gen equal08 = (V085162r +V085163r +V085164r +V085165r +V085166r +V085167r)/6
egen eqmis = rowmiss(V085162r-V085166r)
recode V085162r V085163r V085164r V085165r V085166r V085167r (.=0), gen(V085162a V085163a V085164a V085165a V085166a V085167a)
replace equal08 = (V085162a +V085163a +V085164a +V085165a +V085166a +V085167a)/(6-eqmis) if eqmis&lt;4
sum equal08

*limited government
recode V085105 (1=1)(2=0)(else=.), gen(V085105r)
recode V085106 (1=0)(2=1)(else=.), gen(V085106r)
recode V085107 (1=1)(2=0)(else=.), gen(V085107r)
alpha V085105r V085106r V085107r
gen limgov08 = (V085105r+V085106r+V085107r)/3
egen limmis = rowmiss(V085105r V085106r V085107r)
recode V085105r V085106r V085107r (.=0), gen(V085105a V085106a V085107a)
replace limgov08 = (V085105a+V085106a+V085107a)/2 if limmis==1

*authoritarianism
recode V085158 V085159 V085161 (1=0)(3=.5)(5=1)(else=.), gen(V085158r V085159r V085161r)
recode V085160 (1=1)(3=.5)(5=0)(else=.), gen(V085160r)
alpha V085158r V085159r V085160r V085161r
gen auth08 = (V085158r+V085159r+V085160r+V085161r)/4
egen authmis = anycount(V085158-V085161), val(-9 -8 -2)
recode V085158r V085159r V085160r V085161r (.=0), gen(V085158rm V085159rm V085160rm V085161rm)
replace auth08 = (V085158rm+V085159rm+V085160rm+V085161rm)/(4-authmis) if authmis&lt;3


*EFT
tab1 V085065c V085064y V085064a V085064v  /*ordered W B H A*/
recode V085065c (-9 -2=.)(-8=50), gen(ftw08)
replace ftw08 = ftw08/100
recode V085064y (-9 -2=.)(-8=50), gen(ftb08)
replace ftb08 = ftb08/100
recode V085064a (-9 -2=.)(-8=50), gen(fth08)
replace fth08 = fth08/100
recode V085064v (-9 -2=.)(-8=50), gen(fta08)
replace fta08 = fta08/100

gen igft08 = .
replace igft08 = ftw08 if race08==1
gen wfeelb08 = ftw08-ftb08
gen wfeelh08 = ftw08-fth08
gen wfeela08 = ftw08-fta08
alpha wfeelb08 wfeelh08 wfeela08 if race08==1 /*.87*/
gen ogft08 = .
replace ogft08 = (ftb08+fth08+fta08)/3 if race08==1
gen eft08 = igft08-ogft08
sum eft08
corr e08both eft08 


*****E BASED ON RELIGION
*Religion variable is: V083188x
tab V083188x
recode V083188x (1=1 "Protestant") (2=2 "Catholic") (3=3 "Jewish") (7=7 "Other") (else=.), gen(relig08)
replace relig08 = 1 if V083189 ==2|V083189 ==3|V083189 ==4|V083189 ==5|V083189 ==6|V083189 ==7|V083189 ==10|V083189 ==22
replace relig08 = 4 if V083189 ==1|V083189 ==14|V083189 ==15|V083189 ==16|V083189 ==17|V083189 ==18|V083189 ==19
replace relig08 = 5 if V083189 ==28 
replace relig08 = 6 if V083189 ==29
lab def relig08 4 "Baptist/Fundamentalist" 5 "Hindu" 6"Muslim", modify
tab relig08
lab var relig08 "Religious identification"

*FT variables are: V085064b (Chr Fund); V058064c (Catholics); V085064f (Jews); V085065e (Muslims); V085065f (Hindus); V085065g (Christians); V085065h (Atheists)
recode V085064b V085064c V085064f V085065e V085065f V085065g V085065h (-9 -8 -6 = .)(-2=.), gen(ftchrfund ftcath ftjew ftmuslim fthindu ftchrist ftatheist)
replace ftchrfund = ftchrfund/100
replace ftcath = ftcath/100
replace ftjew = ftjew/100
replace ftmuslim = ftmuslim/100
replace fthindu= fthindu/100
replace ftchrist = ftchrist/100
replace ftatheist = ftatheist/100
recode V085064b V085064c V085064f V085065g V085065h V085065e V085065f (-9 -8 -6 = 1)(-2=.)(0/100=0), gen(ftchrfundmis ftcathmis ftjewmis ftchristmis ftatheistmis ftmuslimmis fthindumis)
sum ftchrfundmis ftcathmis ftjewmis ftchristmis ftatheistmis ftmuslimmis fthindumis

recode V085064b V085064c V085064f V085065g V085065h V085065e V085065f (-9 -8 -6 = 0)(-2=.), gen(ftchrfunda ftcatha ftjewa ftchrista ftatheista ftmuslima fthindua)

egen eftreligmis = anycount(V085064b V085064c V085064f V085065g V085065h V085065e V085065f), val(-9 -8 -6)
replace ftchrfunda = ftchrfunda/100
replace ftcatha = ftcatha/100
replace ftjewa = ftjewa/100
replace ftchrista = ftchrista/100
replace ftatheista = ftatheista/100
replace ftmuslima= ftmuslima/100
replace fthindua= fthindua/100

*CODING BAPTISTS WITH CHRISTIANS, B/C CLOSER TO THEM THAN CHR FUND
gen igftrelig = .
replace igftrelig = ftchrist if relig08==1|relig08==4
replace igftrelig = ftcath if relig08==2
replace igftrelig = ftjew if relig08==3

gen protfeelcath= ftchrist-ftcath
gen protfeeljew = ftchrist-ftjew
gen protfeelhindu = ftchrist-fthindu
gen protfeelmuslim = ftchrist-ftmuslim
gen protfeelath = ftchrist-ftatheist

gen cathfeelprot= ftcath-ftchrist
gen cathfeeljew = ftcath-ftjew
gen cathfeelhindu = ftcath-fthindu
gen cathfeelmuslim = ftcath-ftmuslim
gen cathfeelath = ftcath-ftatheist

gen jewfeelprot= ftjew-ftchrist
gen jewfeelcath = ftjew-ftcath
gen jewfeelhindu = ftjew-fthindu
gen jewfeelmuslim = ftjew-ftmuslim
gen jewfeelath = ftjew-ftatheist

alpha protfeelcath-protfeelath if relig08 ==1 /*.88*/
alpha protfeelcath-protfeelath if relig08==4  /*.86*/
alpha cathfeelprot-cathfeelath if relig08==2 /*.84 */
alpha jewfeelprot - jewfeelath if relig08 ==3 /*.89*/

gen ogftrelig= .
replace ogftrelig= (ftcath+ftjew+fthindu+ftmuslim+ftatheist)/5 if relig08==1|relig08==4
replace ogftrelig= (ftchrist+ftjew+fthindu+ftmuslim+ftatheist)/5 if relig08==2
replace ogftrelig= (ftchrist+ftcath+fthindu+ftmuslim+ftatheist)/5 if relig08==3

*Allowing for missingness
replace ogftrelig= (ftcatha+ftjewa+fthindua+ftmuslima+ftatheista)/4 if relig08==1 &amp; eftreligmis==1
replace ogftrelig= (ftcatha+ftjewa+fthindua+ftmuslima+ftatheista)/4 if relig08==4 &amp; eftreligmis==1
replace ogftrelig = (ftchrista+ftjewa+fthindua+ftmuslima+ftatheista)/4 if relig08==2 &amp; eftreligmis==1
replace ogftrelig = (ftchrista+ftcatha+fthindua+ftmuslima+ftatheista)/4 if relig08==3 &amp; eftreligmis==1

replace ogftrelig = (ftcatha+ftjewa+fthindua+ftmuslima+ftatheista)/3 if relig08==1 &amp; eftreligmis==2
replace ogftrelig = (ftcatha+ftjewa+fthindua+ftmuslima+ftatheista)/3 if relig08==4 &amp; eftreligmis==2
replace ogftrelig = (ftchrista+ftjewa+fthindua+ftmuslima+ftatheista)/3 if relig08==2 &amp; eftreligmis==2
replace ogftrelig = (ftchrista+ftcatha+fthindua+ftmuslima+ftatheista)/3 if relig08==3 &amp; eftreligmis==2

gen eftrelig = igftrelig - ogftrelig
sum eftrelig
lab var eftrelig "E* based on religion"

pwcorr eftrelig eft08 e08both

*************TABLE 2**************
preserve
keep if race08==1
reg ftobama e08both pid_new issue_new ecprog_new educ08 infopty08 female08 hhecon08 
est store with_back
reg ftobama e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08 equal08 limgov08 
est store with_values
reg ftobama e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08 auth08 
est store with_F
reg ftobama eft08 pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08 
est store with_eft
reg ftobama eftrelig pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08 
est store with_erelig
restore
est table with_back with_values with_F with_eft with_erelig , b(%9.2f) star(.1 .05 .01) style(col) eq(1) stats(N)
est table with_back with_values with_F with_eft with_erelig , b(%9.2f) se style(col) eq(1) stats(ll r2_a N)
**********************************

**NOTE 17
gen og08host = -1*(og08both-1)
reg ftobama ig08both og08host pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08 if race08==1


*************RACIAL RESENTMENT************* 
recode V085143 V085146 (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(V085143r V085146r)
recode V085144 V085145 (1=0)(2=.25)(3=.5)(4=.75)(5=1)(else=.), gen(V085144r V085145r)
alpha V085143r V085144r V085145r V085146r
egen resmis = rowmiss(V085143r V085144r V085145r V085146r)
tab resmis
recode V085143r V085144r V085145r V085146r (.=0), gen(V085143a V085144a V085145a V085146a)
gen resent08 = (V085143r +V085144r +V085145r +V085146r )/4
replace resent08 = (V085143a+V085144a+V085145a+V085146a)/(4-resmis) if resmis&lt;3
gen resent_new = resent08*2-1
corr resent_new e08both

*************ANTI-MUSLIM FEELINGS************* 
svy: mean ftmuslim
svy: mean ftchrist
//comparing means across all items
svyset [pweight=V080102a]
foreach v of varlist V085064a - V085065h {
   recode `v' (-9 -8 -6=1)(-2=.)(else=0), gen(`v'x)
}
sum V085064ax - V085065hx 
preserve
mvdecode V085064a - V085065h, mv(-9 -8 -6 -2)
svy: mean V085064a - V085065h if race08==1
restore

gen antimuslimft = (ftmuslim-1)*-1
gen antimus_new = antimuslimft*2-1
pwcorr e08both antimus_new if race08==1, sig

*******************TABLE 3**********
preserve
keep if race08==1
reg ftobama e08both resent_new antimus_new pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
keep if e(sample)
reg ftobama e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
est store with_E
reg ftobama e08both resent_new pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
est store resent
reg ftobama e08both antimus_new pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
est store ftmuslim
reg ftobama e08both resent_new antimus_new pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
est store bothmed
est table with_E resent ftmuslim bothmed, b(%9.2f) star(.1 .05 .01) style(col) eq(1) stats(N)
est table with_E resent ftmuslim bothmed, b(%9.2f) se style(col) eq(1) stats(r2_a F N)
restore
******************************

**********MEDIATION TESTS**********
preserve
keep if race08==1
reg ftobama e08both resent_new antimus_new pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
keep if e(sample)
reg resent_new e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
scalar a1 = _b[e08both]
scalar se_a1 = _se[e08both]
reg antimus_new e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
scalar a2 = _b[e08both]
scalar se_a2 = _se[e08both]
reg ftobama e08both resent_new antimus_new pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08
restore
scalar b1 = _b[resent_new]
scalar se_b1 = _se[resent_new]
scalar b2 = _b[antimus_new]
scalar se_b2 = _se[antimus_new]
scalar num1 = a1*b1
scalar num2 = a2*b2
scalar denom1 = sqrt(b1^2*se_a1^2+a1^2*se_b1^2)
scalar denom2 = sqrt(b2^2*se_a2^2+a2^2*se_b2^2)
scalar z1 = num1/denom1
scalar z2 = num2/denom2
scalar p1 = normal(z1)
scalar p2 = normal(z2)
scalar list z1 z2 p1 p2

save "C:\CDK WORK\Ethnocentrism\Obama\nes08_2011.dta", replace


*****************************************
*********END ANALYSIS OF ANES 2008*******
*****************************************

***********CCAP REPLICATION**************
clear
set mem 700M
set matsize 6297
use "C:\CDK WORK\Ethnocentrism\Obama\CCAP\CCAP0001_Common_Content_R2.1\CCAP0001_Common_Content_R2.1\ccap0001_common_output_r2_1.dta"
set more off

*Race: profile55
tab profile55, nol
gen race08=profile55
lab val race08 profile55
tab race08
drop if race08~=1

**ETHNOCENTRISM
*stereotypes are: scap718w/b/a/h; scap719w/b/a/h; 
*Generating a measure of E
tab1 scap718w scap718b scap718h scap718a scap719w scap719b scap719h scap719a
recode scap718w scap718b scap718h scap718a (1=0 )(2=.17)(3=.33)(4=.5)(5=.67)(6=.83)(7=1)(else=.), gen(wwork08 bwork08 hwork08 awork08)
recode scap719w scap719b scap719h scap719a (1=0 )(2=.17)(3=.33)(4=.5)(5=.67)(6=.83)(7=1)(else=.), gen(wsmt08 bsmt08 hsmt08 asmt08)
lab def work 1"hardworking" 0"lazy"
lab val wwork08 bwork08 hwork08 awork08 work
lab def smt 1"intelligent" 0"unintelligent"
lab val wsmt08 bsmt08 hsmt08 asmt08 smt
gen wworkb = wwork08-bwork08
gen wworkh = wwork08-hwork08
gen wworka = wwork08-awork08
gen wsmtb = wsmt08-bsmt08
gen wsmth = wsmt08-hsmt08
gen wsmta = wsmt08-asmt08

*reliabilities
alpha wworkb wworkh wworka wsmtb wsmth wsmta if race08==1 

recode scap718w scap718b scap718h scap718a (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=0), gen(wwork08a bwork08a hwork08a awork08a)
recode scap719w scap719b scap719h scap719a (1=1 )(2=.83)(3=.67)(4=.5)(5=.33)(6=.17)(7=0)(else=0), gen(wsmt08a bsmt08a hsmt08a asmt08a)

egen emis = rowmiss( scap718w scap718b scap718h scap718a scap719w scap719b scap719h scap719a )
egen emisw = rowmiss(scap718w scap719w)
egen emisb = rowmiss(scap718b scap719b)
egen emish = rowmiss(scap718h scap719h)
egen emisa = rowmiss(scap718a scap719a)

*WHITES
gen wig08 = (wwork08+wsmt08)/2
gen wogb08 = (bwork08+bsmt08)/2
replace wogb08 = bwork08a+bsmt08a if emisb==1
gen wogh08 = (hwork08+hsmt08)/2
replace wogh08 = hwork08a+hsmt08a if emish==1
gen woga08 = (awork08+asmt08)/2
replace woga08 = awork08a+asmt08a if emisa==1
gen wog08 = (wogb08+wogh08+woga08)/3

gen wecb08 = wig08-wogb08
gen wech08 = wig08-wogh08
gen weca08 = wig08-woga08
gen wec08 = wig08-wog08
sum wig08-wec08

*ETHNOCENTRISM
gen e08 = .
replace e08 = wec08 if race08==1
sum e08 if race08==1
lab var e08 "Ethnocentrism"

gen ig08 = .
replace ig08 = wig08 if race08==1
gen og08 = .
replace og08 = wog08 if race08==1

**CONTROL VARIABLES
*Partisanship
tab scap8
recode scap8 (1=1 "Str Dem")(2=.67)(3=.33)(4 8=0)(5=-.33)(6=-.67)(7=-1 "Str Rep") (else=.), gen(pid_new)
tab pid_new

/*Issues
Spending &amp; services - not available
Defense spending -not available
Guaranteed jobs - ocap1104
Health insurance - scap20 
*/

//all issues coded with higher values indicating liberal response//
*GUARANTEED JOBS
tab ocap1104 
recode ocap1104 (1=1)(2=.83)(3=.67)(5 8=.33)(6=.17)(7=0)(else=.), gen(jobsol708)

*HEALTH INSURANCE
tab scap20, nol
recode scap20 (1=1)(2 4=.5)(3=0)(else=.5), gen(hlthins08)

alpha jobsol708 hlthins08
gen issuescale = (jobsol708+hlthins08)/(2) 
sum issuescale
gen issue_new = (issuescale-.5)*2

*economic conditions (national)
tab1 ocap9 
tab ocap9, nol
recode ocap9 (1=1 "much better")(2=.5)(3 6=0)(4=-.5)(5=-1)(else=.), gen(ecprog_new)

*Educ: profile57
tab profile57, nol
recode profile57 (1=0 "&lt;hs")(2=.2)(3=.4)(4=.6)(5=.8)(6=1 "post grad"), gen(educ08)

*Info: profile101-110
sum profile101-profile110
tab1 profile101-profile110
recode profile101 profile102 profile104 profile106 profile108 (1=1)(2=0)(3=0)(8=0)(else=.), gen(profile101r profile102r profile104r profile106r profile108r )
recode profile103 (1=0)(2=0)(3=1)(8=0)(else=.), gen(profile103r )
recode profile105 profile107 profile109 profile110 (1=0)(2=1)(3=0)(8=0)(else=.), gen(profile105r profile107r profile109r profile110r)
alpha profile101r-profile110r /*Alpha = 0.82*/
gen info = (profile101r+profile102r+profile103r+profile104r+profile105r+profile106r+profile107r+profile108r+profile109r+profile110r)/10
sum info

*Female: profile54
recode profile54 (1=0)(2=1), gen(female08)
tab female08

*economic conditions (household)
tab ocap1100
recode ocap1100 (1=1 "much better")(2=.5)(3 6=0)(4=-.5)(5=-1)(else=.), gen(hhecon_new)

*Dependent variable
*vote choice: PCAP600
tab pcap600
tab pcap600, nol
recode pcap600 (1=0 "Obama")(2=1 "McCain")(else=.), gen(voteobama)
recode pcap600 (1=0 "Obama")(2=1 "McCain")(4=2 "Abstained")(else=.), gen(voteobama3)

sum voteobama e08 pid_new issue_new educ08 info female08 ecprog_new hhecon_new if race08==1
tab voteobama3

logit voteobama e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
est store voteobama
est table voteobama, b(%9.2f) se stats(ll chi2 N) style(col)

///MNL produces similar results
mlogit voteobama3 e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1, base(0)


////NOTE 7
logit voteobama e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1 &amp; pid_new&lt;0
est store GOP
logit voteobama e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1 &amp; pid_new==0
est store IND
logit voteobama e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1 &amp; pid_new&gt;0
est store DEM


**************MEDIATION ANALYSIS**************
/*Mediation analysis*/
*Resentment: scap70/1/2/3
tab1 scap70 scap71 scap72 scap73
recode scap70 scap72 (1=1)(2=.75)(3=.5)(4=.25)(5=0)(else=.), gen(scap70r scap72r)
recode scap71 scap73 (1=0)(2=.25)(3=.5)(4=.75)(5=1)(else=.), gen(scap71r scap73r)
alpha scap70r-scap73r /*alpha = 0.86*/
corr scap70r-scap73r 
tab1 scap70r-scap73r 
egen resmis= rowmiss(scap70r-scap73r)
tab resmis
recode scap70r scap71r scap72r scap73r (.=0), gen(scap70a scap71a scap72a scap73a)
gen resent08 = (scap70a +scap71a +scap72a +scap73a)/(4-resmis) if resmis&lt;3
sum resent08
gen resent_new = 2*(resent08-.5)

*Muslim favorability: ocap300MUS
tab ocap300mus
tab ocap300mus, nol
recode ocap300mus (0=0)(1=-.5)(2=-1)(3=.5)(4=1)(5 8=0)(else=.), gen(antimus_new)
tab antimus ocap300mus

***************TABLE 3*************
preserve
keep if race08==1
probit voteobama e08 resent_new antimus_new e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
keep if e(sample)
probit voteobama e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
est store baseline
probit voteobama e08 resent_new pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
est store with_resent
probit voteobama e08 antimus_new pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
est store with_anti
probit voteobama e08 resent_new antimus_new pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
est store with_both
est table baseline with_resent with_anti with_both, b(%9.2f) se stats(ll r2_a N) style(col)
est table baseline with_resent with_anti with_both, b(%9.2f) star(.1 .05 .01) stats(ll r2_a N) style(col)
restore


//sobel-test
preserve
keep if race08==1
reg voteobama e08 resent_new antimus_new e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
keep if e(sample)
reg resent_new e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
scalar a1 = _b[e08]
scalar se_a1 = _se[e08]
reg antimus_new e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
scalar a2 = _b[e08]
scalar se_a2 = _se[e08]
reg voteobama resent_new antimus_new e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new 
scalar b1 = _b[resent_new]
scalar se_b1 = _se[resent_new]
scalar b2 = _b[antimus_new]
scalar se_b2 = _se[antimus_new]
scalar num1 = a1*b1
scalar num2 = a2*b2
scalar denom1 = sqrt(b1^2*se_a1^2+a1^2*se_b1^2)
scalar denom2 = sqrt(b2^2*se_a2^2+a2^2*se_b2^2)
scalar z1 = num1/denom1
scalar z2 = num2/denom2
scalar p1 = normal(z1)
scalar p2 = normal(z2)
scalar list z1 z2 p1 p2
restore


***********TABLE 5**************
**********MCCAIN VS. OBAMA MATCHUP
tab mcap605a if race08==1
recode mcap605a (1=0 "Obama")(2=1 "McCain")(else=.), gen(prefM_O)
tab mcap603m if race08==1
recode mcap603m (1=0 "Clinton")(2=1 "McCain")(else=.), gen(prefM_C)
probit prefM_O e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1 &amp; prefM_C&lt;2
est store prefM_O
**********MCCAIN VS. CLINTON MATCHUP
probit prefM_C e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1 &amp; prefM_O&lt;2
est store prefM_C
/*no E effect*/
est table prefM_O prefM_C, b(%9.2f) se stats(ll chi2 N) style(col)
est table prefM_O prefM_C, b(%9.2f) star(.1 .05 .01) stats(ll chi2 N) style(col)

***********PREDICTED PROBABILITIES**********
preserve
probit prefM_O prefM_C e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
keep if e(sample)
probit prefM_O e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
sum e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
matrix bO = e(b)'
matlist bO
probit prefM_C e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
matrix bC = e(b)'
matlist bC
save "C:\CDK WORK\Ethnocentrism\Obama\CCAP predictions.dta", replace
restore


***********redoing the graph
use "C:\CDK WORK\Ethnocentrism\Obama\CCAP predictions.dta", clear
probit prefM_O prefM_C e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
keep if e(sample) 
probit prefM_O e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
matrix bO = e(b)'
matlist bO
probit prefM_C e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new if race08==1
matrix bC = e(b)'
matlist bC
replace pid_new =0
replace issue_new = -.3
replace ecprog_new = -.7
replace educ08=.5
replace info=.5
replace female = 1
replace hhecon_new=-.3
gen cons = 1

keep e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new cons
mkmat e08 pid_new issue_new ecprog_new educ08 info female08 hhecon_new cons, mat(X)
matrix indexO = X*bO
matrix indexC = X*bC
svmat indexO
svmat indexC
gen phatO = normal(indexO)
gen phatC = normal(indexC)

sort e08
lab var phatO "McCain vs. Obama"
lab var phatC "McCain vs. Clinton"
lab var e08 "Ethnocentrism"

gen phatO_r = 1-phatO
gen phatC_r = 1-phatC
lab var phatO_r "Obama vs. McCain"
lab var phatC_r "Clinton vs. McCain"
********FIGURE 3***********
twoway (connected phatO_r e08, msymbol(i) ) (connected phatC_r e08, msymbol(i)) (histogram e08, yaxis(2) percent), xscale(range(-.5(.2).7)) yscale(range(0(.2)1) axis(1)) yscale(range(0 100) axis(2)) ytitle("Pr(Democrat)") legend(lab(3 "Distribution of E")) scheme(s1mono)

gen O_Cdiff = -(phatC_r-phatO_r)
lab var O_Cdiff "Obama - Clinton difference"
twoway scatter O_Cdiff e08, scheme(s1mono)
tab e08 if O_Cdiff&gt;-.005 &amp; O_Cdiff&lt;=.005
//e08~-.05 is the breaking point between pos &amp; neg values of Obama-Clinton
sum e08 
sum e08 if e08&gt;=-.05
disp 5169/6269

**********************************
************END CCAP ANALYSIS*****
**********************************



*****************************************
************ANES CROSS-YEAR ANALYSIS*****
*these coded datasets available upon request*
*****************************************
clear
set maxvar 32000
set more off
use "C:/CDK WORK/Ethnocentrism/Obama/nes08_2011.dta"
append using "C:/CDK WORK/Ethnocentrism/Ethnocentrism data/nes04_2010.dta"
append using "C:/CDK WORK/Ethnocentrism/Ethnocentrism data/nes00_2010.dta"
append using "C:/CDK WORK/Ethnocentrism/Ethnocentrism data/nes96_2010.dta"
append using "C:/CDK WORK/Ethnocentrism/Ethnocentrism data/nes92_2010.dta"
append using "C:/CDK WORK/Ethnocentrism/Ethnocentrism data/nes88_2010.dta"

gen year = .
replace year = 2008 if V080001~=.
replace year = 2004 if v040001~=.
replace year = 2000 if v000001~=.
replace year = 1996 if v960001~=.
*could remove the panel Rs from 1996, but that would leave only 398 Rs in the cross-section, so leave them in 
replace year = 1992 if v923004~=.
replace year = 1988 if v880001~=.

mlogit voteobama e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new if race08==1, base(0)
est store yr2008

sum e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon08 if race08==1

*DV for 2004
recode v045026 (1=0 "Kerry")(3=1 "Bush")(else=.), gen(votekerry)
replace votekerry = 2 if v045030x==5
lab def votekerry 2"Abstained", modify
tab votekerry
*Issue scales for 2004
recode v043136 (1=0)(2=.17)(3=.33)(4 80 88=.5)(5=.67)(6=.83)(7=1)(else=.), gen(spdsvc04)
recode v043142 (1=1)(2=.83)(3=.67)(4 80 88=.5)(5=.33)(6=.17)(7=0)(else=.), gen(spddef04)
recode v043150 (1=1)(2=.83)(3=.67)(4 80 88=.5)(5=.33)(6=.17)(7=0)(else=.), gen(hlthins04)
recode v043152 (1=1)(2=.83)(3=.67)(4 80 88=.5)(5=.33)(6=.17)(7=0)(else=.), gen(jobsol04)
alpha spdsvc04 spddef04 hlthins04 jobsol04 //alpha = .65//

egen issuemis04 = rowmiss(spdsvc04 spddef04 hlthins04 jobsol04)
recode spdsvc04 spddef04 hlthins04 jobsol04 (.=0), gen(spdsvc04a spddef04a hlthins04a jobsol04a)
gen issuescale04 = ((spdsvc04a+spddef04a+hlthins04a+jobsol04a)/(4-issuemis04))*2-1 if issuemis04&lt;=2
mvdecode pid704, mv(9)
mvdecode aware04, mv(9)
mvdecode ecprog04, mv(9)
mvdecode female04, mv(9)
mvdecode hhecon04, mv(9)
gen educ04 = (v043252)/17
replace educ04 = . if v043252==88
gen pid_new04 = pid704*2-1
gen ecprog_new04 = ecprog04*2-1
gen hhecon_new04 = hhecon04*2-1
mlogit votekerry e04 pid_new04 issuescale04 ecprog_new04 female04 educ04 aware04 hhecon_new04 if race04==1, base(0)
est store yr2004


*NES 2000
*DV for 2000
tab1 v001248 v001249
recode v001249 (1=0 "Gore")(3=1 "Bush")(else=.), gen(votegore)
replace votegore = 2 if v001241&lt;4
lab def votegore 2"Abstained", modify
*Issue scales for 2000
*drop spdsvc00 jobsol00
recode v000545 (1=0)(2=.17)(3=.33)(4 0 8 =.5)(5=.67)(6=.83)(7=1) (else=.), gen(spdsvc00)
recode v000581 (1=1)(2=.83)(3=.67)(4 0 8 = .5)(5=.33)(6=.17)(7=0)(else=.), gen(spddef00)
recode v000609 (1=1)(2=.83)(3=.67)(4 0 8 =.5)(5=.33)(6=.17)(7=0)(else=.), gen(hlthins00)
recode v000615 (1=1)(2=.83)(3=.67)(4 0 8 =.5)(5=.33)(6=.17)(7=0)(else=.), gen(jobsol00)
alpha spddef00 spdsvc00 hlthins00 jobsol00  //alpha = .53
sum spddef00 spdsvc00 hlthins00 jobsol00 
egen issuemis00 = rowmiss(spdsvc00 spddef00 hlthins00 jobsol00)
recode spdsvc00 spddef00 hlthins00 jobsol00 (.=0), gen(spdsvc00a spddef00a hlthins00a jobsol00a)
gen issuescale00 = ((spdsvc00a +spddef00a +hlthins00a +jobsol00a)/(4-issuemis00))*2-1 if issuemis00&lt;=2
sum issuescale00

mvdecode pid700, mv(9)
mvdecode aware00, mv(9)
mvdecode ecprog00, mv(9)
mvdecode female00, mv(9)
mvdecode educ00, mv(9)
mvdecode hhecon00, mv(9)
gen pid_new00 = pid700*2-1
gen ecprog_new00 = ecprog00*2-1
gen hhecon_new00 = hhecon00*2-1

mlogit votegore e00 pid_new00 issuescale00 ecprog_new00 female00 educ00 aware00 hhecon_new00 if race00==1, base(0)
est store yr2000


*NES 1996
*DV for 1996
tab v961082
recode v961082 (1=0 "Clinton")(2=1 "Dole")(else=.), gen(voteclin96)
replace voteclin96 = 2 if v961074==5
lab def voteclin96 2"Abstained", modify
*Issue scales for 1996
drop spdsvc96
tab v960450
recode v960450 (1=0)(2=.17)(3=.33)(4 0 8 9=.5)(5=.67)(6=.83)(7=1)(else=.), gen(spdsvc96)
recode v960463 (1=1)(2=.83)(3=.67)(4 0 8 9=.5)(5=.33)(6=.17)(7=0)(else=.), gen(spddef96)
recode v960479 (1=1)(2=.83)(3=.67)(4 0 8 9=.5)(5=.33)(6=.17)(7=0)(else=.), gen(hlthins96)
drop jobsol96
recode v960483 (1=1)(2=.83)(3=.67)(4 0 8 9=.5)(5=.33)(6=.17)(7=0)(else=.), gen(jobsol96)
alpha spdsvc96 spddef96 hlthins96 jobsol96 //alpha = .59
sum spdsvc96 spddef96 hlthins96 jobsol96
egen issuemis96 = rowmiss(spdsvc96 spddef96 hlthins96 jobsol96)
recode spdsvc96 spddef96 hlthins96 jobsol96 (.=0), gen(spdsvc96a spddef96a hlthins96a jobsol96a)
gen issuescale96 = ((spdsvc96a +spddef96a +hlthins96a +jobsol96a)/(4-issuemis96))*2-1 if issuemis96&lt;=2
sum issuescale96

mvdecode pid796, mv(9)
mvdecode aware96, mv(9)
mvdecode ecprog96, mv(9)
mvdecode hhecon96, mv(9)
mvdecode female96, mv(9)
gen pid_new96 = pid796*2-1
gen ecprog_new96 = ecprog96*2-1
tab v960607
gen educ96 = v960607/17
replace educ96 = . if v960607==99
gen hhecon_new96 = hhecon96*2-1

mlogit voteclin96 e96 pid_new96 issuescale96 ecprog_new96 female96 educ96 aware96 hhecon_new96 if race96==1, base(0)
est store yr1996


*NES 1992
*DV for 1992
tab v925609, nol
recode v925609 (2=0 "Clinton")(1=1 "Bush")(else=.), gen(voteclin92)
replace voteclin92 = 2 if v925601==5
lab def voteclin92 2"Abstained", modify
tab voteclin92
*Setting aside 301 Perot voters
*Issue scales for 1992
drop spdsvc92 jobsol92
recode v923701 (1=0)(2=.17)(3=.33)(4 0 8 9=.5)(5=.67)(6=.83)(7=1)(else=.), gen(spdsvc92)
recode v923707 (1=1)(2=.83)(3=.67)(4 0 8 9=.5)(5=.33)(6=.17)(7=0)(else=.), gen(spddef92)
recode v923716 (1=1)(2=.83)(3=.67)(4 0 8 9=.5)(5=.33)(6=.17)(7=0)(else=.), gen(hlthins92)
recode v923718 (1=1)(2=.83)(3=.67)(4 0 8 9=.5)(5=.33)(6=.17)(7=0)(else=.), gen(jobsol92)

alpha spdsvc92 spddef92 hlthins92 jobsol92 //alpha = .56
sum spdsvc92 spddef92 hlthins92 jobsol92
egen issuemis92 = rowmiss(spdsvc92 spddef92 hlthins92 jobsol92)
recode spdsvc92 spddef92 hlthins92 jobsol92 (.=0), gen(spdsvc92a spddef92a hlthins92a jobsol92a)
gen issuescale92 = ((spdsvc92a +spddef92a +hlthins92a +jobsol92a)/(4-issuemis92))*2-1 if issuemis92&lt;=2
sum issuescale92

mvdecode pid792, mv(9)
mvdecode aware92, mv(9)
mvdecode ecprog92, mv(9)
mvdecode hhecon92, mv(9)
mvdecode female92, mv(9)
mvdecode educ92, mv(9)
gen pid_new92 = pid792*2-1
gen ecprog_new92 = ecprog92*2-1
gen hhecon_new92 = hhecon92*2-1

mlogit voteclin92 e92 pid_new92 issuescale92 ecprog_new92 female92 educ92 aware92 hhecon_new92 if race92==1, base(0)
est store yr1992

est table yr2008 yr2004 yr2000 yr1996 yr1992, b(%9.2f) star(.1 .05 .01) stats(ll chi2 N) style(col) eq(1)
est table yr2008 yr2004 yr2000 yr1996 yr1992, b(%9.2f) se stats(N) style(col) eq(1)


*****Is effect of E in 2008 distinguishable from its effect in previous years???*****
suest yr2008 yr2004 yr2000 yr1996 yr1992
test [yr2008_McCain]e08both=[yr2004_Bush]e04
test [yr2008_McCain]e08both=[yr2000_Bush]e00
test [yr2008_McCain]e08both=[yr1996_Dole]e96
test [yr2008_McCain]e08both=[yr1992_Bush]e92


**********PREDICTED PROBABILITIES**********
//SUBSTANTIVE EFFECTS
mlogit voteobama e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new if race08==1, base(0)

preserve
set more off
collapse e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new  if race08==1
sum e08both pid_new issue_new ecprog_new female08 educ08 infopty08 hhecon_new  
replace female=1
expand 13
egen x = fill(-.5(.1).7)
replace e08=x
estimates restore yr2008
predict p2008, outcome(0)
table e08, c(mean p2008)
restore

mlogit voteclin92 e92 pid_new92 issuescale92 ecprog_new92 female92 educ92 aware92 hhecon_new92 if race92==1, base(0)
preserve
set more off
collapse e92 pid_new92 issuescale92 ecprog_new92 female92 educ92 aware92 hhecon_new92 if race92==1
replace female=1
expand 13
egen x = fill(-.5(.1).7)
replace e92=x
estimates restore yr1992
predict p1992, outcome(0)
table e92, c(mean p1992)
restore



****************ANES PANEL, OBAMA AS MUSLIM***********
*Is Barack Obama a Muslim?
*Weight for wave 9 is wgtcs09; for wave 11 is wgtcs11
use "C:\anes\anes2008_2009panel/anes2008_2009panel.dta", clear
tab der04
drop if der04~=1
svyset [pweight=wgtcs09]
svy: tab w9v3

svyset [pweight=wgtcs11]
svy: tab w11wv3
</pre></body></html>Ztext/plainUUTF-8_Dhttps://my.vanderbilt.edu/cindykam/files/2012/01/AJPS-KAM-KINDER.txtP
