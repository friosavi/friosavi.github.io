global server 	"I:/Shared drives/levy_distribution"
global limew 	$server/projects/LIMEW/LIMEW_ARC/LIMEW2019/
global tuse 	$limew/timeuse  
global data 	$server/data/USA
global morg 	$data/MORG/morg19
global cps_proc $data/ASEC/2020/CPS2020_proc

global cps  	$data/ASEC/2020/cpsmar2020 
** SUB CPS
global cps_p    $data/ASEC/2020/pppub20.dta
global cps_h    $data/ASEC/2020/hhpub20.dta
global cps_f    $data/ASEC/2020/ffpub20.dta

** asectime
 global year     2019

** Atusfiles
global atusum  $data\ATUS\2019\atussum-2019\atussum_2019
global aturost $data\ATUS\2019\atusrost-2019\atusrost_2019.dta
global aturep  $data\ATUS\2019\atusresp-2019\atusresp_2019.dta
global atucps  $data\ATUS\2019\atuscps-2019\atuscps_2019.dta

global oado   $tuse/otherado 
adopath + "$oado" 

use "$tuse\data\timeuse_CPS.dta", clear
tab survey
tab survey [w=nwgt]
gen emp = lfs==1
gen spouse_emp=0 
replace spouse_emp=(splfs==1)+1 if splfs!=0

 tab survey [w=nwgt],     matcell(ms0)
tab sex            survey [w=nwgt],     matcell(ms1)
tab parentyn       survey [w=nwgt],     matcell(ms2)
tab emp            survey [w=nwgt],     matcell(ms3)
tab spouse_present survey [w=nwgt],     matcell(ms4)
tab spouse_emp     survey [w=nwgt],     matcell(ms5)
matrix ms5=ms5[2..3,....]
matrix mmx=[.,.]

mata: 
    tt  = st_matrix("ms0")'
	tb1 = st_matrix("ms1")\
	      st_matrix("ms2")\
		  st_matrix("ms3")\
		  st_matrix("ms4")\
		  st_matrix("ms5")
	tb1 = tb1:/tt*100
	tt  = tt,(tt[,2]:-tt[,1]):/tt[,1]*100
	tb1 = tt\(tb1,tb1[,2]:-tb1[,1])
end

mata: st_matrix("tb1",tb1)


matrix roweq   tb1 = rr1 rr2 rr2 rr3 rr3 rr4 rr4 rr5 rr5 rr6 rr6
esttab matrix(tb1, fmt("0 1" "0 1" "1 1"))  using tb1 , mlabels(, none) md ///
					varlabels(r1 "N"  r2 "Female" r3 "Male" ///
							 r4 "No" r5 "Yes" ///
							 r6 "Not Employed"  r7 "Employed" ///
							 r8 "No" r9 "Yes" ///
							 r10 "Spouse not Employed" ///
							 r11 "Spouse Employed" ) ///
					collab("ASEC" "ATUS" "diff") ///
					eqlabels("" "Sex" "Parental Status" "Labor Force Status" "Spouse present" "Spouse's labor force status") ///
					note(":Summary Statistics, Alignment across Selected Variables {#tbl-tb1}") replace   

**** Table 2

foreach i in hhinccl age_gr race educ child18 adultnum {
	local j = `j'+1
	tab `i' survey [w=nwgt],     matcell(ms`j')
	mata:mm=st_matrix("ms`j'");mm=mm:/colsum(mm)*100;mm=mm,mm[,2]:-mm[,1]
	mata:st_matrix("ms`j'",mm)
	matrix roweq ms`j'=rr`j'
	matrix tb2 = nullmat(tb2)\ms`j'
}
#delimit;
esttab matrix(tb2, fmt("1" "1" "1")), mlabels(, none) md 
varlabels(rr1:r1 "0-14,999"  rr1:r2 "15,000-34,999" rr1:r3 "35,000-49,999" rr1:r4 "50,000-74,999" rr1:r5 "75,000+" 
rr2:r1 "15 to 24" rr2:r2 "25 to 34" rr2:r3 "35 to 44" rr2:r4 "45 to 54" 
rr2:r5 "55 to 64" rr2:r6 "65 to 74" rr2:r7 "75 and older"
rr3:r1 "White" rr3:r2 "Black" rr3:r3 "Hispanic" rr3:r4 "Other"
rr4:r1 "Less than high school" rr4:r2 "High school"
rr4:r3 "Some college" rr4:r4 "College/grad school"
rr5:r1 "0" rr5:r2 "1" rr5:r3 "2" rr5:r4 "3" rr5:r5 "4 or more"
rr6:r1 "1" rr6:r2 "2" rr6:r3 "3" rr6:r4 "4" rr6:r5 "5 or more" ) 
collab("ASEC" "ATUS" "diff") 
eqlabels(
  "Household income category" 
  "Age category" 
  "Race" 
  "Educational attainment"
  "Number of children under 18 in household" 
  "Number of persons in household over 18"
);


gen married = inlist(marstat,1,2,3)
gen emp = lfs==1
gen spouse_emp=0 
replace spouse_emp=(splfs==1)+1 if splfs!=0

tabstat hhprod_wh care core proc, by(survey) save
foreach i in sex parentyn emp married spouse_emp {
	levelsof `i', local(li)
	foreach j of local li {
		tabstat hhprod_wh care core proc [w=nwgt] if `i'==`j', by(survey) save
		matrix tm = r(Stat1)',r(Stat2)'
		mata:tm=st_matrix("tm");tm=tm[,1]:/tm[,2]*100;st_matrix("tm",tm')
		matrix tmt = nullmat(tmt)\tm
	}
}

frame create fig
frame change fig
clear
svmat tmt
gen g0 = 100
gen d =1
replace d=2 if inlist(_n+1,4,6,8,10) 
gen t=15-sum(d)


two (pcspike t g0 t tmt1) (scatter t tmt1, msize(2)) , ///
legend(off) xlabel(88(4)112 ) ytitle("") ///
ylabel(14 "Female" 13 "Male" 11 "Not a Parent" 10 "Parent" ///
8 "Not Working" 7 "Working" 5 "Not Married" 4 "Married" 2 "Souse Not-Present" 1 "Spouse Not-Working" 0 "Spouse Working") //

two (pcspike t g0 t tmt2) (scatter t tmt2, msize(2)) , ///
legend(off) xlabel(88(4)112 ) ytitle("") ///
ylabel(14 "Female" 13 "Male" 11 "Not a Parent" 10 "Parent" ///
8 "Not Working" 7 "Working" 5 "Not Married" 4 "Married" 2 "Souse Not-Present" 1 "Spouse Not-Working" 0 "Spouse Working")

two (pcspike t g0 t tmt3) (scatter t tmt3, msize(2)) , ///
legend(off) xlabel(88(4)112 ) ytitle("") ///
ylabel(14 "Female" 13 "Male" 11 "Not a Parent" 10 "Parent" ///
8 "Not Working" 7 "Working" 5 "Not Married" 4 "Married" 2 "Souse Not-Present" 1 "Spouse Not-Working" 0 "Spouse Working")

two (pcspike t g0 t tmt4) (scatter t tmt4, msize(2)) , ///
legend(off) xlabel(88(4)112 ) ytitle("") ///
ylabel(14 "Female" 13 "Male" 11 "Not a Parent" 10 "Parent" ///
8 "Not Working" 7 "Working" 5 "Not Married" 4 "Married" 2 "Souse Not-Present" 1 "Spouse Not-Working" 0 "Spouse Working")

gen tmt2b=tmt2 + 20
gen g0b=g0+20

gen tmt3b=tmt3 + 40
gen g0c=g0+40

gen tmt4b=tmt4 + 60
gen g0d=g0+60

two (pcspike t g0 t tmt1, pstyle(p1)) (scatter t tmt1, msize(2)  pstyle(p1)) /// 
    (pcspike t g0b t tmt2b, pstyle(p1)) (scatter t tmt2b, msize(2)  pstyle(p1)) ///
	(pcspike t g0c t tmt3b, pstyle(p1)) (scatter t tmt3b, msize(2)  pstyle(p1)) ///
	(pcspike t g0d t tmt4b, pstyle(p1)) (scatter t tmt4b, msize(2)  pstyle(p1)), ///
legend(off) xscale(range(90 170 )) ytitle("") graphregion(margin(t=10)) ///
xlabel( 95 100 105 110 " " ///
115 "95" 120 "100" 125 "105" 130 " "  ///
135 "95" 140 "100" 145 "105" 150 " " ///
155 "95" 160 "100" 165 "105" ) ///
ylabel(14 "Female" 13 "Male" 11 "Not a Parent" 10 "Parent" ///
8 "Not Working" 7 "Working" 5 "Not Married" 4 "Married" 2 "Souse Not-Present" 1 "Spouse Not-Working" 0 "Spouse Working") xsize(10)	ysize(5) ///
text(15 100 "Household Production" ///
15 120 "Care" 15 140 "Core" 15 160 "Procurement" ) ///
note(Ratio = ASEC/ATUS)


matrix drop tb7
foreach i in married parentyn sex emp spouse_emp educ hhinccl age_gr {
	levelsof `i', local(li)
	local k = `k'+1
	foreach j of local li {
		tabstat hhprod_wh [w=nwgt] if `i'==`j', by(survey) stats(mean median) save
		matrix aux 	=[r(Stat1)',r(Stat2)']
		matrix roweq aux = rr`k'
		matrix tb7 = nullmat(tb7)\aux
	}
}
local rq:roweq tb7
mata:tb7=st_matrix("tb7")
mata:tb7=tb7[,1], (tb7[,1]:/tb7[,3]*100),tb7[,2],(tb7[,2]:/tb7[,4]*100)
mata:st_matrix("tb7",tb7)
matrix roweq tb7 = `rq'


#delimit;
esttab matrix(tb7, fmt("1" "1" "1")), mlabels(, none) md 
collab("Mean" "Ratio wrt ATUS" "Median" "Ratio wrt ATUS" )
eqlabels("Marital Status" 
"Parental Status" 
"Sex" 
"Labor Status"
"Spouse's labor Status" 
"Education"
"Household income ($)"
"Age Group")
varlabels(r1 " Not Married"  r2 " Married" 
r3 " Non-parent" r4 " Parent" 
r5 " Female" r6 " Male"
r7 " Not Working" r8 " Working" 
r9 " No Spouse present" r10 " SP not working" r11 "SP working"
r12 " Less than high school" r13 " High school"
r14 " Some college" r15 " College/grad school"
r16 " <$15k" r17 " $15k-$35k" 
r18 " $35k-$50k" r19 " $50k-$75k" r20 " >$75k" 
r21 " 15 to 24" r22 " 25 to 34" r23 " 35 to 44" 
r24 " 45 to 54" r25 " 55 to 64" r26 " 65 to 74" r27 " 75 and older") ;
#delimit cr

varlabels(rr1:r1 "0-14,999"  rr1:r2 "15,000-34,999" rr1:r3 "35,000-49,999" rr1:r4 "50,000-74,999" rr1:r5 "75,000+" 
rr2:r1 "15 to 24" rr2:r2 "25 to 34" rr2:r3 "35 to 44" rr2:r4 "45 to 54" 
rr2:r5 "55 to 64" rr2:r6 "65 to 74" rr2:r7 "75 and older"
rr3:r1 "White" rr3:r2 "Black" rr3:r3 "Hispanic" rr3:r4 "Other"
rr4:r1 "Less than high school" rr4:r2 "High school"
rr4:r3 "Some college" rr4:r4 "College/grad school"
rr5:r1 "0" rr5:r2 "1" rr5:r3 "2" rr5:r4 "3" rr5:r5 "4 or more"
rr6:r1 "1" rr6:r2 "2" rr6:r3 "3" rr6:r4 "4" rr6:r5 "5 or more" ) 
collab("ASEC" "ATUS" "diff") 
eqlabels(
  "Household income category" 
  "Age category" 
  "Race" 
  "Educational attainment"
  "Number of children under 18 in household" 
  "Number of persons in household over 18"
);

gen cx=1
capture matrix drop tb11
foreach i in cx home_prop hhinccl elder famtype racecl {
	levelsof `i', local(li)
	foreach j of local li {
		tabstat2 networth [w=wgt] if `i'==`j',  by(survey) save	stats(mean median)
		matrix tb11 = nullmat(tb11)\r(tmatrix2)
	}
}

mata:tb11=st_matrix("tb11")
mata:tb11=tb11[,1],tb11[,1]:/tb11[,3]*100,tb11[,2],tb11[,2]:/tb11[,4]*100
mata:st_matrix("tb11",tb11)
matrix roweq tb11 = r1 r2 r2 r2 r3 r3 r3 r3 r3 r4 r4 r5 r5 r5 r6 

#delimit;
esttab matrix(tb11, fmt(%10.0fc 1 %10.0fc 1)), mlabels(, none)
collab("Mean-ASEC" "Ratio to SCF" "Median-ASEC" "Ratio to SCF")
eqlabels("Total" 
"Homeownership" 
"Income Group" 
"Age"
"Family type" 
"Race")
varlabels(r1 ""  r2 " Renter" 
r3 " Owner with Mortgage" r4 " Owner w/o Mortgage" 
r5 " <$20k" r6 " $20k-$50k"
r7 " $50k-$75k" r8 " $75k-$100k" 
r9 " >$100k" r10 " Non-Eldery" r11 " Elder"
r12 " Couple" 
r13 " Single female"
r14 " Single male"
r15 " White"
r16 " Black" r17 " Hispanic"
r18 " Other") ;
#delimit cr