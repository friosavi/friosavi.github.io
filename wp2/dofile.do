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