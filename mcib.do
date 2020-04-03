use "H:\zoning\output\mcib\mcib_inc_2010.dta"
 
mcib hhs min max, mean(meanhhy) by(METRO) saving(H:\zoning\output\mcib\results_mcib_inc_2010) replace
 
clear
 
use "H:\zoning\output\mcib\mcib_inc_2016.dta"
 
mcib hhs min max, mean(meanhhy) by(METRO) saving(H:\zoning\output\mcib\results_mcib_inc_2016) replace

clear
 
use "H:\zoning\output\mcib\mcib_rent_2010.dta"
 
mcib hhs min max, mean(meanhhy) by(METRO) saving(H:\zoning\output\mcib\results_mcib_rent_2010) replace

clear
 
use "H:\zoning\output\mcib\mcib_rent_2016.dta"
 
mcib hhs min max, mean(meanhhy) by(METRO) saving(H:\zoning\output\mcib\results_mcib_rent_2016) replace
 
exit