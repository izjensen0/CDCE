**11/11/25.

*Analysis of Provisional Ballot Data in MD.
**Focus is on college precincts.

"Coppin State - Robert Coleman Elementary, county code 3, consolidated polling place with 13-007 and 15-020. 

Hopkins - Enoch Pratt Free Library, Waverly Branch, county code 3, consolidated polling place with both 12-004 and 12-005.  

Morgan State - county code 3, 27-026 - is another one to look at per Sam.

Towson is county code 4, 09-029.

Also, for what it's worth, Ritchie Coliseum is county code 17, 21-002."  


***NEED TO CONFIRM HOPKINS.


*PG County is 17 on LBE_VOTED_IN & Stamp Student Union is "21017"

gen stamp = 0
replace stamp = 1 if LBE_VOTED_IN==17 & POLLING_PLACE_PRECINCT=="21017"

ta stamp
ta POLLING_PLACE_PRECINCT stamp
ta LBE_VOTED_IN stamp

ta PROVISIONAL_STATUS
ta PROVISIONAL_STATUS if stamp==1

*Easier to do it this way.
bysort stamp: ta PROVISIONAL_STATUS
*The Stamp AIF is much smaller than average, AIP much larger, and Rejected a bit smaller.

*Towson is 4 on LBE_VOTED_IN & Towson University South Campus Pavillion 9029.

gen towson = 0
replace towson = 1 if LBE_VOTED_IN==4 & POLLING_PLACE_PRECINCT=="9029"

ta towson
ta POLLING_PLACE_PRECINCT towson

*There are 107 cases that = 0 on townson but have POLLING_PLACE_PRECINCT="9029"
**NOT SURE WHY but they all have LBE = 16.

bysort towson: ta PROVISIONAL_STATUS

*Coppin is 3 on LBE_VOTED_IN  Robert Coleman Elementary consolidated polling place with 13007 and 15020.

gen coppin = 0
replace coppin =1 if LBE_VOTED_IN==3 & POLLING_PLACE_PRECINCT=="13007"
replace coppin =1 if LBE_VOTED_IN==3 & POLLING_PLACE_PRECINCT=="15020"

ta coppin
ta POLLING_PLACE_PRECINCT coppin

*There are 94 cases that = 0 on coppin but have POLLING_PLACE_PRECINCT="15020" & ///
351 from 13007.
*To see where they voted.
ta LBE coppin if POLLING_PLACE_PRECINCT=="13007"
ta LBE coppin if POLLING_PLACE_PRECINCT=="15020"


bysort coppin: ta PROVISIONAL_STATUS
*Coppin has a much larger rejection rate, smaller AIF, and larger AIP, but more balance on AIF and AIP than overall and other colleges.

ta PROVISIONAL_STATUS if coppin==1 & POLLING_PLACE_PRECINCT=="13007"
ta PROVISIONAL_STATUS if coppin==1 & POLLING_PLACE_PRECINCT=="15020"


*Morgan State is 3 on LBE_VOTED_IN & 27026.

gen morganstate = 0
replace morganstate = 1 if LBE_VOTED_IN==3 & POLLING_PLACE_PRECINCT=="27026"

ta morganstate
ta POLLING_PLACE_PRECINCT morganstate

bysort morganstate: ta PROVISIONAL_STATUS

*Hopkins is 3 on LBE_VOTED_IN & POLLING_PLACE_PRECINCT 12004 and 12005.

gen hopkins = 0 
replace hopkins = 1 if LBE_VOTED_IN==3 & POLLING_PLACE_PRECINCT=="12004"
replace hopkins = 1 if LBE_VOTED_IN==3 & POLLING_PLACE_PRECINCT=="12005"

bysort hopkins: ta PROVISIONAL_STATUS

ta PROVISIONAL_STATUS if hopkins==1 & POLLING_PLACE_PRECINCT=="12004"
ta PROVISIONAL_STATUS if hopkins==1 & POLLING_PLACE_PRECINCT=="12005"



I also noticed that McKeldin Gym at Bowie State (14-009, 14-011, 14-014, 14-023) looks like a campus precinct. Can you run the same PB #s there that you did for Stamp to see if the same PWI / HBCU disparity that shows up in Baltimore City and North Carolina shows up here?

*Bowie State is 17 on LBE_VOTED_IN & has 4 precincts at McKeldin Gym 14009, 14011, 14014, 14023.

gen bowie = 0
replace bowie = 1 if LBE_VOTED_IN==17 & POLLING_PLACE_PRECINCT=="14009"
replace bowie = 1 if LBE_VOTED_IN==17 & POLLING_PLACE_PRECINCT=="14011"
replace bowie = 1 if LBE_VOTED_IN==17 & POLLING_PLACE_PRECINCT=="14014"
replace bowie = 1 if LBE_VOTED_IN==17 & POLLING_PLACE_PRECINCT=="14023"

ta bowie
ta POLLING_PLACE_PRECINCT bowie

bysort bowie: ta PROVISIONAL_STATUS

ta PROVISIONAL_STATUS if bowie==1 & POLLING_PLACE_PRECINCT=="14009"
ta PROVISIONAL_STATUS if bowie==1 & POLLING_PLACE_PRECINCT=="14011"
ta PROVISIONAL_STATUS if bowie==1 & POLLING_PLACE_PRECINCT=="14014"
ta PROVISIONAL_STATUS if bowie==1 & POLLING_PLACE_PRECINCT=="14023"

