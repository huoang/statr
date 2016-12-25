import excel H:\pyr\data\cost_all.xlsx, firstrow clear

gen lncost = log(cost)

reg lncost outp opsqr opcubed inp inpsqr inpcubed ///
			wageindex beds fixed cmi if year == 2011 ///
		 ,robust
predict ei , res		 
estat ovtest, rhs		
dwstat

estat hettest, rhs
estat szroeter, rhs
estat imtest, white

hettest
 		 
		 
reg cost outp opsqr opcubed inp inpsqr inpcubed ///
		 wageindex beds fixed cmi
		 
hettest
