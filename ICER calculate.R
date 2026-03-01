#Making the ICER function
#ICER basic function comparing one of our drugs to one of our comparators
#Basic Steps 
#inputs: 
#1. Calculate the QALY per drug Utility x duration 
#inputs: number (util_pf), number (util_pp), number (prog_prob_drug), number (duration)
#outputs: number (QALY_drug)
#step 1: multiply 1 (patient #) by prog_prob_drug
#step 2: Multiply output of step 1 by util_pp
#step 3: Multiply 1 (patient#) by (1 - prog_prob_drug)
#Step 4: Multiply result of step 3 by util_pf
#Step 5: Add the result of Step #2 and Step #4 to get the QALY for drug

#Utility of the patients who progressed 
calculate_qaly_drug_prog <- function(util_pp, prog_prob_drug, patient_count = 1 ){
  prog_patients <- patient_count * prog_prob_drug
  prog_patients * util_pp
}
print(calculate_qaly_drug_prog(.3, .23))



#2. Calculate the QALY per comparator  
#3. Calculate the cost per drug and per comparator
#4. Convert that into a ICER