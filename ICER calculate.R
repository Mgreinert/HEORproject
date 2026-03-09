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

#Utility of the patients who didn't progress
calculate_qaly_drug_pf <- function(util_pf, prog_prob_drug, patient_count = 1 ){
  pf_patients <- patient_count * (1-prog_prob_drug)
  pf_patients * util_pf
}
print(calculate_qaly_drug_pf(.86, .23))

#Add the QALYS of those who progressed to those who remained progression free together

calculate_qaly_drug <- function(calculate_qaly_drug_pf, calculate_qaly_drug_prog){
  qaly_drug <- calculate_qaly_drug_pf + calculate_qaly_drug_prog
}
print(calculate_qaly_drug)

#2. Calculate the QALY per comparator  
#inputs: number (util_pf), number (util_pp), number (prog_prob_comp), number (duration)
#outputs: number (QALY_comp)
#step 1: multiply 1 (patient #) by prog_prob_comp
#step 2: Multiply output of step 1 by util_pp
#step 3: Multiply 1 (patient#) by (1 - prog_prob_comp)
#Step 4: Multiply result of step 3 by util_pf
#Step 5: Add the result of Step #2 and Step #4 to get the QALY for the comparator



#3. Calculate the cost per drug and per comparator
#4. Convert that into a ICER