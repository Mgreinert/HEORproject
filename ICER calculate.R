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
library(readr)
library(dplyr)

data1 <- read_csv("C:/Users/Thesq/Documents/Learning R/HEORproject/parameter_table_v2.csv")
print(data1)



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

combine_qaly_drug <-function(util_pp, util_pf, prog_prob_drug, patient_count =1){
  qaly_drug_prog <- calculate_qaly_drug_prog(util_pp,prog_prob_drug, patient_count)
  qaly_drug_pf <- calculate_qaly_drug_pf(util_pf,prog_prob_drug, patient_count)
  qaly_drug_pf + qaly_drug_prog 
}
print(combine_qaly_drug(.3,.86,.23))

standard_util_pp <- .3
standard_util_pf <- .8 
standard_prog_prob_drug <-.34
standard_prog_prob_comp <-.78

drug_a_qaly <-combine_qaly_drug(standard_util_pp, standard_util_pf, standard_prog_prob_drug)
comp_x_qaly <-combine_qaly_drug(standard_util_pp, standard_util_pf, standard_prog_prob_comp)

print(drug_a_qaly)
print(comp_x_qaly)



# Use mutate to modify an existing column (e.g., convert units)
data1 <- data1 %>%
  mutate(qaly_drug = combine_qaly_drug(util_pp, util_pf, prog_prob_drug))
         


#2. Calculate the QALY per comparator  
#inputs: number (util_pf), number (util_pp), number (prog_prob_comp), number (duration)
#outputs: number (QALY_comp)
#step 1: multiply 1 (patient #) by prog_prob_comp
#step 2: Multiply output of step 1 by util_pp
#step 3: Multiply 1 (patient#) by (1 - prog_prob_comp)
#Step 4: Multiply result of step 3 by util_pf
#Step 5: Add the result of Step #2 and Step #4 to get the QALY for the comparator



#3. Calculate the cost per QALY for drug and per comparator

#4. Convert that into a ICER