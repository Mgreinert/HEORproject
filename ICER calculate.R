#Making the ICER function
#ICER basic function comparing one of our drugs to one of our comparators
#Basic Steps 
#inputs: 
#1. Calculate the QALY per drug Utility x duration 
#inputs: number (util_pf), number (util_pp), number (prog_prob_drug), number (duration)
#outputs: number (qaly_drug)
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
#outputs: number (qaly_comp)

#This is the same steps as for qaly_drug

data1 <- data1 %>% 
  mutate(qaly_comp = combine_qaly_drug(util_pp, util_pf, prog_prob_comp))


#3. Calculate the cost per QALY for drug and per comparator

#Calculate cost per QALY for the drugs

#drug a cost 4500, qaly_drug a = .731  = drug_cost/qaly_drug

calculate_cost_qaly <- function(drug_cost, qaly_drug){
  cost_qaly = drug_cost/qaly_drug
}
print(calculate_cost_qaly(4500, .731))

data1 <- data1 %>% 
  mutate(cost_qaly_drug = calculate_cost_qaly(drug_cost, qaly_drug))

#Calculate cost per QALY for comparators 

data1 <- data1 %>% 
  mutate(cost_qaly_comp = calculate_cost_qaly(comparator_cost, qaly_comp))

#4. Convert that into a ICER

#ICER = (Cost of drug  - Cost of Comp) / (QALY of drug  - QALY of comp)

calculate_icer <- function(drug_cost, comparator_cost, qaly_drug, qaly_comp){
  
  numerator_cost <- drug_cost - comparator_cost
  denominator_qaly <- qaly_drug - qaly_comp
  numerator_cost/denominator_qaly
}

# Drug A vs. Comparator Z 

print(calculate_icer(4500, 3000, .731, .67))


for (i in 1:nrow(data1)) {
  # Create a sample data frame
  row_data <- data1[i, ]
  print(row_data)
  drug_cost <- row_data$drug_cost
  qaly_drug <- row_data$qaly_drug
  for (j in 1:nrow(data1)) {
    row_dataj <- data1[j, ]
    print(calculate_icer(drug_cost, row_dataj$comparator_cost, qaly_drug, row_dataj$qaly_comp))
  }
}
