#Making the ICER function
#ICER basic function comparing one of our drugs to one of our comparators
#Utility is drug agnostic
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

#Utility of the patients who progressed 
calculate_qaly_drug_prog <- function(util_pp, prog_prob_drug, patient_count = 1 ){
  prog_patients <- patient_count * prog_prob_drug
  prog_patients * util_pp
}

#Utility of the patients who didn't progress
calculate_qaly_drug_pf <- function(util_pf, prog_prob_drug, patient_count = 1 ){
  pf_patients <- patient_count * (1-prog_prob_drug)
  pf_patients * util_pf
}

#Add the QALYS of those who progressed to those who remained progression free together

combine_qaly_drug <-function(util_pp, util_pf, prog_prob_drug, patient_count =1){
  qaly_drug_prog <- calculate_qaly_drug_prog(util_pp,prog_prob_drug, patient_count)
  qaly_drug_pf <- calculate_qaly_drug_pf(util_pf,prog_prob_drug, patient_count)
  qaly_drug_pf + qaly_drug_prog 
}

#Calculate cost per QALY for the drugs

#drug a cost 4500, qaly_drug a = .731  = drug_cost/qaly_drug

calculate_cost_qaly <- function(drug_cost, qaly_drug){
  cost_qaly = drug_cost/qaly_drug
}

#ICER = (Cost of drug  - Cost of Comp) / (QALY of drug  - QALY of comp)

calculate_icer <- function(drug_cost, comparator_cost, qaly_drug, qaly_comp){
  
  numerator_cost <- drug_cost - comparator_cost
  denominator_qaly <- qaly_drug - qaly_comp
  numerator_cost/denominator_qaly
}

#Load Data for analysis
data1 <- read_csv("C:/Users/Thesq/Documents/Learning R/HEORproject/parameter_table_v2.csv")
print(data1)

#Function check
print(calculate_qaly_drug_prog(.3, .23))

#Function Check
print(calculate_qaly_drug_pf(.86, .23))

#Function Check
print(combine_qaly_drug(.3,.86,.23))

standard_util_pp <- .3
standard_util_pf <- .8 
standard_prog_prob_drug <-.34
standard_prog_prob_comp <-.78

drug_a_qaly <-combine_qaly_drug(standard_util_pp, standard_util_pf, standard_prog_prob_drug)
comp_x_qaly <-combine_qaly_drug(standard_util_pp, standard_util_pf, standard_prog_prob_comp)

print(drug_a_qaly)
print(comp_x_qaly)

#1. Calculate the QALY per drug Utility x duration
# Use mutate to modify an existing column to add a column for the QALY per drug
data1 <- data1 %>%
  mutate(qaly_drug = combine_qaly_drug(util_pp, util_pf, prog_prob_drug))
         
#2. Calculate the QALY per comparator  
#inputs: number (util_pf), number (util_pp), number (prog_prob_comp), number (duration)
#outputs: number (qaly_comp)

#This is the same steps as for qaly_drug

data1 <- data1 %>% 
  mutate(qaly_comp = combine_qaly_drug(util_pp, util_pf, prog_prob_comp))


#Function Check
print(calculate_cost_qaly(4500, .731))

#Step 3: Calculate the cost per QALY for drugs

data1 <- data1 %>% 
  mutate(cost_qaly_drug = calculate_cost_qaly(drug_cost, qaly_drug))

#Step 4: Calculate cost per QALY for comparators 

data1 <- data1 %>% 
  mutate(cost_qaly_comp = calculate_cost_qaly(comparator_cost, qaly_comp))

#Step 5. Convert that into an ICER comparison

#ICER = (Cost of drug  - Cost of Comp) / (QALY of drug  - QALY of comp)

#This is a nested loop to check for every drug i to compare each one to every comparator j 

icer_results <- data.frame()

for (i in 1:nrow(data1)) {
  for (j in 1:nrow(data1)) {
    
    # Use drug row's utility values for both calculations
    qaly_drug_i <- combine_qaly_drug(data1$util_pp[i], data1$util_pf[i], data1$prog_prob_drug[i])
    qaly_comp_j <- combine_qaly_drug(data1$util_pp[i], data1$util_pf[i], data1$prog_prob_comp[j])

    # This part takes the relevant drug/comparator costs and the Qalys per drug/comparator to make the ICER comparison     
    icer_val <- calculate_icer(
      data1$drug_cost[i],
      data1$comparator_cost[j],
      qaly_drug_i,
      qaly_comp_j
    )
    
    #This part builds the output of the comparisons into a nice results table
    icer_results <- rbind(icer_results, data.frame(
      drug       = data1$drug_name[i],
      comparator = data1$comparator_name[j],
      icer       = round(icer_val, 2),
      delta_cost = data1$drug_cost[i] - data1$comparator_cost[j],
      delta_qaly = round(qaly_drug_i - qaly_comp_j, 4)
    ))
  }
}

#Step 6. Add Dominance check 

icer_results <- icer_results %>%
  mutate(
    interpretation = case_when(
      delta_cost == 0 & delta_qaly > 0  ~ "Same cost, more effective (favorable)",
      delta_cost == 0 & delta_qaly < 0  ~ "Same cost, less effective (unfavorable)",
      delta_cost < 0 & delta_qaly > 0   ~ "Dominant",
      delta_cost > 0 & delta_qaly < 0   ~ "Dominated",
      delta_cost >= 0 & delta_qaly > 0  ~ "More costly, more effective",
      delta_cost <= 0 & delta_qaly < 0  ~ "Less costly, less effective",
      TRUE ~ "Check data"
    )
  )
print(icer_results)


