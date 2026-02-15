#Making a parameter table
#Making a Data Frame with 7 columns matching the variables. "Drug name", "Drug costs", "comparator name", Comparator Cost", "Progression Probability", "Utility Values pf", Utility values p 
parameter_table <- data.frame(
  drug_name = c("a", "b", "c"),
  drug_cost = c(4500, 3000, 3600),
  comparator_name = c("z","y","X"),
  comparator_cost = c(3000, 2400, 3300),
  prog_prob_drug = c(.23, .25, .24),
  prog_prob_comp = c(.34, .36, .38),
  util_pf = c(.86, .87, .82), 
  util_pp = c(.3, .4, .26)
)
#print the parameter table
parameter_table
print(parameter_table)
library(readr)
write_csv(parameter_table,"C:/Users/Thesq/Documents/Learning R/parameter_table.csv")
parameter_table[2,2] = 3300
