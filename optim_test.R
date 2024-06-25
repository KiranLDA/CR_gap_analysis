# Install and load the Rglpk package
# install.packages("Rglpk")
library(Rglpk)

# Read in the data
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/MSC_projects/2023-2024/Jessica_Neil-Boss_seed_CE/"
data = read.csv(paste0(basepath, "Available data.csv"))

#########################################
## Input data
##########################################

# Budget in pounds
budget <- 100000#0 #3551644# # on the cluster: seq(0, sum(actions$cost), length.out = 100)[repetition]

# keep data without NAs
index = which(!(is.na(data$Predicted_ave_seed )) & !(is.na(data$Storage_cost_3_years_per_accession))) # keep only rows where data is complete

# Number of plant species
num_species <- length(data$WCVP.accepted.name[index])

# Species names
species_names = data$WCVP.accepted.name[index]

# Number of seeds produced by each species
seeds_per_species <- as.numeric(data$Predicted_ave_seed[index])

# The cost to collect seeds from each species
cost_per_species <- round(as.numeric(gsub(",", "",data$Banking_cost_per_accession[index])))

# The maximum number of seeds that need to be collected for each species
max_seeds_per_species <- 10000 - (data$Predicted_ave_seed[index] * data$Num_accessions[index])#data$Num_accessions[index]
max_seeds_per_species <- ifelse(max_seeds_per_species < 0, 0 , max_seeds_per_species)

# number of accessions per species
accessions_per_species <- data$Num_accessions[index]

# accessions
max_accessions <-  data$Accessions.needed.for.5[index]#round(runif(num_species,1,5))


# Extinction risk
extinction_risk <- runif(num_species,0,1)

#########################################
## Formulate benefit
##########################################

# Add benefit
benefit = accessions_per_species * extinction_risk# seeds_per_species *


#########################################
##  Do the optimisation
##########################################


# Objective function coefficients (maximize the total number of seeds collected)
objective <- benefit #seeds_per_species

# Constraint matrix
# 1. Budget constraint
constraint_matrix <- matrix(cost_per_species, nrow=1, byrow=TRUE)

# Right-hand side of the constraints
rhs <- c(budget)

# Direction of the constraints
direction <- c("<=")

# Add constraints to ensure that no more than the species-specific maximum seeds are collected
for (i in 1:num_species) {
  new_constraint <- rep(0, num_species)
  # new_constraint[i] <- seeds_per_species[i]
  new_constraint[i] <- accessions_per_species[i]
  constraint_matrix <- rbind(constraint_matrix, new_constraint)
  # rhs <- c(rhs, max_seeds_per_species[i])
  rhs <- c(rhs, max_accessions[i])
  direction <- c(direction, "<=")
}

# Solve the integer linear programming problem
solution <- Rglpk_solve_LP(obj = objective,
                           mat = constraint_matrix,
                           dir = direction,
                           rhs = rhs,
                           types = rep("I", num_species),
                           max = TRUE)



#########################################
## Look at results
##########################################

# Extract the solution
number_of_collections <- solution$solution
total_seeds_collected <- sum(seeds_per_species * number_of_collections)
total_cost <- sum(cost_per_species * number_of_collections)

# Print the results
cat("Total seeds collected:", total_seeds_collected, "\n")
cat("Total cost:", total_cost, "\n")
cat("Number of collections for each species:", number_of_collections, "\n")

for (rowi in 1:length(solution$solution)){
  if(solution$solution[rowi] > 0){
    print(rowi)
    print(paste(species_names[rowi], ":", solution$solution[rowi], "collections of",
                seeds_per_species[rowi] * number_of_collections[rowi], "seeds for",
                cost_per_species[rowi] * number_of_collections[rowi], "pounds"))
  }}



