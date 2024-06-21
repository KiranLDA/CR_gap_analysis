# Install and load the Rglpk package
# install.packages("Rglpk")
library(Rglpk)



# Read in the data
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/MSC_projects/2023-2024/Jessica_Neil-Boss_seed_CE/"
data = read.csv(paste0(basepath, "Available data.csv"))

# Budget in pounds
budget <- 1000000


# keep data without NAs
index = which(!(is.na(data$Predicted_ave_seed )) & !(is.na(data$Storage_cost_3_years_per_accession))) # keep only rows where data is complete

# Number of plant species
num_species <- length(data$WCVP.accepted.name[index])

# Species names
species_names = data$WCVP.accepted.name[index]


# Randomly generate the number of seeds produced by each species (between 100 and 10000)
seeds_per_species <- as.numeric(data$Predicted_ave_seed[index])

# Randomly generate the cost to collect seeds from each species (between £2000 and £4000)
cost_per_species <- round(as.numeric(gsub(",", "",data$Banking_cost_per_accession[index])))

# Randomly generate the maximum number of seeds that need to be collected for each species (between 5000 and 10000)
max_seeds_per_species <- 10000 - data$Num_accessions[index]

# Objective function coefficients (maximize the total number of seeds collected)
objective <- seeds_per_species

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
  new_constraint[i] <- seeds_per_species[i]
  constraint_matrix <- rbind(constraint_matrix, new_constraint)
  rhs <- c(rhs, max_seeds_per_species[i])
  direction <- c(direction, "<=")
}

# Solve the integer linear programming problem
solution <- Rglpk_solve_LP(obj = objective,
                           mat = constraint_matrix,
                           dir = direction,
                           rhs = rhs,
                           types = rep("I", num_species),
                           max = TRUE)

# Extract the solution
number_of_collections <- solution$solution
total_seeds_collected <- sum(seeds_per_species * number_of_collections)
total_cost <- sum(cost_per_species * number_of_collections)

# Print the results
cat("Total seeds collected:", total_seeds_collected, "\n")
cat("Total cost:", total_cost, "\n")
cat("Number of collections for each species:", number_of_collections, "\n")

