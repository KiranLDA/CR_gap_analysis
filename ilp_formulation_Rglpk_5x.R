# Load the Rglpk package
library(Rglpk)

# Example data (replace with your actual data)
set.seed(123)
n_species <- 400
seeds_per_species <- runif(n_species, 500, 10000)
storage_behaviour <- runif(n_species, 0, 1)
accessions_per_species <- sample(0:10, n_species, replace = TRUE)
max_accessions <- sample(0:5, n_species, replace = TRUE)
cost_per_species <- runif(n_species, 2000, 5000)
budget <- 100000

# Define the benefit function
benefit <- seeds_per_species * storage_behaviour * max_accessions

# Set up the linear programming problem

# Objective function: maximize benefit
objective <- benefit

# Constraints matrix
# - The first set of constraints will be the budget constraint
# - The next set of constraints will ensure that no species is collected more than max_accessions times

# Create a matrix to store the constraints
constraints <- matrix(0, nrow = n_species + 1, ncol = n_species)

# Budget constraint
constraints[1, ] <- cost_per_species

# Max accessions constraints
for (i in 1:n_species) {
  constraints[i + 1, i] <- 1
}

# Right-hand side of the constraints
rhs <- c(budget, max_accessions)

# Constraint directions
dir <- c("<=", rep("<=", n_species))

# Solve the LP problem
result <- Rglpk_solve_LP(obj = objective, mat = constraints, dir = dir, rhs = rhs, types = "I", max = TRUE)

# Output the results
print(result)

# The optimal number of collections for each species
optimal_collections <- result$solution

# Print the optimal number of collections
print(optimal_collections)

# Print the total benefit
total_benefit <- sum(optimal_collections * benefit)
print(total_benefit)
