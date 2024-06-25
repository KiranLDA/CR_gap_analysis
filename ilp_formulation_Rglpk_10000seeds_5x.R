# Install and load the Rglpk package if not already installed
if (!require("Rglpk")) {
  install.packages("Rglpk")
  library(Rglpk)
}

# Given data
set.seed(42)
num_species <- 400
budget <- 100000#00

# Generate random data for seeds produced and collection costs
seeds_produced <- sample(100:10000, num_species, replace = TRUE)
collection_costs <- sample(2000:4000, num_species, replace = TRUE)
min_collections <- sample(3:5, num_species, replace = TRUE)
max_seeds <- sample(5000:10000, num_species, replace = TRUE)

# Decision variable count
total_vars <- sum(min_collections)

# Objective function coefficients (maximize total seeds collected)
objective <- rep(seeds_produced, min_collections)

# Constraint matrix and rhs
constraints <- matrix(0, nrow = num_species * 2 + 1, ncol = total_vars)
rhs <- numeric(num_species * 2 + 1)
direction <- rep("<=", num_species * 2 + 1)

# Populate constraints for max seeds per species and budget constraint
index <- 1
for (i in 1:num_species) {
  constraints[i, index:(index + min_collections[i] - 1)] <- seeds_produced[i]
  constraints[num_species + i, index:(index + min_collections[i] - 1)] <- 1
  constraints[num_species * 2 + 1, index:(index + min_collections[i] - 1)] <- collection_costs[i]
  rhs[i] <- max_seeds[i]
  rhs[num_species + i] <- min_collections[i]
  index <- index + min_collections[i]
}
rhs[num_species * 2 + 1] <- budget

# Set the bounds for the variables (all integers)
bounds <- list(lower = list(ind = 1:total_vars, val = rep(0, total_vars)),
               upper = list(ind = 1:total_vars, val = rep(Inf, total_vars)))

# Solve the LP problem using Rglpk
result <- Rglpk_solve_LP(obj = objective, mat = constraints, dir = direction, rhs = rhs,
                         types = rep("I", total_vars), bounds = bounds, max = TRUE)

# Output the results
if (result$status == 0) {
  print("Optimal solution found:")
  total_seeds_collected <- sum(result$solution * objective)
  total_cost <- sum(result$solution * collection_costs[rep(1:num_species, min_collections)])
  print(paste("Total seeds collected:", total_seeds_collected))
  print(paste("Total cost:", total_cost))

  index <- 1
  for (i in 1:num_species) {
    if (sum(result$solution[index:(index + min_collections[i] - 1)]) > 0) {
      for (j in 1:min_collections[i]) {
        if (result$solution[index + j - 1] > 0) {
          print(paste("Species", i, "Collection", j, "Times collected:", result$solution[index + j - 1]))
        }
      }
    }
    index <- index + min_collections[i]
  }
} else {
  print("No optimal solution found.")
}

