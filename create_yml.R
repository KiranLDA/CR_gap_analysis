if (!requireNamespace("yaml", quietly = TRUE)) {
  install.packages("yaml")
}
library(yaml)

# Read the YAML file
packages_info <- read_yaml("packages_snapshot.yml")

# Prepare the dependencies list for Conda YAML
conda_dependencies <- lapply(names(packages_info), function(pkg) {
  version <- packages_info[[pkg]]
  paste("r-", pkg, "=", version, sep="")
})

# Combine the dependencies into a character vector
conda_dependencies <- unlist(conda_dependencies)

# Create a list representing the environment YAML structure
conda_env <- list(
  name = "your_env_name",
  channels = c("defaults", "conda-forge", "bioconda"),
  dependencies = c(
    "r-base",
    conda_dependencies
  )
)

# Write to a YAML file
write_yaml(conda_env, "conda_env_with_r_packages.yml")

