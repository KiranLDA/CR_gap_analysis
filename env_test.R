##set up for reading in a dataframe with a single column (= genus species), no header
library(shiny)
library(gbm)
library(dismo)
library(doParallel)
library(rWCVP)
library(dplyr)
library(stringr)

x = loadedNamespaces()
unlist(lapply(1:length(x), function(i) {utils::packageVersion(x[i])}))
getRversion(x)
packageDescription(x)$Version



library(reticulate)
conda_create("test-export", packages = x)
conda_export( "test-export", file = "environment.yml")




install.packages('yaml')
test = yaml::as.yaml(x)
y = installed.packages()


#
# # Step 1: Get the list of loaded namespaces
# loaded_packages <- loadedNamespaces()
#
# # Step 2: Get the version of each loaded package
# package_versions <- sapply(loaded_packages, utils::packageVersion, simplify = FALSE)
#
# # Step 3: Convert versions to character format
# package_versions <- lapply(package_versions, as.character)
#
# # Step 4: Save to a YAML file
# if (!requireNamespace("yaml", quietly = TRUE)) {
#   install.packages("yaml")
# }
# library(yaml)
#
# # Combine package names and versions into a named list
# packages_info <- setNames(package_versions, loaded_packages)
#
# # Write to YAML file
# write_yaml(packages_info, "packages_snapshot.yml")
# Load required libraries
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



conda env update -f conda_env_with_r_packages.yml
