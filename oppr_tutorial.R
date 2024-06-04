# Here we will provide a short tutorial showing how the oppr R package can be used
# to prioritize funding for conservation projects. This package is a general purpose
# project prioritization decision support tool. It can generate solutions for
# species-based project prioritization problems (Joseph et al. 2009; Bennett et al. 2014)
# and priority threat management problems (Carwardine et al. 2018).
# To develop a project prioritization, this package requires data for
# (i) conservation projects, (ii) management actions data, and (iii) biodiversity features.

# Briefly, biodiversity features are the biological entities that we wish would persist
# into the future (e.g. threatened populations, native species, eco-systems).
# These biodiversity features can (and ideally should) include non-threatened species,
# but should not include threatening processes that we wish to eradicate (e.g. populations of invasive species).

# Management actions are acts that can be undertaken to enhance biodiversity
# (e.g. planting native species, reintroducing native species, trapping pest species).
# Each action should pertain to a specific location (e.g. a national park)
# or area (e.g. an entire country), and should be associated with a cost estimate.

# To guide the prioritization, the management actions are grouped into
# conservation projects (also termed “strategies”). Typically, management
# actions are grouped into conservation projects based on
# spatial (e.g. management actions that pertain to the same area),
# taxonomic (e.g. management actions that pertain to the same pest species or threatened species), or
# thematic criteria (e.g. management actions that pertain to pest eradication are grouped into a
# “pest project”, and actions that pertain to habitat restoration are grouped
# into a “habitat project”).

# Additionally, some conservation projects can be combinations of other projects
# (e.g. a “pest and habitat project”). Each conservation project should be associated
# with (i) a probability of succeeding if it is implemented (also termed “feasibility”),
# (ii) information about which management actions are associated with it, and
# (iii) an estimate of how likely each conservation feature affected by the project
# is to persist into the future if the project is implemented (often derived using expert elicitation).

# The conservation projects should also include a “baseline project” that represents
# a “do nothing scenario” which has a 100% chance of succeeding and is associated with
# an action that costs nothing to implement. This is important because we can’t find a
# cost-effective solution if we don’t know how much each project improves a species’
# chance at persistence. For more background information on project prioritization,
# please refer to Carwardine et al. (2018).

# To start off, we will initialize the random number generator to ensure reproducibility.
# Next, we will load the oppr R package. And then we will load the ggplot2, tibble, and tidyr R
# packages to help with data for visualizations and wrangling.



# set random number generated seed
set.seed(1000)
# load oppr package for project prioritization
library(oppr)
# load ggplot2 package for plotting
library(ggplot2)
# load tibble package for working with tabular data
library(tibble)
# load tidyr package for reshaping tabular data
library(tidyr)

# simulate data
sim_data <- simulate_ptm_data(number_projects = 70, number_actions = 30,
                              number_features = 40)

# extract project, action, feature data
projects <- sim_data$projects
actions <- sim_data$actions
features <- sim_data$features

# manually set feature weights for teaching purposes
features$weight <- exp(runif(nrow(features), 1, 15))

# print data
print(projects) # data for conservation projects

print(actions)  # data for management actions

print(features) # data for biodiversity features

# build problem
p1 <- problem(projects = projects, actions = actions, features = features,
              "name", "success", "name", "cost", "name") %>%
  add_max_richness_objective(budget = 1000) %>%
  add_binary_decisions()

# print problem
print(p1)

# solve problem
s1 <- solve(p1)

# print solution
print(s1)

# extract names of funded projects
s1_projects <- s1[, c("solution", p1$project_names())] %>%
  gather(project, status, -solution)

# print project results
print(s1_projects)


# print names of completely funded projects
s1_projects$project[s1_projects$status > 0]

# calculate cost-effectiveness of each project
p1_pce <- project_cost_effectiveness(p1)

# print output
print(p1_pce)

# print the ranks of the priority projects
p1_pce$rank[s1_projects$status > 0]

# print folder where the file will be saved
cat(getwd(), "\n")

# save table to file
write.table(s1, "solutions.csv" , sep = ",", row.names = FALSE)
write.table(s1_projects, "project_statuses.csv" , sep = ",", row.names = FALSE)
write.table(p1_pce, "project_ce.csv" , sep = ",", row.names = FALSE)

# plot solution
plot(p1, s1)

# print features table
print(features)


# plot histogram of feature weights
ggplot(data = features, aes(weight)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels = scales::comma) +
  xlab("Feature weight") +
  ylab("Frequency")

# build on existing problem and add feature weights
p2 <- p1 %>%
  add_feature_weights("weight")

# print problem
print(p2)

# solve problem
s2 <- solve(p2)

# print solution
print(s2)

# plot solution
plot(p2, s2)

# print actions prioritized for funding in first solution
actions$name[which(as.logical(as.matrix(s1[, action_names(p1)])))]


# print actions prioritized for funding in second solution
actions$name[which(as.logical(as.matrix(s2[, action_names(p2)])))]


# calculate number of actions funded in both solutions
sum((as.matrix(s1[, action_names(p1)]) == 1) &
      (as.matrix(s2[, action_names(p2)]) == 1))

# create new problem, with Gurobi solver added and request multiple solutions
# note that we set the gap to 0.5 because we are not necessarily interested
# in the top 1,000 solutions (for more information on why read the Gurobi
# documentation on solution pools)
p3 <- p2 %>%
  add_gurobi_solver(gap = 0.5, number_solution = 1000)

# solve problem
s3 <- solve(p3)

# print solution
print(s3)

# plot histogram of objective values, and add red dashed line to indicate the
# objective value for the optimal solution
ggplot(data = s3, aes(obj)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = s2$obj, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::comma) +
  xlab("Expected richness (objective function)") +
  ylab("Frequency") +
  theme(plot.margin = unit(c(5.5, 10.5, 5.5, 5.5), "pt"))

# print the solution object to remind ourselves what it looks like
print(s3)


# calculate percentage of times each action was selected for
# funding in the solutions
actions$sel_freq <- apply(as.matrix(s3[, actions$name]), 2, mean) * 100

# print the actions table with the new column
print(actions)


# print top 10 most important actions based on selection frequency
head(actions[order(actions$sel_freq, decreasing = TRUE), ], n = 10)


# plot histogram showing solution frequency
ggplot(data = actions, aes(sel_freq)) +
  geom_histogram(bins = 30) +
  coord_cartesian(xlim = c(0, 100)) +
  xlab("Selection frequency (%)") +
  ylab("Frequency")

# print p2 to remind ourselves about the problem
print(p2)


# print s2 to remind ourselves about the solution
print(s2)


# calculate replacement costs for each priority action selected in s2
r2 <- replacement_costs(p2, s2)

# print output
print(r2)

# add replacement costs to action table
actions$rep_cost <- r2$rep_cost

# print actions, ordered by replacement cost
print(actions[order(actions$rep_cost, decreasing = TRUE), ])


# test correlation between selection frequencies and replacement costs
cor.test(x = actions$sel_freq, y = actions$rep_cost, method = "pearson")


# plot histogram of replacement costs,
ggplot(data = actions, aes(rep_cost)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  xlab("Replacement cost") +
  ylab("Frequency")

# build problem
p4 <- problem(projects = projects, actions = actions, features = features,
              "name", "success", "name", "cost", "name") %>%
  add_max_targets_met_objective(budget = 1000) %>%
  add_absolute_targets(0.7) %>%
  add_binary_decisions()

# print problem
print(p4)

# solve problem
s4 <- solve(p4)

# print solution
print(s4)


# plot solution
plot(p4, s4)

# specify budgets, ranging between zero and the total cost of all the budgets,
# with the total number of different budgets equaling 50
# (note that we would use a higher number for publications)
budgets <- seq(0, sum(actions$cost), length.out = 50)

# specify targets
targets <- c(0.7, 0.85)

# run prioritizations and compile results
comp_data <- lapply(targets, function(i) {
  o <- lapply(budgets, function(b) {
    problem(projects = projects, actions = actions, features = features,
            "name", "success", "name", "cost", "name") %>%
      add_max_targets_met_objective(budget = b) %>%
      add_absolute_targets(i) %>%
      add_binary_decisions() %>%
      add_default_solver(verbose = FALSE) %>%
      solve()
  })
  o <- as_tibble(do.call(rbind, o))
  o$budget <- budgets
  o$target <- paste0(i * 100, "%")
  o
})
comp_data <- as_tibble(do.call(rbind, comp_data))

# plot the relationship between the number of features that meet the target
# in a solution and the cost of a solution
ggplot(comp_data, aes(x = cost, y = obj, color = target)) +
  geom_step() +
  xlab("Solution cost ($)") +
  ylab("Number of features with targets") +
  labs(color = "Target")

# build problem
p5 <- problem(projects = projects, actions = actions, features = features,
              "name", "success", "name", "cost", "name") %>%
  add_min_set_objective() %>%
  add_absolute_targets(0.99) %>%
  add_binary_decisions()

# print problem
print(p5)

# attempt to solve problem, but this will throw an error
s5 <- solve(p5)

# build problem
p6 <- problem(projects = projects, actions = actions, features = features,
              "name", "success", "name", "cost", "name") %>%
  add_min_set_objective() %>%
  add_absolute_targets(0.60) %>%
  add_binary_decisions()

# print problem
print(p6)

# solve problem
s6 <- solve(p6)

# print solution
print(s6)

# plot solution
plot(p6, s6)

# set budgets for which to create multiple solutions
budgets <- seq(0, sum(actions$cost), length.out = 100)

# generate solutions using heuristic algorithms
s7 <- lapply(budgets, function(b) {
  problem(projects = projects, actions = actions, features = features,
          "name", "success", "name", "cost", "name") %>%
    add_max_richness_objective(budget = b) %>%
    add_feature_weights("weight") %>%
    add_binary_decisions() %>%
    add_heuristic_solver(verbose = FALSE) %>%
    solve()
})
s7 <- as_tibble(do.call(rbind, s7))
s7$budget <- budgets

# print solutions
print(s7)

# generate random solutions under the various budgets and store
# the objective value of the best and worst solutions
s8 <- lapply(budgets, function(b) {
  o <- problem(projects = projects, actions = actions, features = features,
               "name", "success", "name", "cost", "name") %>%
    add_max_richness_objective(budget = b) %>%
    add_feature_weights("weight") %>%
    add_binary_decisions() %>%
    add_random_solver(verbose = FALSE, number_solutions = 100) %>%
    solve()
  data.frame(budget = b, min_obj = min(o$obj), max_obj = max(o$obj))
})
s8 <- as_tibble(do.call(rbind, s8))

# print solutions
print(s8)

# make plot
ggplot() +
  geom_ribbon(aes(x = budget, ymin = min_obj, ymax = max_obj), data = s8,
              color = "#3366FF26", fill = "#3366FF26") +
  geom_step(aes(x = budget, y = obj), data = s7, color = "orange") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Budget available ($)") +
  ylab("Expected richness (objective function)") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1))

# generate solutions
s9 <- lapply(budgets, function(b) {
  problem(projects = projects, actions = actions, features = features,
          "name", "success", "name", "cost", "name") %>%
    add_max_richness_objective(budget = b) %>%
    add_feature_weights("weight") %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE) %>%
    solve()
})
s9 <- as_tibble(do.call(rbind, s9))
s9$budget <- budgets

# print solutions
print(s9)

# make plot
ggplot() +
  geom_ribbon(aes(x = budget, ymin = min_obj, ymax = max_obj), data = s8,
              color = "#3366FF26", fill = "#3366FF26") +
  geom_step(aes(x = budget, y = obj), data = s7, color = "orange") +
  geom_step(aes(x = budget, y = obj), data = s9, color = "red") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Budget available ($)") +
  ylab("Expected richness (objective function)") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 1))
