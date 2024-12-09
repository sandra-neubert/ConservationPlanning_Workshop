# Workshop on Conservation Planning (2h)
# Spatial Prioritization - Creating and solving the conservation problem
# Sandra Neubert (s.neubert@uq.edu.au) and Tin Buenafe (k.buenafe@uq.edu.au)

# Run prioritisation

# Preliminaries ------------------------------------------------------

source("02_SpatPrior_Prioritisation.R")

# Extract feature names
col_name <- features %>%
  select(-"cellID") %>%
  st_drop_geometry() %>%
  colnames()

# Create targets object
# Same target for all
targets <- rep(0.3, length(col_name))

# Assign higher target for species with tracking data
targets <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(target = if_else(Category == "Tracking Data", 50 / 100, 5 / 100))


# Create a conservation problem -------------------------------------------

# If you already have a solver in your machine, comment these out and make sure that the solver is loaded (e.g., library(gurobi))

# If you are a Windows user, lpsympony might work better
# if (!require(remotes)) install.packages("remotes")
# remotes::install_bioc("lpsymphony")
# Check their website for more details: https://www.bioconductor.org/packages/release/bioc/html/lpsymphony.html
# library(lpsymphony)

# If you are a Mac/Linux, rcbc might work better
# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("dirkschumacher/rcbc")
# Check their README for more details https://github.com/dirkschumacher/rcbc
library(rcbc)
library(lpsymphony)

dat_problem <- problem(out_sf,
                       features = col_name,
                       cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem ----------------------------------------------

dat_soln <- dat_problem %>%
  solve.ConservationProblem()

# saveRDS(dat_soln, file.path("Output", "Solution1.rds"))


# Plot the solution -------------------------------------------------------

# Plot solution with a function we have defined (i.e., it is not in prioritizr)
# This makes a prettier plot than using the default plot function in prioritizr
(gg_sol <- splnr_plot_Solution(dat_soln))

ggsave(file.path("Figures", "gg_sol.png"),  width = 6, height = 8, dpi = 200)

#look at target coverage
targ_coverage <- eval_target_coverage_summary(dat_problem, dat_soln[, "solution_1"])
