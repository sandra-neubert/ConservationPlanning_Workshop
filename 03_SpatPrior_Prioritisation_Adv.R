# Workshop on Conservation Planning (2h)
# Spatial Prioritization - Creating and solving the conservation problem: Advanced
# Sandra Neubert (s.neubert@uq.edu.au) and Tin Buenafe (k.buenafe@uq.edu.au)

# Run prioritisation


# Preliminaries ------------------------------------------------------

source("01_SpatPrior_PrepData.R")
source("utils-functions.R")

# Add penalties
datEx_problem_P <- problem(out_sf,
                           features = col_name,
                           cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_boundary_penalties(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0.5, verbose = FALSE) # Larger optimality gap to speed up code

# Solve problem
datEx_soln_P <- datEx_problem_P %>%
  solve.ConservationProblem()

(gg_solnP <- splnr_plot_Solution(datEx_soln_P) +
    geom_sf(data = landmass, 
            colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Add locked-in areas
# load current MPAs

mpas <- readRDS(file.path(inputDat, "MPAs", "mpas.rds"))

# First look at the data
(gg_mpas <- ggplot() +
    geom_sf(data = mpas, aes(fill = .data$mpas), 
            colour = NA, size = 0.001, show.legend = FALSE))

# Set-up problem
datEx_problem_LIC <- problem(out_sf,
                             features = col_name,
                             cost_column = "cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_locked_in_constraints(as.logical(mpas$mpas)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_LIC <- datEx_problem_LIC %>%
  solve.ConservationProblem()

(gg_solnLIC <- splnr_plot_Solution(datEx_soln_LIC) +
    geom_sf(data = landmass, 
            colour = "black", fill = "black", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Adding Zones
# Create Targets
targetsZones <- data.frame(feature = col_name) %>%
  mutate(Category = c(rep("Geomorphic", 12), rep("Tracking Data", 5))) %>%
  mutate(
    targetZ1 = dplyr::if_else(Category == "Tracking Data", 30 / 100, 0),
    targetZ2 = dplyr::if_else(Category != "Tracking Data", 10 / 100, 0)
  ) %>%
  dplyr::select("targetZ1", "targetZ2") %>%
  as.matrix()

# Create zones object
z1 <- zones("zone 1" = col_name, "zone 2" = col_name)

# Set up conservation problem
# NOTE: when using sf input, we need as many cost columns as we have zones
datEx_problem_zones <- prioritizr::problem(
  out_sf %>%
    mutate(
      Cost1 = rep(1, nrow(.)),
      Cost2 = rep(1, nrow(.))
    ),
  z1,
  cost_column = c("Cost1", "Cost2")
) %>%
  add_min_set_objective() %>%
  add_relative_targets(targetsZones) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Solve conservation problem
datEx_soln_zones <- datEx_problem_zones %>%
  solve.ConservationProblem()

(gg_soln_zones <- splnr_plot_Solution(
  datEx_soln_zones,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "#003366"),
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
) +
    geom_sf(data = landmass, 
            colour = "black", fill = "lightgrey", show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PUs)$xlim, ylim = st_bbox(PUs)$ylim))

# Feature representation
targetsMet_zones <- datEx_soln_zones %>%
  select(tidyselect::starts_with(c("solution"))) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  eval_feature_representation_summary(datEx_problem_zones, .)