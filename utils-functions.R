# Helper functions adapted from spatialplanr R package (mathmarecol.github.io/spatialplanr/)
# 14/10/2024
# Sandra Neubert (s.neubert@uq.edu.au)

splnr_plot_Solution <- function(soln, colorVals = c("#c6dbef", "#3182bd"),
                                showLegend = TRUE, legendLabels = c("Not selected", "Selected"),
                                plotTitle = "Solution", legendTitle = "Planning Units",
                                zones = FALSE) {
  if (zones == FALSE) {
    soln <- soln %>%
      dplyr::select("solution_1") %>%
      dplyr::mutate(solution = as.factor(.data$solution_1))
    nrows <- 2
  } else if (zones == TRUE) {
    oldName <- soln %>%
      dplyr::select(tidyselect::starts_with(c("solution"))) %>%
      sf::st_drop_geometry() %>%
      tibble::as_tibble() %>%
      names()
    
    newName <- gsub("1_zone", "", oldName) # to make data a bit nicer to work with
    nrows <- (length(newName) + 1)
    
    solnNewNames <- soln %>%
      dplyr::rename_at(dplyr::vars(tidyselect::all_of(oldName)), ~newName) %>%
      dplyr::select(tidyselect::starts_with(c("solution")))
    
    for (i in 2:(length(newName))) {
      solnNewNames <- solnNewNames %>%
        dplyr::mutate(
          !!rlang::sym(newName[i]) := dplyr::case_when(
            !!rlang::sym(newName[i]) == 1 ~ i,
            !!rlang::sym(newName[i]) == 0 ~ 0
          )
        )
    }
    
    soln <- solnNewNames %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        solution = sum(dplyr::c_across(cols = tidyselect::starts_with("solution_"))),
        solution = factor(.data$solution, levels = 0:(length(newName)))
      )
  }
  
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution), colour = NA, size = 0.1, show.legend = showLegend) +
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    ggplot2::scale_fill_manual(
      name = legendTitle,
      values = colorVals,
      labels = legendLabels,
      aesthetics = c("colour", "fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = nrows,
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::labs(subtitle = plotTitle)
}
