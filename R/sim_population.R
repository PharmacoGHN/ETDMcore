#' @title Generate population data from mrgsolve model with IIV
#'
#' @description decompose model to create a table of n indiviuals with all the necesary parameters to simulate a population profile
#'
#' @param model mrgsolve model object
#' @param n number of individuals to simulate
#' @param seed logical, if TRUE set a seed for reproducibility
#'
#' @return data.frame with n rows and columns for id and all parameters that are generated with IIV where applicable
#'
#' @examples
#' \dontrun{
#' library(mrgsolve)
#' model <- mread("path_to_your_model.mod")
#' pop_data <- make_population_data(model, n = 500)
#' head(pop_data)
#' }
#'
#' @importFrom stringr str_detect
#' @importFrom stats rlnorm
#' @importFrom mrgsolve mread
#' @author Romain Garreau
#'
#' @export
makePopulationData <- function(model, n = 1000, seed = TRUE) {
  # create seed for reproducibility
  if (seed) set.seed(868487)

  # extract param and param names
  param <- as.data.frame(model$param)
  names(param) <- names(model)$param

  # extract IIV parameters
  names(model)$omega
  omega_labels <- names(model)$omega_labels[[1]]
  omega_labels_no_eta <- gsub("ETA", "", omega_labels)

  # check which parameter names contain any of the omega labels (without "ETA") and return the indexes
  param_contains_eta <- stringr::str_detect(names(param), paste(omega_labels_no_eta, collapse = "|"))
  param_with_eta <- which(param_contains_eta)

  # create omega matrix
  omega <- model$omega[[1]]

  # create population data
  pop_data <- data.frame(id = 1:n)


  for (i in 1:ncol(param)) {
    param_name <- names(param)[i]
    typical_value <- param[1, i]

    # check if this parameter has IIV
    # return indices instead:
    if (i %in% param_with_eta) {
      # get corresponding eta parameter
      eta_param_name <- paste0("ETA", gsub("TV", "", param_name))
      eta_param <- omega[eta_param_name, eta_param_name]
      # generate random values for the parameter
      pop_data[[gsub("TV", "", param_name)]] <- round(rlnorm(n, meanlog = log(typical_value), sdlog = eta_param), 3)
    } else {
      pop_data[[gsub("TV", "", param_name)]] <- round(typical_value, 3)
    }
  }

  return(pop_data)
}

#' @title Simulate population data using mrgsolve model
#'
#' @description Simulate population data using a mrgsolve model and individual parameter data
#' @param model mrgsolve model object
#' @param population_data data.frame with individual parameters (output of makePopulationData)
#' @param amt numeric, amount to administer to each individual
#' @param plot logical, if TRUE return a plot of the simulation
#' @return mrgsolve simulation object or plot if plot = TRUE
#' @examples
#' \dontrun{
#' library(mrgsolve)
#' model <- mread("path_to_your_model.mod")
#' pop_data <- make_population_data(model, n = 500)
#' sim_data <- runSim(model, pop_data, amt = 1000, plot = TRUE)
#' }
#' @importFrom mrgsolve idata_set ev mrgsim
#' @importFrom graphics plot
#' @author Romain Garreau
#' @export
runSim <- function(model, population_data, amt, plot = FALSE) {
  model_sim <- model |>
    mrgsolve::idata_set(population_data) |>
    mrgsolve::ev(amt = amt) |>
    mrgsolve::mrgsim()

  return(model_sim)
}

#' @title Plot simulation results
#' @description Plot simulation results from mrgsolve simulation object
#' @param sim_data mrgsolve simulation object
#' @param unit character, unit of concentration to display on y-axis
#' @param log_scale logical or character, if TRUE use log10 scale for y-axis, if "pseudo" use pseudo-log scale
#' @param plotly logical, if TRUE return a plotly object instead of ggplot2
#' @param concentration data.frame, optional dataframe with columns 'time' and 'concentration' for observed data
#' @return plot of simulation results (ggplot2 or plotly object)
#' @examples
#' \dontrun{
#' library(mrgsolve)
#' model <- mread("path_to_your_model.mod")
#' pop_data <- make_population_data(model, n = 500)
#' sim_data <- runSim(model, pop_data, amt = 1000)
#' plotSim(sim_data)
#' # With observed data
#' obs_data <- data.frame(time = c(1, 2, 4, 8), concentration = c(10, 8, 5, 2))
#' plotSim(sim_data, concentration = obs_data, plotly = TRUE)
#' }
#' @import ggplot2
#' @importFrom scales pseudo_log_trans
#' @author Romain Garreau
#' @export
#'
#'

plotSim <- function(sim_data, unit = "mg/L", log_scale = FALSE, plotly = FALSE, concentration = NULL) {
  # get 95% prediction interval
  sim_data <- sim_data |>
    dplyr::group_by(time) |>
    dplyr::summarise(
      q2.5 = stats::quantile(CENT, 0.025, na.rm = TRUE),
      q97.5 = stats::quantile(CENT, 0.975, na.rm = TRUE),
      median = stats::median(CENT, na.rm = TRUE)
    )


  # plot 95 range of prediction interval
  plot <- ggplot2::ggplot(sim_data, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q2.5, ymax = q97.5),
      fill = "lightblue",
      alpha = 0.5
    ) +
    ggplot2::geom_line(ggplot2::aes(y = median), color = "blue", size = 1) +
    ggplot2::labs(
      title = "Population Simulation",
      x = "Time",
      y = paste0("Concentration (", unit, ")")
    ) +
    ggplot2::theme_bw()

  # add observed concentration data if provided
  if (!is.null(concentration)) {
    # validate that concentration has required columns
    if (!all(c("time", "concentration") %in% names(concentration))) {
      stop("concentration dataframe must have 'time' and 'concentration' columns")
    }
    plot <- plot + ggplot2::geom_point(
      data = concentration,
      ggplot2::aes(x = time, y = concentration),
      color = "red",
      size = 3,
      shape = 16
    )
  }

  # set log axis if specified
  if (log_scale == "pseudo") {
    plot <- plot + ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(base = 10))
  }
  if (log_scale == TRUE) {
    plot <- plot + ggplot2::scale_y_log10()
  }

  # convert to plotly if requested
  if (plotly) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package 'plotly' is required for plotly output. Please install it.")
    }
    plot <- plotly::ggplotly(plot)
  }

  return(plot)
}
