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
make_population_data <- function(model, n = 1000, seed = TRUE) {
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
      pop_data[[gsub("TV", "", param_name)]] <- rlnorm(n, meanlog = log(typical_value), sdlog = eta_param)
    } else {
      pop_data[[gsub("TV", "", param_name)]] <- typical_value
    }
  }

  return(pop_data)
}
