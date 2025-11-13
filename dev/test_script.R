library(mrgsolve)
model <- mread("dev/test_model.mod")

# exploration (not in function)
names(model)

# extract param and param names
param <- as.data.frame(model$param)
names(param) <- names(model)$param # names(param) = c("TVCL", "TVV1", "TVQ2", "TVV2", "Q3", "V3", "KA", "WT" )

# extract IIV parameters
names(model)$omega
omega_labels <- names(model)$omega_labels[[1]] # return c("ETACL", "ETAV1")
omega_labels_no_eta <- gsub("ETA", "", omega_labels) # return c("CL", "V1")

# check which parameter names contain any of the omega labels (without "ETA")
param_contains_eta <- stringr::str_detect(names(param), paste(omega_labels_no_eta, collapse = "|"))
param_contains_eta

# return indices instead:
param_with_eta <- which(param_contains_eta)

omega <- model$omega[[1]]


# create data

pop_data <- data.frame(id = 1:100)

for (i in 1:ncol(param)) {
  param_name <- names(param)[i]
  typical_value <- param[1, i]

  # check if this parameter has IIV
  # return indices instead:
  param_with_eta <- which(param_contains_eta)
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
