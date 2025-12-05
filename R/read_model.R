
#' read_model
#' 
#' @export
read_model <- function(model, cache = FALSE) {

  # call function based on cache parameters
  fct <- switch(cache,
    "mread",
    "mread_cached"
  )

  compile_model <- fct(model)

  return(compile_model)
}
