devtools::load_all()
library(mrgsolve)

# create concentration data
conc <- data.frame(
  time = c(0.5, 1, 2, 4, 8, 12, 24),
  concentration = c(15.3, 25.1, 30.2, 22.5, 12.4, 7.8, 1.2)
)

model <- mrgsolve::mread_cache("dev/test_model.mod")
pop_test <- makePopulationData(model, 125)
sim_test <- runSim(model, pop_test, amt = 20, ii = 24, until = 120)
plotSim(sim_test, log_scale = TRUE, plotly = TRUE, concentration = conc)
