devtools::load_all()
library(mrgsolve)

model <- mrgsolve::mread_cache("dev/test_model.mod")
pop_test <- makePopulationData(model, 125)
sim_test <- runSim(model, pop_test, amt = 20)
plotSim(sim_test, log_scale = TRUE)
