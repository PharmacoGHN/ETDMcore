devtools::load_all()
library(mrgsolve)

model <- mrgsolve::mread("dev/test_model.mod")
pop_test <- makePopulationData(model, 10)
sim_test <- runSim(model, pop_test, amt = 250)
plotSim(sim_test, log_scale = T)
