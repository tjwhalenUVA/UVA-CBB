source('models/packages.R')
source('models/data_prep.R')

#Models ---------------------------------------------------------------------
#Chalk Model
"
Get a result from only choosing the higher seed.
Ties are decided by end of season ranking (i.e. AP or Coaches Poll)
"
compare <-
    tourney_games %>% 
    mutate(T_1_Win = as.numeric(as.character(T_1_Win)),
           Prediction = ifelse(as.integer(T_1_Seed) <= as.integer(T_2_Seed), 1, 0),
           Correct = ifelse(T_1_Win == Prediction, 1, 0)) %>% 
    select(T_1_Win, T_1_Seed, T_2_Seed, Prediction, Correct)

table(compare$T_1_Win, compare$Prediction)
mean(compare$Correct)


#Setting up H2o Cluster ----------------------------------------------------------------------
localH2O <- h2o.init(nthreads = -1)
#data to h2o cluster
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
#Variables
y.dep <- 'T_1_Win'
x.indep <- names(train)[-1]

#RF ----------------------------------------------------------------------
# rf_baseline <- h2o.randomForest(x = x.indep, y = y.dep,
#                                 model_id = "rf_baseline",
#                                 training_frame = train.h2o,
#                                 nfolds = 5,
#                                 seed = 1234,
#                                 verbose = T)

# save the model
# rf_baseline_path <- h2o.saveModel(object=rf_baseline, 
#                                   path="./models", 
#                                   force=TRUE)
# load the model
rf_baseline <- h2o.loadModel("./models/rf_baseline")

h2o.performance(rf_baseline)
h2o.varimp(rf_baseline)
h2o.varimp_plot(rf_baseline)
predict.rf <- as.data.frame(h2o.predict(rf_baseline, test.h2o))
caret::confusionMatrix(predict.rf$predict, test$T_1_Win)

#RF Grid Search--------------------------------------------------------------
hyper_params_grid <- list(ntrees = c(50, 100, 150), 
                          max_depth = c(2, 3, 5),
                          min_rows = c(1, 2))

expand.grid(hyper_params_grid)

rf_grid <- h2o.grid("randomForest", 
                    x = x.indep, 
                    y = y.dep,
                    training_frame = train.h2o,
                    grid_id = 'rf_grid',
                    nfolds = 3,
                    hyper_params = hyper_params_grid,
                    parallelism = 0)

rf_grid_path <- h2o.saveGrid(grid_directory = "./models/grid_models", 
                             grid_id = 'rf_grid')
# Remove everything from the cluster or restart it
h2o.removeAll()
rf_grid <- h2o.loadGrid(rf_grid_path)



models <- h2o.getGrid(grid_id = "rf_grid", sort_by = "accuracy", 
                      decreasing = TRUE)



best_rf_acc <- h2o.getModel(models@model_ids[[1]])

h2o.performance(model = best_rf_acc, newdata = test.h2o)


rf_grid_predict <- as.data.frame(h2o.predict(best_rf_acc, test.h2o))
caret::confusionMatrix(rf_grid_predict$predict, test$T_1_Win)


#Shutdown h2o Instance-------------------------------------------------------
h2o.shutdown()
#DL
# dlearning.model <- h2o.deeplearning(y = y.dep, x = x.indep, training_frame = train.h2o,
#                                     epoch = 60,
#                                     hidden = c(100,100),
#                                     activation = "Rectifier",
#                                     seed = 1122)
# 
# h2o.performance(dlearning.model)
# predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
# mean(predict.dl2$predict == test$T_1_Win)
