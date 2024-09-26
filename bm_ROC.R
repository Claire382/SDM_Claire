# mod = BIOMOD.models.out object
# mod_methods = vector of algos, e.g. c("GBM", "XGBOOST")
#Function to plot ROC curves with biomod2 from Ed 

require(pROC)

bm_ROC <- function(mod, mod_methods){
  
  obs <- get_formal_data(mod)@data.species
  train <- get_calib_lines(mod)
  cv_runs <- ncol(train)
  
  par(mfrow = c(length(mod_methods), cv_runs), mar = c(12, 5, 5, 5))
  
  for(i in 1:length(mod_methods)){
    for(j in 1:ncol(train)){
      predictions <- get_predictions(mod, model.as.col = T, algo = mod_methods[i])
      predictions <- predictions / 1000
      p_train <- predictions[train[,j],j]
      p_test <- predictions[!train[,j],j]
      r_train <- obs[train[,j]]
      r_test <- obs[!train[,j]]
      
      n <- paste0(mod_methods[i], "_RUN", j)
      pROC::plot.roc(r_train, p_train, col = "blue", main = n, quiet = T)
      pROC::plot.roc(r_test, p_test, col = "red", add = T, quiet = T)   
    }
  }
}