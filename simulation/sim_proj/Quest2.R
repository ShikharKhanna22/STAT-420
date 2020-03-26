data_study2 <- read.csv(file="study_2.csv")

beta_0=0
beta_1=5 
beta_2=-4 
beta_3=1.6
beta_4=-1.1 
beta_5=0.7 
beta_6=0.3
sigma_trials=c(1,2,4)
sig=0
n=500
simulations=100
study2 = data_study2


rmse_model1_trn=matrix(0,3,simulations)
rmse_model1_tst=matrix(0,3,simulations)
rmse_model2_trn=matrix(0,3,simulations)
rmse_model2_tst=matrix(0,3,simulations)
rmse_model3_trn=matrix(0,3,simulations)
rmse_model3_tst=matrix(0,3,simulations)
rmse_model4_trn=matrix(0,3,simulations)
rmse_model4_tst=matrix(0,3,simulations)
rmse_model5_trn=matrix(0,3,simulations)
rmse_model5_tst=matrix(0,3,simulations)
rmse_model6_trn=matrix(0,3,simulations)
rmse_model6_tst=matrix(0,3,simulations)
rmse_model7_trn=matrix(0,3,simulations)
rmse_model7_tst=matrix(0,3,simulations)
rmse_model8_trn=matrix(0,3,simulations)
rmse_model8_tst=matrix(0,3,simulations)
rmse_model9_trn=matrix(0,3,simulations)
rmse_model9_tst=matrix(0,3,simulations)
lowest_RMSE=matrix(0,3,simulations)

cal_rmse = function(model, data){
  resid = data$y - predict(model, newdata=data)
  rmse = sqrt(sum(resid^2)/ length(resid))
  return(rmse)
}


birthday = 19930501
set.seed(birthday)

for (sigma in sigma_trials) {
  sig = sig + 1 
  for (i in 1:simulations){
    
    study2$y = beta_0 + beta_1*study2$x1 + beta_2*study2$x2 + beta_3*study2$x3 + 
      beta_4*study2$x4 + beta_5*study2$x5 + beta_6*study2$x6 + 
      rnorm(n, mean=0, sd=sigma)
    
    trn_index = sample(1:nrow(study2), 250)
    study2_trn = study2[trn_index, ]  # use the index to generate train data
    study2_tst = study2[-trn_index, ] # use the index to generate test data
    
    model1 = lm(y ~ x1, data=study2_trn) # setup the model
    rmse_model1_trn[sig, i] = cal_rmse(model=model1, data=study2_trn) # store train rmse
    rmse_model1_tst[sig, i] = cal_rmse(model=model1, data=study2_tst) # store test rmse 
    
    model2 = lm(y ~ x1 + x2, data=study2_trn) 
    rmse_model2_trn[sig, i] = cal_rmse(model=model2, data=study2_trn) 
    rmse_model2_tst[sig, i] = cal_rmse(model=model2, data=study2_tst) 
    
    model3 = lm(y ~ x1 + x2+ x3, data=study2_trn) 
    rmse_model3_trn[sig, i] = cal_rmse(model=model3, data=study2_trn) 
    rmse_model3_tst[sig, i] = cal_rmse(model=model3, data=study2_tst) 
    
    model4 = lm(y ~ x1 + x2 + x3 + x4, data=study2_trn) 
    rmse_model4_trn[sig, i] = cal_rmse(model=model4, data=study2_trn)
    rmse_model4_tst[sig, i] = cal_rmse(model=model4, data=study2_tst) 
    
    model5 = lm(y ~ x1 + x2 + x3 + x4 + x5, data=study2_trn) 
    rmse_model5_trn[sig, i] = cal_rmse(model=model5, data=study2_trn) 
    rmse_model5_tst[sig, i] = cal_rmse(model=model5, data=study2_tst) 
    
    model6 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data=study2_trn) 
    rmse_model6_trn[sig, i] = cal_rmse(model=model6, data=study2_trn) 
    rmse_model6_tst[sig, i] = cal_rmse(model=model6, data=study2_tst) 
    
    model7 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data=study2_trn) 
    rmse_model7_trn[sig, i] = cal_rmse(model=model7, data=study2_trn)
    rmse_model7_tst[sig, i] = cal_rmse(model=model7, data=study2_tst)
    
    model8 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, data=study2_trn)
    rmse_model8_trn[sig, i] = cal_rmse(model=model8, data=study2_trn) 
    rmse_model8_tst[sig, i] = cal_rmse(model=model8, data=study2_tst) 
    
    model9 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data=study2_trn) 
    rmse_model9_trn[sig, i] = cal_rmse(model=model9, data=study2_trn) 
    rmse_model9_tst[sig, i] = cal_rmse(model=model9, data=study2_tst) 
    
    lowest_RMSE[sig, i] = which.min(c(rmse_model1_tst[sig, i],
                                      rmse_model2_tst[sig, i],
                                      rmse_model3_tst[sig, i],
                                      rmse_model4_tst[sig, i],
                                      rmse_model5_tst[sig, i],
                                      rmse_model6_tst[sig, i],
                                      rmse_model7_tst[sig, i],
                                      rmse_model8_tst[sig, i],
                                      rmse_model9_tst[sig, i]))
  }
  
}


# store the mean train rmse for every model constructed in simulation
avg_rmse_trn_per_sig = cbind(rowMeans(rmse_model1_trn),
                             rowMeans(rmse_model2_trn),
                             rowMeans(rmse_model3_trn),
                             rowMeans(rmse_model4_trn),
                             rowMeans(rmse_model5_trn),
                             rowMeans(rmse_model6_trn),
                             rowMeans(rmse_model7_trn),
                             rowMeans(rmse_model8_trn),
                             rowMeans(rmse_model9_trn)) 
## row.names(avg_rmse_trn_per_sig) = c("sigma_1", "sigma_2", "sigma_4")


# store the mean test rmse for every model constructed in simulation
avg_rmse_tst_per_sig = cbind(rowMeans(rmse_model1_tst),
                             rowMeans(rmse_model2_tst),
                             rowMeans(rmse_model3_tst),
                             rowMeans(rmse_model4_tst),
                             rowMeans(rmse_model5_tst),
                             rowMeans(rmse_model6_tst),
                             rowMeans(rmse_model7_tst),
                             rowMeans(rmse_model8_tst),
                             rowMeans(rmse_model9_tst)) 
# row.names(avg_rmse_tst_per_sig) = c("sigma_1", "sigma_2", "sigma_4")

barplot(table(lowest_RMSE[1,]), main="Model-size selection distribution for Sigma=1", 
        xlab="Model-size", ylab="Model selection frequency", col="grey")
barplot(table(lowest_RMSE[2,]), main="Model-size selection distribution for Sigma=2", 
        xlab="Model-size", ylab="Model selection frequency", col="grey")
barplot(table(lowest_RMSE[3,]), main="Model-size selection distribution for Sigma=4", 
        xlab="Model-size", ylab="Model selection frequency", col="grey")

model_size=1:9 # intiatlize a vector for model size

func_plot = function(RMSE_trn, RMSE_tst, plot_title){
  plot(RMSE_trn ~ model_size,
       type = "l", lwd=2, ylim = c(1.0, 6.0),
       xlab = "Model Size",
       ylab = "RMSE",
       main = plot_title,
       col  = "red")
  points(model_size, RMSE_trn, col="red",  pch=20, cex=1)
  points(model_size, RMSE_tst, col="blue", pch=20, cex=1)
  lines(model_size, RMSE_tst, col="blue", lwd=2)
  legend("topright", c("Train RMSE", "Test RMSE"), lty = c(1, 1), lwd = 2, cex=0.6,
         col = c("red", "blue"))
}

func_plot(RMSE_trn=avg_rmse_trn_per_sig[1,], 
          RMSE_tst=avg_rmse_tst_per_sig[1,],
          plot_title="Model Size vs RMSE for Sigma=1")

func_plot(RMSE_trn=avg_rmse_trn_per_sig[2,], 
          RMSE_tst=avg_rmse_tst_per_sig[2,],
          plot_title="Model Size vs RMSE for Sigma=2")

func_plot(RMSE_trn=avg_rmse_trn_per_sig[3,], 
          RMSE_tst=avg_rmse_tst_per_sig[3,],
          plot_title="Model Size vs RMSE for Sigma=4")









plot(avg_rmse_trn_per_sig[1,] ~ model_size,
     type = "l", lwd=1, ylim = c(1.0, 6.0),
     xlab = "Model Size",
     ylab = "RMSE",
     main = "Model Size vs RMSE for Sigma=1",
     col  = "red")
points(model_size, avg_rmse_trn_per_sig[1,] , col = "red", pch  = 20, cex  = 1)
lines(model_size, avg_rmse_tst_per_sig[1,], col="blue", lwd=1)
points(model_size, avg_rmse_tst_per_sig[1,] , col = "blue", pch  = 20, cex  = 1)
legend("topright", c("Train RMSE", "Test RMSE"), lty = c(1, 1), lwd = 2,
       col = c("red", "blue"))


plot(avg_rmse_trn_per_sig[2,] ~ model_size,
     type = "l", lwd=1, ylim = c(1.0, 6.0),
     xlab = "Model Size",
     ylab = "RMSE",
     main = "Model Size vs RMSE for Sigma=2",
     col  = "red")
points(model_size, avg_rmse_trn_per_sig[2,] , col = "red", pch  = 20, cex  = 1)
lines(model_size, avg_rmse_tst_per_sig[2,], col="blue", lwd=1)
points(model_size, avg_rmse_tst_per_sig[2,] , col = "blue", pch  = 20, cex  = 1)
legend("topright", c("Train RMSE", "Test RMSE"), lty = c(1, 1), lwd = 2,
       col = c("red", "blue"))


plot(avg_rmse_trn_per_sig[3,] ~ model_size,
     type = "l", lwd=1, ylim = c(1.0, 6.0),
     xlab = "Model Size",
     ylab = "RMSE",
     main = "Model Size vs RMSE for Sigma=4",
     col  = "red")
points(model_size, avg_rmse_trn_per_sig[3,] , col = "red", pch  = 20, cex  = 1)
lines(model_size, avg_rmse_tst_per_sig[3,], col="blue", lwd=2)
points(model_size, avg_rmse_tst_per_sig[3,] , col = "blue", pch  = 20, cex  = 1)
legend("topright", c("Train RMSE", "Test RMSE"), lty = c(1, 1), lwd = 2, cex=0.8,
       col = c("red", "blue"))
