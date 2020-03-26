lm_simulations=100



func_NULL_rejects <- function(beta_1, sigma, n){
NUll_rej_cnt=0
x=seq(0, 5, length = n)
for (i in 1:lm_simulations){
  
y = beta_1 * x + rnorm(n, mean = 0, sd = sigma)
sim_data = data.frame(predictor = x, response = y)
sim_fit = lm(response ~ predictor, data = sim_data)

# Did the model reject the Null Hypothesis?
NUll_rej_cnt = NUll_rej_cnt + as.numeric(summary(sim_fit)$coefficients[2, "Pr(>|t|)"] < 0.05)

}
return(NUll_rej_cnt/lm_simulations)
}


# func_NULL_rejects_mat = function(n){
#     beta_1_vec = seq(-2, 2, by = 0.1)
#     sigma_level = c(1,2,4)
#     NUll_rej_mat = matrix(0,length(sigma_level), length(beta_1_vec))
#     
#     for (sig in sigma_level){
#       for (beta_1 in beta_1_vec){
#       NUll_rej_mat[which(sigma_level==sig), which(beta_1_vec==beta_1)] = 
#         func_NULL_rejects(beta_1, sig, n)
#       }
#     }
# colnames(NUll_rej_mat) = beta_1_vec
# rownames(NUll_rej_mat) = sigma_level
# return(NUll_rej_mat)
# }

func_NULL_rejects_mat = function(sig){
  beta_1_vec = seq(-2, 2, by = 0.1)
  n_vec = c(10,20,30)
  NUll_rej_mat = matrix(0,length(n_vec), length(beta_1_vec))
  
  for (n in n_vec){
    for (beta_1 in beta_1_vec){
      NUll_rej_mat[which(n_vec==n), which(beta_1_vec==beta_1)] = 
        func_NULL_rejects(beta_1, sig, n)
    }
  }
  colnames(NUll_rej_mat) = beta_1_vec
  rownames(NUll_rej_mat) = n_vec
  return(NUll_rej_mat)
}

power_sigma_1 = func_NULL_rejects_mat(1)
power_sigma_2 = func_NULL_rejects_mat(2)
power_sigma_4 = func_NULL_rejects_mat(4)


beta_1_vec = seq(-2, 2, by = 0.1)
plot(beta_1_vec,seq(from=0.0,to=1.0, length=length(beta_1_vec)), 
     xlim = range(beta_1_vec), ylab = "Power", type="n",
     xlab = expression(beta), 
     main= "Power Vs Signal Strength")
lines(beta_1_vec,power_sigma_1[1,], type = "b", lty=3, col="green")




plot(power_sigma_1[1,] ~ beta_1_vec,
     type = "b", lwd=1,
     pch  = 20, cex  = 1,
     xlab = expression(beta),
     xlim = range(beta_1_vec),
     ylab = "Power",
     main = "Power Vs beta for Sigma=1",
     col  = "red")
lines(beta_1_vec,power_sigma_1[2,], type = "b", col="green", pch=20, cex=1)
lines(beta_1_vec,power_sigma_1[3,], type = "b", col="blue", pch=20, cex=1)
legend("bottomright",lty=c(1, 1, 1), c("sample-size 10","sample-size 20","sample-size 30"), 
       col=c("red","green","blue"), y.intersp=0.5,x.intersp=0.5, text.width=1, bty = "n")




# points(model_size, avg_rmse_trn_per_sig[1,] , col = "red", pch  = 20, cex  = 1)
# lines(model_size, avg_rmse_tst_per_sig[1,], col="blue", lwd=1)
# points(model_size, avg_rmse_tst_per_sig[1,] , col = "blue", pch  = 20, cex  = 1)
# legend("topright", c("Train RMSE", "Test RMSE"), lty = c(1, 1), lwd = 2,
#        col = c("red", "blue"))




