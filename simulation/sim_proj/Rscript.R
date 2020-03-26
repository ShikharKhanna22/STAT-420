study1 <- read.csv(file="study_1.csv")
n=25
sim=2500
sigma_trial=c(1,5,10)


f_mod_sig=matrix(0,3,sim)
f_mod_nul=matrix(0,3,sim)
p_mod_sig=matrix(0,3,sim)
p_mod_nul=matrix(0,3,sim)
r_mod_sig=matrix(0,3,sim)
r_mod_nul=matrix(0,3,sim)
trial=0

for (sigma in sigma_trial) {
  trial = trial + 1
for (i in 1:sim) {
# Generate Y via simulation of noise
y_sig = 3 + 1*study1[,2] + 1*study1[,3]+ 1*study1[,4] + rnorm(n, mean = 0, sd = sigma)
y_nul = 3 + 0*study1[,2] + 0*study1[,3]+ 0*study1[,4] + rnorm(n, mean = 0, sd = sigma)

# Fit both models
mod_sig = lm(y_sig ~. , data = cbind(study1[-1],y_sig))
mod_nul = lm(y_nul ~. , data = cbind(study1[-1],y_nul))


# Record F-Statistic
f_mod_sig[trial,i] = summary(mod_sig)$fstatistic[[1]]
f_mod_nul[trial,i] = summary(mod_nul)$fstatistic[[1]]

# Record p-value of F Statistic
p_mod_sig[trial,i] =pf(summary(mod_sig)$fstatistic[1],
              summary(mod_sig)$fstatistic[2],
              summary(mod_sig)$fstatistic[3],lower.tail=FALSE)
p_mod_nul[trial,i] =pf(summary(mod_nul)$fstatistic[1],
              summary(mod_nul)$fstatistic[2],
              summary(mod_nul)$fstatistic[3],lower.tail=FALSE)

# Record R-Square
r_mod_sig[trial,i] = summary(mod_sig)$r.squared[[1]]
r_mod_nul[trial,i] = summary(mod_sig)$r.squared[[1]]
}
}


par(mfrow=c(1,3),oma = c(0, 0, 3, 0)) # generate a side by side plot 
# get the fscores for the null model and create a side by side plot 
hist(f_mod_sig[1,], prob = TRUE, xlab ="F-score",
     main = "Hist of Fscores for sigma=1", border = "black", col="yellow", xlim = c(0,150), ylim = c(0.0,0.8))
curve(df(x, df1=3, df2=21),col = "orange", add = TRUE, lwd = 1.8, from=0, to=150) # true distibution


hist(f_mod_sig[2,], prob = TRUE, xlab ="F-score",
     main = "Hist of Fscores for sigma=5", border = "black", col="dodgerblue",xlim=c(0,20), ylim = c(0.0,0.8))
curve(df(x, df1=3, df2=21),col = "orange", add = TRUE, lwd = 1.8, from=0, to=20) # true distibution

hist(f_mod_sig[3,], prob = TRUE, xlab ="F-score",
     main = "Hist of Fscores for sigma=10", border = "black", col="green",xlim=c(0,20), ylim = c(0.0,0.8))
curve(df(x, df1=3, df2=21),col = "orange", add = TRUE, lwd = 1.8, from=0, to=20) # true distibution

mtext("Fscores for significant model", outer = TRUE, cex = 1.2)


