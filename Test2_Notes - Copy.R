################ Week 7 ################ 

data("ToothGrowth")

str(ToothGrowth)
# 60 obs. of  3 variables
# len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
# supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
# dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...

# Y is tooth length
# x_1 is the dose in milligrams per day
# x_2 is a dummy variable that takes value 1 when the supp is ascorbic acid(VC)
# Hint: supp==OJ corresponds to x2=0

mod_tooth = lm(len ~ dose * supp, data=ToothGrowth)
#  (Intercept)         dose       suppVC  dose:suppVC  
#   11.550            7.811       -8.255        3.904


# estimate of the change in mean tooth length for a dose increase 
# of 1 milligram per day, when the supplement type is orange juice.
coef(mod_tooth)["dose"]  # 7.811429 

# estimate of the change in mean tooth length for an dose increase of 1 
# milligram per day, when the supplement type is ascorbic acid.
coef(mod_tooth)["dose"] + coef(mod_tooth)["dose:suppVC"] # 11.71571 

# Test for interaction between dose and supplement type. 
# Report the p-value of the test.
summary(mod_tooth)$coefficients["dose:suppVC", "Pr(>|t|)"] # 0.025

# The previous model, using dose as numeric, assumed that the difference 
# between a dose of 0.5 and 1.0 is the same as the difference between a 
# dose of 1.0 and 1.5, but allowed us to make predictions for any dosage.
# Considering dose a categorical variable, we will only be able to 
# make predictions at the three existing dosages, but no longer is the 
# relationship between dose and response constrained to be linear.
unique(ToothGrowth$dose) # 0.5 1.0 2.0


# Fit Y as
# Y is tooth length
# x_1 is a dummy variable that takes the value 1 when the dose is 1.0 milligrams per day
# x_2 is a dummy variable that takes the value 1 when the dose is 2.0 milligrams per day
# x_3 is a dummy variable that takes the value 1 when the supp type is ascorbic acid

# Use this model to obtain an estimate of the diff in mean tooth length for dosages 
# of 1.0 and 2.0 milligrams per day for both supplement types. (Since we are not 
# considering interactions, the supplement type does not matter.)

(mod_tooth2 = lm(len ~ as.factor(dose) + supp , data=ToothGrowth))
# (Intercept)  as.factor(dose)1  as.factor(dose)2            suppVC  
#     12.46              9.13             15.49             -3.70  

(mod_tooth2 = lm(len ~ 0+as.factor(dose)+supp , data=ToothGrowth))
# as.factor(dose)0.5    as.factor(dose)1    as.factor(dose)2              suppVC  
#             12.45               21.58               27.95               -3.70 


# Fit Y as
# Y is tooth length
# x_1 is a dummy variable that takes the value 1 when the dose is 0.5 milligrams per day
# x_2 is a dummy variable that takes the value 1 when the dose is 1.0 milligrams per day
# x_3 is a dummy variable that takes the value 1 when the dose is 2.0 milligrams per day
# x_4 is a dummy variable that takes the value 1 when the supp type is ascorbic acid
# Calculate  γ3?
(mod_tooth2 = lm(len ~ 0+as.factor(dose)+supp , data=ToothGrowth))
# (Intercept) + as.factor(dose)2 OR as.factor(dose)2 = 27.95  



# Ex:2/3: Use the interaction model to estimate the change in average heart weight when body weight 
# is increased by 1 kilogram, for a female cat and Male Cat
mod_int = lm(Hwt ~ Bwt * Sex, data = cats)
# (Intercept)          Bwt         SexM     Bwt:SexM  
#       2.981        2.636       -4.165        1.676  
coef(mod_int)["Bwt"]  # female cat
coef(mod_int)["Bwt"] + coef(mod_int)["Bwt:SexM"] # female cat


# Ex4: Use the additive model to estimate the difference in the change in average heart weight 
# when body weight is increased by 1 kilogram between a male and female cat.
coef(lm(Hwt ~ Bwt + Sex, data=cats))
# (Intercept)         Bwt        SexM 
# -0.41495263  4.07576892 -0.08209684 
# ANS = 0


# Example: Same slope and diff intercepts.
mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)
# am=1(manual trans) and am=0(auto trans)
# (Intercept)           hp           am  # For Manual
#    26.58491     -0.05889      5.27709
# (Intercept)           hp             # For Auto
#    26.58491     -0.05889      
# average mpg for a car with an automatic transmission and 0 hp = beta_0 = 26.58
# average mpg for a car with a manual transmission and 0 hp = beta_0 + beta_2 = 31.861
# diff in avg mpg for cars with manual trans vs those with auto trans, for any hp = beta_2 = 5.28
# change in avg mpg for an increase in one hp, for either transmission types. = beta1 = -0.058
# diff in the change in avg mpg for an increase in one hp between auto and manual trans types = 0


# Ex5/6: Use an F test to compare the additive and interaction models. 
mod_add = lm(Hwt ~ Bwt + Sex, data = cats)
mod_int = lm(Hwt ~ Bwt * Sex, data = cats)
anova(mod_add, mod_int)[2, "F"]
# Res.Df  RSS     Df  Sum of Sq      F  Pr(>F)  
# 141     299.38                              
# 140     291.05  1    8.3317   4.0077 0.04722 *
anova(mod_add, mod_int)[2, "Pr(>F)"] < 0.05 # TRUE, so int



iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
# Ex7: Using the model predict the sepal length of a versicolor with a petal length of 5.10.
new_flower =  data.frame(Petal.Length = 5.10, Species = "versicolor")
predict(iris_add, new_flower)

# Ex8: Create a 90% confidence interval for the difference in mean sepal length between 
# virginicas and setosas for a given petal length.
confint(iris_add, parm = "Speciesvirginica", level = 0.90)[, "5 %"]
#                       5 %      95 %
#   Speciesvirginica -2.570345 -1.664993

# Ex9: Perform a test that compares this model to one without an effect for species
anova(lm(Sepal.Length ~ Petal.Length, data = iris), iris_add)[2, "F"]


iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
# Ex10: Now consider the model with interaction. Excluding σ2, how many parameters 
# does this model have? Stated another way, if written mathematically, how many β parameters 
# are in the model?
length(coef(iris_int))

# Ex11: create a 99% prediction interval for the sepal length of a versicolor with a petal length 
# of 5.10. Report the upper bound of this interval.
new_flower =  data.frame(Petal.Length = 5.10, Species = "versicolor")
predict(iris_int, new_flower, interval = "prediction", level = 0.99)[, "upr"]


# Ex12: obtain an est of the change in mean petal length for a sepal length increase of 1 unit, 
# for a versicolor.
coef(iris_int)["Petal.Length"] + coef(iris_int)["Petal.Length:Speciesversicolor"]

# Ex13: Compare the two previous models, additive and interaction models using an ANVOA F test.
iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
anova(iris_add, iris_int)[2, "Pr(>F)"] < 0.01 # FALSE, so additive



# Fit an multiple linear model with Fertility as the response and Education,  Catholic, and Mortality 
# as predictors. Use the first order terms as well as all two and three-way interactions.
swiss_int = lm(Fertility ~ Education * Catholic * Infant.Mortality, data = swiss)
coef(swiss_int)


# Ex14: estimate the change in mean Fertility for an increase of Education of one unit when 
# Catholic is 90.0 and  Infant.Mortality is 20.0.
coef(swiss_int)["Education"] + 
  coef(swiss_int)["Education:Catholic"] * 90.0 + 
  coef(swiss_int)["Education:Infant.Mortality"] * 20.0 + 
  coef(swiss_int)["Education:Catholic:Infant.Mortality"] * 90.0 * 20.0


# Ex15: Test for the significance of the three-way interaction. Report the p-value of this test.
summary(swiss_int)$coefficients["Education:Catholic:Infant.Mortality", "Pr(>|t|)"] # 0.3912921
OR
swiss_two_way = lm(Fertility ~ (Education + Catholic + Infant.Mortality) ^ 2, data = swiss)
anova(swiss_two_way, swiss_int)[2, "Pr(>F)"] # 0.3912921

################ Week 8 ################ 

# Y=5−2x+ϵ
# ϵ ∼ N(0,σ2=|x|/4)
# Calculate P[Y>1 ∣ X=3]
x=3 ; mean = 5-2*x ; sd = sqrt(x/4)
pnorm(1, mean, sd, lower.tail=FALSE)

gen_data = function(sample_size = 20, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = exp(2 + 3 * x + 0.35 * x ^ 2 + rnorm(n = sample_size, sd = 3))
  data.frame(x = x, y = y)
}

quiz_data = gen_data()

fit = lm(y ~ x, data = quiz_data)
# Fit a SLR model. What is the Cook’s distance for the observation with the largest leverage?
# Calculate the p-value of the Shapiro-Wilk test for the normality assumption.
unname(cooks.distance(fit)[which.max(hatvalues(fit))])
shapiro.test(resid(fit))$p.value

# fit as per - log(y) = β0 + β1x + β2x^2 + ϵ
fit = lm(log(y) ~ x + I(x ^ 2), data = quiz_data)

# Use the Shapiro-Wilk test to asses the normality assumption for this model. Use α=0.05.
shapiro.test(resid(fit))$p.value # 0.4021 , so Fail to Reject H0. Normality assumption is not suspect.

# Calculate the residual sum of squares (RSS) in the original units of y.
sum((exp(fitted(fit)) - quiz_data$y) ^ 2) / 1000000000 # 42.27957
sum((exp(predict(fit)) - quiz_data$y)^2)/1000000000
# Cannot use resid(fit) due to log



gen_data_1 = function(sample_size = 25, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rnorm(n = sample_size)
  data.frame(x = x, y = y)
}

gen_data_2 = function(sample_size = 25, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rt(n = sample_size, df = 2)
  data.frame(x = x, y = y)
}

data_1 = gen_data_1()
data_2 = gen_data_2()


fit_1 = lm(y ~ x, data = data_1)
fit_2 = lm(y ~ x, data = data_2)
# Ex1: For both fitted regressions, create a Normal Q-Q Plot.
par(mfrow = c(1, 2))
qqnorm(resid(fit_1), pch = 20)
qqline(resid(fit_1))
qqnorm(resid(fit_2), pch = 20)
qqline(resid(fit_2))
# create Fitted versus Residuals plot.
par(mfrow = c(1, 2))
plot(fitted(fit_1), resid(fit_1), pch = 20)
abline(h = 0)
plot(fitted(fit_2), resid(fit_2), pch = 20)
abline(h = 0)



mod = lm(sr ~ ., data = LifeCycleSavings)
# Ex3: Fit a MLR model with sr as the response and the remaining variables as predictors. 
# What proportion of observations have a standardized residual less than 2 in magnitude?
# Which country (observation) has the standardized residual with the largest magnitude?
mean(abs(rstandard(mod)) < 2)
names(which.max(abs(rstandard(mod))))

# How many observations have “high” leverage? Use twice the average leverage as the cutoff for “high.”
# Which country (observation) has the largest leverage?
sum(hatvalues(mod) > 2 * mean(hatvalues(mod)))
names(which.max(hatvalues(mod)))

# Report the largest Cook’s Distance for observations in this dataset.
max(cooks.distance(mod))

# Find the obs that are influential. Use 4/n as the cutoff for labeling an observation influential.
# Create a subset of the original data that excludes these influential observations and refit the 
# same model to this new data. Report the sum of the estimated regression cofficients.
keep = cooks.distance(mod) < 4 / length(resid(mod))
new_mod = lm(sr ~ ., data = LifeCycleSavings, subset = keep)
sum(coef(new_mod))


# We will use Ozone as the response and Temp as a single predictor.
airquality = na.omit(airquality)
fit_quad = lm(Ozone ~ Temp + I(Temp ^ 2), data = airquality)

# Test for the significance of the quadratic term. Report the p-value of this test.
summary(fit_quad)$coefficients[3, "Pr(>|t|)"]

# Y=β0+β1x+β2x^2+β3x^3+β4x^4+ϵ
# Test to compare this model. Report the p-value of this test.
fit_quar = lm(Ozone ~ Temp + I(Temp ^ 2) + I(Temp ^ 3) + I(Temp ^ 4), data = airquality)
anova(fit_quad, fit_quar)[2, "Pr(>F)"] # 0.02436082


# Use the Shapiro-Wilk test to asses the normality assumption for the model. Use α=0.01.
shapiro.test(resid(fit_quar))$p.value # 5.00861e-11, Reject H0. Normality assumption is suspect.

# log(y)=β0+β1x+ϵ. Use the Shapiro-Wilk test to asses the normality assumption. Use α=0.01.
fit_log = lm(log(Ozone) ~ Temp, data = airquality)
shapiro.test(resid(fit_log))$p.value # 0.04867205, Fail to Reject H0. Normality assumption is not suspect.

# create a 90% prediction interval for Ozone when the temperate is 84 degree Fahrenheit. 
# Report the upper bound of this interval
exp(predict(fit_log, newdata = data.frame(Temp = 84), interval = "prediction", level = 0.90)[, "upr"])

# calculate the ratio of:
# The sample variance of residuals for obersations with a fitted value less than 3.5
# The sample variance of residuals for obersations with a fitted value greater than 3.5
# (While not a formal test for the equal variance assumption, we would hope that this value is close to 1.)
var(resid(fit_log)[fitted(fit_log) < 3.5]) / var(resid(fit_log)[fitted(fit_log) > 3.5]) # 1.353182

