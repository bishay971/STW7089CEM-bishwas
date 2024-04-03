
install.packages("tidyverse")
library(tidyverse)
install.packages("nlstools")
library(nlstools)
install.packages("car")
library(car)
install.package("caret")
library(caret)
installed.package("propagate")
library(propagate)

x <- read_csv("C:\\Users\\bishe\\OneDrive\\Desktop\\Personal\\Softwerica\\Stat\\x.csv", col_names = FALSE) %>%
  mutate(id = row_number())
y <- read_csv("C:\\Users\\bishe\\OneDrive\\Desktop\\Personal\\Softwerica\\Stat\\y.csv", col_names = FALSE) %>% 
  rename(Y = X1) %>% 
  mutate(id = row_number())

time <- read_csv("C:\\Users\\bishe\\OneDrive\\Desktop\\Personal\\Softwerica\\Stat\\time.csv", col_names = FALSE)


######### merge datasets by ID to make it into single dataset.

df <- merge(x,y, by="id") %>% dplyr::select(-id)


# Plot to check if the data behaves as curvilinear

ggplot(data = df, aes(x = X1, y = Y))+
  geom_point(color = "#69b3a2")+ # Add testing data points
  geom_smooth(color = "red", linewidth = 0.5)+
  ggtitle("Original Data")


# formula_1 <- as.formula(Y ~ a1*X4 + a2*X1^2 + a3*X1^3 + a4*X2^4 + a5*X1^4)
# formula_2 <- as.formula(Y ~ a1*X4 + a2*X1^3 + a3*X3^4)
# formula_3 <- as.formula(Y ~ a1*X3 + a2*X3^4 )
# formula_4 <- as.formula(Y ~ a1*X2 + a2*X1^3 + a4*X3^4)
# formula_5 <- as.formula(Y ~ a1*X4 + a2*X1^2 + a3*X1^3 + a4*X3^4)

#------------------------------------------------------------------------- #
#------------- Task 2.1 - # Estimation of parameters
#----------------------------------------------------------------------- #

### For equation 1
# ------------------ 

# Define the design matrix
X_1 <- cbind(df$X4, df$X1^2, df$X1^3, df$X2^4, df$X1^4)
Y <- df$Y

# Solve for parameters
parameters_1 <- solve(t(X_1) %*% X_1) %*% t(X_1) %*% Y



### For equation 2
# ------------------ 
# Define the design matrix
X_2 <- cbind(df$X4, df$X1^3, df$X3^4)
Y <- df$Y

# Solve for parameters
parameters_2 <- solve(t(X_2) %*% X_2) %*% t(X_2) %*% Y

### For equation 3
# ------------------ 

# Define the design matrix
X_3 <- cbind(df$X3, df$X3^4 )
Y <- df$Y

# Solve for parameters
parameters_3 <- solve(t(X_3) %*% X_3) %*% t(X_3) %*% Y


### For equation 4
# ------------------ 
X_4 <- cbind(df$X2, df$X1^3, df$X3^4)
Y <- df$Y

# Solve for parameters
parameters_4 <- solve(t(X_4) %*% X_4) %*% t(X_4) %*% Y

### For equation 5
# ------------------ 
X_5 <- cbind(df$X4, df$X1^2, df$X1^3, df$X3^4)
Y <- df$Y

# Solve for parameters
parameters_5 <- solve(t(X_5) %*% X_5) %*% t(X_5) %*% Y


#-------------------------------------------------------------------------#
#------------- Task 2.2 - # RSS
#------------------------------------------------------------------------#


### For equation 1
# ------------------ 
fitted_values <- X_1 %*% parameters_1
# Calculate residuals
residuals_1 <- Y - fitted_values
RSS_1 <- sum(residuals_1^2)

### For equation 2
# ------------------ 
fitted_values <- X_2 %*% parameters_2
# Calculate residuals
residuals_2 <- Y - fitted_values
RSS_2 <- sum(residuals_2^2)


### For equation 3
# ------------------ 
fitted_values <- X_3 %*% parameters_3
# Calculate residuals
residuals_3 <- Y - fitted_values
RSS_3 <- sum(residuals_3^2)


### For equation 4
# ------------------ 
fitted_values <- X_4 %*% parameters_4
# Calculate residuals
residuals_4 <- Y - fitted_values
RSS_4 <- sum(residuals_4^2)

# predicted_4 <- predict(fit_4)
# residuals_4 <- df$Y - predicted_4
# model_residual_4 <- abs(residuals_4) # gives the absolute difference between the predicted and observed value.
# RSS_4 <- sum(residuals_4^2)

### For equation 5
# ------------------ 
fitted_values <- X_5 %*% parameters_5
# Calculate residuals
residuals_5 <- Y - fitted_values
RSS_5 <- sum(residuals_5^2)
#-------------------------------------------------------------------------#
#------------- Task 2.3 - # Log Liklihood function
#------------------------------------------------------------------------#


### For equation 1
# ------------------ 
n_1 <- length(residuals_1)  # number of observations
sigma_1 <- sqrt(sum(residuals_1^2) / n_1) # Estimate of standard deviation
log_likelihood_1 <- -n_1/2 * log(2 * pi) - n_1/2 * log(sigma_1^2) - 1/(2*sigma_1^2) * RSS_1


### For equation 2
# ------------------ 
n_2 <- length(residuals_2)  # number of observations
sigma_2 <- sqrt(sum(residuals_2^2) / n_2) # Estimate of standard deviation
log_likelihood_2 <- -n_2/2 * log(2 * pi) - n_2/2 * log(sigma_2^2) - 1/(2*sigma_2^2) * RSS_2


### For equation 3
# ------------------ 
n_3 <- length(residuals_3)  # number of observations
sigma_3 <- sqrt(sum(residuals_3^2) / n_3) # Estimate of standard deviation
log_likelihood_3 <- -n_3/2 * log(2 * pi) - n_3/2 * log(sigma_3^2) - 1/(2*sigma_3^2) * RSS_3


### For equation 4
# ------------------ 
n_4 <- length(residuals_4)  # number of observations
sigma_4 <- sqrt(sum(residuals_4^2) / n_4) # Estimate of standard deviation
log_likelihood_4 <- -n_4/2 * log(2 * pi) - n_4/2 * log(sigma_4^2) - 1/(2*sigma_4^2) * RSS_4


### For equation 5
# ------------------ 
n_5 <- length(residuals_5)  # number of observations
sigma_5 <- sqrt(sum(residuals_5^2) / n_5) # Estimate of standard deviation
log_likelihood_5 <- -n_5/2 * log(2 * pi) - n_5/2 * log(sigma_5^2) - 1/(2*sigma_5^2) * RSS_5



#-------------------------------------------------------------------------#
#------------- Task 2.4 # AIC and BIC
#------------------------------------------------------------------------#

n <- nrow(df)  # number of observations
### For equation 1
# ------------------ 
k <- length(parameters_1)  # number of parameters
AIC_1 <- 2 * k - 2 * log_likelihood_1
BIC_1 <- k * log(n) - 2 * log_likelihood_1


### For equation 2
# ------------------ 
k <- length(parameters_2)  # number of parameters
AIC_2 <- 2 * k - 2 * log_likelihood_2
BIC_2 <- k * log(n) - 2 * log_likelihood_2


### For equation 3
# ------------------ 
k <- length(parameters_3)  # number of parameters
AIC_3 <- 2 * k - 2 * log_likelihood_3
BIC_3 <- k * log(n) - 2 * log_likelihood_3


### For equation 4
# ------------------ 
k <- length(parameters_4) # number of parameters
AIC_4 <- 2 * k - 2 * log_likelihood_4
BIC_4 <- k * log(n) - 2 * log_likelihood_4


### For equation 5
# ------------------ 
k <- length(parameters_5)  # number of parameters
AIC_5 <- 2 * k - 2 * log_likelihood_5
BIC_5 <- k * log(n) - 2 * log_likelihood_5

#-------------------------------------------------------------------------#
#------------- Task 2.5 # Distribution of residuals
#------------------------------------------------------------------------#

### For equation 1
# ------------------ 
res_1 <-data.frame(residuals_1)
colnames(res_1) <- "res"

# Histogram, density plot and normal distribution curve.
ggplot(res_1, aes(x=res)) + 
  geom_histogram(aes(y=..density..),colour="black",fill=c("white"), bins = 100)+
  geom_density(aes(x=res,y=after_stat(density)),col="blue")+
  stat_function(fun = dnorm, args = list(mean = mean(res_1$res), sd = sd(res_1$res)),col="red")+
  ylab("Density") + xlab("Residuals")

# qqplot
qqPlot(res_1$res, distribution = "norm")


### For equation 2
# ------------------ 
res_2 <-data.frame(residuals_2)
colnames(res_2) <- "res"

# Histogram, density plot and normal distribution curve.
ggplot(res_2, aes(x=res)) + 
  geom_histogram(aes(y=..density..),colour="black",fill=c("white"), bins = 100)+
  geom_density(aes(x=res,y=after_stat(density)),col="blue")+
  stat_function(fun = dnorm, args = list(mean = mean(res_2$res), sd = sd(res_2$res)),col="red")+
  ylab("Density") + xlab("Residuals")

# qqplot
qqPlot(res_2$res, distribution = "norm")


### For equation 3
# ------------------ 
res_3 <-data.frame(residuals_3)
colnames(res_3) <- "res"

# Histogram, density plot and normal distribution curve.
ggplot(res_3, aes(x=res)) + 
  geom_histogram(aes(y=..density..),colour="black",fill=c("white"), bins = 100)+
  geom_density(aes(x=res,y=after_stat(density)),col="blue")+
  stat_function(fun = dnorm, args = list(mean = mean(res_3$res), sd = sd(res_3$res)),col="red")+
  ylab("Density") + xlab("Residuals")

# qqplot
qqPlot(res_3$res, distribution = "norm")


### For equation 4
# ------------------ 
res_4 <-data.frame(residuals_4)
colnames(res_4) <- "res"

# Histogram, density plot and normal distribution curve.
ggplot(res_4, aes(x=res)) + 
  geom_histogram(aes(y=..density..),colour="black",fill=c("white"), bins = 100)+
  geom_density(aes(x=res,y=after_stat(density)),col="blue")+
  stat_function(fun = dnorm, args = list(mean = mean(res_4$res), sd = sd(res_4$res)),col="red")+
  ylab("Density") + xlab("Residuals")

# qqplot
qqPlot(res_4$res, distribution = "norm")


### For equation 5
# ------------------ 
res_5 <-data.frame(residuals_5)
colnames(res_5) <- "res"

# Histogram, density plot and normal distribution curve.
ggplot(res_5, aes(x=res)) + 
  geom_histogram(aes(y=..density..),colour="black",fill=c("white"), bins = 100)+
  geom_density(aes(x=res,y=after_stat(density)),col="blue")+
  stat_function(fun = dnorm, args = list(mean = mean(res_5$res), sd = sd(res_5$res)),col="red")+
  ylab("Density") + xlab("Residuals")

# qqplot
qqPlot(res_5$res, distribution = "norm")

#-------------------------------------------------------------------------#
#------------- Task 2.7 # Training and testing (selecting equation 5)
#------------------------------------------------------------------------#


trainIndex <- createDataPartition(df$Y, p = 0.7, list = FALSE)

trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]
testData_subset <- testData[, c("X1", "X3", "X4")]

nrow(trainData)
nrow(testData)

# Model 5
formula_5 <- as.formula(Y ~ a1*X4 + a2*X1^2 + a3*X1^3 + a4*X3^4)

# Initial values for parameters
initial_values <- list(a1 = 1, a2 = 1, a3 = 1, a4 = 1)

# Fit the model using nonlinear least squares
model_nls <- nls(formula_5, data = trainData, start = initial_values)

# Print a summary of the model
summary(model_nls)
coef(model_nls)
overview(model_nls)


predictions <- predict(model_nls, newdata = testData,se.fit = TRUE)
residuals <- residuals(model_nls)
residual_se <- sqrt(sum(residuals^2) / (length(residuals) - length(model_nls$coefficients)))
# Compute 95% confidence intervals for predictions
lower_bound <- predictions - qnorm(0.975) * residual_se
upper_bound <- predictions + qnorm(0.975) * residual_se

# Bind predictions with confidence intervals
predictions_with_intervals <- cbind(testData,predictions, lower_bound, upper_bound)# Calculate 95% confidence intervals for predictions


# Plot the testing data and model predictions with confidence intervals
ggplot(data = predictions_with_intervals, aes(x = X1, y = predictions))+
  geom_point(color = "#69b3a2")+ # Add testing data points
  geom_smooth(aes(ymin = lower_bound , ymax = upper_bound), color = "red", linewidth = 0.5)+
  geom_point(aes(x = X1, y = Y), color = "#6B5B95")+
  ggtitle("Model Predictions with 95% Confidence Intervals")

#-------------------------------------------------------------------------#
#------------- Task 3 # ABC
#------------------------------------------------------------------------#


# Number of samples to generate
n_samples <- 1000

# Define the uniform prior range for parameters a1 and a2
a1_range <- c(-2, 2)
a2_range <- c(-2, 2)

# Generate samples from the uniform prior for parameters a1 and a2
samples <- matrix(NA, nrow = n_samples, ncol = 2)
for (i in 1:n_samples) {
  a1_sample <- runif(1, min = a1_range[1], max = a1_range[2])
  a2_sample <- runif(1, min = a2_range[1], max = a2_range[2])
  samples[i,] <- c(a1_sample, a2_sample)
}


# Perform rejection ABC
accepted_samples <- matrix(NA, nrow = n_samples, ncol = 2)
count_accepted <- 0
for (i in 1:n_samples) {
  a1 <- samples[i, 1]
  a2 <- samples[i, 2]
  # Perform some simulation or computation
  # In this example, let's check if the sum of a1 and a2 is within a certain range
  if (sum(df$X4 * a1 + df$X1^2 * a2) > 0) {  # Adjust acceptance condition based on your specific problem
    count_accepted <- count_accepted + 1
    accepted_samples[count_accepted, ] <- c(a1, a2)
  }
}

install.packages("MASS")
library(MASS)  # For kde2d function

# Remove missing or infinite values from accepted samples
accepted_samples_clean <- accepted_samples[complete.cases(accepted_samples), ]

# Plot joint posterior distribution (2D KDE)
plot_title <- "Joint Posterior Distribution (KDE)"
dens <- kde2d(accepted_samples_clean[,1], accepted_samples_clean[,2])
contour(dens, xlab = "a1", ylab = "a2", main = plot_title,col = heat.colors(10))

# Plot marginal posterior distribution for a1
hist(accepted_samples[,1], main = "Marginal Posterior Distribution for a1", xlab = "a1", freq = FALSE)

# Plot marginal posterior distribution for a2
hist(accepted_samples[,2], main = "Marginal Posterior Distribution for a2", xlab = "a2", freq = FALSE)

