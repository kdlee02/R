options(scipen = 999)

# load the data
data <- read.table("Field.Goal.Kicking.txt", header = TRUE)

# add success probability and squared distance
data$Success_Prob <- data$Success / data$Attempts
data$Distance_Squared <- data$Distance^2

# subset data for NFL and AFL
nfl_data <- subset(data, League == 'NFL')
afl_data <- subset(data, League == 'AFL')

# logistic regression function
fit_logistic_model <- function(df) {
  glm(cbind(Success, Attempts - Success) ~ Distance + Distance_Squared, 
      data = df, family = binomial(link = "logit"))
}

# fit models for NFL and AFL
nfl_model <- fit_logistic_model(nfl_data)
afl_model <- fit_logistic_model(afl_data)


# summarize results
summary(glm(cbind(Success, Attempts - Success) ~ Distance + Z, 
    data = data, family = binomial(link = "logit")))
summary(nfl_model)
summary(afl_model)
