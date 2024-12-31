# 데이터 입력
data <- data.frame(
  번호 = 1:9,
  Y = c(11, 7, 7, 19, 9, 4, 3, 1, 3),
  N = c(0.0950, 0.1920, 0.0750, 0.2078, 0.1382, 0.0540, 0.1292, 0.0503, 0.0629)
)

# 최소제곱법 회귀분석
lm_model <- lm(Y ~ N, data = data)

# 포아송 회귀분석
poisson_model <- glm(Y ~ N, family = poisson(link = "log"), data = data)

# 최소제곱법: 잔차 플롯 및 Q-Q 플롯
par(mfrow = c(1, 2)) # 1행 2열로 플롯 나누기
plot(lm_model, which = 1, main = "linear regression residual plot") # 잔차 플롯
plot(lm_model, which = 2, main = "Linear Model Q-Q Plot") # Q-Q 플롯

# 포아송 회귀: 잔차 플롯 및 Q-Q 플롯
# pearson residuals 사용
par(mfrow = c(1, 2)) # 1행 2열로 플롯 나누기
residuals_poisson <- residuals(poisson_model, type = "pearson") # 포아송 잔차
fitted_poisson <- fitted(poisson_model)

# 잔차 플롯
plot(fitted_poisson, residuals_poisson,
     main = "Poisson Model Residual Plot",
     xlab = "Fitted values",
     ylab = "Pearson Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Q-Q 플롯
qqnorm(residuals_poisson, main = "Poisson Model Q-Q Plot")
qqline(residuals_poisson, col = "red")

# summary of the model
summary(lm_model)
summary(poisson_model)
