data <- read.csv("SwedishMotorInsurance.csv")
str(data)
summary(data)

set.seed(123)  

train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))

train_data <- data[train_index, ]
validation_data <- data[-train_index, ]

mean(train_data$Claims)
mean(train_data$Payment)
mean(train_data$Insured)

sum(train_data$Claims)
sum(train_data$Payment)

ggplot(train_data, aes(x = Claims)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of Claims")

model_freq <- glm(Claims ~ Kilometres + Zone + Bonus + Make,
                  data = train_data,
                  family = poisson,
                  offset = log(Insured))
summary(model_freq)
pred_freq <- predict(model_freq,
                     newdata = validation_data,
                     type = "response")
head(pred_freq)

train_data_sev <- train_data[train_data$Claims > 0, ]
train_data_sev$Severity <- train_data_sev$Payment / train_data_sev$Claims
ggplot(train_data_sev, aes(x = log(Severity))) +
  geom_histogram(bins = 30) +
  ggtitle("Log Severity Distribution") +
  xlab("log(Severity)") +
  ylab("Frequency")
model_sev <- glm(Severity ~ Kilometres + Zone + Bonus + Make,
                 data = train_data_sev,
                 family = Gamma(link = "log"))
summary(model_sev)
pred_sev <- predict(model_sev,
                     newdata = validation_data,
                     type = "response")
head(pred_sev)

pred_premium <- pred_freq * pred_sev
actual <- validation_data$Payment
rmse <- sqrt(mean((actual - pred_premium)^2))
rmse
ggplot(validation_data, aes(x = actual, y = pred_premium)) +
  geom_point() +
  ggtitle("Actual vs Predicted Payment") +
  xlab("Actual Payment") +
  ylab("Predicted Payment")
