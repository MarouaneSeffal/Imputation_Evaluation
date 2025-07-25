---
  title: "Imputation Strategy Evaluation"
format: html
execute:
  echo: false
---

  ```{r}
# Load necessary libraries
library(ggplot2)

# Simulated original and imputed data
set.seed(123)
original <- rnorm(100, mean = 50, sd = 10)
imputed <- original + rnorm(100, mean = 0, sd = 5)

# Compute evaluation metrics
mae <- mean(abs(original - imputed))
rmse <- sqrt(mean((original - imputed)^2))
mape <- mean(abs((original - imputed) / original)) * 100
corr <- cor(original, imputed)

# Display the results
evaluation_table <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE", "Correlation"),
  Value = c(mae, rmse, mape, corr)
)

knitr::kable(evaluation_table, caption = "Evaluation Metrics")

# Scatter plot showing correlation
ggplot(data.frame(original, imputed), aes(x = original, y = imputed)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Correlation Between Original and Imputed Values", x = "Original", y = "Imputed") +
  theme_minimal()
```
