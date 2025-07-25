


generate_imputation_report <- function(imp_opt, output_file = "imputation_report_generated.qmd") {
  # Convert objects to their R-code representations.
  imp_opt_str <- paste(capture.output(dput(imp_opt)), collapse = "\n")

  # Build the QMD content dynamically.
  qmd_content <- sprintf(
    "---
title: \"Imputation Strategy Evaluation\"
format: html
execute:
  echo: false
  warning: false
  message: false
---

```{r}
library(here)
# Load the data created by generate_imputation_report()
source(here('R', 'metrics_1var_1iter.R'))
source(here('R', 'scenario_get_simulated_dataset.R'))

# Define data directly (embedded by the function)
imp_opt <- %s

evaluation_table <- metrics_1var_1iter(imp_opt, iter=1, var=\"eat\")

knitr::kable(evaluation_table$metrics_df, caption = \"Evaluation Metrics\")

# Plot the results
library(ggplot2)
ggplot(data.frame(Original = evaluation_table$orig, Imputed = evaluation_table$imp), aes(x = Original, y = Imputed)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = \"lm\", se = FALSE, color = \"blue\") +
  labs(title = \"Correlation Between Original and Imputed Values\", x = \"Original\", y = \"Imputed\") +
  theme_minimal()
```
", imp_opt_str)
  # Write the QMD file to disk.
  writeLines(qmd_content, output_file)

  # Knit (render) the Quarto document to HTML.
  # This system call runs the Quarto CLI, which produces the final output.
  system(sprintf("quarto render %s", output_file))

  cat("Report generated successfully as HTML.\n")
}
