#' Compute Imputation Metrics
#'
#' This function calculates various metrics to evaluate imputation performance, including
#' Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), Mean Absolute Percentage Error (MAPE),
#' and Correlation (CORR) between original and imputed values.
#'
#' @param imputation_case A list containing imputation results, original data and missingness scenario.
#' @param unit_id A string specifying the column name used as an identifier.
#' @param iter An integer specifying which iteration to use.
#' @param var A string indicating the variable for which metrics are computed.
#'
#' @return A data frame with computed metrics: MAE, RMSE, MAPE, CORR, and the number of observations (N).
#' @export
#'
#' @examples
#' # Example usage:
#' imputed_metrics <- metrics_1var_1iter(imputation_case, "ID", 1, "variable_name")
#' print(imputed_metrics)

metrics_1var_1iter <- function(imputation_case,
                               unit_id=imputation_case$meta$missingness_scenario$meta$unit_id,
                               iter=NULL,
                               var){

  # Initialize lists to store the metrics for each column
  mae_list   <- list()  # Mean Absolute Error
  rmse_list  <- list()  # Root Mean Squared Error
  mape_list  <- list()  # Mean Absolute Percentage Error
  cor_list  <-  list()  # Correlation between orig and imp

  original <- imputation_case$meta$missingness_scenario$original_data
  miss_iterations <- scenario_get_iteration(imputation_case$meta$missingness_scenario, iter)$scenario_iteration

  if (is.null(iter)){
    imp_opt <- imputation_case$imputations[, !names(imputation_case$imputations) %in% "iter"]
  }  else {
    imp_opt <- imputation_case$imputations[imputation_case$imputations$iter==iter, !names(imputation_case$imputations) %in% "iter"]
  }
  # Every unit in imp_opt is included, since only imputed units should show up in there
  units_imp_var <- imp_opt[imp_opt$variable==var,unit_id]
  # Keep units that are missing in missing scenario but not missing in original data
  units_value_orig <- !is.na(original[original[[unit_id]] %in% units_imp_var,var]) &
    is.na(miss_iterations[miss_iterations[[unit_id]] %in% units_imp_var,var])
  df <- data.frame(
    orig = original[original[[unit_id]] %in% units_imp_var,var][units_value_orig],
    imp = imp_opt[imp_opt$variable==var,"value"][units_value_orig]
  )

  mae_list[[var]] <- round(mean(abs(df$orig - df$imp), na.rm = TRUE), 1)
  rmse_list[[var]] <- round(sqrt(mean((df$orig - df$imp)^2, na.rm = TRUE)), 1)
  mape_list[[var]] <- round(mean(ifelse(df$orig != 0, abs((df$orig - df$imp)/df$orig), NA), na.rm = TRUE)*100, 2)
  cor_list[[var]] <- round(cor(df$orig, df$imp, use = "complete.obs"), 2)

  metrics_df <- data.frame(
    variable = var,
    iter = iter,
    N = length(df$orig),
    MAE = unlist(mae_list),
    RMSE = unlist(rmse_list),
    MAPE = unlist(mape_list),
    CORR = unlist(cor_list),
    row.names = NULL
  )
  return(list(metrics_df = metrics_df, orig = df$orig, imp = df$imp))
}

