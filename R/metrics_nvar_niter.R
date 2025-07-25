#' Compute Imputation Metrics for multiple iterations and variables
#'
#' This function generalizes metrics_1var_1iter by calculating various metrics for multiple iterations
#' and variables. These metrics are calculated to evaluate imputation performance, including
#' Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), Mean Absolute Percentage Error (MAPE),
#' and Correlation (CORR) between original and imputed values.
#'
#' @param imputation_case A list containing imputation results, original data and missingness scenario.
#' @param unit_id A string specifying the column name used as an identifier.
#' @param iter An integer or a vector of integers specifying which iteration to use.
#' @param var A string or a vector of strings indicating the variable for which metrics are computed.
#'
#' @return A data frame with computed metrics: MAE, RMSE, MAPE, CORR, and the number of observations (N).
#' @export
#'
#' @examples
#' # Example usage:
#' imputed_metrics <- metrics_nvar_niter(imputation_case, "ID", 1, "variable_name")
#' print(imputed_metrics)

metrics_nvar_niter <- function(imputation_case,
                               unit_id=imputation_case$meta$missingness_scenario$meta$unit_id,
                               iter=NULL,
                               var=NULL){
  if (is.null(iter)){
    n <- imputation_case$meta$missingness_scenario$meta$n
    iter <- seq_len(n)
  }

  metrics <-  do.call(rbind, lapply(iter, function(l){
    if (is.null(var)){
      var <- unique(imputation_case$imputations[imputation_case$imputations$iter==l, "variable"])
    }
    do.call(rbind, lapply(var, function(v){
      metrics_1var_1iter(imp_opt, iter=l, var=v)$metrics_df
    }))
  }))
  return(metrics[order(metrics$variable,metrics$iter),])
}
