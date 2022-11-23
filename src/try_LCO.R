# Author: T.Y.
# Date created: 20.10.2021
source("../commons/src/orchestrator.R")


CENTER_FOLD_MAPPING = list()
FOLD_CENTER_MAPPING = list()


##### ************************************************************************************************************************************************************
#### Exported functions
##### ************************************************************************************************************************************************************

try_perform_leave_center_out_analysis <- function(df1, log_to_file = T,
                                                  logfile_prefix = "",
                                                  vselc_bs_agree_stren = 1.0,
                                                  skip_internal_validations = F,
                                                  skip_external_validations =  F,
                                                  center_col_nm = "center",
                                                  outcome_var = "outcome") {
  LCO_RUNNING =  T
  logfile_prefix = paste0(logfile_prefix, "-LCO")
  if(log_to_file)
    start_logging_to_file(logfile_prefix)

  df1 = rename_column_df(df1, outcome_var, "outcome_of_interest")
  df1 = rename_column_df(df1, center_col_nm, "center_column")
  original_center_col_nm = center_col_nm
  center_col_nm = "center_column"


  folds_created = create_lco_train_test_folds(df1, center_col_nm)
  CENTER_FOLD_MAPPING = folds_created$CENTER_FOLD_MAPPING
  FOLD_CENTER_MAPPING = folds_created$FOLD_CENTER_MAPPING

  vars_selected_per_fold <- list()
  lr_LCO_restuls_list <- list()
  internal_val_reses<- list()  # todo: this may be an unused variable

  fold_tm_start <- as.numeric(Sys.time())
  fold_tm_end <- as.numeric(Sys.time())

  for(cur_fold in 1:len(folds_created$folds)){
    cur_df_train <- folds_created$train_dfs[[cur_fold]]
    cur_df_test <- folds_created$test_dfs[[cur_fold]]

    # timing & printing
    fold_tm_end <- as.numeric(Sys.time())
    if(cur_fold != 1)
      try_log_info("|%s| FOLD #%d Elapsed time: %0.1fs", logfile_prefix,  cur_fold-1, fold_tm_end - fold_tm_start)
    fold_tm_start <- as.numeric(Sys.time())
    try_log_debug("|%s| LCO status: Entering fold #%d (centrum = %s)", logfile_prefix, cur_fold, FOLD_CENTER_MAPPING[[cur_fold]])

    cur_df_train = cur_df_train[,remove_strings_matching_regex(cns(cur_df_train), "center_column_cat_\\d+_")]
    cur_df_test = cur_df_test[,remove_strings_matching_regex(cns(cur_df_test), "center_column_cat_\\d+_")]

    # external vali
    if(!skip_external_validations) {
      strat_s_res <- derive_model_multi_imputation(cur_df_train, cur_df_test, vselc_bs_agree_stren, logfile_prefix, cur_fold)
      vars_selected_per_fold[[cur_fold]] <- strat_s_res$aic_res
      lr_LCO_restuls_list[[cur_fold]] <- strat_s_res$lr_res
    }

    # internal vali
    if(!skip_internal_validations) {
      try_log_debug("@try_perform_leave_center_out_analysis- entering internal valiadtion for center %s (using 10-fold CV)", FOLD_CENTER_MAPPING[[cur_fold]])
      interal_val_res = try_kfold_cv(cur_df_train, log_to_file = F, logfile_prefix = paste0(logfile_prefix, "-center-", cur_fold, "-internal-val"), vselc_bs_agree_stren = vselc_bs_agree_stren)
      internal_val_reses[[cur_fold]] <- interal_val_res
    }
  }

  # convert metric lists into dfs to compare results
  logreg_LCO_results_df <- NULL
  metric_nms <- c("aucroc", "aucpr", "bss", "bs", "cali_slope", "cali_intercept",
                  "cali_slope_ci_lo", "cali_slope_ci_hi", "cali_intercept_ci_lo",
                  "cali_intercept_ci_hi", "cali_p_val")

  if(!skip_external_validations) {
    logreg_LCO_results_df <- convert_results_list_to_df(lr_LCO_restuls_list, metric_nms)
    if(nrow(logreg_LCO_results_df) > 0) {
      logreg_LCO_results_df$model_type = "lr"
      try_log_info("@try_perform_leave_center_out_analysis- mean AUC-ROC logreg = %0.5f", mean(logreg_LCO_results_df$aucroc, na.rm = T ))
    }
  }

  try_log_debug(0, "@try_perform_leave_center_out_analysis- COMPLETE")
  print(Sys.time())
  if(log_to_file)
    sink()
  try_log_debug(0, "@try_perform_leave_center_out_analysis- COMPLETE")

  LCO_RUNNING = F

  return(list(lr=logreg_LCO_results_df))
}

##### ************************************************************************************************************************************************************
#### Local functions
##### ************************************************************************************************************************************************************

create_lco_train_test_folds <- function(df1, center_col_nm) {
  train_dfs <- list()
  test_dfs <- list()
  folds <- list()

  df1 = set_onehot_cols_in_df (df1, center_col_nm, onehot_enc_data(df1[,center_col_nm], col_nm = center_col_nm))
  cen_cols = grep_on_colnames(df1, sprintf("%s_cat_\\d+_",center_col_nm))
  centers_present_names <- umap(cen_cols, function(x){ if(all(df1[,x] == 0)) NULL else x })

  for(i in 1:len(centers_present_names)) {
    try_log_info("@create_lco_train_test_folds- Mapping Center %s -> to fold #%s;",
                 centers_present_names[i], i)
    CENTER_FOLD_MAPPING[[centers_present_names[i]]] = i
    FOLD_CENTER_MAPPING[[i]] = centers_present_names[i]
  }

  fold_cntr = 0
  for(cur_center in centers_present_names){
    fold_cntr = fold_cntr + 1
    cur_center_rows <- which(df1[,cur_center] == 1)
    folds[[fold_cntr]] = cur_center_rows
    train_dfs[[fold_cntr]] = df1[-cur_center_rows,] # all center rows, apart from the current one
    test_dfs[[fold_cntr]] = df1[cur_center_rows,] # only rows from current center
  }
  return(list(folds = folds, train_dfs =  train_dfs, test_dfs = test_dfs,
              FOLD_CENTER_MAPPING = FOLD_CENTER_MAPPING,
              CENTER_FOLD_MAPPING = CENTER_FOLD_MAPPING))
}


##### ************************************************************************************************************************************************************
#### Example usage
##### ************************************************************************************************************************************************************

RUN_CODE = F

if(RUN_CODE) {

  log_to_file = F
  logfile_prefix = "my-experiment"
  skip_external = F
  skip_internal = T # DO YOU WANT 10-CV ON EACH INTERNAL SET? DEFAULT = NO
  vselc_bs_agree_stren = 1.0
  df3 = datasets::infert

  df3 = rbind(df3, df3)
  df3 = rbind(df3, df3)
  df3$random_predictor = df3$case + sample( (1:100)/200, nrow(df3), replace = T)

  lco_res = try_perform_leave_center_out_analysis(df3, log_to_file,
                                                  logfile_prefix, vselc_bs_agree_stren,
                                                  skip_external_validations = skip_external,
                                                  skip_internal_validations = skip_internal,
                                                  center_col_nm = "spontaneous",
                                                  outcome_var = "case"
  )
  if(log_to_file)
    sink()

}





