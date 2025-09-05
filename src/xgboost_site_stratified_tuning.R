#' @title XGboost model training with site-stratified hyperparameter tuning
#'
#' @description
#' This script with take in training/validation data, hyper parameters, site info on train/val splits and column info and then train/hypertune XGBoost models using caret
#'
#' @param data Data frame containing the features and target variable. This data should be normalized/standardized already and should not contain any testing data
#'
#' @param tune_grid expanded grid of hyperparameters to tune over. Please double check with `caret` and `xgboost` packages before inputting hyperparameters.
#' If NULL, a default grid will be used.
#'
#' @param target_col Character string indicating column of the target variable to be predicted.
#'
#' @param site_col Character string indicating column of the site ids to retrieve data for.
#'
#' @param fold_ids dataframe containing the fold number and a list of the corresponding validation site ids
#'
#' @param save_fold_models logical for whether the model itself should be saved
#'
#' @return A caret model object containing the trained XGBoost model for each fold and performance metrics (train/val RMSE, MAE, Bias).
#' Best performing models determined by taking the top 10 val RMSE scores and then selecting the one with the smallest train-val gap.
#'
#'
#' @examples
#' folds <- tibble(
#'   fold = 1:4,
#'   val_ids = list(val_set_1, val_set_2, val_set_3, val_set_4)
#'  )

#' model <- xgboost_site_stratified_tuning(
#'    data = train_val %>% select(TOC, id, any_of(features)),
#'    tune_grid = expand.grid(
#'      nrounds = 10000,
#'      max_depth = c(2, 3, 4),
#'      eta = c(0.005, 0.01, 0.1),
#'      gamma = c(0.6, 0.8),
#'      colsample_bytree = c(0.5, 0.8),
#'      min_child_weight = c(2,4, 6),
#'      subsample = c(0.5, 0.8)
#'    ),
#'  target_col = "TOC",
#'  site_col = "id",
#' fold_ids =  folds
#')

xgboost_site_stratified_tuning <- function(data, target_col = "target", site_col = "site",
                                           tune_grid = NULL, fold_ids,
                                           save_fold_models = TRUE) {

  # Create default grid if not provided
  if (is.null(tune_grid)) {
    tune_grid <- expand.grid(
      nrounds = 10000,
      max_depth = c(2, 3, 4),
      eta = c(0.005, 0.01, 0.1),
      gamma = c(0.4, 0.6),
      colsample_bytree = c(0.5, 0.8),
      min_child_weight = c(2, 4, 6),
      subsample = c(0.5, 0.8)
    )
  }

  #Set up the fold indices
  n_folds <- nrow(fold_ids)

  fold_indices <- list()
  fold_indices_out <- list()

  for (i in 1:n_folds) {
    val_sites <- fold_ids$val_ids[[i]]

    # Indices for validation
    val_idx <- which(data[[site_col]] %in% val_sites)
    # Indices for training (complement)
    train_idx <- setdiff(1:nrow(data), val_idx)

    fold_indices[[i]] <- train_idx
    fold_indices_out[[i]] <- val_idx
  }

  # Prepare formula
  features <- setdiff(names(data), c(target_col, site_col))

  # Train model
  cat("Starting site-stratified hyperparameter tuning...\n")

  fold_models <- list()
  fold_results <- list()
  best_params <- list()

  for (i in seq_along(fold_indices)) {
    cat(paste0("Tuning fold ", i, " of ", n_folds, "...\n"))

    perf <- data.frame()

    # Split data
    train_data <- data[fold_indices[[i]], ]
    val_data   <- data[fold_indices_out[[i]], ]

    #set up data for xgb

    # weights: 5 if target > 5, else 1
    w_train <- ifelse(train_data[[target_col]] > 5, 7.5, 1)
    w_val   <- ifelse(val_data[[target_col]] > 5, 7.5, 1)

    # set up data for xgb
    dtrain <- xgb.DMatrix(
      data = as.matrix(train_data[, features]),
      label = train_data[[target_col]],
      weight = w_train
    )

    dval <- xgb.DMatrix(
      data = as.matrix(val_data[, features]),
      label = val_data[[target_col]],
      weight = w_val
    )


    # Set train vs val for early stopping
    watchlist <- list(train = dtrain, eval = dval)

    for (j in 1:nrow(tune_grid)) {

      #setup parameters for tune
      params <- list(
        objective        = "reg:squarederror",
        eval_metric      = "rmse",
        eta              = tune_grid$eta[j],
        gamma            = tune_grid$gamma[j],
        alpha            = tune_grid$alpha[j],
        lambda           = tune_grid$lambda[j],
        max_depth        = tune_grid$max_depth[j],
        subsample        = tune_grid$subsample[j],
        colsample_bytree = tune_grid$colsample_bytree[j],
        min_child_weight  = tune_grid$min_child_weight[j]
      )

      #train model with hyper parameters
      model_ij <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = tune_grid$nrounds[j],
        watchlist = watchlist,
        #change early stopping rounds based on eta (smaller eta, larger rounds)
        early_stopping_rounds = ifelse(tune_grid$eta[j] >= 0.1, 250,
                                       ifelse(tune_grid$eta[j] >= 0.01, 500,
                                              1000)),
        print_every_n = 1000,
        verbose = 0
      )

      # Predictions on validation set
      preds_val <- predict(model_ij, dval)
      rmse_val  <- RMSE(preds_val, val_data[[target_col]])
      mae_val   <- MAE(preds_val, val_data[[target_col]])
      bias_val  <- bias(preds_val, val_data[[target_col]])

      # Predictions on training set (to measure overfitting)
      preds_train <- predict(model_ij, dtrain)
      rmse_train  <- RMSE(preds_train, train_data[[target_col]])
      mae_train   <- MAE(preds_train, train_data[[target_col]])
      bias_train  <- bias(preds_train, train_data[[target_col]])

      perf <- rbind(perf, data.frame(
        fold       = i,
        grid_id    = j,
        best_iter  = model_ij$best_iteration,  # from early stopping
        rmse_val   = rmse_val,
        mae_val    = mae_val,
        bias_val   = bias_val,
        rmse_train = rmse_train,
        mae_train  = mae_train,
        bias_train = bias_train,
        diff       = abs(rmse_val - rmse_train)
      ))


      # Save model temporarily
      fold_models[[paste0("fold", i, "_grid", j)]] <- model_ij
    }

    p <- ggplot(perf) +
      geom_density(aes(x = rmse_val, color = "Val")) +
      geom_density(aes(x = rmse_train, color = "Train")) +
      labs(title = paste("Fold", i, "Val & Train RMSE by Grid ID"),
           x = "RMSE", y = "density", color = "Group") +
      theme_minimal()

    plot(p)

    # --- Pick top 10 by validation RMSE ---
    top10 <- perf[order(perf$rmse_val), ][1:10, ]

    # --- From those, choose 6 smallest train-val gap ---
    best_rows  <- top10[order(top10$diff), ][1:6,]


    eval_plots <- list()

    for (k in 1:nrow(best_rows)) {
      fold_id <- best_rows$fold[k]
      grid_id <- best_rows$grid_id[k]

      model_key <- paste0("fold", fold_id, "_grid", grid_id)

      eval_log <- fold_models[[model_key]][["evaluation_log"]]
      params <- fold_models[[model_key]][["params"]]
      #remove objective, eval metric, validate parameters
      params <- params[!names(params) %in% c("objective", "eval_metric", "validate_parameters")]

      # collapse params into a single string
      param_text <- paste(
        names(params), "=", unlist(params),
        collapse = "\n"
      )
      # Get best iteration and corresponding RMSE
      best_iter <- which.min(eval_log$eval_rmse)
      best_rmse <- eval_log$eval_rmse[best_iter]

      plot <-
        ggplot(eval_log, aes(x = iter)) +
        geom_line(aes(y = train_rmse, color = "Train RMSE")) +
        geom_line(aes(y = eval_rmse, color = "Eval RMSE")) +
        geom_vline(xintercept = best_iter, linetype = "dashed", color = "blue") +
        labs(title = paste("Grid", grid_id, ":Best Iter:", best_iter, "Val RMSE:", round(best_rmse, 4)),
             x = "Iteration", y = "RMSE", color = "Metric") +
        theme_minimal()+
        annotate(
          "text",
          x = max(eval_log$iter) * .5,   # position on x-axis
          y = max(c(eval_log$train_rmse, eval_log$eval_rmse)) * 0.9,  # position on y-axis
          label = param_text,
          hjust = 0,
          vjust = 1,
          size = 4,
          color = "black"
        )
      eval_plots[[k]] <- plot

    }

    #Create plot with top 6 eval plots
    eval_grid <- wrap_plots(eval_plots, nrow = 2, ncol = 3, common.legend = TRUE) +
      theme(legend.position = "bottom")

    print(eval_grid)

    #Ask user for best grid
    best_choice <- as.integer(readline(prompt = "Enter the grid ID of the best model from the top 5 (or type 0 to select the one with smallest train-val gap): "))

    #Select user's choice or default to smallest train-val gap
    if (best_choice %in% best_rows$grid_id) {
      best_row <- best_rows[best_rows$grid_id == best_choice, ]
    } else {
      best_row <- best_rows[1, ]  # Default to smallest train-val gap
      cat("Invalid choice. Defaulting to model with smallest train-val gap.\n")
    }

    model_key <- paste0("fold", i, "_grid", best_row$grid_id)
    best_params <- fold_models[[model_key]][["params"]]%>%
      as_tibble%>%
      select(-c(objective, eval_metric, validate_parameters))
    best_params[[i]] <- best_params


    # Save only the best model for this fold
    fold_results[[i]] <- list(
      model = fold_models[[paste0("fold", i, "_grid", best_row$grid_id)]],
      perf  = best_row
    )

    cat("Best params for fold", i, ":\n")
    print(best_params[[i]])
    cat("\n Model performance:\n")
    print(fold_results[[i]]$perf)
  }

  return(fold_results)
}



