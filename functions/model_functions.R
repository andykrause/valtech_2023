#***************************************************************************************************
#
#   Modeling Functions
#
#***************************************************************************************************

### MCE Functions ----------------------------------------------------------------------------------

mceWrapper <- function(data_df,
                       exp_par,
                       model_par,
                       ...){

  # Set up capture lists
  mce_results <- comp_results <- mcelvmce_results <- NULL

  # Start loop through time periods
  for (tp in exp_par$time_periods){

    cat('\n..MCE Estimator, Time Period ending:', tp, '\n')

    data_ <- prepareData(x_df = data_df,
                         submarket = model_par$submarket,
                         max_period = tp,
                         test_length = exp_par$testing_periods,
                         train_length = exp_par$training_periods,
                         cat_feat = model_par$cat_feat,
                         cat_fix = exp_par$cat_fix,
                         cat_lim = exp_par$cat_lim)

    # Estimate MCE Model
    mce_ <- purrr::map(.x = data_,
                       .f = medCompEstimator,
                       model_par = model_par)

    # Extract predictions and comps
    mce_pdf <- lapply(mce_, function(x) x$pred) %>%
      dplyr::bind_rows()

    mce_cdf <- lapply(mce_, function(x) x$comps) %>%
      dplyr::bind_rows()

    # Store Results
    mce_results[[length(mce_results) + 1]] <- mce_pdf
    comp_results[[length(comp_results) + 1]] <- mce_cdf

  }

  # Summarize Errors and Save
  mce_rdf <- mce_results %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(., data_df %>%
                       dplyr::select(trans_id, price),
                     by = 'trans_id') %>%
    calcError(.)

  mce_comps <- comp_results %>%
    dplyr::bind_rows()

  return(list(
    values = mce_rdf,
    comps = mce_comps
  ))
}

#'
#' Estimate value via the median value of the comps
#'
#' Naive estimator that uses the median value of the comps to created a predicted value,
#' Effectively a KNN regression
#'
#' @param x_ A list of train/test data from within prepareData()
#' @param features [c('latitude', 'longitude')] A list of features to use to determine
#' proximity (select comps)
#' @param k [5] Number of comps to select
#' @param algorithm ['kd_tree'] Algorithm to use to find comps
#' @param ... Additional arguments
#' @return list of 1) a data.frame with predicted values and 2) a data.frame with comp ids
#' @importFrom dplyr arrange left_join group_by summarize
#' @export

medCompEstimator <- function(x_,
                             model_par,
                             test_data = 'test',
                             ...){

  # Ensure test is ordered by unique id
  x_[[test_data]] <- x_[[test_data]] %>%
    dplyr::arrange(trans_id)

  # Find Comps
  comps_df <- compFinder(train_df = x_$train,
                         test_df = x_[[test_data]],
                         features = model_par$comp_feat,
                         k = model_par$k,
                         algorithm = model_par$algorithm,
                         ...)

  # Add prices of comps
  comps_df <- comps_df %>%
    dplyr::left_join(x_$train %>%
                       dplyr::select(comp_id = trans_id, adjusted_value = adjusted_price),
                     by = 'comp_id')

  # Create prediction
  pred_df <- comps_df %>%
    dplyr::group_by(trans_id) %>%
    dplyr::summarize(pred = model_par$valuefunction(adjusted_value))

  # Export both
  list(pred = pred_df,
       comps = comps_df)

}

#'
#' Comps finding engine
#'
#' Routine to locate comparable
#'
#' @param train_df Training data
#' @param test_df Test or query data
#' @param features [c('latitude', 'longitude')] A list of features to use to determine
#' proximity (select comps)
#' @param k [5] Number of comps to select
#' @param algorithm ['kd_tree'] Algorithm to use to find comps
#' @param ... Additional arguments
#' @return Data.frame of subject:comp pairs
#' @importFrom FNN knnx.index
#' @export

compFinder <- function(train_df,
                       test_df,
                       features = c('latitude', 'longitude'),
                       k = 5,
                       algorithm = 'kd_tree',
                       ...){

  # Get indexes of k nearest observation in the training data
  results <- FNN::knnx.index(data = train_df[, features],
                             query = test_df[, features],
                             k = k,
                             algorithm = algorithm)

  # Convert into a long data.frame with each row showing the subject:comp pairing, ordered by subject
  data.frame(trans_id = sort(rep(test_df$trans_id, k)),
             comp_id = train_df$trans_id[as.vector(t(results))])

}

### CMA Functions ------------------------------------------------------------------------

#'
#' CMA model
#'
#' Wrapper to calculate values via a CMA approach
#'
#' @param x_ List of train and test data (from prepareData())
#' @param mod_spec Adjustment model specification
#' @param comps [NULL] pre-selected comps for each test observation.  Optional
#' @param ... Additional arguments
#' @return List of 1) "pred" prediction data frame; 2) "comps" data.frame of comps with
#' adjustment values
#' @importFrom dplyr left_join select
#' @export


cmaWrapper <- function(data_df,
                       comps_df,
                       exp_par,
                       model_par){

  # Set up capture lists
  cma_results <- comp_results <- NULL

  # Start loop through time periods
  for (tp in exp_par$time_periods){

    cat('\n..CMA Estimator, Time Period ending:', tp, '\n')

    data_ <- prepareData(x_df = data_df,
                         submarket = model_par$submarket,
                         max_period = tp,
                         test_length = exp_par$testing_periods,
                         train_length = exp_par$training_periods,
                         cat_feat = model_par$cat_feat,
                         cat_fix = exp_par$cat_fix,
                         cat_lim = exp_par$cat_lim)

    # Estimate MCE Model
    cma_ <- purrr::map(.x = data_,
                       .f = cmaModel,
                       mod_spec = model_par$adj_spec,
                       comps = comps_df)

    # Extract predictions and comps
    cma_pdf <- lapply(cma_, function(x) x$pred) %>%
      dplyr::bind_rows()

    cma_cdf <- lapply(cma_, function(x) x$comps) %>%
      dplyr::bind_rows()

    # Store Results
    cma_results[[length(cma_results) + 1]] <- cma_pdf
    comp_results[[length(comp_results) + 1]] <- cma_cdf

  }

  # Summarize Errors and Save
  cma_rdf <- cma_results %>%
    dplyr::bind_rows() %>%
    calcError(.)

  cma_comps <- comp_results %>%
    dplyr::bind_rows()

  return(list(
    values = cma_rdf,
    comps = cma_comps
  ))
}

#'
#' Wrapper to calculate values via a CMA approach
#'
#' @param train_df training data
#' @param test_df test data
#' @param comp_df Comps for each test observation.
#' @param mod_spec Model specification for the adjustment model
#' @param ... Additional arguments
#' @return List of 1) "pred" prediction data frame; 2) "comps" data.frame of comps with
#' adjustment values
#' @importFrom dplyr left_join group_by summarize
#' @importFrom stats median
#' @export

compAdjModel <- function(train_df,
                         test_df,
                         comp_df,
                         mod_spec,
                         ...){

  mod <- lm(mod_spec, data = train_df)
  coefs <- mod$coefficients
  coefs <- coefs[!is.na(coefs)]

  test_ids <- match(comp_df$trans_id, test_df$trans_id)
  comp_ids <- match(comp_df$comp_id, train_df$trans_id)

  comp_df$unadj_value <- train_df$adjusted_price[comp_ids]
  for(k in names(coefs)[2:length(coefs)]){
    comp_df[[paste0(k, '_adj')]] <- ((test_df[test_ids,k] - train_df[comp_ids, k]) *
                                       as.numeric(coefs[k]))
  }
  comp_df['total_adj'] <- apply(comp_df[,4:ncol(comp_df)], 1, sum)
  comp_df$adjusted_value <- comp_df$unadj_value * (1+comp_df$total_adj)

  return(
    list(
      pred = test_df %>%
        dplyr::left_join(.,
                         comp_df %>%
                           dplyr::group_by(trans_id) %>%
                           dplyr::summarize(pred = stats::median(adjusted_value)),
                         by = 'trans_id') %>%
        dplyr::select(trans_id, price, pred),
      comps = comp_df))
}


cmaModel <- function(x_,
                     mod_spec,
                     comps,
                     ...){

  comps_df <- x_$test %>%
      dplyr::left_join(., comps, by = 'trans_id') %>%
      dplyr::select(trans_id, comp_id)


  compAdjModel(train_df = x_$train,
               test_df = x_$test,
               comp_df = comps_df,
               mod_spec = mod_spec,
               ...)
}


nnEmbedder <- function(x_,
                       full_df,
                       emb_feature,
                       emb_name = 'x',
                       resp_feature = 'adjusted_price',
                       emb_size = 5,
                       epochs = 10,
                       batch_size = round(nrow(x_$train) / 10, 0)){

  ## Set up input data for embedding
  full_df <- full_df[order(full_df[[emb_feature]]), ]
  input_df = data.frame(embfeat = as.numeric(as.factor(full_df[[emb_feature]])),
                        scaled_resp = scale(log(full_df[[resp_feature]]))) 
  
  ## Set up embedding space and model
  embedding_size <- emb_size
  emb_model <- keras_model_sequential()
  emb_model %>%
    layer_embedding(input_dim = length(unique(input_df$embfeat)) + 1,
                    output_dim = embedding_size,
                    input_length = 1,
                    name="embedding") %>%
    layer_flatten()  %>%
    layer_dense(units=40, activation = "relu") %>%
    layer_dense(units=10, activation = "relu") %>%
    layer_dense(units=1)

  emb_model %>%
    compile(loss = "mse", optimizer = "sgd", metric="accuracy")

  ## Fit model
  emb_model %>%
    fit(x = as.matrix(input_df$embfeat),
        y = as.matrix(input_df$scaled_resp),
        epochs = epochs,
        batch_size = batch_size)

  ## Convert embedding weights into a data.frame with proper names
  emb_layer <- get_layer(emb_model, "embedding")
  emb_df <- data.frame(emb_layer$get_weights()[[1]])
  emb_df$name <- c('no_embedding_feature', sort(unique(full_df[[emb_feature]])))
  names(emb_df) <- c(paste0(emb_name, '_', 1:emb_size), emb_feature)

  emb_df
}
