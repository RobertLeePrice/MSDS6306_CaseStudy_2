library(Hmisc)

#' Function to create a dataframe of pair-wise Pearson correlations, n,
#' and p-values. 
#' 
#' Args 
#'  df (data.frame): dataframe of numeric features
#'  alpha (float): significance level for p-value
#'  
#' Returns 
#'  (data.frame) formatted dataframe
format_corr_df <- function(df, alpha) { 
  
  # calculate correlation coefficients
  rcorr_df <- rcorr(as.matrix(df))
  mapped_df <- (map(rcorr_df, ~data.frame(.x)))
  
  # transform dataframe to a pair-wise, tidy structure
  res <- mapped_df %>% map(~rownames_to_column(.x, var="Measure_1")) %>%
    map(~pivot_longer(.x, -Measure_1, "Measure_2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(
      sig_p = ifelse(P < alpha, T, F),
      p_if_sig = ifelse(P < alpha, P, NA),
      r_if_sig = ifelse(P < alpha, r, NA))
  
  return (res)
}

#' Function that splits a dataframe into a training set and test set based 
#' on a specified split proportion
#'
#' Args
#'  df (data.frame): input dataframe to be split
#'  split_perc (int): percentage of data assigned to training set 
#'  
#' Returns
#' (list) a list of dataframes: one training and one testing dataset
train_test_split = function(df, split_perc){
  
  train_ind = sample(1:dim(df)[1], round(split_perc * dim(df)[1]))
  train = df[train_ind,]
  test = df[-train_ind,]
  
  return(list(train=train, test=test))
}

#' Calculate scoring metrics from a confusion matrix.
#' 
#' Args
#'  cm: confusion matrix
#'  
#' Returns  
calculate_scoring_metrics <- function(cm) {
  
  tp <- cm_test[1, 1]
  fn <- cm_test[1, 2]
  fp <- cm_test[2, 1]
  tn <- cm_test[2, 2]
  
  res <- data.frame(
    sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp),
    precision = tp / (tp + fp),
    accuracy = (tp + tn) / (tp + fn + fp + tn)
  )
  
  return (res)
}

