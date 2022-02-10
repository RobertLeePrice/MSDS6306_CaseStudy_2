library(Hmisc)

#' Function to create a dataframe of pair-wise Pearson correlations, n,
#' and p-values. 
#' 
#' Args 
#'  df: dataframe of numeric features
#'  alpha: significance level for p-value
#'  
#' Returns
#'  formatted dataframe
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