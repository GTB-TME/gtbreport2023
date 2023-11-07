# Function to calculate outcome percentages for plotting as stacked bars ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

calculate_outcomes_pct <- function(df, prefix_string) {
  
  # Simplify outcome variable names so can calculate %
  # Remove the prefix) so data frame has columns called coh, succ, fail, died, lost and c_neval
  
  names(df) <- str_replace(names(df), prefix_string, "")
  
  df <- mutate(df,
               `Treatment success` = ifelse(NZ(coh) > 0,
                                            succ * 100 / coh,
                                            NA),
               Failure = ifelse(NZ(coh) > 0,
                                fail * 100 / coh,
                                NA),
               Died = ifelse(NZ(coh) > 0,
                             died * 100 / coh,
                             NA),
               `Lost to follow-up` = ifelse(NZ(coh) > 0,
                                            lost * 100 / coh,
                                            NA),
               `Not evaluated` = ifelse(NZ(coh) > 0,
                                        c_neval * 100 / coh,
                                        NA))
  
  return(df)
  
}