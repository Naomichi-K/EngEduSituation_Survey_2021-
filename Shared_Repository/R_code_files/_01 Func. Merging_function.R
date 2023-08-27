# 1) Rate data merging
merge_rate_data <- function(df, row_combinations) {
  for (comb in row_combinations) {
    first_row <- comb[1]
    other_rows <- comb[-1]
    sample_size_first_row <- df[first_row, 1]
    total_sample_size <- sum(df[comb, 1])
    
    # Iterate over all the columns except the first one
    for (col in 2:ncol(df)) {
      # Check if the column contains rate data (values between 0 and 1)
      if (all(df[comb, col] >= 0 & df[comb, col] <= 1)) {
        weighted_rate <- sum(df[other_rows, 1] * df[other_rows, col]) / total_sample_size
        df[first_row, col] <- (sample_size_first_row * df[first_row, col] + weighted_rate * total_sample_size) / total_sample_size
      }
    }
  }
  return(df)
}

# 2) Count data merging
add_count_data <- function(df, row_combinations) {
  for (comb in row_combinations) {
    first_row <- comb[1]
    other_rows <- comb[-1]
    
    # Iterate over all the columns
    for (col in 1:ncol(df)) {
      # Check if the column contains count data (values outside the range 0 to 1)
      if (any(df[comb, col] < 0 | df[comb, col] > 1)) {
        df[first_row, col] <- sum(df[comb, col])
      }
    }
  }
  return(df)
}

# 3) Integrated function of the two functions above
merge_rate_count <- function(df, row_combinations) {
  # First, apply the rate data merging
  df <- merge_rate_data(df, row_combinations)
  
  # Then, apply the count data merging
  df <- add_count_data(df, row_combinations)
  
  return(df)
}

# 4) Normality check
Norm_check_vec <- function(vector,log_switch){
  if(log_switch==1){
    par(mfrow=c(1,2))
    hist(log(vector))
    qqnorm(log(vector),main=paste("p.value: ",round(shapiro.test(log(vector))$p.value,4)))
    qqline(log(vector))
    shapiro.test(log(vector))
  }else{
    par(mfrow=c(1,2))
    hist(vector)
    qqnorm(vector,main=paste("p.value: ",round(shapiro.test(vector)$p.value,4)))
    qqline(vector)
    shapiro.test(vector)  
  }
}

Norm_check_df <- function(data,log_switch,number){
  col_names <- colnames(data)
  if(log_switch==1){
    for(i in number){
      par(mfrow=c(1,2))
      hist(log(data[,i]),main=paste("log(",col_names[i],")"))
      qqnorm(log(data[,i]),main=paste("p.value: ",round(shapiro.test(log(data[,i]))$p.value,4)))
      qqline(log(data[,i]))
    }
  }else{
    for(i in number){
      par(mfrow=c(1,2))
      hist(data[,i],main=paste(col_names[i]))
      qqnorm(data[,i],main=paste("p.value: ",round(shapiro.test(data[,i])$p.value,4)))
      qqline(data[,i])
    }
  }  
}
