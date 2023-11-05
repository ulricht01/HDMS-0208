library(VIM)
library(ggplot2)
library(mice)

df_sleep <- read.csv('dataset_sleep.csv')
df_sleep$max_life_span <- as.numeric(df_sleep$max_life_span)
df_sleep$gestation_time <- as.numeric(df_sleep$gestation_time)
df_sleep$total_sleep <- as.numeric(df_sleep$total_sleep)

#Napsat funkci, která pøijímá jako parametry data, která obsahují chybìjící hodnoty. 
#Funkce vrátí nejlepší data za mse kriteriem. Probrat metody: 
#pmm, midastouch, cart, rf, norm, norm.boot, norm.predict, lasso.norm, quadratic.  

best_imputation <- function(data, m = 5, maxit = 5, seed = 123, methods) {
  best_mse <- Inf
  best_data <- NULL
  best_method <- NULL
  
  for (method in methods) {
    set.seed(seed)
    imputed <- mice(data, method = method, m = m, maxit = maxit, seed = seed)
    completed <- complete(imputed, 1) # Choosing the first imputed dataset for simplicity
    
    model <- lm(total_sleep ~ poly(max_life_span, 2) + poly(gestation_time, 2), data = completed)
    mse <- mean(model$residuals^2)
    
    if (mse < best_mse) {
      best_mse <- mse
      best_data <- completed
      best_method <- method
    }
  }
  
  cat("Best Method: ", best_method, "\nMSE: ", round(best_mse, 2), "\n")
  return(list(best_data = best_data, best_method = best_method, mse = best_mse))
}

methods_to_try <- c("pmm", "midastouch", "cart", "rf", "norm", "norm.boot", "norm.predict", "lasso.norm")
result <- best_imputation(df_sleep, methods = methods_to_try)
