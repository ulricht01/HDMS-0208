# Na�ten� knihoven
library(dplyr)
library(stats)
library(ggplot2)

# Na�ten� dat z CSV souboru
mtcars <- read.csv('mtcars.csv')

# Vytvo�en� nov�ho DataFrame s vybran�mi sloupci
mtcars_short <- mtcars %>%
  select(mpg, hp, wt, cyl, disp, drat, qsec)

# Definice stup�� mocnin
degrees <- c(1, 2, 3, 4)

# Seznam sloupc�, na kter� budete aplikovat mocniny
columns_to_power <- c("hp", "wt", "cyl", "disp", "drat", "qsec")

# Vytvo�en� sloupc� s mocninami
for (col in columns_to_power) {
  for (degree in degrees) {
    mtcars_short <- mtcars_short %>%
      mutate(!!paste0(col, degree) := get(col) ^ degree)
  }
}

# Seznam prom�nn�ch
variables <- c('hp', 'wt', 'cyl', 'disp', 'drat', 'qsec')

# Funkce pro v�po�et R^2
calculate_r2 <- function(degree, variable, data) {
  if (length(unique(data[[variable]])) >= degree) {
    poly_variable <- poly(data[[variable]], degree, raw = TRUE)
    model <- lm(mpg ~ poly_variable, data = data)
    return(summary(model)$r.squared)
  } else {
    return(NA)
  }
}

# Data frame pro ukl�d�n� v�sledk�
results <- data.frame()

# Pro ka�dou prom�nnou
for (variable in variables) {
  r2_values <- numeric()
  
  # Pro r�zn� stupn� polynomu
  for (degree in 1:4) {
    r2 <- calculate_r2(degree, variable, mtcars_short)
    if (!is.na(r2)) {
      r2_values <- c(r2_values, r2)
    }
  }
  
  # Vytvo�en� data frame pro graf
  df <- data.frame(Degree = 1:length(r2_values), R2 = r2_values, Variable = variable)
  results <- rbind(results, df)
}

# Vykreslen� grafu
ggplot(results, aes(x = Degree, y = R2, color = Variable)) +
  geom_line() +
  geom_point() +
  labs(title = "R^2 vs. Polynomial Degree",
       x = "Polynomial Degree",
       y = "R^2") +
  facet_wrap(~Variable) +
  theme_minimal()

X <- mtcars_short %>%
  mutate(hp2 = hp^2, wt2 = wt^2, cyl2 = cyl^2, disp2 = disp^2, drat2 = drat^2, qsec2 = qsec^2,
         qsec3 = qsec^3, qsec4 = qsec^4)

# Vytvo�en� regresn�ho modelu
model <- lm(mpg ~ ., data = X)

# Predikce na z�klad� modelu
y_pred <- predict(model, X)


# Vytvo�en� matice X s kvadratick�mi a kubick�mi �leny
X <- mtcars_short %>%
  mutate(hp2 = hp^2, wt2 = wt^2, cyl2 = cyl^2, disp2 = disp^2, drat2 = drat^2, qsec2 = qsec^2,
         qsec3 = qsec^3, qsec4 = qsec^4)

# P�id�n� konstanty pro intercept
X <- cbind(1, X)

# Vytvo�en� regresn�ho modelu
model <- lm(mpg ~ ., data = X)

# Predikce na z�klad� modelu
y_pred <- predict(model, X)

# V�po�et Mean Squared Error (MSE)
mse <- mean((mtcars_short$mpg - y_pred)^2)

# V�po�et Determination Index (R^2)
r2 <- 1 - sum((mtcars_short$mpg - y_pred)^2) / sum((mtcars_short$mpg - mean(mtcars_short$mpg))^2)

# V�po�et Akaike Information Criterion (AIC)
aic <- AIC(model)

# V�po�et Bayesian Information Criterion (BIC)
bic <- BIC(model)

# V�stup
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Determination Index (R^2):", r2, "\n")
cat("Akaike Information Criterion (AIC):", aic, "\n")
cat("Bayesian Information Criterion (BIC):", bic, "\n")