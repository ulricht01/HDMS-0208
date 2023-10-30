# Naètení knihoven
library(dplyr)
library(stats)
library(ggplot2)

# Naètení dat z CSV souboru
mtcars <- read.csv('mtcars.csv')

# Vytvoøení nového DataFrame s vybranými sloupci
mtcars_short <- mtcars %>%
  select(mpg, hp, wt, cyl, disp, drat, qsec)

# Definice stupòù mocnin
degrees <- c(1, 2, 3, 4)

# Seznam sloupcù, na které budete aplikovat mocniny
columns_to_power <- c("hp", "wt", "cyl", "disp", "drat", "qsec")

# Vytvoøení sloupcù s mocninami
for (col in columns_to_power) {
  for (degree in degrees) {
    mtcars_short <- mtcars_short %>%
      mutate(!!paste0(col, degree) := get(col) ^ degree)
  }
}

# Seznam promìnných
variables <- c('hp', 'wt', 'cyl', 'disp', 'drat', 'qsec')

# Funkce pro výpoèet R^2
calculate_r2 <- function(degree, variable, data) {
  if (length(unique(data[[variable]])) >= degree) {
    poly_variable <- poly(data[[variable]], degree, raw = TRUE)
    model <- lm(mpg ~ poly_variable, data = data)
    return(summary(model)$r.squared)
  } else {
    return(NA)
  }
}

# Data frame pro ukládání výsledkù
results <- data.frame()

# Pro každou promìnnou
for (variable in variables) {
  r2_values <- numeric()
  
  # Pro rùzné stupnì polynomu
  for (degree in 1:4) {
    r2 <- calculate_r2(degree, variable, mtcars_short)
    if (!is.na(r2)) {
      r2_values <- c(r2_values, r2)
    }
  }
  
  # Vytvoøení data frame pro graf
  df <- data.frame(Degree = 1:length(r2_values), R2 = r2_values, Variable = variable)
  results <- rbind(results, df)
}

# Vykreslení grafu
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

# Vytvoøení regresního modelu
model <- lm(mpg ~ ., data = X)

# Predikce na základì modelu
y_pred <- predict(model, X)


# Vytvoøení matice X s kvadratickými a kubickými èleny
X <- mtcars_short %>%
  mutate(hp2 = hp^2, wt2 = wt^2, cyl2 = cyl^2, disp2 = disp^2, drat2 = drat^2, qsec2 = qsec^2,
         qsec3 = qsec^3, qsec4 = qsec^4)

# Pøidání konstanty pro intercept
X <- cbind(1, X)

# Vytvoøení regresního modelu
model <- lm(mpg ~ ., data = X)

# Predikce na základì modelu
y_pred <- predict(model, X)

# Výpoèet Mean Squared Error (MSE)
mse <- mean((mtcars_short$mpg - y_pred)^2)

# Výpoèet Determination Index (R^2)
r2 <- 1 - sum((mtcars_short$mpg - y_pred)^2) / sum((mtcars_short$mpg - mean(mtcars_short$mpg))^2)

# Výpoèet Akaike Information Criterion (AIC)
aic <- AIC(model)

# Výpoèet Bayesian Information Criterion (BIC)
bic <- BIC(model)

# Výstup
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Determination Index (R^2):", r2, "\n")
cat("Akaike Information Criterion (AIC):", aic, "\n")
cat("Bayesian Information Criterion (BIC):", bic, "\n")