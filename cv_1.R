#1. Histogram

library(ggplot2)

mtcars <- read.csv('C:/Users/tomik/OneDrive/Plocha/Škola/0208/mtcars.csv')

ggplot(data=mtcars, aes(x = hp)) +
  geom_histogram(bins =  20, fill = 'red', color = 'white', ) +
  labs(x = 'Horsepower', y = 'Count') + 
  ggtitle('Histogram of Horsepower')

ggplot(data=mtcars, aes(x = mpg)) +
  geom_histogram(bins= 20, fill = 'black', color = 'white', ) +
  labs(x = 'Miles per Gallon', y = 'Count') + 
  ggtitle('Histogram of Miles per Gallon')

ggplot(data=mtcars, aes(x = wt)) +
  geom_histogram(bins = 20, fill = 'cyan', color = 'white', ) +
  labs(x = 'Weight(t)', y = 'Count') + 
  ggtitle('Histogram of Weight')


#Scatterplot

plot(x = mtcars$hp, y = mtcars$mpg, main = 'Horsepower vs Miles per Gallon', xlab = 'Horsepower', ylab = 'Miles per Gallon', col = 'blue', pch = 16)

plot(x = mtcars$mpg, y = mtcars$wt, main = 'Miles per Gallon vs Weight', xlab = 'Miles per Gallon', ylab = 'Weight', col = 'blue', pch = 16)

plot(x = mtcars$wt, y = mtcars$hp, main = 'Weight vs Horsepower', xlab = 'Weight', ylab = 'Horsepower', col = 'blue', pch = 16)


#3. Boxplot

boxplot(mpg ~ cyl, data = mtcars, col = "green", main = "Miles per Gallon Grouped by Cylinders", xlab = "Cylinders", ylab = "Miles per Gallon")

boxplot(hp ~ cyl, data = mtcars, col = "purple", main = "Horsepower Grouped by Cylinders", xlab = "Cylinders", ylab = "Horsepower")

boxplot(wt ~ cyl, data = mtcars, col = "pink", main = "Weight Grouped by Cylinders", xlab = "Cylinders", ylab = "Weight")


