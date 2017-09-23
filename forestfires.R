library('dplyr')
library('ggplot2')

ff <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")

# Filter and take a log of area
ff <- ff %>%
  filter(area > 0) %>%
  mutate(logArea = log(area))


# Create a histogram of logArea
ff %>%
  filter(area > 0) %>%
  mutate(logArea = log(area)) %>%
  ggplot(aes(x = logArea)) +
  geom_histogram()

# Create boxplots
ggplot(ff, aes(x = as.factor(X), y=logArea)) +
  geom_boxplot()