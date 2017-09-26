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

# Create correlation matrix
numericFF <- ff %>%
  select(-X, -Y, -month, -day, -area)

M <- round(cor(numericFF), 2)

# Recode the month variable
#forestfires$season <- rep("spring", 270)

for (i in 1:270){
  if(ff$month[i] %in% c("dec", "jan", "feb"))
      ff$season[i] <- "winter"
  else if (ff$month[i] %in% c("sep", "oct", "nov"))
      ff$season[i] <- "fall"
  else if (ff$month[i] %in% c("jun", "jul", "aug"))
      ff$season[i] <- "summer"
  else
      ff$season[i] <- "spring"
}

# Create a histogram to check the distribution of values faceted by season.
ggplot(ff, aes(x = logArea)) +
  geom_histogram() + 
  facet_grid(~as.factor(season))

# Plot scatter plots
ggplot(ff, aes(x = FFMC, y = logArea)) +
  geom_point()

# Extract only numeric variables
nFF <- ff %>%
  select(-X, -Y, -month, -day, -season, -area)


# Regression 

ggplot(forestfires, aes(x=FFMC, y=logArea)) +
  geom_point() +
  geom_abline(intercept = mean(logArea), slope = 0) +
  geom_smooth(method="lm", se=FALSE)

ggplot(forestfires, aes(x=FFMC, y=logArea)) +
  geom_point() +
  geom_abline(intercept = 3.54, slope = -0.019)

  ggplot(forestfires, aes(x=FFMC, y=logArea)) +
    geom_point() +
    geom_smooth(method = "lm", se=FALSE)
  
#### APPROACH 2: log(area + 1) then remove zeros
