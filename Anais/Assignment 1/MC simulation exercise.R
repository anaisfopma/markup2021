# Specify samplesize, mean, and SD
SS <- 5000
mu <- 5
SD <- 1

# Set a seed
set.seed(456)

# Take 100 samples
library(plyr)
samples <- rlply(100, rnorm(SS, mu, SD))

# Create function to calculate the performance measures
info <- function(x){ 
  M <- mean(x)
  DF <- length(x) - 1
  SE <- 1 / sqrt(length(x))
  INT <- qt(.975, DF) * SE
  return(c(M, M - 0, SE, M - INT, M + INT))
}
format <- c("Mean" = 0, "Bias" = 0, "Std.Err" = 0, "Lower" = 0, "Upper" = 0)

# Create a pipe to apply the function to the samples
library("magrittr")
results <- samples %>%
  vapply(., info, format) %>%
  t()

# Calculate the coverage
results <- results %>%
  as.data.frame() %>%
  mutate(Covered = Lower < mu & mu < Upper)

# Obtain the estimates by taking the column means
colMeans(results)

# The coverage is 96%, so the CI contains the population value 96 out of 100 times

# Put the results of only the non-coverers in a table
table <- results[!results$Covered, ]
library(knitr)
library(kableExtra)
kable(table)

kable(table, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F,
                position = "float_right")

# Make a plot of the results, distinguishing between coverers and non-coverers
library(ggplot2)
limits <- aes(ymax = results$Upper, ymin = results$Lower)
ggplot(results, aes(y=Mean, x=1:100, colour = Covered)) + 
  geom_hline(aes(yintercept = 0), color = "dark grey", size = 2) + 
  geom_pointrange(limits) + 
  xlab("Simulations 1-100") +
  ylab("Means and 95% Confidence Intervals")


