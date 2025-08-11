# Load necessary libraries
library(ggplot2)
library(SDAResources)
library(SDaA)

# Ensure dataset is available
head(heart_2020_cleaned)
dim(heart_2020_cleaned)

# Define population size (N) and variable of interest (e.g., BMI)
N = dim(heart_2020_cleaned)[1]
bmi_data = heart_2020_cleaned$BMI  # Extract BMI variable

# Check for missing values
sum(is.na(bmi_data))  # Count missing values in BMI

# Histogram of population BMI distribution
hist(bmi_data, breaks = 40, xlab = "BMI", main="Population BMI Distribution")

# SIMPLE RANDOM SAMPLING WITHOUT REPLACEMENT (SRSWOR)
set.seed(99)  # Ensure reproducibility
n = 5000  # Sample size
sample_ind = sample(1:N, n, replace = FALSE)
sample_SRSWOR = bmi_data[sample_ind]

# Check for missing values in sample
sum(is.na(sample_SRSWOR))

# Histogram of SRSWOR sample
ggplot(data.frame(sample_SRSWOR), aes(x = sample_SRSWOR)) + 
  geom_histogram(color = "white", fill = topo.colors(100)[2], bins = 15) + 
  xlab("BMI") + ylab("Frequency") + 
  ggtitle("Simple Random Sampling Without Replacement (SRSWOR)") + 
  theme_bw()

# Compute sample mean and variance
ybarWOR = mean(sample_SRSWOR, na.rm = TRUE)
syWOR   = sqrt(var(sample_SRSWOR, na.rm = TRUE))
vybarWOR = (1 - n/N) * syWOR^2 / n  # Unbiased variance estimator
ybarWOR
syWOR
vybarWOR

# SIMPLE RANDOM SAMPLING WITH REPLACEMENT (SRSWR)
set.seed(99)
sample_ind0 = sample(1:N, n, replace = TRUE)
sample_ind = unique(sample_ind0)
length(sample_ind)  # Check unique samples selected

sample_SRSWR = bmi_data[sample_ind]
sum(is.na(sample_SRSWR))  # Check for missing values

# Histogram of SRSWR sample
ggplot(data.frame(sample_SRSWR), aes(x = sample_SRSWR)) + 
  geom_histogram(color = "white", fill = topo.colors(100)[2], bins = 15) + 
  xlab("BMI") + ylab("Frequency") + 
  ggtitle("Simple Random Sampling With Replacement (SRSWR)") + 
  theme_bw()

# Compute sample mean and variance
ybarWR = mean(sample_SRSWR, na.rm = TRUE)
syWR   = sqrt(var(sample_SRSWR, na.rm = TRUE))
mWR    = length(sample_ind)
vybarWR = (1/mWR - 1/N) * syWR^2  # Unbiased variance estimator
ybarWR
syWR
mWR
vybarWR

# KEEPING DUPLICATES IN SRSWR
sample_SRSWR2 = bmi_data[sample_ind0]
sum(is.na(sample_SRSWR2))  # Check for missing values

# Histogram of SRSWR sample with duplicates
ggplot(data.frame(sample_SRSWR2), aes(x = sample_SRSWR2)) + 
  geom_histogram(color = "white", fill = topo.colors(100)[2], bins = 15) + 
  xlab("BMI") + ylab("Frequency") + 
  ggtitle("Simple Random Sampling With Replacement (Duplicates Kept)") + 
  theme_bw()

# Compute sample mean and variance
ybar = mean(sample_SRSWR2, na.rm = TRUE)
sy   = sqrt(var(sample_SRSWR2, na.rm = TRUE))
