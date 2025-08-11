# Load necessary libraries
library(ggplot2)
library(SDAResources)
library(SDaA)

# Ensure dataset is available
head(heart_2020_cleaned)
dim(heart_2020_cleaned)

# Define population size (N) and variable of interest (e.g., SleepTime)
N = dim(heart_2020_cleaned)[1]
SleepTime_data = heart_2020_cleaned$SleepTime  # Extract SleepTime variable

# Check for missing values
sum(is.na(SleepTime_data))  # Count missing values in SleepTime

#Population Parameters based off SleepTime
summary(heart_2020_cleaned)
summary(SleepTime_data)
var(SleepTime_data)
standev2 = sqrt(var(SleepTime_data))
standev2

# Histogram of population BMI distribution
hist(SleepTime_data, breaks = 40, xlab = "SleepTime", main="Population Sleep Time Distribution")

# SIMPLE RANDOM SAMPLING WITHOUT REPLACEMENT (SRSWOR)
set.seed(99)  # Ensure reproducibility
n = 5000  # Sample size
sample_ind = sample(1:N, n, replace = FALSE)
sample_SRSWOR = SleepTime_data[sample_ind]
count(SleepTime_data)
# Check for missing values in sample
sum(is.na(sample_SRSWOR))

# Histogram of SRSWOR sample
ggplot(data.frame(sample_SRSWOR), aes(x = sample_SRSWOR)) + 
  geom_histogram(color = "white", fill = topo.colors(100)[2], bins = 15) + 
  xlab("BMI") + ylab("Frequency") + 
  ggtitle("Simple Random Sampling Without Replacement (SRSWOR)") + 
  theme_bw()

# Compute sample mean and variance
ybarWOR2 = mean(sample_SRSWOR, na.rm = TRUE)
varWOR2  = var(sample_SRSWOR, na.rm = TRUE)
syWOR2   = sqrt(var(sample_SRSWOR, na.rm = TRUE))
vybarWOR2 = (1 - n/N) * syWOR^2 / n  # Unbiased variance estimator
ybarWOR2
varWOR2
syWOR2
vybarWOR2

# SIMPLE RANDOM SAMPLING WITH REPLACEMENT (SRSWR)
set.seed(99)
sample_ind0 = sample(1:N, n, replace = TRUE)
sample_ind = unique(sample_ind0)
length(sample_ind)  # Check unique samples selected

sample_SRSWR = SleepTime_data[sample_ind]
sum(is.na(sample_SRSWR))  # Check for missing values

# Histogram of SRSWR sample
ggplot(data.frame(sample_SRSWR), aes(x = sample_SRSWR)) + 
  geom_histogram(color = "white", fill = topo.colors(100)[2], bins = 15) + 
  xlab("SleepTime") + ylab("Frequency") + 
  ggtitle("Simple Random Sampling With Replacement (SRSWR)") + 
  theme_bw()

# Compute sample mean and variance
ybarWR2 = mean(sample_SRSWR, na.rm = TRUE)
syWR2   = sqrt(var(sample_SRSWR, na.rm = TRUE))
mWR2    = length(sample_ind)
vybarWR2 = (1/mWR - 1/N) * syWR^2  # Unbiased variance estimator
ybarWR2
syWR2
mWR2
vybarWR2

# KEEPING DUPLICATES IN SRSWR
sample_SRSWR2 = SleepTime_data[sample_ind0]
sum(is.na(sample_SRSWR2))  # Check for missing values

# Histogram of SRSWR sample with duplicates
ggplot(data.frame(sample_SRSWR2), aes(x = sample_SRSWR2)) + 
  geom_histogram(color = "white", fill = topo.colors(100)[2], bins = 15) + 
  xlab("SleepTime") + ylab("Frequency") + 
  ggtitle("Simple Random Sampling With Replacement (Duplicates Kept)") + 
  theme_bw()

# Compute sample mean and variance
ybar2 = mean(sample_SRSWR2, na.rm = TRUE)
sy2   = sqrt(var(sample_SRSWR2, na.rm = TRUE))
