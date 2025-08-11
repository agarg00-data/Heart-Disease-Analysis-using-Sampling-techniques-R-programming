#----- Chapter 3 PART I Stratified Sampling -----#
#heart_2020_cleaned <- read.csv("~/MATH 3430/heart_2020_cleaned.csv")
library(SDAResources)
#install.packages("dplyr")  Remove # and install dplyr if you haven't already
library(dplyr)

heart_2020_cleaned <- heart_2020_cleaned %>%
  mutate(Group = case_when(
    Sex == "Female" & HeartDisease == "Yes" ~ "Female with Heart Disease",
    Sex == "Male" & HeartDisease == "Yes" ~ "Male with Heart Disease",
    Sex == "Female" & HeartDisease == "No" ~ "Female with No Heart Disease",
    Sex == "Male" & HeartDisease == "No" ~ "Male with No Heart Disease",
    TRUE ~ "Other"
  ))
print(heart_2020_cleaned)
# forming a group of females with yes heart disease 
female_with_heart_disease <- heart_2020_cleaned %>% filter(Group == "Female with Heart Disease")
print(female_with_heart_disease)
# Create a frequency table of the 'Group' column
group_counts <- table(heart_2020_cleaned$Group)
print(group_counts)
# Plot the frequency table
# Create a frequency table as a data frame
group_counts_df <- as.data.frame(table(heart_2020_cleaned$Group))
colnames(group_counts_df) <- c("Group", "Count")

# Adjust column width when printing
print(format(group_counts_df, width = 20))  # Adjust width as needed
plot(group_counts, main = "Count of Observations by Group", xlab = "Group", ylab = "Frequency", col = "skyblue")
# Example: Histogram of SleepTime for each group
library(ggplot2)
ggplot(heart_2020_cleaned, aes(x = SleepTime, fill = Group)) +
  geom_histogram(binwidth = 2, color = "black", alpha = 0.7) +
  facet_wrap(~ Group, scales = "free") +
  labs(
    title = "Distribution of SleepTime by Group",
    x = "SleepTime",
    y = "Frequency"
  ) +
  theme_minimal()

popstrata = table(heart_2020_cleaned $Group )
plot(table(heart_2020_cleaned $Group ))

n = 5000
N = dim(heart_2020_cleaned)[1]
frac = n/N

num.sample = round(frac*table(heart_2020_cleaned $Group),0)

Group = unique(heart_2020_cleaned $Group) # groups  are treated as strata

# sample size, sample mean, sample variance of each strata
strata.N = Wh = strata.n = strata.avg = strata.var = strata.missing = rep(NA, 4)


set.seed(9)
for(i in 1:4)
{
  gru = Group[i]
  dat = heart_2020_cleaned[which(heart_2020_cleaned$Group == gru),]
  strata.N[i] = dim(dat)[1]
  strata.n[i] = round(frac*strata.N[i],0)
  sampleind = sample(1:strata.N[i], strata.n[i], replace = FALSE)
  Wh[i] = strata.N[i]/N
  
  stranumbmi = dat$SleepTime
  strata.sample.SRSWOR = stranumbmi[sampleind]
  strata.missing[i] = length(which(is.na(strata.sample.SRSWOR)))
  
  strata.avg[i] = mean(strata.sample.SRSWOR, na.rm = TRUE)
  strata.var[i] = var(strata.sample.SRSWOR, na.rm = TRUE)
}


result = data.frame(Group = Group, SampleSize = strata.n, Average = strata.avg, Variance = strata.var, Wh = Wh, 
                    Nh = strata.N, nh = strata.n, Missing = strata.missing)
result


# unbiased mean estimator
sum(result$Wh*result$Average)

# unbiased variance estimator for \bar{Yst}
sum((result$Wh)^2*(1/result$nh-1/result$Nh)*result$Variance)
