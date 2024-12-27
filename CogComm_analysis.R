#Setup chunk
setwd("/Users/szjgd/Documents/Cognitive Science Bsc/Cog & Com/Exam project")
data <- read.csv("Phantasia_data.csv", sep = ",")
#Changing column names for analysis
colnames(data) <- c("Time","consent","VVIQ", "Memory1",
                    "Memory2", "Memory3", "Memory4", "Memory5",
                    "Comprehension1", "Comprehension2", "Comprehension3",
                    "Comprehension4", "Comprehension5", "Immersion1",
                    "Immersion2", "Immersion3", "Immersion4", "Immersion5")
data <- data[-1, ] #Removing the first row as it was me

#Filtering answers
library(dplyr)
correct_answers <- list(Q3 = "Amelia", Q4 = "It was elegant and cursive", 
                        Q5 = "The house by the willow tree",
                        Q6 = "Sad and outraged", 
                        Q7 = "There was a storm with raindrops hitting the window in a steady rythmn",
                        Q8 = "The letter revealed a story of love, longing, and unspoken goodbyes involving her family.",
                        Q9 = "The letters hinted at a hidden family history involving forbidden love and decisions that shaped her family’s past.",
                        Q10 = "It signifies a connection to Emma’s grandmother’s house, linking the letters to her own family."
                        ) # Replace with actual correct answers

data <- data %>%
  mutate(
    Memory1 = ifelse(Memory1 == correct_answers$Q3, "1", "0"),
    Memory2 = ifelse(Memory2 == correct_answers$Q4, "1", "0"),
    Memory3 = ifelse(Memory3 == correct_answers$Q5, "1", "0"),
    Memory4 = ifelse(Memory4 == correct_answers$Q6, "1", "0"),
    Memory5 = ifelse(Memory5 == correct_answers$Q7, "1", "0"),
    Comprehension1 = ifelse(Comprehension1 == correct_answers$Q8, "1", "0"),
    Comprehension2 = ifelse(Comprehension2 == correct_answers$Q9, "1", "0"),
    Comprehension3 = ifelse(Comprehension3 == correct_answers$Q10, "1", "0")
  )

# Add a participant ID column
data <- data %>%
  mutate(ParticipantID = 1:47)


#Semantic analysis Comprehension4: compare answers' similarity to gold standard
reference_answer <- c("The stages of the storm reflect her emotional turmoil")
library(text2vec)

# Create a vocabulary and vectorize the texts
vocab <- create_vocabulary(itoken(c(data$Comprehension4, reference_answer), tokenizer = word_tokenizer))
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(itoken(c(data$Comprehension4, reference_answer), tokenizer = word_tokenizer), vectorizer)

# Compute cosine similarity
similarity_matrix <- sim2(dtm, method = "cosine", norm = "l2")


# Extract similarities with the reference text
similarities <- similarity_matrix[1:48, 49]  # Assuming last row/column is the reference

# Combine into a data frame
Similarity_C4 <- data.frame(Text = data$Comprehension4, Similarity = similarities)
data$similarity_C4 <- Similarity_C4$Similarity

#Semantic analysis Comprehension5
reference_answer2 <- c("Uncovering family secrets often leads to ongoing reflection and emotional impact, leaving Emma with more questions than answers.")
library(text2vec)

# Create a vocabulary and vectorize the texts
vocab2 <- create_vocabulary(itoken(c(data$Comprehension5, reference_answer2), tokenizer = word_tokenizer))
vectorizer2 <- vocab_vectorizer(vocab2)
dtm2 <- create_dtm(itoken(c(data$Comprehension5, reference_answer2), tokenizer = word_tokenizer), vectorizer2)

# Compute cosine similarity
similarity_matrix2 <- sim2(dtm2, method = "cosine", norm = "l2")


# Extract similarities with the reference text
similarities2 <- similarity_matrix[1:48, 49]  # Assuming last row/column is the reference

# Combine into a data frame
Similarity_C5 <- data.frame(Text = data$Comprehension5, Similarity = similarities2)
data$similarity_C5 <- Similarity_C5$Similarity

#Sentiment analysis
install.packages("vader")
library(vader)

sentiment_vader <- vader_df(data$Immersion1)
data$vader1 <- sentiment_vader$compound

sentiment2_vader <- vader_df(data$Immersion2)
data$vader2 <- sentiment2_vader$compound

sentiment3_vader <- vader_df(data$Immersion3)
data$vader3 <- sentiment3_vader$compound

sentiment4_vader <- vader_df(data$Immersion4)
data$vader4 <- sentiment4_vader$compound

sentiment5_vader <- vader_df(data$Immersion5)
data$vader5 <- sentiment5_vader$compound

#ANALYSIS
#Investigating the data visually and statistically
#Calculate means and standard errors for the memory scores by VVIQ
data$Memory1 <- as.numeric(data$Memory1)
data$Memory2 <- as.numeric(data$Memory2)
data$Memory3 <- as.numeric(data$Memory3)
data$Memory4 <- as.numeric(data$Memory4)
data$Memory5 <- as.numeric(data$Memory5)
data$VVIQ <- as.factor(data$VVIQ)
data$VVIQ <- factor(data$VVIQ, 
                    levels = c("Aphantasia","Phantasia", "Hyperphantasia"))

mean_answer_rate <- data %>%
  group_by(VVIQ) %>%
  summarize(
    mean_rate = mean(c_across(Memory1:Memory5)),
    se = sd(c_across(Memory1:Memory5)) / sqrt(n()), # Standard Error
    .groups = "drop"
  )
mean_answer_rate$VVIQ <- factor(mean_answer_rate$VVIQ, 
                             levels = mean_answer_rate$VVIQ[order(mean_answer_rate$mean_rate)])

print(mean_answer_rate)

#Plotting interaction effect
install.packages("ggplot2")
library(ggplot2)
ggplot(mean_answer_rate, aes(x = VVIQ, y = mean_rate, fill = VVIQ)) +
  geom_bar(stat = "identity", color = "black") + # Bar plot with black borders
  geom_errorbar(aes(ymin = mean_rate - se, ymax = mean_rate + se), 
                width = 0.2, color = "black") +
  theme_minimal() +  
  labs(
    title = "Mean Answer Rate in Memory Questions by VVIQ Groups",
    x = "VVIQ Groups",
    y = "Mean Answer Rate"
  ) +
  scale_fill_brewer(palette = "Set2")  

#Calculate means and standard errors for the comprehension scores by VVIQ
#Remove unnecessary columns
data <- data %>% select(-c(Comprehension4, Comprehension5, Immersion1, Immersion2, 
                           Immersion3, Immersion4, Immersion5))
# Move 'ParticipantsID' to the first position, keep the rest in their original order
data <- data %>% select(ParticipantID, everything())


data$Comprehension1 <- as.numeric(data$Comprehension1)
data$Comprehension2 <- as.numeric(data$Comprehension2)
data$Comprehension3 <- as.numeric(data$Comprehension3)
data$similarity_C4 <- as.numeric(data$similarity_C4)
data$similarity_C5 <- as.numeric(data$similarity_C5)

mean_answer_rate2 <- data %>%
  group_by(VVIQ) %>%
  summarize(
    mean_rate = mean(c_across(Comprehension1:similarity_C5)),
    se = sd(c_across(Comprehension1:similarity_C5)) / sqrt(n()), # Standard Error
    .groups = "drop"
  )
print(mean_answer_rate2)

#Plotting interaction effect
library(ggplot2)
ggplot(mean_answer_rate2, aes(x = VVIQ, y = mean_rate, fill = VVIQ)) +
  geom_bar(stat = "identity", color = "black") + # Bar plot with black borders
  geom_errorbar(aes(ymin = mean_rate - se, ymax = mean_rate + se), 
                width = 0.2, color = "black") +
  theme_minimal() +  
  labs(
    title = "Mean Answer Rate in Comprehension Questions by VVIQ Groups",
    x = "VVIQ Groups",
    y = "Mean Answer Rate"
  ) +
  scale_fill_brewer(palette = "Set2")  


#Calculate means and standard errors for the immersion scores by VVIQ
#There are negative values in the vader analysis (positive vs negative sentiment)
#I am taking the absolute sentiment as I am interested in the magnitude of sentiment
data$mean_vader_abs <- rowMeans(abs(data[, c("vader1", "vader2", "vader3", "vader4", "vader5")]), na.rm = TRUE)

mean_answer_rate3 <- data %>%
  group_by(VVIQ) %>%
  summarize(
    mean_rate = mean(mean_vader_abs, na.rm = TRUE),
    se = sd(mean_vader_abs, na.rm = TRUE)
  )
print(mean_answer_rate3)

#Plotting interaction effect
ggplot(mean_answer_rate3, aes(x = VVIQ, y = mean_rate, fill = VVIQ)) +
  geom_bar(stat = "identity", color = "black") + # Bar plot with black borders
  geom_errorbar(aes(ymin = mean_rate - se, ymax = mean_rate + se), 
                width = 0.2, color = "black") +
  theme_minimal() +  
  labs(
    title = "Mean Answer Rate in Emotioanl Immersion Questions by VVIQ Groups",
    x = "VVIQ Groups",
    y = "Mean Answer Rate"
  ) +
  scale_fill_brewer(palette = "Set2")  

#Investigating data structure
#I have 3 dependent variables: memory, comprehension and immersion
#I have one independent factorial variable: VVIQ
#Each dependent variable captures a distinct aspect of my study and therefore analyzed separately
#I shall conduct 3 one-way ANOVAs
library(pastecs)
data <- data %>%
  mutate(mean_comprehension = rowMeans(select(., Comprehension1:similarity_C5), na.rm = TRUE))

stat.desc(data$mean_comprehension, basic = TRUE, norm = TRUE)
# -2.1028 suggests significant negative skewness
# 6.2904 suggests a leptokurtic distribution (sharp peak, heavy tails).
# p = 2.483e-06 (very small) confirms the distribution is not normal.

ggplot(data, aes(sample = mean_comprehension)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Comprehension Scores", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  theme_minimal()

#Normalize the data: remove outliers
Q1 <- quantile(data$mean_comprehension, 0.25)
Q3 <- quantile(data$mean_comprehension, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

#Identifying participants with outliers
participants_with_outliers <- unique(data$ParticipantID[
  data$mean_comprehension < lower_bound | data$mean_comprehension > upper_bound
])

df_no_outliers <- data[!data$ParticipantID %in% participants_with_outliers, ]

#I check for normality again
stat.desc(df_no_outliers$mean_comprehension, basic = FALSE, norm = TRUE)
#Data does not deviate significantly from normality, we can proceed with ANOVA

#Homogeneity of Variance
install.packages("car")
library(car)
leveneTest(mean_comprehension ~ VVIQ, data = df_no_outliers)
#Since the p-value is > 0.05, I fail to reject the null hypothesis. 
#This indicates the assumption of homogeneity of variances is met.

comprehension_anova <- aov(mean_comprehension ~ VVIQ, data = df_no_outliers)
summary(comprehension_anova)
#Since the p-value (0.512) is greater than 0.05, you fail to reject the null hypothesis.
#This means there is no statistically significant difference in the dependent variable across the VVIQ groups.

data <- data %>%
  mutate(mean_memory = rowMeans(select(., Memory1:Memory5), na.rm = TRUE))
stat.desc(data$mean_memory, basic = TRUE, norm = TRUE)
#Skewness: -0.214 (slightly left-skewed but close to symmetric).
#Kurtosis: -1.013 (platykurtic; flatter distribution than normal).
#The low p-value (< 0.05) indicates the data deviates significantly from normality.

ggplot(data, aes(sample = mean_memory)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Memory Scores", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  theme_minimal()

#Since this data is not transformable to make it normal, I will use the Kruskal-Wallis Test
# The Kruskal-Wallis test uses the ranks of the data across groups and calculates a test statistic that follows a chi-squared distribution.
#Checking chi squared distribution

ggplot(data, aes(x = mean_memory)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.5) +
  geom_density(fill = "red", alpha = 0.3) +
  labs(title = "Histogram and Density Plot of Memory scores", 
       x = "Mean Memory", 
       y = "Density") +
  theme_minimal()

krustal_memory <- kruskal.test(mean_memory ~ VVIQ, data = data)
print(krustal_memory)
#p-value = 0.3445: The p-value is above 0.05, meaning the result is not statistically significant.

stat.desc(data$mean_vader_abs, basic = TRUE, norm = TRUE)
#The Shapiro-Wilk test statistic is 0.937, which is close to 1, indicating that the data might not significantly deviate from normality.
#The p-value of 0.0224 indicates that the data significantly deviates from a normal distribution, suggesting that it may not be normally distributed.

ggplot(data, aes(sample = mean_vader_abs)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot for Immersion Scores", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  theme_minimal()

#Log transformation fits here very well
data$transformed_vader <- sqrt(data$mean_vader_abs)
stat.desc(data$transformed_vader, basic = FALSE, norm = TRUE)

#The data appears to be normally distributed based on the p-value from the normality test (0.2154), which is greater than 0.05, suggesting the data does not significantly deviate from normality.

#Homogeneity of variance
leveneTest(transformed_vader ~ VVIQ, data = data)

#A p-value greater than 0.05 (in this case, 0.3321) suggests that there is no significant difference in variances between the groups.
#This result indicates that the assumption of homogeneity of variances is not violated

immersion_anova <- aov(transformed_vader ~ VVIQ, data = data)
summary(immersion_anova)
#The dot (.) indicates a trend toward significance (0.05 < p ≤ 0.1), but it's not statistically significant.

model_vader = lm(transformed_vader ~ VVIQ, 
              data = data)
summary(model_vader)
#Hyperphantasia is significantly different from Aphantasia in transformed vader scores.
#Phantasia shows a weaker trend and does not reach significance.
#The model's explanatory power (R-squared) is modest, suggesting other factors may influence transformed vader scores.
#The overall F-test indicates the groups collectively explain some variance, but not conclusively (p = 0.06577).

#Pairwise comparison
install.packages("emmeans")
library(emmeans)
posthoc_VVIQ <- emmeans(immersion_anova, pairwise ~ VVIQ, adjust = "bonferroni")
summary(posthoc_VVIQ)

#The only noteworthy comparison is Aphantasia vs. Hyperphantasia, which shows a trend toward significance but does not meet the adjusted threshold.
pairs(posthoc_VVIQ, adjust="bon") 


#Additional power analysis
install.packages("pwr")
library(pwr)

# Power analysis for a one-way ANOVA
pwr.anova.test(k = 4, f = 0.25, sig.level = 0.05, power = 0.8)
