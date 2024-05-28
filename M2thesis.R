# Introduction #########################################################################################################
library(readxl)
library(plm) 
library(knitr)
library(broom)
library(tidyverse)
library(stargazer) #regression outputs
library(lmtest) #coeftest : re-calculate a coefficient table with a different 
library(gplots) #data visualisation
library(dplyr)
library(readxl)

data <- read_excel("Desktop/m2 thesis/cleandata_thesis.xlsx")
View(data)       

head(data)
tail(data,n=3)
dim(data) #dimension 
str(data) # 606 obs from 6 years and 101 regions
sapply(data,class)

data$participant.payoff <- as.numeric(data$participant.payoff)

data <- data %>%
  mutate(participant.cognitive_test_payment = as.numeric(participant.cognitive_test_payment),
         participant.cognitive_test_payment = case_when(
           participant.cognitive_test_payment == 45419 ~ 7.5,
           participant.cognitive_test_payment == 45434 ~ 22.5,
           TRUE ~ participant.cognitive_test_payment
         ))

summary(data$french.6.player.gender)
summary(data$french.6.player.age)
table(data$french.6.player.prof)
table(data$french.6.player.gender)

# since the order is randomized, to have a common basis for everyone, I create new variables
# for 6 rounds, each of them corresding to 6 treatments ####

data <- data %>%
  mutate(round1 = case_when(
    french.1.player.contract_type == "low_simple" & french.1.player.task_type == "medium" ~ 1,
    french.1.player.contract_type == "medium_simple" & french.1.player.task_type == "medium" ~ 2,
    french.1.player.contract_type == "high_simple" & french.1.player.task_type == "medium" ~ 3,
    french.1.player.contract_type == "complex" & french.1.player.task_type == "simple" ~ 4,
    french.1.player.contract_type == "complex" & french.1.player.task_type == "medium" ~ 5,
    french.1.player.contract_type == "complex" & french.1.player.task_type == "difficult" ~ 6,
  ))

data <- data %>%
  mutate(round2 = case_when(
    french.2.player.contract_type == "low_simple" & french.2.player.task_type == "medium" ~ 1,
    french.2.player.contract_type == "medium_simple" & french.2.player.task_type == "medium" ~ 2,
    french.2.player.contract_type == "high_simple" & french.2.player.task_type == "medium" ~ 3,
    french.2.player.contract_type == "complex" & french.2.player.task_type == "simple" ~ 4,
    french.2.player.contract_type == "complex" & french.2.player.task_type == "medium" ~ 5,
    french.2.player.contract_type == "complex" & french.2.player.task_type == "difficult" ~ 6,
  ))

data <- data %>%
  mutate(round3 = case_when(
    french.3.player.contract_type == "low_simple" & french.3.player.task_type == "medium" ~ 1,
    french.3.player.contract_type == "medium_simple" & french.3.player.task_type == "medium" ~ 2,
    french.3.player.contract_type == "high_simple" & french.3.player.task_type == "medium" ~ 3,
    french.3.player.contract_type == "complex" & french.3.player.task_type == "simple" ~ 4,
    french.3.player.contract_type == "complex" & french.3.player.task_type == "medium" ~ 5,
    french.3.player.contract_type == "complex" & french.3.player.task_type == "difficult" ~ 6,
  ))

data <- data %>%
  mutate(round4 = case_when(
    french.4.player.contract_type == "low_simple" & french.4.player.task_type == "medium" ~ 1,
    french.4.player.contract_type == "medium_simple" & french.4.player.task_type == "medium" ~ 2,
    french.4.player.contract_type == "high_simple" & french.4.player.task_type == "medium" ~ 3,
    french.4.player.contract_type == "complex" & french.4.player.task_type == "simple" ~ 4,
    french.4.player.contract_type == "complex" & french.4.player.task_type == "medium" ~ 5,
    french.4.player.contract_type == "complex" & french.4.player.task_type == "difficult" ~ 6,
  ))

data <- data %>%
  mutate(round5 = case_when(
    french.5.player.contract_type == "low_simple" & french.5.player.task_type == "medium" ~ 1,
    french.5.player.contract_type == "medium_simple" & french.5.player.task_type == "medium" ~ 2,
    french.5.player.contract_type == "high_simple" & french.5.player.task_type == "medium" ~ 3,
    french.5.player.contract_type == "complex" & french.5.player.task_type == "simple" ~ 4,
    french.5.player.contract_type == "complex" & french.5.player.task_type == "medium" ~ 5,
    french.5.player.contract_type == "complex" & french.5.player.task_type == "difficult" ~ 6,
  ))

data <- data %>%
  mutate(round6 = case_when(
    french.6.player.contract_type == "low_simple" & french.6.player.task_type == "medium" ~ 1,
    french.6.player.contract_type == "medium_simple" & french.6.player.task_type == "medium" ~ 2,
    french.6.player.contract_type == "high_simple" & french.6.player.task_type == "medium" ~ 3,
    french.6.player.contract_type == "complex" & french.6.player.task_type == "simple" ~ 4,
    french.6.player.contract_type == "complex" & french.6.player.task_type == "medium" ~ 5,
    french.6.player.contract_type == "complex" & french.6.player.task_type == "difficult" ~ 6,
  ))

#### I create correct answer variables for each treatment #####

data <- data %>%
  mutate(total_lm = case_when(
    round1 == 1 ~ french.6.player.total_answers_round_1,
    round2 == 1 ~ french.6.player.total_answers_round_2,
    round3 == 1 ~ french.6.player.total_answers_round_3,
    round4 == 1 ~ french.6.player.total_answers_round_4,
    round5 == 1 ~ french.6.player.total_answers_round_5,
    round6 == 1 ~ french.6.player.total_answers_round_6,
  ))

data <- data %>%
  mutate(total_mm = case_when(
    round1 == 2 ~ french.6.player.total_answers_round_1,
    round2 == 2 ~ french.6.player.total_answers_round_2,
    round3 == 2 ~ french.6.player.total_answers_round_3,
    round4 == 2 ~ french.6.player.total_answers_round_4,
    round5 == 2 ~ french.6.player.total_answers_round_5,
    round6 == 2 ~ french.6.player.total_answers_round_6,
  ))

data <- data %>%
  mutate(total_hm = case_when(
    round1 == 3 ~ french.6.player.total_answers_round_1,
    round2 == 3 ~ french.6.player.total_answers_round_2,
    round3 == 3 ~ french.6.player.total_answers_round_3,
    round4 == 3 ~ french.6.player.total_answers_round_4,
    round5 == 3 ~ french.6.player.total_answers_round_5,
    round6 == 3 ~ french.6.player.total_answers_round_6,
  ))

data <- data %>%
  mutate(total_cs = case_when(
    round1 == 4 ~ french.6.player.total_answers_round_1,
    round2 == 4 ~ french.6.player.total_answers_round_2,
    round3 == 4 ~ french.6.player.total_answers_round_3,
    round4 == 4 ~ french.6.player.total_answers_round_4,
    round5 == 4 ~ french.6.player.total_answers_round_5,
    round6 == 4 ~ french.6.player.total_answers_round_6,
  ))

data <- data %>%
  mutate(total_cm = case_when(
    round1 == 5 ~ french.6.player.total_answers_round_1,
    round2 == 5 ~ french.6.player.total_answers_round_2,
    round3 == 5 ~ french.6.player.total_answers_round_3,
    round4 == 5 ~ french.6.player.total_answers_round_4,
    round5 == 5 ~ french.6.player.total_answers_round_5,
    round6 == 5 ~ french.6.player.total_answers_round_6,
  ))

data <- data %>%
  mutate(total_ch = case_when(
    round1 == 6 ~ french.6.player.total_answers_round_1,
    round2 == 6 ~ french.6.player.total_answers_round_2,
    round3 == 6 ~ french.6.player.total_answers_round_3,
    round4 == 6 ~ french.6.player.total_answers_round_4,
    round5 == 6 ~ french.6.player.total_answers_round_5,
    round6 == 6 ~ french.6.player.total_answers_round_6,
  ))

#### I create correct answer variables for each treatment #####

data <- data %>%
  mutate(correct_lm = case_when(
    round1 == 1 ~ french.6.player.correct_answers_round_1,
    round2 == 1 ~ french.6.player.correct_answers_round_2,
    round3 == 1 ~ french.6.player.correct_answers_round_3,
    round4 == 1 ~ french.6.player.correct_answers_round_4,
    round5 == 1 ~ french.6.player.correct_answers_round_5,
    round6 == 1 ~ french.6.player.correct_answers_round_6,
  ))

data <- data %>%
  mutate(correct_mm = case_when(
    round1 == 2 ~ french.6.player.correct_answers_round_1,
    round2 == 2 ~ french.6.player.correct_answers_round_2,
    round3 == 2 ~ french.6.player.correct_answers_round_3,
    round4 == 2 ~ french.6.player.correct_answers_round_4,
    round5 == 2 ~ french.6.player.correct_answers_round_5,
    round6 == 2 ~ french.6.player.correct_answers_round_6,
  ))

data <- data %>%
  mutate(correct_hm = case_when(
    round1 == 3 ~ french.6.player.correct_answers_round_1,
    round2 == 3 ~ french.6.player.correct_answers_round_2,
    round3 == 3 ~ french.6.player.correct_answers_round_3,
    round4 == 3 ~ french.6.player.correct_answers_round_4,
    round5 == 3 ~ french.6.player.correct_answers_round_5,
    round6 == 3 ~ french.6.player.correct_answers_round_6,
  ))

data <- data %>%
  mutate(correct_cs = case_when(
    round1 == 4 ~ french.6.player.correct_answers_round_1,
    round2 == 4 ~ french.6.player.correct_answers_round_2,
    round3 == 4 ~ french.6.player.correct_answers_round_3,
    round4 == 4 ~ french.6.player.correct_answers_round_4,
    round5 == 4 ~ french.6.player.correct_answers_round_5,
    round6 == 4 ~ french.6.player.correct_answers_round_6,
  ))

data <- data %>%
  mutate(correct_cm = case_when(
    round1 == 5 ~ french.6.player.correct_answers_round_1,
    round2 == 5 ~ french.6.player.correct_answers_round_2,
    round3 == 5 ~ french.6.player.correct_answers_round_3,
    round4 == 5 ~ french.6.player.correct_answers_round_4,
    round5 == 5 ~ french.6.player.correct_answers_round_5,
    round6 == 5 ~ french.6.player.correct_answers_round_6,
  ))

data <- data %>%
  mutate(correct_ch = case_when(
    round1 == 6 ~ french.6.player.correct_answers_round_1,
    round2 == 6 ~ french.6.player.correct_answers_round_2,
    round3 == 6 ~ french.6.player.correct_answers_round_3,
    round4 == 6 ~ french.6.player.correct_answers_round_4,
    round5 == 6 ~ french.6.player.correct_answers_round_5,
    round6 == 6 ~ french.6.player.correct_answers_round_6,
  ))

data <- data %>%
  mutate(performance_lm = correct_lm / total_lm)
data <- data %>%
  mutate(performance_mm = correct_mm / total_mm)
data <- data %>%
  mutate(performance_hm = correct_hm / total_hm)
data <- data %>%
  mutate(performance_cs = correct_cs / total_cs)
data <- data %>%
  mutate(performance_cm = correct_cm / total_cm)
data <- data %>%
  mutate(performance_ch = correct_ch / total_ch)

data <- data %>%
  mutate(payoff_lm = 5 * correct_lm)

data <- data %>%
  mutate(payoff_mm = 7.5 * correct_mm)

data <- data %>%
  mutate(payoff_hm = 10 * correct_hm)

data <- data %>%
  mutate(payoff_cs = case_when(
    correct_cs < 14 ~ 5 * correct_cs,
    correct_cs > 20 ~ 10 * correct_cs,
    correct_cs >= 14 & correct_cs <= 20 ~ 7.5 * correct_cs
  ))

data <- data %>%
  mutate(payoff_cm = case_when(
    correct_cm < 14 ~ 5 * correct_cm,
    correct_cm > 20 ~ 10 * correct_cm,
    correct_cm >= 14 & correct_cm <= 20 ~ 7.5 * correct_cm
  ))

data <- data %>%
  mutate(payoff_ch = case_when(
    correct_ch < 14 ~ 5 * correct_ch,
    correct_ch > 20 ~ 10 * correct_ch,
    correct_ch >= 14 & correct_ch <= 20 ~ 7.5 * correct_ch
  ))

# Demographics manipulation ###########
data$french.6.player.gender <- ifelse(data$french.6.player.gender=="Femme",1,0)
data$french.6.player.gender <- as.numeric(data$french.6.player.gender)
sum(data$french.6.player.gender) # 44 female, 42 male

summary(data$french.6.player.age) #min 18 max 24. mean is 21.57

table(data$french.6.player.education)
data$french.6.player.education <- as.factor(data$french.6.player.education)

table(data$french.6.player.study)
data <- data %>%
  mutate(study_group = case_when(
    grepl("économie|Economie|commerce|Comptabilité|gestion|Management|Economie et gestion", french.6.player.study, ignore.case = TRUE) ~ "Economics and Business",
    grepl("droit|Administration publique|immobilier|Economie et droit", french.6.player.study, ignore.case = TRUE) ~ "Law and Public Administration",
    grepl("biologie|chirurgie dentaire|Sciences de la vie", french.6.player.study, ignore.case = TRUE) ~ "Sciences",
    grepl("informatique|ingénierie|Engineering", french.6.player.study, ignore.case = TRUE) ~ "Engineering and IT",
    grepl("histoire|Sociologie|psychologie|Philosophie|Art|Economie et sociologie", french.6.player.study, ignore.case = TRUE) ~ "Humanities and Social Sciences",
    grepl("mathématique|Maths info|Mathématiques et Economie", french.6.player.study, ignore.case = TRUE) ~ "Mathematics and Statistics",
    TRUE ~ "Other"
  ))

data$study_group <- as.factor(data$study_group)

table(data$french.6.player.prof)
data <- data %>% 
  mutate(profession_group = if_else(
    grepl("étudiant|Etudiant|étudiante|Etudiante", french.6.player.prof, ignore.case = TRUE),
    "1",
    "0"
  )) #student is 1, others are 0.

data$profession_group <- as.numeric(data$profession_group)

summary(data$french.6.player.cognitive_test_score)

data <- data %>%
  mutate(cog_ab_high = case_when (
    french.6.player.cognitive_test_score >= 8 ~ 1,
    french.6.player.cognitive_test_score < 8 ~ 0
  ) )

data <- data %>%
  mutate(cog_ab_low = case_when (
    french.6.player.cognitive_test_score <= 2 ~ 1,
    french.6.player.cognitive_test_score > 2 ~ 0
  ) )

# Estimation of ability / cost of effort ####

data_long <- data %>%
  pivot_longer(cols = c(correct_lm, correct_mm, correct_hm),
               names_to = "treatment",
               values_to = "effort") %>%
  mutate(piece_rate = case_when(
    treatment == "correct_lm" ~ 5,
    treatment == "correct_mm" ~ 7.5,
    treatment == "correct_hm" ~ 10
  ))

# Ensure efforts and piece rates are handled as numeric
data_long$effort <- as.numeric(data_long$effort)
data_long$piece_rate <- as.numeric(data_long$piece_rate)

# Create an empty list to store the models
ability <- list()

# Loop through each unique participant
for (id in unique(data_long$participant.code)) {
  # Subset the data for the participant
  participant_data <- data_long[data_long$participant.code == id,]
  
  # Fit the model
  if (nrow(participant_data) > 0 && all(participant_data$effort > 0) && all(participant_data$piece_rate > 0)) {
    model <- try(nls(effort ~ exp(gamma * log(piece_rate)), data = participant_data, start = list(gamma = 0.1)), silent = TRUE)
    if (!inherits(model, "try-error")) {
      ability[[as.character(id)]] <- model
    } else {
      ability[[as.character(id)]] <- NA
    }
  } else {
    ability[[as.character(id)]] <- NA
  }
}

# To view the results, ignoring failed model fits
results <- sapply(ability, function(x) {
  if (inherits(x, "nls") && !is.null(coef(x))) {
    return(coef(x)["gamma"])  # Return the gamma coefficient if the model is valid
  } else {
    return(NULL)  # Return NULL for unsuccessful fits, which sapply will ignore
  }
})

# Remove NULL entries from the results
results <- results[!sapply(results, is.null)]

print(results)
#Integrating gamma_i (ability) to our original data 

ability <- data.frame(
  participant_code = names(results),
  ability = unlist(results),
  stringsAsFactors = FALSE
)

# Step 2: Merge this data frame with the original data
data <- data %>%
  left_join(ability, by = c("participant.code" = "participant_code"))
data$ability <- as.numeric(data$ability)

#Descriptive stats of gamma
summary(data$ability)

data <- data %>%
  mutate(ability_high = case_when (
    ability >= 1.3097 ~ 1,
    ability < 1.3097 ~ 0
  ) )

data <- data %>%
  mutate(ability_low = case_when (
    ability <= 1.0891 ~ 1,
    ability > 1.0891 ~ 0
  ) )

#Distribution of ability

abi <- ggplot(data, aes(x = ability)) +
  geom_histogram(bins = 85, fill="steelblue", color="black") +
  #geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_lm") +
  xlab("Ability level") +
  ylab("Frequency") +
  xlim(0.6, 1.5) +
  ylim(0,6.5) +
  geom_vline(aes(xintercept=mean(ability, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(ability, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

print(abi)

# Computing gamma_i for each treatment
data <- data %>% 
  mutate(gamma1 = log(correct_lm)/ log(5))

data <- data %>% 
  mutate(gamma2 = log(correct_mm)/ log(7.5))

data <- data %>% 
  mutate(gamma3 = log(correct_hm)/log(10))

# Identify rows with NA, NaN, or Inf in gamma values
data %>%
  filter(is.na(gamma1) | is.infinite(gamma1) | is.nan(gamma1) |
           is.na(gamma2) | is.infinite(gamma2) | is.nan(gamma2) |
           is.na(gamma3) | is.infinite(gamma3) | is.nan(gamma3)) %>%
  select(participant.code, total_lm, total_mm, total_hm, gamma1, gamma2, gamma3)

data <- data %>%
  filter(!is.na(gamma1) & !is.infinite(gamma1) & !is.nan(gamma1) &
           !is.na(gamma2) & !is.infinite(gamma2) & !is.nan(gamma2) &
           !is.na(gamma3) & !is.infinite(gamma3) & !is.nan(gamma3))


filtered_data <- data %>%
  filter(gamma1 > gamma2 & gamma2 > gamma3)

# Count the number of rows that meet the condition
count_rows <- nrow(filtered_data)

# Print the count
print(count_rows)

# Now proceed with reformatting for ANOVA
gamma_long <- data %>%
  pivot_longer(
    cols = c(gamma1, gamma2, gamma3),
    names_to = "treatment",
    values_to = "gamma_value"
  ) %>%
  mutate(treatment = sub("gamma", "", treatment))

# Preliminary check
prep <- plm(gamma_value ~ treatment, data = gamma_long)
summary(prep)
stargazer(prep)

# Regression of gamma on individual characteristics
library(stats)
ability <- lm(ability ~ french.6.player.correct_answers_round_1 + french.6.player.correct_answers_round_2 + french.6.player.correct_answers_round_3 + french.6.player.correct_answers_round_4 + french.6.player.correct_answers_round_5 +
                french.6.player.correct_answers_round_6 + french.6.player.cognitive_test_score + french.6.player.age + french.6.player.ordinateur + french.6.player.education + study_group + profession_group + french.6.player.gender, data=data)
summary(ability)

# Estimating subjective piece-rate for the complex contract ####
data <- data %>% 
  mutate(subjective_pr = correct_cm ^ (1/ability))

summary(data$subjective_pr)
stargazer(data$subjective_pr, 
           min.max=TRUE, mean.sd = TRUE, 
          nobs = FALSE, median = TRUE, iqr = FALSE,
          digits=1, align=T,
          title = "Summary Statistics of the Subjective Piece-Rate")

data <- data %>% 
  mutate(subjective_pr_high10 = case_when (
    subjective_pr > 10 ~ 1,
    subjective_pr <= 10 ~ 0
  ))

#Regressing subjective piece-rate ####

piece1 <- lm(subjective_pr  ~ correct_lm + correct_mm + correct_hm + french.6.player.cognitive_test_score + cog_ab_low, data=data)
summary(piece1)

piece2 <- glm(subjective_pr_high10 ~ correct_lm + correct_mm + correct_hm + french.6.player.cognitive_test_score + cog_ab_low,
              family = binomial(link = "probit"),
              data=data)
summary(piece2)

piece3 <- glm(subjective_pr_high10 ~ correct_lm + correct_mm + correct_hm + french.6.player.cognitive_test_score + cog_ab_low,
              family = "binomial" ,
              data=data)
summary(piece3)

#piece4 <- glm(subjective_pr_high10 ~ correct_lm + correct_mm + correct_hm + french.6.player.cognitive_test_score + cog_ab_low,
             # family = "binomial" ,
              # data=filtered_data)
#summary(piece4)

stargazer(piece1, piece3)

###########################################################################################################
############################################################################################################
### DATA ANALYSIS #######################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)

##### bar plotting the means for the incentive effects with total answers ###############

library(ggplot2)
library(ggsignif)

# Calculate means and standard deviations
mean1 <- mean(data$total_lm)
sd1 <- sd(data$total_lm)

mean2 <- mean(data$total_mm)
sd2 <- sd(data$total_mm)

mean3 <- mean(data$total_hm)
sd3 <- sd(data$total_hm)


# Create data frame for bar plot
df <- data.frame(
  Treatment = c("Low piece-rate", "Medium piece-rate", "High piece-rate"),
  mean = c(mean1, mean2, mean3),
  sd = c(sd1, sd2, sd3)
)

# Perform t-tests
lm_mm_ttest <- t.test(data$total_lm, data$total_mm)
mm_hm_ttest <- t.test(data$total_mm, data$total_hm)
hm_lm_ttest <- t.test(data$total_hm, data$total_lm)

df$Treatment <- factor(df$Treatment, levels = c("Low piece-rate", "Medium piece-rate", "High piece-rate"))

# Create bar plot with significance levels
# Create bar plot with t-test significance levels
plot <- ggplot(df, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2) +
  labs(y = "Mean Effort Provision") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) 

plot

##### bar plotting the means for the incentive effects with correct answers ##############

mean1.2 <- mean(data$correct_lm)
sd1.2 <- sd(data$correct_lm)

mean2.2 <- mean(data$correct_mm)
sd2.2 <- sd(data$correct_mm)

mean3.2 <- mean(data$correct_hm)
sd3.2 <- sd(data$correct_hm)


# Create data frame for bar plot
df2 <- data.frame(
  Treatment = c("Low piece-rate", "Medium piece-rate", "High piece-rate"),
  mean = c(mean1.2, mean2.2, mean3.2),
  sd = c(sd1.2, sd2.2, sd3.2)
)

# Perform t-tests
lm_mm_ttest2 <- t.test(data$total_lm, data$total_mm)
mm_hm_ttest2 <- t.test(data$total_mm, data$total_hm)
hm_lm_ttest2 <- t.test(data$total_hm, data$total_lm)

df2$Treatment <- factor(df$Treatment, levels = c("Low piece-rate", "Medium piece-rate", "High piece-rate"))

# Create bar plot with significance levels
# Create bar plot with t-test significance levels
plot2 <- ggplot(df2, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2) +
  labs(y = "Mean Correct Answers") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) 

plot2

#combining the plots ##############
library(gridExtra)
library(grid)
grid_plot <- grid.arrange(plot, plot2, nrow = 1, widths = c(0.5, 0.5))

###### distribution of correct answers #########
library(ggplot2)

# Plot for low piece rate (correct_lm)
p1 <- ggplot(data, aes(x = correct_lm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_lm") +
  xlab("Correct answers under low piece-rate") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(correct_lm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(correct_lm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.line = element_line(color = "black"))

# Plot for medium piece rate (correct_mm)
p2 <- ggplot(data, aes(x = correct_mm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_mm") +
  xlab("Correct answers under medium piece-rate") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(correct_mm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(correct_mm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Plot for high piece rate (correct_hm)
p3 <- ggplot(data, aes(x = correct_hm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_hm") +
  xlab("Correct answers under high piece-rate") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(correct_hm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(correct_hm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Use grid.arrange to combine the plots
combined_plot <- grid.arrange(p1, p2, p3, nrow=3)

# Print the combined plot
print(combined_plot)

###### distribution of total answers #########

# Plot for low piece rate (total_lm)
p4 <- ggplot(data, aes(x = total_lm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.5, color=NA) +
  #ggtitle("Histogram and Density of correct_lm") +
  xlab("Total answers under low piece-rate") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(total_lm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(total_lm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Plot for medium piece rate (total_mm)
p5 <- ggplot(data, aes(x = total_mm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.5, color=NA) +
  #ggtitle("Histogram and Density of correct_mm") +
  xlab("Total answers under medium piece-rate") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(total_mm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(total_mm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Plot for high piece rate (total_hm)
p6 <- ggplot(data, aes(x = total_hm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.5, color=NA) +
  #ggtitle("Histogram and Density of correct_hm") +
  xlab("Total answers under high piece-rate") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(total_hm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(total_hm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Create the ggplot objects for each variable as described previously

# Use grid.arrange to combine the plots
combined_plot2 <- grid.arrange(p4, p5, p6, nrow=3)

# Print the combined plot
print(combined_plot2)

#Testing the distributions

# Reshape the data
data_test<- data %>%
  pivot_longer(
    cols = c(correct_lm, correct_mm, correct_hm),
    names_to = "score_type",
    values_to = "scores"
  )
# Pairwise Wilcoxon Tests
pairwise.wilcox.test(data_test$scores, data_test$score_type, p.adjust.method = "BH")

# Kruskal-Wallis Test
kruskal.test(scores ~ score_type, data = data_test)


########### maybe the incentive effects didn't work because of the income effect. let's see.... ####

data <- data %>%
  mutate (payoff_lm = 5 * correct_lm)

data <- data %>%
  mutate (payoff_mm = 7.5 * correct_mm)

data <- data %>%
  mutate (payoff_hm = 10 * correct_hm)

mean_p1 <- mean(data$payoff_lm)
sd_p1 <- sd(data$payoff_lm)

mean_p2 <- mean(data$payoff_mm)
sd_p2 <- sd(data$payoff_mm)

mean_p3 <- mean(data$payoff_hm)
sd_p3 <- sd(data$payoff_hm)

# Create data frame for bar plot
df3 <- data.frame(
  Treatment = c("Low piece-rate", "Medium piece-rate", "High piece-rate"),
  mean = c(mean_p1, mean_p2, mean_p3),
  sd = c(sd_p1, sd_p2, sd_p3)
)

df3$Treatment <- factor(df$Treatment, levels = c("Low piece-rate", "Medium piece-rate", "High piece-rate"))

# Create bar plot with significance levels
# Create bar plot with t-test significance levels
plot3 <- ggplot(df3, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2) +
  labs(y = "Mean Payoff") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) 

plot3

#distributions of payoffs ####

# Plot for low piece rate (payoff_lm)
p7 <- ggplot(data, aes(x = payoff_lm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.5, color=NA) +
  #ggtitle("Histogram and Density of correct_lm") +
  xlab("Payoffs under low piece-rate") +
  ylab("Frequency") +
  xlim(0, 200) +
  geom_vline(aes(xintercept=mean(payoff_lm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(payoff_lm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Plot for medium piece rate (payoff_mm)
p8 <- ggplot(data, aes(x = payoff_mm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.5, color=NA) +
  #ggtitle("Histogram and Density of payoff_mm") +
  xlab("Payoffs under medium piece-rate") +
  ylab("Frequency") +
  xlim(0, 200) +
  geom_vline(aes(xintercept=mean(payoff_mm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(payoff_mm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Plot for high piece rate (payoff_hm)
p9 <- ggplot(data, aes(x = payoff_hm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.5, color=NA) +
  #ggtitle("Histogram and Density of correct_hm") +
  xlab("Payoffs under high piece-rate") +
  ylab("Frequency") +
  xlim(0, 200) +
  geom_vline(aes(xintercept=mean(payoff_hm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(payoff_hm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))

# Create the ggplot objects for each variable as described previously

# Use grid.arrange to combine the plots
combined_plot3 <- grid.arrange(p7, p8, p9, nrow=3)

# Print the combined plot
print(combined_plot3)

##### testing it ####

data_test<- data %>%
  pivot_longer(
    cols = c(payoff_mm, payoff_hm),
    names_to = "score_type",
    values_to = "scores"
  )
# Pairwise Wilcoxon Tests
pairwise.wilcox.test(data_test$scores, data_test$score_type, p.adjust.method = "BH")

# Kruskal-Wallis Test
kruskal.test(scores ~ score_type, data = data_test)

####################### let's look at performance a little bit ########

data <- data %>% 
  mutate (average_tot_simple = (total_lm + total_mm + total_hm) / 3 )

mean(data$average_tot_simple)

data <- data %>% 
  mutate (average_cor_simple = (correct_lm + correct_mm + correct_hm) / 3 )

mean(data$average_cor_simple)

data <- data %>%
  mutate(average_perf_simple = (performance_lm + performance_mm + performance_hm) / 3 )

summary(data$average_perf_simple)

data <- data %>%
  mutate(average_perf_comp = correct_cm / total_cm)

summary(data$average_perf_comp) #even performance is higher in complex one.

#### is the order really randomized??? ##########
summary(data$round6) # looks random

mean(data$french.6.player.total_answers_round_1)
mean(data$french.6.player.total_answers_round_2)
mean(data$french.6.player.total_answers_round_3)
mean(data$french.6.player.total_answers_round_4)
mean(data$french.6.player.total_answers_round_5)
mean(data$french.6.player.total_answers_round_6)

#### let's check what is going on with the task difficulty #####

mean_cs <- mean(data$total_cs)
sd_cs <- sd(data$total_cs)

mean_cm <- mean(data$total_cm)
sd_cm <- sd(data$total_cm)

mean_ch <- mean(data$total_ch)
sd_ch <- sd(data$total_ch)



complex_data <- data.frame(
  Treatment = factor(c("Simple task", "Medium task", "Difficult task")),
  mean = c(mean_cs, mean_cm, mean_ch),
  sd = c(sd_cs, sd_cm, sd_ch)
)

# Create bar plot with significance levels
plot4 <- ggplot(complex_data, aes(x = Treatment, y = mean, group = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "cornflowerblue", color = "darkblue", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(0.7), color = "red", width = 0.25) +
  labs(y = "Mean Effort", x = "Task Complexity", title = "Average Effort by Task Complexity") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot4

#Distribution of the effort under complex contracts ####

# Plot for simple task (correct_cm)

pl1 <- ggplot(data, aes(x = correct_cs)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_lm") +
  xlab("Correct answers under simple task") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(correct_cs, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(correct_cs, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

# Plot for medium task (correct_cm)
pl2 <- ggplot(data, aes(x = correct_cm)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_mm") +
  xlab("Correct answers under medium task") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(correct_cm, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(correct_cm, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

# Plot for difficult task (correct_ch)
pl3 <- ggplot(data, aes(x = correct_ch)) +
  geom_histogram(bins = 30, fill="steelblue", color="black") +
  geom_density(aes(y = ..count..), fill="midnightblue", alpha=0.4, color=NA) +
  #ggtitle("Histogram and Density of correct_ch") +
  xlab("Correct answers under difficult task") +
  ylab("Frequency") +
  xlim(0, 30) +
  geom_vline(aes(xintercept=mean(correct_ch, na.rm=TRUE)), color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(correct_ch, na.rm=TRUE), label="", y=0), vjust=-0.5, color="red") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"))

# Use grid.arrange to combine the plots
combined_plot3 <- grid.arrange(pl3, pl2, pl1, nrow=3)

# Print the combined plot
print(combined_plot3)


##### clustering participants based on their performance in complex contract
####### BUT THIS, I DON'T USE #########

summary(data$total_cm)
std_dev <- sd(data$total_cm)
mean_value <- mean(data$total_cm)

# Determine the thresholds based on standard deviation
low_threshold <- mean_value - std_dev
high_threshold <- mean_value + std_dev

# You can print out the thresholds if you want
print(paste("Low Threshold:", low_threshold))
print(paste("High Threshold:", high_threshold))

# Assign clusters based on the thresholds
data$cluster <- ifelse(data$total_cm < 14, 'low',
                       ifelse(data$total_cm > 20, 'high', 'medium'))

####### THIS CLUSTER I USE ############################

data$cluster_cor <- ifelse(data$correct_cm < 14, 'low',
                       ifelse(data$correct_cm > 20, 'high', 'medium'))

cluster_cor_counts <- data %>%
  count(cluster_cor)
print(cluster_cor_counts) #11 high, 32 low and 43 medium

###### creating new subsets of data to better analyze the data

low_data <- data %>%
  filter(cluster_cor == "low")

medium_data <- data %>%
  filter(cluster_cor == "medium")

high_data <- data %>%
  filter(cluster_cor == "high")

#### plotting effort levels for simple and complex contract ####
library(tidyr)
data_long <- data %>%
  pivot_longer(
    cols = c(correct_lm, correct_mm, correct_hm),
    names_to = "piece_rate",
    values_to = "correct_answers"
  ) %>%
  mutate(piece_rate_level = case_when(
    piece_rate == "correct_lm" ~ 5,
    piece_rate == "correct_mm" ~ 7.5,
    piece_rate == "correct_hm" ~ 10
  ))

# Classifying correct_cm into low, medium, and high
data$cluster_cor <- ifelse(data$correct_cm < 14, 'low',
                           ifelse(data$correct_cm > 20, 'high', 'medium'))

# Enhance cluster_data to include correct_lm, correct_mm, and correct_hm
cluster_data <- data %>%
  select(correct_cm, correct_lm, correct_mm, correct_hm, cluster_cor) %>%
  mutate(
    piece_rate_level = case_when(
      cluster_cor == "low" ~ 5,
      cluster_cor == "medium" ~ 7.5,
      cluster_cor == "high" ~ 10
    ),
    black_dot_value = case_when(
      piece_rate_level == 5 ~ correct_lm,
      piece_rate_level == 7.5 ~ correct_mm,
      piece_rate_level == 10 ~ correct_hm
    )
  )

plot <- ggplot() +
  geom_point(data = cluster_data, aes(x = correct_cm, y = piece_rate_level), color = "darkred",shape= 24, size = 3, fill="red") +
  geom_point(data = cluster_data, aes(x = black_dot_value, y = piece_rate_level), color = "darkgreen", shape = 25 , size = 3, fill="green") +
  scale_y_continuous(breaks = c(5, 7.5, 10), labels = c("Low", "Medium", "High")) +
  labs(x = "Correct Answers", y = "Piece Rate Level", title = "Distribution of Correct Answers by Piece Rate Level") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )

plot <- ggplot() +
  geom_point(data = cluster_data, aes(x = black_dot_value, y = piece_rate_level, color = "Simple Contract", shape = "Simple Contract"), size = 3) +
  geom_point(data = cluster_data, aes(x = correct_cm, y = piece_rate_level, color = "Complex Contract", shape = "Complex Contract"), size = 3) +
  scale_y_continuous(breaks = c(5, 7.5, 10), labels = c("Low", "Medium", "High")) +
  scale_fill_manual(values = c("Complex Contract" = "red3", "Simple Contract" = "darkgreen")) +
  scale_color_manual(values = c("Complex Contract" = "red3", "Simple Contract" = "darkgreen")) +
  scale_shape_manual(values = c("Complex Contract" = 18, "Simple Contract" = 17)) +
  labs(x = "Correct Answers", y = "Piece Rate Level", color = "Contract Type", shape = "Contract Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )


# Print the plot
print(plot)

##### manipulating the dataset of low performers #####

# Creating the expanded dataset
low_data_expanded <- low_data %>%
  uncount(2)

# treatment == 0 simple, treatment == 1 complex

low_data_expanded <- low_data_expanded %>%
  mutate(treatment = if_else(row_number() %% 2 == 0, 1, 0))

low_data_expanded <- low_data_expanded %>%
  mutate(low_effort = if_else(treatment == 0, total_lm, total_cm)) #treatment 1 ise complex contract

low_data_expanded <- low_data_expanded %>%
  mutate(low_correct = if_else(treatment == 0, correct_lm, correct_cm))

low_data_expanded <- low_data_expanded %>%
  mutate(low_perf = if_else(treatment == 0, performance_lm, performance_cm))

low_data_expanded <- low_data_expanded %>%
  mutate(panel = if_else(row_number() %% 2 == 0, 1, 2))

low_data_expanded <- low_data_expanded %>%
  mutate(binary_lower = if_else(total_lm > total_cm, 1, 0)) #it takes value 1 if the effort is lower in complex treatment.

low_data_expanded <- low_data_expanded %>%
  mutate(effort_dif_low = total_cm - total_lm) # effort difference. Complex-Simple 

View(low_data_expanded)
###### manipulating the dataset of medium performers ####

medium_data_expanded <- medium_data %>%
  uncount(2)

medium_data_expanded <- medium_data_expanded %>%
  mutate(treatment = if_else(row_number() %% 2 == 0, 1, 0))

medium_data_expanded <- medium_data_expanded %>%
  mutate(medium_effort = if_else(treatment == 0, total_mm, total_cm))

medium_data_expanded <- medium_data_expanded %>%
  mutate(medium_correct = if_else(treatment == 0, correct_mm, correct_cm))

medium_data_expanded <- medium_data_expanded %>%
  mutate(medium_perf = if_else(treatment == 0, performance_mm, performance_cm))

medium_data_expanded <- medium_data_expanded %>%
  mutate(panel = if_else(row_number() %% 2 == 0, 1, 2))

medium_data_expanded <- medium_data_expanded %>%
  mutate(binary_lower = if_else(total_mm > total_cm, 1, 0))

medium_data_expanded <- medium_data_expanded %>%
  mutate(effort_dif_med = total_cm - total_mm)

##### manipulating the dataset of high performers #######
high_data_expanded <- high_data %>%
  uncount(2)

high_data_expanded <- high_data_expanded %>%
  mutate(treatment = if_else(row_number() %% 2 == 0, 1, 0))

high_data_expanded <- high_data_expanded %>%
  mutate(high_effort = if_else(treatment == 0, total_hm, total_cm))

high_data_expanded <- high_data_expanded %>%
  mutate(high_correct = if_else(treatment == 0, correct_hm, correct_cm))

high_data_expanded <- high_data_expanded %>%
  mutate(high_perf = if_else(treatment == 0, performance_hm, performance_cm))

high_data_expanded <- high_data_expanded %>%
  mutate(panel = if_else(row_number() %% 2 == 0, 1, 2))

high_data_expanded <- high_data_expanded %>%
  mutate(binary_lower = if_else(total_hm > total_cm, 1, 0))

high_data_expanded <- high_data_expanded %>%
  mutate(effort_dif_high = total_cm - total_hm)

#### learning effect with correct answers ######

library(ggplot2)

# Calculate the means for each round
mean_round1 <- mean(data$french.6.player.correct_answers_round_1, na.rm = TRUE)
mean_round2 <- mean(data$french.6.player.correct_answers_round_2, na.rm = TRUE)
mean_round3 <- mean(data$french.6.player.correct_answers_round_3, na.rm = TRUE)
mean_round4 <- mean(data$french.6.player.correct_answers_round_4, na.rm = TRUE)
mean_round5 <- mean(data$french.6.player.correct_answers_round_5, na.rm = TRUE)
mean_round6 <- mean(data$french.6.player.correct_answers_round_6, na.rm = TRUE)

se_round1 <- sd(data$french.6.player.correct_answers_round_1, na.rm = TRUE) / sqrt(nrow(data))
se_round2 <- sd(data$french.6.player.correct_answers_round_2, na.rm = TRUE) / sqrt(nrow(data))
se_round3 <- sd(data$french.6.player.correct_answers_round_3, na.rm = TRUE) / sqrt(nrow(data))
se_round4 <- sd(data$french.6.player.correct_answers_round_4, na.rm = TRUE) / sqrt(nrow(data))
se_round5 <- sd(data$french.6.player.correct_answers_round_5, na.rm = TRUE) / sqrt(nrow(data))
se_round6 <- sd(data$french.6.player.correct_answers_round_6, na.rm = TRUE) / sqrt(nrow(data))


# Create a dataframe for plotting
plot_data <- data.frame(
  t = 1:6,
  mean_correct_answers = c(mean_round1, mean_round2, mean_round3, mean_round4, mean_round5, mean_round6),
  se = c(se_round1, se_round2, se_round3, se_round4, se_round5, se_round6)
)

# Create the plot with error bars
ggplot(plot_data, aes(x = t, y = mean_correct_answers)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_correct_answers - se, ymax = mean_correct_answers + se), width = 0.1) +
  xlab("Time (t)") +
  ylab("Average Correct Answers") +
  ggtitle("Average Correct Answers Over Time") +
  ylim(8, 20) # Set y-axis limits

ggplot(plot_data, aes(x = t, y = mean_correct_answers)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_correct_answers - se, ymax = mean_correct_answers + se), width = 0.1) +
  xlab("Time (t)") +
  ylab("Average Correct Answers") +
  #ggtitle("Average Correct Answers Over Time") +
  ylim(8, 20) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),  # Set the panel background to white and keep the border
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black")) 

#### learning effect with total answers ######

# Calculate the means for each round
mean1_round1 <- mean(data$french.6.player.total_answers_round_1, na.rm = TRUE)
mean1_round2 <- mean(data$french.6.player.total_answers_round_2, na.rm = TRUE)
mean1_round3 <- mean(data$french.6.player.total_answers_round_3, na.rm = TRUE)
mean1_round4 <- mean(data$french.6.player.total_answers_round_4, na.rm = TRUE)
mean1_round5 <- mean(data$french.6.player.total_answers_round_5, na.rm = TRUE)
mean1_round6 <- mean(data$french.6.player.total_answers_round_6, na.rm = TRUE)

se1_round1 <- sd(data$french.6.player.total_answers_round_1, na.rm = TRUE) / sqrt(nrow(data))
se1_round2 <- sd(data$french.6.player.total_answers_round_2, na.rm = TRUE) / sqrt(nrow(data))
se1_round3 <- sd(data$french.6.player.total_answers_round_3, na.rm = TRUE) / sqrt(nrow(data))
se1_round4 <- sd(data$french.6.player.total_answers_round_4, na.rm = TRUE) / sqrt(nrow(data))
se1_round5 <- sd(data$french.6.player.total_answers_round_5, na.rm = TRUE) / sqrt(nrow(data))
se1_round6 <- sd(data$french.6.player.total_answers_round_6, na.rm = TRUE) / sqrt(nrow(data))


# Create a dataframe for plotting
plot_data1 <- data.frame(
  t = 1:6,
  mean1_total_answers = c(mean1_round1, mean1_round2, mean1_round3, mean1_round4, mean1_round5, mean1_round6),
  se1 = c(se1_round1, se1_round2, se1_round3, se1_round4, se1_round5, se1_round6)
)

# Create the plot with error bars
ggplot(plot_data1, aes(x = t, y = mean1_total_answers)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin = mean1_total_answers - se1, ymax = mean1_total_answers + se1), width = 0.1) +
  xlab("Time (t)") +
  ylab("Average Total Answers") +
  ggtitle("Average Total Answers Over Time") +
  ylim(10, 20) # Set y-axis limits

######################### Regression analysis 1 ###################################################################

#### the dependent variable is 'total answers' #######

library(plm)
low_data_expanded <- pdata.frame(low_data_expanded, index=c("panel", "participant.code"))

model_low <- lm(low_effort ~ treatment + ability + french.6.player.cognitive_test_score +  cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education , data=low_data_expanded)
summary(model_low)

model_low2 <- lm(low_effort ~ treatment + ability + french.6.player.cognitive_test_score + treatment*ability +  cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education , data=low_data_expanded)
summary(model_low2)

model_med <- lm(medium_effort ~ treatment + ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(model_med)

model_med2 <- lm(medium_effort ~ treatment + ability + treatment*ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(model_med2)

model_high <- lm(high_effort ~ treatment + ability + french.6.player.cognitive_test_score  + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(model_high)

model_high2 <- lm(high_effort ~ treatment + ability + treatment*ability + french.6.player.cognitive_test_score  + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(model_high2)

stargazer(model_low, model_low2, model_med, model_med2)
stargazer(model_high, model_high2)

#### the dependent variable is 'correct answers' ######

model_cor_1 <- lm(low_correct ~ treatment + ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=low_data_expanded)
summary(model_cor_1)

model_cor_1.1 <- lm(low_correct ~ treatment + ability + treatment*ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=low_data_expanded)
summary(model_cor_1.1)

model_cor_2 <- lm(medium_correct ~ treatment + ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(model_cor_2)

model_cor_2.1 <- lm(medium_correct ~ treatment + ability + treatment*ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(model_cor_2.1)

model_cor_3 <- lm(high_correct ~ treatment + ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(model_cor_3)

model_cor_3.1 <- lm(high_correct ~ treatment + ability + treatment*ability + treatment*ability_high + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(model_cor_3.1)

stargazer(model_cor_1, model_cor_1.1, model_cor_2,model_cor_2.1)
stargazer(model_cor_3, model_cor_3.1)

#### the dependent variable is 'performance' ######

model_perf_low_1 <- lm(low_perf ~ treatment + ability + cog_ab_low  + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=low_data_expanded)
summary(model_perf_low_1)

model_perf_low <- lm(low_perf ~ treatment + ability + treatment*ability + cog_ab_low  + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=low_data_expanded)
summary(model_perf_low)

model_perf_med_1 <- lm(medium_perf ~ treatment + ability + cog_ab_low + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(model_perf_med_1)

model_perf_med <- lm(medium_perf ~ treatment + ability + treatment*ability + cog_ab_low + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(model_perf_med)

model_perf_high_1 <- lm(high_perf ~ treatment + ability  + cog_ab_low + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(model_perf_high_1)

model_perf_high <- lm(high_perf ~ treatment + ability + treatment*ability + cog_ab_low + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(model_perf_high)

stargazer(model_perf_low_1, model_perf_low, model_perf_med_1, model_perf_med)
stargazer(model_perf_high, model_perf_high_1)

#### prob ability models for "lower effort under complex contract" binary variable ######

#let's start with probit ###

library(margins)
library(plm)

probit_low <- glm(binary_lower ~  ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education,
                      family = binomial(link = "probit"), 
                      data = low_data_expanded)
summary(probit_low)

probit_medium <- glm(binary_lower ~ ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education  ,
                  family = binomial(link = "probit"), 
                  data = medium_data_expanded)
summary(probit_medium)

probit_high <- glm(binary_lower ~  ability + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education ,
                  family = binomial(link = "probit"), 
                  data = high_data_expanded)
summary(probit_high)

# let's do some logit because #why nottttt ####

logit_low <- glm(binary_lower ~ ability + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education,
                  family = "binomial", 
                  data = low_data_expanded)
summary(logit_low)

logit_medium <- glm(binary_lower ~ ability + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education,
                    family = "binomial", 
                     data = medium_data_expanded)
summary(logit_medium)

logit_high <- glm(binary_lower ~ ability + french.6.player.cognitive_test_score + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education,
                   family = "binomial", 
                   data = high_data_expanded)
summary(logit_high)

stargazer(probit_low, logit_low, probit_medium, logit_medium)

#### the dependent variable is 'effort difference' ####

dif_low <- lm(effort_dif_low ~ ability  + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education , data=low_data_expanded)
summary(dif_low)

dif_low2 <- lm(effort_dif_low ~ ability + ability_high + ability_low + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education , data=low_data_expanded)
summary(dif_low2)

dif_med <- lm(effort_dif_med ~ ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(dif_med)

dif_med2 <- lm(effort_dif_med ~ ability + ability_high + ability_low  + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=medium_data_expanded)
summary(dif_med2)

dif_high <- lm(effort_dif_high ~  ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=high_data_expanded)
summary(dif_high)

stargazer(dif_low, dif_med)
stargazer(dif_low2, dif_med2)

######################## Regression analysis 2 for task difficulty #####
#### manipulating data for task difficulty #####
# Assuming 'data' is your original dataset and it has been expanded
data_difficulty <- data %>%
  uncount(3) %>%
  group_by(participant.code) %>%
  mutate(
    row_id = row_number(),  # Creates a sequence number within each group
    difficulty = case_when(
      row_id == 1 ~ correct_cs,
      row_id == 2 ~ correct_cm,
      row_id == 3 ~ correct_ch
    )
  ) %>%
  ungroup()  
View(data_difficulty) #row_id here is the treatment
data_difficulty$row_id <- as.factor(data_difficulty$row_id)


#linear regression for task difficulty ####

data_difficulty$row_id <- relevel(data_difficulty$row_id, ref = "2")

model_td <- lm(difficulty ~ row_id + ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=data_difficulty)
summary(model_td)

model_td_1 <- lm(difficulty ~ row_id + ability + row_id*ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=data_difficulty)
summary(model_td_1)

model_td_2 <- lm(difficulty ~ row_id + ability + row_id*french.6.player.cognitive_test_score + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=data_difficulty)
summary(model_td_2)

stargazer(model_td, model_td_1, model_td_2)

#### the dependent variable is 'effort difference' ####

data <- data %>%
  mutate(effort_dif_comp_med_easy = correct_cm - correct_cs)

data <- data %>%
  mutate(effort_dif_comp_dif_med = correct_ch - correct_cm)

yeter1 <- lm(effort_dif_comp_med_easy ~ ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=data)
summary(yeter1)

yeter2 <- lm(effort_dif_comp_dif_med ~ ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=data)
summary(yeter2)

stargazer(yeter1, yeter2)

#### the dependent variable is 'effort difference' 2 ####

data_difficulty2 <- data %>%
  uncount(2) %>%
  group_by(participant.code) %>%
  mutate(
    row = row_number(),  # Creates a sequence number within each group
    effort_difference = case_when(
      row == 1 ~ correct_cm - correct_cs,
      row == 2 ~ correct_ch - correct_cm,
    )
  ) %>%
  ungroup()  
View(data_difficulty2)

data_difficulty2$row <- as.factor(data_difficulty2$row)

yeter3 <- lm(effort_difference ~ row + ability + french.6.player.cognitive_test_score + cog_ab_low + french.6.player.gender + french.6.player.ordinateur + study_group + profession_group + french.6.player.education, data=data_difficulty2)
summary(yeter3)

stargazer(yeter1, yeter2, yeter3)
