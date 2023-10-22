# Qualitative Data Tutorial

setwd("/Users/chungchenran/Documents/Google_Certificate/Coding_Club_R_Tutorial2/CC-Qualit-master")

install.packages("tidytext")
install.packages("wordcloud")
install.packages("viridis")

library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(R.utils)
library(wordcloud)
library(viridis)

# The survey responses
sust_data <- read_csv("sust_behaviour.csv")

# A lookup table which connects each column in `sust_data` to the actual question on the survey
sust_lookup <- read_csv("sust_lookup.csv")

# A list of boring and non-useful words, bundled with `tidytext`
data(stop_words)

unique(sust_data$sustainability_daily_think)
sust_data$sustainability_daily_think <- factor(sust_data$sustainability_daily_think,
                                               levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"),
                                               ordered = TRUE)

head(sust_data)

glimpse(sust_data)

unique(sust_data$housework_week)
sust_data$housework_week <- factor(sust_data$housework_week,
                                   levels = c("Less than 1 hour", "1-3 hours", "4-6 hours", "7-10 hours", "10+ hours"),
                                   ordered = TRUE)

unique(sust_data$interest_sustainability)
sust_data$interest_sustainability <- factor(sust_data$interest_sustainability,
                                            levels = c("No", "Somewhat", "Yes"),
                                            ordered = TRUE)

unique(sust_data$energy_action_often)
sust_data$energy_action_often <- factor(sust_data$energy_action_often,
                                        levels = c("Never", "Sometimes", "Often", "All the time"),
                                        ordered = TRUE)

unique(sust_data$water_action_often)
sust_data$water_action_often <- factor(sust_data$water_action_often,
                                       levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"),
                                       ordered = TRUE)

unique(sust_data$food_action_often)
sust_data$food_action_often <- factor(sust_data$food_action_often,
                                      levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"),
                                      ordered = TRUE)

unique(sust_data$waste_action_often)
sust_data$waste_action_often <- factor(sust_data$waste_action_often,
                                       levels = c("Rarely", "Sometimes", "Often", "All the time"),
                                       ordered = TRUE)

unique(sust_data$other_action_often)
sust_data$other_action_often <- factor(sust_data$other_action_often,
                                       levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"),
                                       ordered = TRUE)

sust_data$energy_action_n <- nchar(as.character(sust_data$energy_action))

sust_lookup

sust_data$sustainability_daily_think

sust_think_summ_wide <- sust_data %>%
  group_by(gender, sustainability_daily_think) %>%  # grouping by these two variables
  tally() %>%  # counting the number of responses
  mutate(perc = n / sum(n) * 100) %>%
  dplyr::select(-n) %>%
  group_by(gender) %>%
  spread(sustainability_daily_think, perc)

sust_think_summ_hi_lo <- sust_think_summ_wide %>%
  mutate(midlow = Sometimes / 2,
         midhigh = Sometimes / 2) %>%
  dplyr::select(gender, Never, Rarely, midlow, midhigh, Often, `All the time`) %>%
  gather(key = response, value = perc, 2:7) %>%
  `colnames<-`(c("gender", "response", "perc"))

sust_think_summ_hi <- sust_think_summ_hi_lo %>%
  filter(response %in% c("All the time", "Often", "midhigh")) %>%
  mutate(response = factor(response, levels = c("All the time", "Often", "midhigh")))

sust_think_summ_lo <- sust_think_summ_hi_lo %>%
  filter(response %in% c("midlow", "Rarely", "Never")) %>%
  mutate(response = factor(response, levels = c("Never", "Rarely", "midlow")))

# Use RColorBrewer to store a preset diverging colour palette as a vector of colour codes
legend_pal <- brewer.pal(name = "RdBu", n = 5)

# Duplicate the middle value, remember that "Sometimes" is actually two groups, "midhigh" and "midlow"
#legend_pal <- insert(legend_pal, ats = 3, legend_pal[3])  # the method "insert" does not exist
legend_pal <- append(legend_pal, legend_pal[3], after = 3)

# Replace the ugly white colour for "Sometimes" with a pleasant dishwater grey
legend_pal <- gsub("#F7F7F7", "#9C9C9C", legend_pal)

# Assign names to the vector based on the colours we want for each group
names(legend_pal) <- c("All the time", "Often", "midhigh", "midlow", "Rarely", "Never" )

(plot <- ggplot() +
    geom_bar(data = sust_think_summ_hi, aes(x = gender, y=perc, fill = response), stat="identity") +
    geom_bar(data = sust_think_summ_lo, aes(x = gender, y=-perc, fill = response), stat="identity") +
    geom_hline(yintercept = 0, color =c("black")) +
    scale_fill_manual(values = legend_pal,
                      breaks = c("All the time", "Often", "midhigh", "Rarely", "Never"),
                      labels = c("All the time", "Often", "Sometimes", "Rarely", "Never")) +
    coord_flip() +
    labs(x = "Gender", y = "Percentage of respondents (%)") +
    ggtitle(sust_lookup$survey_question[sust_lookup$column_title == "sustainability_daily_think"]) +
    theme_classic())

(barchart <- ggplot(sust_data, aes(x =energy_action_n, fill = age)) +
    geom_bar() +
    scale_fill_viridis_d() +
    scale_x_continuous(breaks = seq(1:8)) +
    theme_classic())

sust_bubble <- sust_data %>%
  group_by(age, sustainability_daily_think) %>%
  tally()

(bubbleplot <- ggplot(sust_bubble, aes(x = age, y = sustainability_daily_think)) +
    geom_point(aes(size = n)) +
    theme_classic())

head(sust_data$energy_action_comment, 20)

sust_comm_gather <- sust_data %>%
  dplyr::select(id, gender, energy_action_comment,
                food_action_comment, water_action_comment,
                waste_action_comment, other_action_comment) %>%
  gather(action, comment, -id, -gender) %>%
  mutate(comment = as.character(comment))

sust_comm_tidy <- sust_comm_gather %>%
  unnest_tokens(output = comment_word,
                input = comment) %>%
  filter(!(is.na(comment_word)),
         is.na(as.numeric(comment_word)),
         !(comment_word %in% stop_words$word)) %>%
  group_by(gender, comment_word) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 10)

male_female_pal <- c("#0389F0", "#E30031")
names(male_female_pal) <- c("Male", "Female")

(occurrence <- ggplot(sust_comm_tidy, aes(x = comment_word, y = n, fill = gender)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = male_female_pal) +
    theme_classic())

tidy_energy_often_comment <- sust_data %>%
  mutate(energy_action_comment = as.character(energy_action_comment)) %>%
  unnest_tokens(output = energy_action_comment_word,
                input = energy_action_comment) %>%
  filter(!(is.na(energy_action_comment_word)),
         is.na(as.numeric(energy_action_comment_word)),
         !(energy_action_comment_word %in% stop_words$word)) %>%
  group_by(gender, energy_action_comment_word) %>%
  summarise(n = n()) %>%
  ungroup()

tidy_energy_often_comment_summ <- tidy_energy_often_comment %>%
  filter(n > 10) %>%
  mutate(energy_action_comment_word = reorder(energy_action_comment_word, n ))

(most_common_plot <- ggplot(tidy_energy_often_comment_summ, aes(x = energy_action_comment_word, y = n)) +
    geom_col() +
    xlab(NULL) +  # this means we don't want an axis title
    coord_flip() +
    theme_classic())

tidy_energy_often_comment %>%
  with(wordcloud(words = energy_action_comment_word, freq = n, max.words = 100))

gender_think_chi <- chisq.test(sust_data$gender, sust_data$sustainability_daily_think)
gender_think_chi

energy_action_pois <- glm(energy_action_n ~ gender, family = "poisson", data = sust_data)
summary(energy_action_pois)

energy_action_pois_int <- glm(energy_action_n ~ gender * age, family = "poisson", data = sust_data)
summary(energy_action_pois_int)

