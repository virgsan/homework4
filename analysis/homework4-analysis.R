
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

library(ggplot2)

#1
final.plans <- final.data %>%
  filter(snp == "No" & partd == "No" &
           (planid < 800 | planid >= 900))
question1 <- final.plans %>% group_by(county, year) %>% count() 

q1.plot <- question1 %>%
  ggplot(aes(x = factor(year), y = log(n))) +
  geom_boxplot() +
  labs (
    x="Year", 
    y = "Number of Plans"
  ) +
  theme_bw()

q1.plot
save.image("Hw4_workspace.Rdata")

#question2
data_filtered <- final.data %>% 
  filter(year %in% c(2009, 2012, 2015)) %>% 
  group_by(year, Star_Rating) %>% 
  summarise(count = n()) %>% 
  ungroup()
my_colors <- c("lightgreen", "purple", "skyblue")

ggplot(data_filtered, aes(x = Star_Rating, y = count, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = "Star Ratings", y = "Count", title = "Distribution of Star Ratings (2009, 2012, 2015)") +
  scale_fill_manual(values = my_colors, name = "Year") +
  ylim(0, 25000) +
  xlim(1, 5) +
  theme_classic()

#question3
avg_payments <- aggregate(ssa ~ year, data = benchmark.final, FUN = mean)

q3.plot <- ggplot(avg_payments, aes(x = year, y = ssa)) +
  geom_line() +
  labs(x = "Year", y = "Average Benchmark Payment ($)") +
  ggtitle("Average Benchmark Payment Over Time")

q3.plot
save.image("Hw4_workspace.Rdata")

#question4

#question5
final.data.2009 <- final.data %>% filter(year == 2009)

final.data.2009$running.variable <- ifelse(final.data.2009$Star_Rating < 3,
                                           final.data.2009$Star_Rating,
                                           ifelse(final.data.2009$Star_Rating == 3,
                                                  0.64995,
                                                  ifelse(final.data.2009$Star_Rating == 3.5,
                                                         0.72495,
                                                         ifelse(final.data.2009$Star_Rating == 4,
                                                                0.77495,
                                                                ifelse(final.data.2009$Star_Rating == 4.5,
                                                                       0.82495,
                                                                       0.875)))))

final.data.2009$star_rating_rounded <- round(final.data.2009$Star_Rating * 2) / 2
final.data.2009$star_rating_category <- paste0(final.data.2009$star_rating_rounded, "-star")

table(final.data.2009$star_rating_category)



#question6
library(rdrobust)
library(dplyr)



library(rdrobust)

# Estimate effect of 3-star versus 2.5-star rating on enrollments
model_3v2.5 <- rdrobust(y = final.data.2009$avg_enrollment,
                        x = final.data.2009$Star_Rating, 
                        h = 0.125)

# View summary of results
summary(model_3v2.5)




# Create a table summarizing the results
table_results <- data.frame(
  Cutoff = c(3, 3.5, 4, 4.5),
  Enrollment_Effect = c(results$`3`, results$`4`, results$`4.5`, results$`5`)
)

print(table_results)



