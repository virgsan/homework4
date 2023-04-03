
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

library(ggplot2)
install.packages("rdrobust")
library(rdrobust)

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

q2.plot <- ggplot(data_filtered, aes(x = Star_Rating, y = count, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = "Star Ratings", y = "Count", title = "Distribution of Star Ratings (2009, 2012, 2015)") +
  scale_fill_manual(values = my_colors, name = "Year") +
  ylim(0, 25000) +
  xlim(1, 5) +
  theme_classic()

q2.plot
save.image("Hw4_workspace.Rdata")

#question3
avg_payments <- aggregate(ssa ~ year, data = benchmark.final, FUN = mean)

q3.plot <- ggplot(avg_payments, aes(x = year, y = ssa)) +
  geom_line() +
  labs(x = "Year", y = "Average Benchmark Payment ($)") +
  ggtitle("Average Benchmark Payment Over Time")

q3.plot
save.image("Hw4_workspace.Rdata")

#question4

final_data_four <- final.data %>%
  filter(year >= 2009 & year <= 2015)

final_data_four_new <- final_data_four %>% 
  mutate(dummy_data = ifelse(partd == "No", 0, 1))

q4_plot <- final_data_four_new %>%
  group_by(year) %>%
  summarise(avg_dummy_data = mean(dummy_data)) %>%
  ggplot(aes(year,avg_dummy_data)) + 
  geom_line() + 
  labs(x = "Year", y = "Average Share of Medicare Advantage",
       title = "Average Share of Medicare Advantage from 2009 through 2015")

q4_plot
save.image("Hw4_workspace.Rdata")

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
save.image("Hw4_workspace.Rdata")

#ian's5
final.data.2009 <- final.data %>%
  filter(year == 2009)

final.data.2009.new <- final.data.2009 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment,state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate)

final.data.2009.new <- final.data.2009.new[!is.na(final.data.2009.new$Star_Rating), ]

final.data.2009.new$indicator <- ifelse(final.data.2009.new$Star_Rating > final.data.2009.new$raw_rating, 1,0)

table_5 <- final.data.2009.new %>% group_by(Star_Rating) %>% 
  summarise(mean(indicator))
table_5
save.image("Hw4_workspace.Rdata")

#question6


ma.rd6 <- final.data.2009.new %>%
  mutate(score1 = raw_rating - 2.75,
         score2 =raw_rating - 3.25, 
         score3 = raw_rating - 3.75, 
         score4 = raw_rating - 4.5,
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share))


regression6_1 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                          h=0.125, p=1, kernel="uniform", vce="hc0",
                          masspoints="off")

summary(regression6_1)

save.image("Hw4_workspace.Rdata")

regression6_2 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score2, c=0,
                          h=0.125, p=1, kernel="uniform", vce="hc0",
                          masspoints="off")

summary(regression6_2)
save.image("Hw4_workspace.Rdata")

regression6_3 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score3, c=0,
                          h=0.125, p=1, kernel="uniform", vce="hc0",
                          masspoints="off")

summary(regression6_3)
save.image("Hw4_workspace.Rdata")


#question7

reg_1 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                  h=0.1, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_1
reg_2 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                  h=0.12, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")
reg_2
reg_3 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                  h=0.13, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_4 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                  h=0.14, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_5 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                  h=0.15, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

#question8
graph_7a <- rdplot(y=ma.rd6$mkt_share, x=ma.rd6$score1, binselect = "es", 
                   title = "RD Plot: Market Share", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7a
save.image("Hw4_workspace.Rdata")

graph_7b <- rdplot(y=ma.rd6$mkt_share, x=ma.rd6$score2, binselect = "es", 
                   title = "RD Plot: Market Share", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7b
save.image("Hw4_workspace.Rdata")

graph_7c <- rdplot(y=ma.rd6$mkt_share, x=ma.rd6$score3, binselect = "es", 
                   title = "RD Plot: Market Share", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7c
save.image("Hw4_workspace.Rdata")
