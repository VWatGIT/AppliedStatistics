library(ggplot2)
library(dplyr)

dat = read.csv("health_extended.csv", header = TRUE, sep=";")

#first producing a graph of the total diseases over the years
dat_focused <- dat |> 
  filter(X2_variable_attribute_label != "Total" & X3_variable_attribute_label != "Total" & X4_variable_attribute_label != "Total") |> 
  select(c("time", "X2_variable_attribute_label", "X3_variable_attribute_label", "X4_variable_attribute_label", "value"))

total_by_year <- aggregate(dat_focused["value"], by = list(time = dat_focused$time), FUN = sum)

#plotting the cancer totals, aggregating gender and age
ggplot(total_by_year, aes(time, value, group=1)) +
  geom_line()

total_by_year_and_gender <- aggregate(dat_focused["value"], by = list(time = dat_focused$time, dat_focused$X2_variable_attribute_label), FUN = sum)

#plotting and faceting by gender
ggplot(total_by_year_and_gender, aes(time, value, group=1)) +
  geom_line() +
  facet_wrap(~ Group.2)

total_by_year_and_age <- aggregate(dat_focused["value"], by = list(time = dat_focused$time, dat_focused$X3_variable_attribute_label), FUN = sum)

#plotting and faceting by age group
# total_by_1_to_25 <- total_by_year_and_age |>
#   filter(as.numeric(substr(Group.2, 1, 2)) <= 20) |>
#     filter(!is.na(Group.2))

ggplot(total_by_year_and_age, aes(time, value, color=Group.2)) +
  geom_line()

#creating contingency table based on age and year
cont_table_ages = ftable(xtabs(value ~ Group.2+time, data=total_by_year_and_age))

#creating expected probability distribution for cancers where it is assumed that each age group is equally likely to suffer from cancer
expected_probability_table = matrix(1/22, 22, 25)

#performing a chi squared test
chisq.test(x = cont_table_ages, p = expected_probability_table) 
# I guess the null hypothesis is so very rejected lol
