library(ggplot2)
library(dplyr)

dat = read.csv("health.csv", header = TRUE, sep=";")

#first producing a graph of the total diseases over the years
dat_no_totals = dat[dat$X2_variable_attribute_label != "Total",]
dat_focused = dat_no_totals[c("time", "X2_variable_attribute_label", "value")]

dat_focused$value <- as.numeric(dat_focused$value)
dat_focused$time <- as.numeric(dat_focused$time)
dat_focused = dat_focused[!is.na(dat_focused$value), ]

total_by_year <- aggregate(dat_focused["value"], by = list(time = dat_focused$time), FUN = sum)

ggplot(total_by_year, aes(time, value)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(total_by_year$time),
                                  max(total_by_year$time),
                                  by = 1))

#extracting only the numbers of cancer in-patients
dat_cancer = dat[dat$X2_variable_attribute_code == "ICD10-C00-C97",]
ggplot(dat_cancer, aes(time, value, group = 1)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(total_by_year$time),
                                  max(total_by_year$time),
                                  by = 1))