# Shuya Kong 1505077
# ECON 294 Assignment 5

install.packages("ggplot2")
library(ggplot2)

#############
# 1a:
ggplot(data = diamonds,
       aes(x = x*y*z, y = price,size = carat, colour = clarity),
       scale_x_log10(),
       scale_y_log10()) +
  geom_point()


# 1b:

ggplot(diamonds, aes(x = carat, y = ..density.., fill = clarity)) +
  geom_histogram() +
  facet_grid("cut~.")


# 1c:

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin() +
  geom_jitter(alpha = 0.01)


# 3a: (not skipping questions but just marking the number as shown on the assignment )

library(dplyr)
library(foreign)
org <- read.dta(file = "D:/RStudio/Rdata/org_example.dta")

org <- org %>%
  filter(!is.na(rw)) %>%
  group_by(year, month) %>%
  mutate(
    median.rw = median(rw),
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
    ) %>%
  tbl_df()


ggplot(org, aes(date, median.rw)) +
  geom_ribbon(alpha = 0.2, aes(ymin = quantile(rw, 0.10), ymax = quantile(rw, 0.90))) +
  geom_ribbon(aes(ymin = quantile(rw, 0.25), ymax = quantile(rw, 0.75))) +
  geom_line()


# 3b:

org <- org %>%
  filter(!is.na(rw)) %>%
  group_by(educ, year, month) %>%
  mutate(
    median.rw = median(rw),
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  tbl_df()

ggplot(org, aes(date, median.rw, colour = educ)) +
  geom_line()

##############