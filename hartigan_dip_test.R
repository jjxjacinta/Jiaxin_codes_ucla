install.packages("diptest")   # 只安装一次

library(diptest)
library(dplyr)

data <- read.csv("D:/desktop/example_one_dimensional_gmm.csv")

#three groups
library(purrr)
library(tidyr)

dip_results <- data %>%
  filter(!is.na(percent_voiced)) %>%
  group_by(group) %>%
  nest() %>%
  mutate(
    test = map(
      data,
      ~ dip.test(.x$percent_voiced)
    ),
    n = map_int(data, nrow),
    dip = map_dbl(
      test,
      ~ unname(.x$statistic)
    ),
    p = map_dbl(
      test,
      ~ .x$p.value
    )
  ) %>%
  select(group, n, dip, p) %>%
  mutate(
    dip = round(dip, 3),
    p_display = format.pval(
      p,
      digits = 3,
      eps = 0.001
    )
  )

dip_results

#density plot
library(ggplot2)

ggplot(
  data,
  aes(
    x = percent_voiced,
    color = group
  )
) +
  geom_density(linewidth = 1) +
  labs(
    x = "Percent voiced",
    y = "Density",
    color = "Group"
  ) +
  theme_classic()
