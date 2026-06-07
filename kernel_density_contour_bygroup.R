data <- read.csv("D:/desktop/example_vowel_kde_data.csv")

head(data)
table(data$group, data$vowel)

#normal contour
library(ggplot2)
library(dplyr)

e_data <- data %>%
  filter(vowel == "e")

ggplot(
  e_data,
  aes(
    x = F2_norm,
    y = F1_norm,
    color = group
  )
) +
  geom_point(alpha = 0.18, size = 1) +
  geom_density_2d(
    aes(group = group),
    bins = 5,
    linewidth = 1
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    x = "Normalized F2",
    y = "Normalized F1",
    color = "Group"
  ) +
  theme_classic()

#with fillings
ggplot(
  e_data,
  aes(x = F2_norm, y = F1_norm)
) +
  geom_density_2d_filled(
    aes(fill = after_stat(level)),
    bins = 6,
    alpha = 0.7
  ) +
  geom_point(alpha = 0.12, size = 0.7) +
  facet_wrap(~ group) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    x = "Normalized F2",
    y = "Normalized F1",
    fill = "Density"
  ) +
  theme_classic()
