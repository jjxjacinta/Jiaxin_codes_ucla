library(dplyr)
library(ggplot2)

data <- read.csv("D:/desktop/example_f1f2_by_speaker.csv")

table(data$group, data$vowel)
table(data$speaker, data$pattern)

#by-speaker density contours
bi_data <- data %>%
  filter(
    group == "BiSpanish",
    vowel == "e"
  )

ggplot(
  bi_data,
  aes(
    x = F2_norm,
    y = F1_norm
  )
) +
  geom_point(
    alpha = 0.25,
    size = 0.7
  ) +
  geom_density_2d(
    bins = 5,
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ speaker,
    ncol = 4
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Bilingual Spanish /e/ by speaker",
    x = "Normalized F2",
    y = "Normalized F1"
  ) +
  theme_classic()

#pooled by-speaker contour
ggplot(
  bi_data,
  aes(
    x = F2_norm,
    y = F1_norm,
    group = speaker
  )
) +
  geom_density_2d(
    bins = 4,
    linewidth = 0.6,
    alpha = 0.6
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Bilingual Spanish /e/: by-speaker density contours",
    x = "Normalized F2",
    y = "Normalized F1"
  ) +
  theme_classic()

#by-speaker mean and SD
speaker_summary <- bi_data %>%
  group_by(speaker, pattern) %>%
  summarise(
    n = n(),
    mean_F1 = mean(F1_norm),
    mean_F2 = mean(F2_norm),
    sd_F1 = sd(F1_norm),
    sd_F2 = sd(F2_norm),
    .groups = "drop"
  )

speaker_summary

#speaker centers
ggplot(
  speaker_summary,
  aes(
    x = mean_F2,
    y = mean_F1,
    label = speaker
  )
) +
  geom_point(size = 3) +
  geom_text(
    nudge_y = -0.035,
    size = 3
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Mean location of each bilingual speaker",
    x = "Mean normalized F2",
    y = "Mean normalized F1"
  ) +
  theme_classic()

#by-speaker fillings
ggplot(
  bi_data,
  aes(
    x = F2_norm,
    y = F1_norm
  )
) +
  geom_density_2d_filled(
    bins = 5,
    alpha = 0.75
  ) +
  geom_point(
    alpha = 0.12,
    size = 0.5
  ) +
  facet_wrap(
    ~ speaker,
    ncol = 4
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Bilingual Spanish /e/ by speaker",
    x = "Normalized F2",
    y = "Normalized F1",
    fill = "Density"
  ) +
  theme_classic()
