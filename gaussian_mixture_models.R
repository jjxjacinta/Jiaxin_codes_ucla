install.packages("mclust")

library(mclust)
library(dplyr)
library(ggplot2)

data <- read.csv("D:/desktop/example_vowel_kde_data.csv")

data$group <- factor(data$group)
data$vowel <- factor(data$vowel)

# BiSpanish /e/
e_bispanish <- data %>%
  filter(
    vowel == "e",
    group == "BiSpanish",
    !is.na(F1_norm),
    !is.na(F2_norm)
  )

# 二维数据矩阵
X <- e_bispanish %>%
  select(F1_norm, F2_norm)

# 比较 1、2、3 个 components
gmm_e <- Mclust(
  X,
  G = 1:3
)

summary(gmm_e)

#drawing distribution
e_bispanish$component <- factor(gmm_e$classification)

ggplot(
  e_bispanish,
  aes(
    x = F2_norm,
    y = F1_norm,
    color = component
  )
) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(
    aes(group = component),
    type = "norm",
    linewidth = 0.9
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Gaussian mixture model: BiSpanish /e/",
    x = "Normalized F2",
    y = "Normalized F1",
    color = "Component"
  ) +
  theme_classic()

#compare one component vs two components
gmm_1 <- Mclust(
  X,
  G = 1
)

gmm_2 <- Mclust(
  X,
  G = 2
)

summary(gmm_1)
summary(gmm_2)

#extract BIC
bic_comparison <- data.frame(
  components = c(1, 2),
  BIC = c(
    max(gmm_1$bic, na.rm = TRUE),
    max(gmm_2$bic, na.rm = TRUE)
  )
)

bic_comparison

#calculate BIC: delta_BIC>0, two-component better; delta_BIC<0, one-component better
delta_BIC <- bic_comparison$BIC[2] -
  bic_comparison$BIC[1]

delta_BIC

#draw BIC: highest BIC, best model
plot(
  gmm_e,
  what = "BIC"
)

#check the centers of the two components
component_centers <- as.data.frame(
  t(gmm_e$parameters$mean)
)

component_centers$component <-
  rownames(component_centers)

component_centers

#compare with Portuguese baseline
#Portuguese /e/ center
portuguese_e_center <- data %>%
  filter(
    vowel == "e",
    group == "MonoPortuguese"
  ) %>%
  summarise(
    F1_norm = mean(F1_norm, na.rm = TRUE),
    F2_norm = mean(F2_norm, na.rm = TRUE)
  )

portuguese_e_center

#calculate Euclidean distance between each bilingual component and Portuguese center
component_centers %>%
  mutate(
    distance_to_Portuguese =
      sqrt(
        (F1_norm - portuguese_e_center$F1_norm)^2 +
          (F2_norm - portuguese_e_center$F2_norm)^2
      )
  )

#GMM for Portuguese /e/
e_portuguese <- data %>%
  filter(
    vowel == "e",
    group == "MonoPortuguese"
  )

X_portuguese <- e_portuguese %>%
  select(F1_norm, F2_norm)

gmm_portuguese <- Mclust(
  X_portuguese,
  G = 1:3
)

summary(gmm_portuguese)

gmm_portuguese$parameters$mean

#GMM for all groups and vowels
run_gmm <- function(df, max_components = 3) {
  
  complete_df <- df %>%
    filter(
      !is.na(F1_norm),
      !is.na(F2_norm)
    )
  
  if (nrow(complete_df) < 10) {
    return(NULL)
  }
  
  X <- complete_df %>%
    select(F1_norm, F2_norm)
  
  model <- Mclust(
    X,
    G = 1:max_components
  )
  
  data.frame(
    group = as.character(complete_df$group[1]),
    vowel = as.character(complete_df$vowel[1]),
    n = nrow(complete_df),
    best_components = model$G,
    model_name = model$modelName,
    BIC = max(model$bic, na.rm = TRUE)
  )
}

gmm_results <- data %>%
  group_by(group, vowel) %>%
  group_split() %>%
  lapply(run_gmm) %>%
  bind_rows()

gmm_results


#one-dimensional data, e.g., 'percent_voiced'
s_data <- data_s %>%
  filter(
    group == "BiSpanish",
    !is.na(percent_voiced)
  )

gmm_s <- Mclust(
  s_data$percent_voiced,
  G = 1:3
)

summary(gmm_s)

table(gmm_s$classification)

#draw
s_data$component <- factor(
  gmm_s$classification
)

ggplot(
  s_data,
  aes(
    x = percent_voiced,
    fill = component
  )
) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Percent voiced",
    y = "Density",
    fill = "Component"
  ) +
  theme_classic()
