library(dplyr)
library(purrr)
library(tidyr)
library(mclust)

data <- read.csv("D:/desktop/example_f1f2_by_speaker.csv")

bi_data <- data %>%
  filter(
    group == "BiSpanish",
    vowel == "e",
    !is.na(F1_norm),
    !is.na(F2_norm)
  )

#GMM by speaker
run_speaker_gmm <- function(df, max_components = 3) {
  
  X <- df %>%
    select(F1_norm, F2_norm)
  
  # token 太少时不运行
  if (nrow(X) < 20) {
    return(NULL)
  }
  
  model <- Mclust(
    X,
    G = 1:max_components
  )
  
  data.frame(
    speaker = df$speaker[1],
    n = nrow(df),
    best_components = model$G,
    model_name = model$modelName,
    best_BIC = max(model$bic, na.rm = TRUE)
  )
}

speaker_gmm_results <- bi_data %>%
  group_by(speaker) %>%
  group_split() %>%
  map_dfr(run_speaker_gmm)

speaker_gmm_results

#save to models
speaker_models <- bi_data %>%
  group_by(speaker) %>%
  nest() %>%
  mutate(
    model = map(
      data,
      function(df) {
        X <- df %>%
          select(F1_norm, F2_norm)
        
        if (nrow(X) < 20) {
          return(NULL)
        }
        
        Mclust(
          X,
          G = 1:3
        )
      }
    )
  )

#check models for speaker BS07
model_BS07 <- speaker_models %>%
  filter(speaker == "BS07") %>%
  pull(model) %>%
  .[[1]]

summary(model_BS07)

#to table
extract_centers <- function(model) {
  
  if (is.null(model)) {
    return(NULL)
  }
  
  centers <- as.data.frame(
    t(model$parameters$mean)
  )
  
  names(centers) <- c(
    "F1_center",
    "F2_center"
  )
  
  centers %>%
    mutate(
      component = row_number(),
      n_components = model$G
    ) %>%
    select(
      component,
      n_components,
      F1_center,
      F2_center
    )
}

speaker_centers <- speaker_models %>%
  mutate(
    centers = map(
      model,
      extract_centers
    )
  ) %>%
  select(
    speaker,
    centers
  ) %>%
  unnest(centers)

speaker_centers

#add back to the original data
add_gmm_classification <- function(df) {
  
  X <- df %>%
    select(F1_norm, F2_norm)
  
  if (nrow(X) < 20) {
    df$component <- NA_integer_
    df$uncertainty <- NA_real_
    return(df)
  }
  
  model <- Mclust(
    X,
    G = 1:3
  )
  
  df$component <- model$classification
  df$uncertainty <- model$uncertainty
  df$best_G <- model$G
  
  df
}

bi_classified <- bi_data %>%
  group_by(speaker) %>%
  group_modify(
    ~ add_gmm_classification(.x)
  ) %>%
  ungroup()

head(bi_classified)

#draw GMM by-speaker
library(ggplot2)

ggplot(
  bi_classified,
  aes(
    x = F2_norm,
    y = F1_norm,
    color = factor(component)
  )
) +
  geom_point(
    alpha = 0.75,
    size = 1.2
  ) +
  stat_ellipse(
    aes(group = component),
    type = "norm",
    linewidth = 0.6
  ) +
  facet_wrap(
    ~ speaker,
    ncol = 4
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "By-speaker Gaussian mixture models",
    x = "Normalized F2",
    y = "Normalized F1",
    color = "Component"
  ) +
  theme_classic()

#compare one-component with two-component
compare_1_vs_2 <- function(df) {
  
  X <- df %>%
    select(F1_norm, F2_norm)
  
  if (nrow(X) < 20) {
    return(NULL)
  }
  
  bic_models <- mclustBIC(
    X,
    G = 1:2
  )
  
  bic_matrix <- as.matrix(bic_models)
  
  bic_1 <- max(
    bic_matrix["1", ],
    na.rm = TRUE
  )
  
  bic_2 <- max(
    bic_matrix["2", ],
    na.rm = TRUE
  )
  
  data.frame(
    speaker = df$speaker[1],
    n = nrow(df),
    BIC_1 = bic_1,
    BIC_2 = bic_2,
    delta_BIC_2_minus_1 = bic_2 - bic_1
  )
}

speaker_bic_comparison <- bi_data %>%
  group_by(speaker) %>%
  group_split() %>%
  map_dfr(compare_1_vs_2)

speaker_bic_comparison

#combine best model and BIC
speaker_gmm_summary <- speaker_gmm_results %>%
  left_join(
    speaker_bic_comparison,
    by = c("speaker", "n")
  ) %>%
  arrange(desc(delta_BIC_2_minus_1))

speaker_gmm_summary
