data <- read.csv("D:/desktop/example_pillai_e_comparisons.csv")

table(data$group, data$vowel)

#create functions
library(dplyr)

run_pillai_vowels <- function(
    df,
    group_1,
    vowel_1,
    group_2,
    vowel_2,
    comparison_label = NULL
) {
  
  comparison_data <- df %>%
    filter(
      (group == group_1 & vowel == vowel_1) |
        (group == group_2 & vowel == vowel_2)
    ) %>%
    filter(
      !is.na(F1_norm),
      !is.na(F2_norm)
    ) %>%
    mutate(
      comparison_group = case_when(
        group == group_1 & vowel == vowel_1 ~
          paste(group_1, vowel_1),
        
        group == group_2 & vowel == vowel_2 ~
          paste(group_2, vowel_2)
      ),
      comparison_group = factor(comparison_group)
    )
  
  if (nlevels(comparison_data$comparison_group) != 2) {
    stop("The comparison does not contain exactly two valid groups.")
  }
  
  model <- manova(
    cbind(F1_norm, F2_norm) ~ comparison_group,
    data = comparison_data
  )
  
  stats <- summary(
    model,
    test = "Pillai"
  )$stats
  
  if (is.null(comparison_label)) {
    comparison_label <- paste(
      group_1, vowel_1,
      "vs.",
      group_2, vowel_2
    )
  }
  
  data.frame(
    comparison = comparison_label,
    n_group_1 = sum(
      comparison_data$group == group_1 &
        comparison_data$vowel == vowel_1
    ),
    n_group_2 = sum(
      comparison_data$group == group_2 &
        comparison_data$vowel == vowel_2
    ),
    pillai = unname(
      stats["comparison_group", "Pillai"]
    ),
    F = unname(
      stats["comparison_group", "approx F"]
    ),
    df1 = unname(
      stats["comparison_group", "num Df"]
    ),
    df2 = unname(
      stats["comparison_group", "den Df"]
    ),
    p = unname(
      stats["comparison_group", "Pr(>F)"]
    )
  )
}


#three comparisons
#bispanish /e/ vs monospanish /e/
result_spanish <- run_pillai_vowels(
  df = data,
  group_1 = "BiSpanish",
  vowel_1 = "e",
  group_2 = "MonoSpanish",
  vowel_2 = "e",
  comparison_label =
    "BiSpanish /e/ vs. MonoSpanish /e/"
)
#bispanish /e/ vs portuguese close-mid
result_portuguese_close <- run_pillai_vowels(
  df = data,
  group_1 = "BiSpanish",
  vowel_1 = "e",
  group_2 = "MonoPortuguese",
  vowel_2 = "e_close",
  comparison_label =
    "BiSpanish /e/ vs. Portuguese close-mid /e/"
)

#BiSpanish /e/ vs Portuguese open-mid /ɛ/
result_portuguese_open <- run_pillai_vowels(
  df = data,
  group_1 = "BiSpanish",
  vowel_1 = "e",
  group_2 = "MonoPortuguese",
  vowel_2 = "e_open",
  comparison_label =
    "BiSpanish /e/ vs. Portuguese open-mid /ɛ/"
)

#to a table
library(tibble)

pillai_results_e <- bind_rows(
  result_spanish,
  result_portuguese_close,
  result_portuguese_open
) %>%
  as_tibble() %>%
  mutate(
    pillai = round(pillai, 3),
    F = round(F, 2),
    p_display = format.pval(
      p,
      digits = 3,
      eps = 0.001
    )
  ) %>%
  select(
    comparison,
    n_group_1,
    n_group_2,
    pillai,
    F,
    df1,
    df2,
    p_display
  )

print(pillai_results_e, width = Inf)

#calculate the center position
vowel_centers <- data %>%
  filter(
    (group == "BiSpanish" &
       vowel == "e") |
      
      (group == "MonoSpanish" &
         vowel == "e") |
      
      (group == "MonoPortuguese" &
         vowel %in% c("e_close", "e_open"))
  ) %>%
  group_by(group, vowel) %>%
  summarise(
    mean_F1 = mean(F1_norm, na.rm = TRUE),
    mean_F2 = mean(F2_norm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

vowel_centers
#calculate the euclidean distance between bispanish /e/ and the other three baselines
bi_center <- vowel_centers %>%
  filter(
    group == "BiSpanish",
    vowel == "e"
  ) %>%
  select(mean_F1, mean_F2)

distance_results <- vowel_centers %>%
  filter(
    !(group == "BiSpanish" & vowel == "e")
  ) %>%
  mutate(
    distance_from_BiSpanish =
      sqrt(
        (mean_F1 - bi_center$mean_F1)^2 +
          (mean_F2 - bi_center$mean_F2)^2
      )
  )

distance_results

