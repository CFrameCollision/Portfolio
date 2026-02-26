library(tidyverse)
library(readxl)
library(visdat)
library(naniar)

if (Sys.info()['sysname'] == c("Linux")) {
  setwd("~/Desktop/repositories/Killian-Research/")
} else {
  print("Windows boo")
}

main.data <- read_excel(
  "data/WSWBS Combined.xlsx",
  col_types = c(
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip"
  )
)

library(magrittr)

dplyr::count(main.data["D6"])

ggData <- main.data %>%
  dplyr::mutate(
    genderID = case_when(
      D6 == 1 ~ "Male",
      D6 == 2 ~ "Female",
      D6 == 3 ~ "Non-binary/third gender",
      D6 == 4 ~ "Prefer not to say",
      D6 == 5 ~ "Other gender identity",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate(
    raceID = case_when(
      D4 == 1 ~ "Native/Alaskan Native",
      D4 == 2 ~ "Asian",
      D4 == 3 ~ "Black/AA",
      D4 == 4 ~ "AAPI",
      D4 == 5 ~ "Hispanic/Latinx",
      D4 == 6 ~ "Caucasian",
      D4 == 7 ~ "Two+",
      D4 == 8 ~ "International",
      D4 == 9 ~ "Unknown",
      TRUE ~ NA_character_
    )
  ) %>%
  pivot_longer(
    cols = c("genderID", "raceID"),
    names_to = "Var",
    values_to = "Value",
  ) %>%
  mutate(
    Var = case_when(
      Var == "genderID" ~ "Gender",
      Var == "raceID" ~ "Race"
    )
  )

# descrip <- list()
# descrip$gender <- table(main.data["genderID"])

ggplot(ggData, aes(x = Value, fill = Var)) +
  geom_bar() +
  labs(x = NULL, y = "Count", title = "Gender & Race of Respondents") +
  facet_wrap(~Var, scales = "free", ) +
  scale_fill_manual(values = c("Gender" = "#9DD6AD", "Race" = "#EF9967")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -25, hjust = 0, vjust = 1),
    legend.position = "None"
  )

ggsave(
  "genderStats.png",
  path = "plots/ggplot",
  width = 2200,
  height = 1100,
  units = "px"
)

ggData <- main.data %>%
  dplyr::mutate(
    Standing = case_when(
      D1 == 1 ~ "Freshman",
      D1 == 2 ~ "Sophmore",
      D1 == 3 ~ "Junior",
      D1 == 4 ~ "Senior",
      D1 == 5 ~ "Continueing",
      D1 == 6 ~ "Graduate",
      TRUE ~ NA_character_
    ),
    Standing = factor(
      Standing,
      levels = c(
        "Freshman",
        "Sophmore",
        "Junior",
        "Senior",
        "Continueing",
        "Graduate"
      )
    ),
    Housing = case_when(
      D10 == 1 ~ "On",
      D10 == 2 ~ "Off",
      D10 == 0 ~ "Unknown",
      TRUE ~ NA_character_
    ),
    Housing = factor(
      Housing,
      levels = c("On", "Off", "Unknown")
    ),
  ) %>%
  pivot_longer(
    cols = c("Standing", "Housing"),
    names_to = "Var",
    values_to = "Value"
  )

ggplot(data = subset(ggData, !is.na(Value)), aes(x = Value)) +
  geom_bar() +
  labs(x = NULL, y = "Count", title = "Standing & Housing of Respondents") +
  facet_wrap(~Var, scales = "free", ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -25, hjust = 0, vjust = 1),
    legend.position = "None"
  )

ggsave(
  "standingHousingStats.png",
  path = "plots/ggplot",
  width = 2200,
  height = 1100,
  units = "px"
)

# Standing and housing as percentage

ggData <- ggData %>%
  filter(!is.na(Value)) %>%
  dplyr::count(Var, Value) %>%
  group_by(Var) %>%
  mutate(
    Percentage = n / sum(n)
  ) %>%
  ungroup()

ggplot(
  data = subset(ggData, !is.na(Value)),
  aes(x = Value, Percentage, fill = Var)
) +
  geom_col() +
  labs(x = NULL, y = "Percent", title = "Standing and Housing of Respondents") +
  facet_wrap(~Var, scales = "free_x") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("Standing" = "#9DD6AD", "Housing" = "#EF9967")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
    legend.position = "None"
  )

ggsave(
  "standingHousingStatsPct.png",
  path = "plots/ggplot",
  width = 2200,
  height = 1100,
  units = "px"
)

# Visualize Data missingness
vis_miss(main.data[39:53]) +
  theme(
    axis.text.x = element_text(angle = 80, vjust = 0, hjust = 0),
    plot.margin = unit(c(0.2, 1, 0.2, 1), "cm")
  )
ggsave("plots/descripMiss.jpg", width = 1900, height = 1900, units = "px")
vis_miss(main.data[1:38]) +
  theme(
    axis.text.x = element_text(angle = 80, vjust = 0, hjust = 0),
    plot.margin = unit(c(0.2, 1, 0.2, 1), "cm")
  )
ggsave("plots/dataMiss.jpg", width = 1900, height = 1900, units = "px")

factorStats <- read.csv("factorstats.csv")

corMatrix <- factorStats[9:17]
corMatrix <- bind_cols(corMatrix, main.data[39:53])
corMatrixFacsDemo <- stats::cor(
  factorStats[9:30],
  use = "pairwise.complete.obs"
)

corMatrixFacsDemo %>% write.csv("corFacsDemo.csv")
