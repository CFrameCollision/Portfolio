library(VIM)

if (exists("main.data", envir = .GlobalEnv) == T) {
  print("Good to go!")
} else {
  source("factorAnalysisStuff.R")
}

imp1 <- read.csv("impData.csv")
imp1 <- imp1[-1]
data1 <- main.data[-c(1, 7, 27:30, 33, 39:53)]

# Comparing imputation vs data
for (a in c(1:31)) {
  paired_df <- data.frame(imp1 = imp1[a], data1 = data1[a])

  if (a >= 6) {
    a <- a + 1
    if (a >= 26) {
      a <- a + 2
      if (a >= 30) {
        a <- a + 1
      } else {}
    } else {}
  } else {}

  colnames(paired_df) <- c(paste0("Imp", a, sep = ""), paste0("Q", a, sep = ""))

  long_df <- paired_df %>%
    pivot_longer(
      cols = c(paste0("Imp", a, sep = ""), paste0("Q", a, sep = "")),
      names_to = "Dataset",
      values_to = "Value"
    )
  print(
    ggplot(long_df, aes(x = Value, fill = Dataset)) +
      geom_density(alpha = 0.2) +
      labs(
        title = paste("Q", a, ": ", "Imp vs Data", sep = ""),
        x = "Value",
        y = "Density"
      )
  )
  print(
    ggplot(long_df, aes(x = Value, fill = Dataset)) +
      geom_histogram(position = "identity", alpha = 0.5) +
      labs(
        title = paste("Q", a, ": ", "Imp vs Data", sep = ""),
        x = "Value",
        y = "Count"
      )
  )
}

# Diagnosing data's missingness. Need to redo imputation.

patternMissingness <- md.pattern(data1, plot = F)

flux(data1)
fluxplot(data1)
for (a in 1:31) {
  paste("==== ", "Q", a, " ====", sep = "") %>% print()
  n_miss(data1[a]) %>% print()
  pct_miss(data1[a]) %>% print()
}

# Various diagnostic graphs
pct_miss_case(data1)
n_case_miss(data1)
gg_miss_var(data1)
vis_miss(data1)
aggr(data1, numbers = TRUE, prop = FALSE, sortVar = TRUE)
gg_miss_upset(data1)
