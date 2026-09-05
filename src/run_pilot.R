library(rjags)
library(readr)
library(dplyr)

cat("=== Pilot EZBHDDM (no regression) ===\n")

raw_data <- read_csv("../data/processed/summarized_demographics_experiment_data_numeric.csv",
                      show_col_types = FALSE)
df <- raw_data %>%
  select(mean_RT_sec, variance_RT_sec, total_accuracy, nTrials) %>%
  rename(meanRT = mean_RT_sec, varRT = variance_RT_sec, correct = total_accuracy) %>%
  na.omit()

cat("Observations:", nrow(df), "\n")

data_list <- list(
  N_obs   = nrow(df),
  meanRT  = df$meanRT,
  varRT   = df$varRT,
  correct = df$correct,
  nTrials = df$nTrials
)

cat("Compiling...\n"); flush.console()
t0 <- proc.time()
model <- jags.model("model_pilot.bug", data = data_list,
                     n.chains = 2, n.adapt = 500, quiet = FALSE)
t1 <- proc.time()
cat("Compile + adapt:", (t1 - t0)[3], "sec\n\n")

cat("Burn-in 500...\n"); flush.console()
t0 <- proc.time()
update(model, 500)
t1 <- proc.time()
cat("Burn-in:", (t1 - t0)[3], "sec\n\n")

cat("Sampling 500...\n"); flush.console()
t0 <- proc.time()
samp <- coda.samples(model, c("drift_mean", "bound_mean", "nondt_mean"), n.iter = 500)
t1 <- proc.time()
cat("Sampling:", (t1 - t0)[3], "sec\n\n")

cat("Summary:\n")
print(summary(samp))
