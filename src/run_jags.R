library(rjags); library(readr); library(dplyr)
cat("=== EZBHDDM — 3 sequential single-model chains ===\n")
cat("Start:", format(Sys.time()), "\n\n"); flush.console()

df <- read_csv("../data/processed/summarized_demographics_experiment_data_numeric.csv",
                show_col_types=FALSE) %>%
  select(mean_RT_sec, variance_RT_sec, total_accuracy, participant_id, covid,
         nTrials, StudyTimeCoded, ChoiceUrgencyCoded, ProbOfChangeCoded,
         ProbeTypeCoded, age, sex_numeric, education_numeric) %>%
  rename(meanRT=mean_RT_sec, varRT=variance_RT_sec, correct=total_accuracy) %>%
  na.omit() %>%
  mutate(participant_idx=as.integer(factor(participant_id)),
         age_z=scale(age)[,1], education_z=scale(education_numeric)[,1],
         sex_bin=ifelse(sex_numeric==2,1,0))
N_p <- length(unique(df$participant_idx)); N_o <- nrow(df)
cat("N_p:", N_p, " N_o:", N_o, "\n\n"); flush.console()

dl <- list(N_obs=N_o, N_participants=N_p, participant_idx=df$participant_idx,
  meanRT=df$meanRT, varRT=df$varRT, correct=df$correct, nTrials=df$nTrials,
  covid=df$covid, StudyTime=df$StudyTimeCoded, ChoiceUrgency=df$ChoiceUrgencyCoded,
  ProbOfChange=df$ProbOfChangeCoded, ProbeType=df$ProbeTypeCoded,
  age=df$age_z, sex=df$sex_bin, education=df$education_z)

mk <- function(seed) {
  set.seed(seed)
  list(drift_mean=1.2+rnorm(1,0,.05), bound_mean=1.7+rnorm(1,0,.03),
       nondt_mean=0.7+rnorm(1,0,.02),
       drift_sdev=0.3, bound_sdev=0.2, nondt_sdev=0.1,
       drift_subj=rnorm(N_p,0,.01), bound_subj=rnorm(N_p,0,.01),
       nondt_subj=rnorm(N_p,0,.01),
       beta_covid_drift=0,beta_covid_bound=0,beta_covid_nondt=0,
       beta_ST_drift=0,beta_CU_bound=0,beta_PC_drift=0,
       beta_PT_drift=0,beta_PT_bound=0,beta_PT_nondt=0,
       beta_age_drift=0,beta_age_bound=0,beta_age_nondt=0,
       beta_sex_drift=0,beta_sex_bound=0,beta_sex_nondt=0,
       beta_edu_drift=0,beta_edu_bound=0,beta_edu_nondt=0)
}

params <- c("drift_mean","bound_mean","nondt_mean",
  "drift_sdev","bound_sdev","nondt_sdev",
  "beta_covid_drift","beta_covid_bound","beta_covid_nondt",
  "beta_ST_drift","beta_CU_bound","beta_PC_drift",
  "beta_PT_drift","beta_PT_bound","beta_PT_nondt",
  "beta_age_drift","beta_age_bound","beta_age_nondt",
  "beta_sex_drift","beta_sex_bound","beta_sex_nondt",
  "beta_edu_drift","beta_edu_bound","beta_edu_nondt",
  "drift_subj","bound_subj","nondt_subj")

chain_list <- list()
for (ch in 1:3) {
  cat(sprintf("--- Chain %d ---\n", ch)); flush.console()
  t0 <- proc.time()
  m <- jags.model("model_covid.bug", data=dl, inits=list(mk(ch*137)),
                   n.chains=1, n.adapt=0, quiet=TRUE)
  cat(sprintf("  compiled %.0fs\n", (proc.time()-t0)[3])); flush.console()

  t0 <- proc.time(); adapt(m, 1000)
  cat(sprintf("  adapted %.0fs\n", (proc.time()-t0)[3])); flush.console()

  t0 <- proc.time(); update(m, 2000)
  cat(sprintf("  burn-in %.0fs\n", (proc.time()-t0)[3])); flush.console()

  t0 <- proc.time()
  s <- coda.samples(m, variable.names=params, n.iter=3000)
  cat(sprintf("  sampled %.0fs\n", (proc.time()-t0)[3])); flush.console()

  chain_list[[ch]] <- s[[1]]
  rm(m, s); gc(verbose=FALSE)
}

samp <- mcmc.list(chain_list)
combined <- do.call(rbind, samp)
summ <- summary(samp)
gel <- tryCatch(gelman.diag(samp, multivariate=FALSE), error=function(e) NULL)
fs <- cbind(summ$statistics[,c("Mean","SD")], summ$quantiles[,c("2.5%","50%","97.5%")])
if (!is.null(gel)) fs <- cbind(fs, Rhat=gel$psrf[rownames(fs),1])
neff <- effectiveSize(samp); fs <- cbind(fs, n.eff=neff[rownames(fs)])
colnames(fs)[1:2] <- c("mean","sd")

key <- c("drift_mean","bound_mean","nondt_mean",
         "beta_covid_drift","beta_covid_bound","beta_covid_nondt",
         "beta_ST_drift","beta_CU_bound","beta_PC_drift")
cat("\n=== Key results ===\n"); print(round(fs[key,], 4))

pn_all <- colnames(combined); sl <- list()
for (pn in unique(gsub("\\[.*","",pn_all))) {
  cols <- grep(paste0("^",gsub("\\.","\\\\.",pn),"(\\[|$)"), pn_all)
  if (length(cols)==1) sl[[pn]] <- as.vector(combined[,cols])
  else sl[[pn]] <- as.matrix(combined[,cols])
}
cache_dir <- "../cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive=TRUE)
saveRDS(list(samples=list(BUGSoutput=list(sims.list=sl, summary=fs)),
             data_for_jags=df, data_to_jags=dl, timestamp=Sys.time()),
        file.path(cache_dir, "jags_samples.rds"))
cat("\nCached. End:", format(Sys.time()), "\n")
