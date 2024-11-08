# *****************************************
# Fresh new working environment
graphics.off() # closing windows
rm(list = ls())  # clearing memory

library(readr)
library(R2jags)
library(dplyr)

# Step 1: Prepare the Data
dfile <- "../data/processed/summarized_demographics_experiment_data_numeric.csv"
raw_data <- read_csv(dfile)

# Clean and prepare the data for DDM analysis
data_for_jags <- raw_data %>%
  select(mean_RT_sec, variance_RT_sec, total_accuracy, participant_id, covid, 
         nTrials, StudyTimeCoded, ChoiceUrgencyCoded, ProbOfChangeCoded,
         ProbeTypeCoded, age, raceNew_numeric, ethnicity_numeric,
         sex_numeric, education_numeric, version_numeric) %>%
  rename(meanRT  = mean_RT_sec, 
         varRT   = variance_RT_sec, 
         correct = total_accuracy) %>%
  na.omit()

data_for_jags$version_numeric <- 1 +
  1 * data_for_jags$StudyTimeCoded +
  2 * data_for_jags$ChoiceUrgencyCoded + 
  4 * data_for_jags$ProbOfChangeCoded + 
  8 * data_for_jags$ProbeTypeCoded

# Step 2: Define the JAGS Model
model_code <- "
model {
        ##### Priors for hierarchical DDM parameters
        drift_mean ~ dnorm(0.00, 1.00)    # Intercept for drift rate model
        bound_mean ~ dnorm(1.50, (0.20^-2))T( 0.10, 3.00)
        nondt_mean ~ dnorm(0.30, (0.06^-2))T( 0, )
        drift_sdev ~ dunif(0.01, 3.00)
        inter_sdev ~ dunif(0.01, 3.00)
        bound_sdev ~ dunif(0.01, 1.00)
        nondt_sdev ~ dunif(0.01, 0.50)

        # Priors for covid prediction model coefficients
        alpha0 ~ dnorm(0, 1)  # Intercept for covid prediction
        alpha_drift ~ dnorm(0, 1)
        alpha_bound ~ dnorm(0, 1)
        alpha_nondt ~ dnorm(0, 1)
        alpha_age ~ dnorm(0, 1)
        alpha_race ~ dnorm(0, 1)
        alpha_ethnicity ~ dnorm(0, 1)
        alpha_sex ~ dnorm(0, 1)
        alpha_education ~ dnorm(0, 1)

        # Effect of task version
        for (v in 1:V) {
            intercept[v] ~ dnorm(0, (inter_sdev^-2))
        }
        
        # Hierarchical distributions of individual DDM parameters
        for (p in 1:P) {
            bound[p] ~ dnorm(bound_mean, (bound_sdev^-2))T( 0.10, 3.00)
            nondt[p] ~ dnorm(nondt_mean, (nondt_sdev^-2))T( 0.05, )
            drift[p] ~ dnorm(drift_mean, (drift_sdev^-2))

            logit(prob_covid[p]) <- alpha0 + 
                                    alpha_drift      * drift[p] + 
                                    alpha_bound      * bound[p] + 
                                    alpha_nondt      * nondt[p] +
                                    alpha_age        * age[p] + 
                                    alpha_race       * raceNew_numeric[p] +
                                    alpha_ethnicity  * ethnicity_numeric[p] + 
                                    alpha_sex        * sex_numeric[p] +
                                    alpha_education  * education_numeric[p]
            covid[p] ~ dbern(prob_covid[p])
        }
        
        for (n in 1:N) {
            cdrift[n] <- drift[person[n]] + intercept[version[n]]
            ey[n]  = exp(-bound[person[n]] * cdrift[n])
            Pc[n]  = 1 / (1 + ey[n])
            PRT[n] = 2 * pow(cdrift[n], 3) / bound[person[n]] * 
                        pow(ey[n] + 1, 2) / (2 * -bound[person[n]] * 
                        cdrift[n] * ey[n] - ey[n] * ey[n] + 1)
            MDT[n] = (bound[person[n]] / (2 * cdrift[n])) * 
                        (1 - ey[n]) / (1 + ey[n])
            MRT[n] = MDT[n] + nondt[person[n]]

            # Sampling distributions for summary statistics
            correct[n] ~ dbin(Pc[n], nTrials[n])
            varRT[n]   ~ dnorm(1/PRT[n], 
                               0.5*(nTrials[n]-1) * PRT[n] * PRT[n])
            meanRT[n]  ~ dnorm(MRT[n], PRT[n] * nTrials[n])
        }
}
"

# Step 3: Set Up Data for JAGS
data_jags <- list(
  meanRT             = data_for_jags$meanRT,
  varRT              = data_for_jags$varRT,
  correct            = data_for_jags$correct,
  covid              = data_for_jags$covid,
  age                = data_for_jags$age,
  raceNew_numeric    = data_for_jags$raceNew_numeric,
  ethnicity_numeric  = data_for_jags$ethnicity_numeric,
  sex_numeric        = data_for_jags$sex_numeric,
  education_numeric  = data_for_jags$education_numeric,
  version            = data_for_jags$version_numeric,
  P                  = length(unique(data_for_jags$participant_id)),
  V                  = length(unique(data_for_jags$version_numeric)),
  person             = as.numeric(factor(data_for_jags$participant_id)),
  N                  = nrow(data_for_jags),
  nTrials            = data_for_jags$nTrials
)

# Define initial values function with drift close to zero
inits <- function() {
  list(
    drift = array(runif(data_jags$P, -0.01, 0.01)),
    intercept = array(runif(data_jags$V, -0.01, 0.01)),
    alpha0 = 0,
    alpha_drift = 0,
    alpha_bound = 0,
    alpha_nondt = 0,
    alpha_age = 0,
    alpha_race = 0,
    alpha_ethnicity = 0,
    alpha_sex = 0,
    alpha_education = 0
  )
}

# Step 4: Run the JAGS Model with updated parameters and initial values
jags_fit <- jags(
  data = data_jags,
  inits = inits,
  parameters.to.save = c("prob_covid", "alpha0", "alpha_drift",
                         "alpha_bound", "alpha_nondt", "alpha_age", 
                         "alpha_race", "alpha_ethnicity", "alpha_sex", 
                         "alpha_education", "intercept"),
  model.file = textConnection(model_code),
  n.chains = 3,
  n.iter = 5000,
  n.burnin = 1000,
  n.thin = 10
)

# Extract and display summary statistics for alpha parameters
alpha_summary <- jags_fit$BUGSoutput$summary[grep(
  "^alpha|intercept", rownames(jags_fit$BUGSoutput$summary)), ]

inference_table <- as.data.frame(alpha_summary) %>%
  mutate(
    credibly_nonzero = ifelse(`2.5%` > 0 | `97.5%` < 0, "Yes", "No")
  )

print(inference_table)
