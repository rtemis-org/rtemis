# Create synthetic longitudinal dataset for xtdescribe() example
# ::rtemis::
# 2025 EDG

set.seed(2025)

# Create a small longitudinal dataset with various participation patterns
# 10 participants measured at up to 5 time points (years 2020-2024)

patient_id <- c(
  rep(1, 5), # Complete participation (all 5 time points)
  rep(2, 5), # Complete participation
  rep(3, 4), # Missing last time point
  rep(4, 4), # Missing first time point
  rep(5, 3), # Only first, middle, last
  rep(6, 2), # Only first two time points
  rep(7, 2), # Only last two time points
  rep(8, 3), # Missing 2nd and 4th time points
  rep(9, 1), # Only baseline
  rep(10, 1) # Only final time point
)

year <- c(
  2020:2024, # ID 1: all years
  2020:2024, # ID 2: all years
  2020:2023, # ID 3: missing 2024
  2021:2024, # ID 4: missing 2020
  c(2020, 2022, 2024), # ID 5: intermittent
  2020:2021, # ID 6: early dropout
  2023:2024, # ID 7: late entry
  c(2020, 2022, 2024), # ID 8: intermittent
  2020, # ID 9: baseline only
  2024 # ID 10: final only
)

# Generate outcome variable (e.g., blood pressure)
blood_pressure <- round(rnorm(length(patient_id), mean = 120, sd = 15), 1)

# Generate treatment group
treatment <- rep(c("A", "B"), length.out = length(patient_id))

# Create data.frame
xt_example <- data.frame(
  patient_id = patient_id,
  year = year,
  blood_pressure = blood_pressure,
  treatment = treatment
)

# Save to data/
usethis::use_data(xt_example, overwrite = TRUE)
