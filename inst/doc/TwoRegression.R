## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  
#  ?TwoRegression::TwoRegression
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  if (!"remotes" %in% installed.packages)
#    install.packages("remotes")
#  
#  if (!"AGread" %in% installed.packages())
#    remotes::install_github("paulhibbing/AGread")
#  

## -----------------------------------------------------------------------------

data(count_data, package = "TwoRegression")
data(all_data, package = "TwoRegression")


## -----------------------------------------------------------------------------

utils::head(count_data)


## -----------------------------------------------------------------------------

all_data <- all_data[ ,setdiff(
  names(all_data),
  ## These are the variables to remove:
  c(
    "file_source_PrimaryAccel", "date_processed_PrimaryAccel",
    "file_source_IMU", "date_processed_IMU", "day_of_year", "minute_of_day",
    ## Remove the following because they'll be recalculated later
    "ENMO_CV10s", "GVM_CV10s", "Direction"
  )
)]

utils::head(all_data)

## -----------------------------------------------------------------------------

crouter2006_results <- TwoRegression::TwoRegression(
  count_data, "Crouter 2006", movement_var = "Axis1", time_var = "time"
)

crouter2010_results <- TwoRegression::TwoRegression(
  count_data, "Crouter 2010", movement_var = "Axis1", time_var = "time"
)

crouter2012_va_results <- TwoRegression::TwoRegression(
  count_data, "Crouter 2012", movement_var = "Axis1",
  time_var = "time", model = "VA", check = FALSE
)

crouter2012_vm_results <- TwoRegression::TwoRegression(
  count_data, "Crouter 2012", movement_var = "Vector.Magnitude",
  time_var = "time", model = "VM", check = FALSE
)


## -----------------------------------------------------------------------------

utils::head(crouter2006_results)
utils::head(crouter2010_results)
utils::head(crouter2012_va_results)
utils::head(crouter2012_vm_results)


## -----------------------------------------------------------------------------

hibbing2018_results <- TwoRegression::TwoRegression(
  
  all_data, "Hibbing 2018", accel_var = "ENMO",
  gyro_var = "Gyroscope_VM_DegPerS",
  direction_var = "mean_magnetometer_direction",
  
  ## Here is where we can select an algorithm from multiple sites:
  site = c("Left Ankle", "Right Ankle"),
  
  ## And here is where we can select multiple algorithms
  ## (1 = accelerometer only; 2 = accelerometer and gyroscope;
  ## 3 = accelerometer, gyroscope, and magnetometer)
  algorithm = 1:2,
  
  ## We can also ask the function to collapse data every minute by making an
  ## extra call to `smooth_2rm`
  smooth = TRUE
)

utils::head(hibbing2018_results)


## -----------------------------------------------------------------------------

set.seed(307)

fake_sed <- c("Lying", "Sitting")
fake_lpa <- c("Sweeping", "Dusting")
fake_cwr <- c("Walking", "Running")
fake_ila <- c("Tennis", "Basketball")

fake_activities <- c(fake_sed, fake_lpa, fake_cwr, fake_ila)

all_data$Activity <- sample(fake_activities, nrow(all_data), TRUE)

all_data$fake_METs <- ifelse(
  all_data$Activity %in% c(fake_sed, fake_lpa),
  runif(nrow(all_data), 1, 2),
  runif(nrow(all_data), 2.5, 8)
)


## -----------------------------------------------------------------------------

all_data$PID <- rep(
  c("Test1", "Test2"),
  each = ceiling(nrow(all_data) / 2)
)[seq(nrow(all_data))]

all_data$ENMO_CV10s <- TwoRegression::cv_2rm(all_data$ENMO)


## -----------------------------------------------------------------------------

my_model <- TwoRegression::fit_2rm(
  data = all_data,
  activity_var = "Activity",
  sed_cp_activities = c(fake_sed, fake_lpa),
  sed_activities = fake_sed,
  sed_cp_var = "ENMO",
  sed_METs = 1.25,
  walkrun_activities = fake_cwr,
  walkrun_cp_var = "ENMO_CV10s",
  met_var = "fake_METs",
  walkrun_formula = "fake_METs ~ ENMO",
  intermittent_formula = "fake_METs ~ ENMO + I(ENMO^2) + I(ENMO^3)"
)


## -----------------------------------------------------------------------------

summary(
  my_model,
  subject_var = "PID",
  MET_var = "fake_METs",
  activity_var = "Activity"
)


## ---- fig.align='center', fig.width=12, fig.height=6, out.width="85%"---------

## You have to explicitly type `object = ` for this to work
plot(
  object = my_model,
  sed_cp_activities = c(fake_sed, fake_lpa),
  sed_activities = fake_sed,
  sed_cpVar = "ENMO",
  activity_var = "Activity",
  met_var = "fake_METs",
  walkrun_activities = fake_cwr,
  walkrun_cpVar = "ENMO_CV10s",
  print = TRUE
)


## -----------------------------------------------------------------------------

new_results <- predict(my_model, all_data)

utils::head(new_results)


## -----------------------------------------------------------------------------

results_default <- predict(my_model, all_data, verbose = TRUE)


my_model$method <- "My Customized 2RM"


results_updated <- predict(my_model, all_data, verbose = TRUE)


## -----------------------------------------------------------------------------

## This code illustrates collapsing every 60 seconds. (This is the default
## period and also the typical recommendation, but you could do anything,
## e.g., "10 sec", "30 sec", or "0.25 hour")
TwoRegression::smooth_2rm(results_updated, "Timestamp", "60 sec")


