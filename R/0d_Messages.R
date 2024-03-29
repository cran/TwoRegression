message_update <- function(
  message_number, file, vm_variables, duration, cvs,
  window_size, start_epoch, epoch, missing_vars,
  is_message = FALSE, n = 1, method = "unspecified"
) {

  note <-
    switch(
      message_number,
      paste("\nProcessing", basename(file), "..."),
      paste(
        "\n     Getting VM for variables searched on the following criteri(a/on):",
        vm_variables,
        "\n"
      ),
      "Failed to detect start time in file header",
      paste("\nFile processed. Processing took", round(duration/60, 2), "minutes.\n"),
      "\n\n-- Filtering Gyroscope...",
      " Done.\n",
      "\n-- Calculating Vector Magnitudes...",
      "\n     Vector magnitude calculation complete.\n",
      "Number of rows not divisible by samp_rate*output_window\nTruncating data.",
      "\n-- Collapsing data. This could take awhile...",
      paste("\nCalculating CV per 10s for:", paste(cvs, collapse = " and ")),
      "\nThis could take awhile. Be patient...",
      paste("\n... Calculating CVs using sliding windows of size", window_size),
      "\n\n",
      "All two-regression processing complete.",
      paste(
        "\nTwo-Regression processing complete. Total processing time:",
        duration,
        "minutes.\n"
      ),
      "No IMU file detected, yet Algorithm is not set to 1. Setting to 1.",
      "Error in file formatting. Returning NULL.",
      "Length of X and Y differ. Returning NULL.",
      paste("Determining direction from mean values of x and y, replicating", n, "times."),
      "Unable to detect sampling rate. Defaulting to 100",
      "IMU file provided, but Algorithm 1 selected. Ignoring IMU file. Set IMU_ignore_A1 = FALSE to override.",
      "hibbing_2018 only has algorithms 1-3. Removing selections outside that range.",
      "No valid algorithms specified. Setting Algorithm to 1.",
      "Wear_Location must be one or more of c(\"Hip\", \"Left Wrist\", \"Right Wrist\", \"Left Ankle\", \"Right Ankle\").",
      "No valid Wear_Location specified. Defaulting to Hip.",
      "Primary accelerometer file is formatted unexpectedly. Processing with read.csv() -- be prepared to wait.",
      "Aborting AG_smooth because minute_of_day variable not found",
      paste("Aborting AG_smooth because ending epoch length (",
        epoch, "-s) is not a multiple of starting epoch length (",
        start_epoch, "-s)", sep = ""),
      paste("Aborting AG_smooth because there is currently no support for collapsing the following variable(s):\n ",
        paste(missing_vars, collapse = "\n  ")),
      paste("... Calculating CVs using non-overlapping blocks of size", window_size),
      paste("\nCalculating EE using the", sQuote(method), "two-regression model"),
      "33"
    )
  if (is_message) {
    message(note)
  } else{
    cat(note)
  }
}
