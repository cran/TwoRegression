#' Process Data from Wearable Research Devices Using Two-Regression Algorithms
#'
#' The TwoRegression package is designed to make implementation of two-regression algorithms quick, easy, and accurate.
#'
#' @section Core functions:
#'
#' \code{\link{get_cvPER}}
#'
#' \code{\link{hibbing18_twoReg_process}}
#'
#' @examples
#' \dontrun{
#' raw_file <-
#'     system.file("extdata",
#'         "TestID_LeftWrist_RAW.csv",
#'         package = "TwoRegression")
#'
#' imu_file <-
#'     system.file("extdata",
#'         "TestID_LeftWrist_IMU.csv",
#'         package = "TwoRegression")
#'
#' wear <- "Left Wrist"
#' id <- "Test"
#' alg <- 1:2
#'
#' hibbing18_twoReg_process(raw_file, imu_file, wear, id, alg)
#' }
#'
#' @section Associated References:
#' Hibbing PR, LaMunion SR, Kaplan AS, & Crouter SE (2017). Estimating
#' energy expenditure with ActiGraph GT9X Inertial Measurement Unit.
#' \emph{Medicine and Science in Sports and Exercise}. Advance online publication.
#' doi: 10.1249/MSS.0000000000001532
#'
#' @docType package
#' @name TwoRegression
NULL

#' @importFrom magrittr %>%
NULL

#' @importFrom stats predict sd setNames
NULL

#' @importFrom utils read.csv
NULL
