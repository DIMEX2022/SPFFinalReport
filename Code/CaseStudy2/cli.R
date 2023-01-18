library(here)
library(optparse)
library(magrittr)

cli <- function() {
  OptionParser(option_list = list(
    make_option(c("--step"),
                help = "DIMEX processing step"),
    make_option(c("--prefix"),
                default = "~/Dropbox/Github/SPFFinalReport",
                help = "top-level directory containing Code/ and Data/")
  )) %>%
  parse_args %>%
  validate_args
}

validate_args <- function(opts) {
  if (!file.exists(opts$prefix)) {
    stop(paste("directory", opts$prefix, "does not exist. See --prefix option."))
  }
  opts
}

script_name <- function(step) {
  if (step == "1a") {
    output <- "1a_DataPrep_StudyRegion.R"
  } else if (step == "1b") {
    output <- "1b_DataPrep_Population.R"
  } else if (step == "1c") {
    output <- "1c_DataPrep_TUS.R"
  } else if (step == "1d") {
    output <- "1d_DataPrep_PM25.R"
  } else if (step == "2") {
    output <- "2_Activities_2021.R"
  } else if (step == "3a") {
    output <- "3a_Exposures_July_2021.R"
  } else if (step == "3b") {
    output <- "3b_Exposures_Q1_2021.R"
  } else if (step == "4a") {
    output <- "4a_CollateResults_July_2021.R"
  } else if (step == "4b") {
    output <- "4b_CollateResults_Q1_2021.R"
  } else if (step == "5a") {
    output <- "5a_PlotResults_July_2021.R"
  } else if (step == "5b") {
    output <- "5b_PlotResults_Q1_2021.R"
  } else {
    stop(paste("unknown step", step))
  }
  output
}

# Main program
main <- function() {
  opts <- cli()

  # Working directory
  setwd(opts$prefix)

  # Run particular step
  source(here("Code", "CaseStudy2", script_name(opts$step)))
  
}

main()