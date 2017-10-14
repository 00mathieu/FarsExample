library(testthat)
library(FarsExample)
library(dplyr)
library(readr)
# getwd()
# test_check("FarsExample")

test_that("fars_summarize_years returns data frame",{

    # old_wd <- getwd()
    # setwd("...")

    year_summary <- fars_summarize_years(c(2013))

    expect_is(year_summary,"data.frame")

    # setwd(old_wd)
})





