#------------------------------------------------------------------
#' Read csv
#'
#' This is a function that reads in a csv names \code{filename} and
#' turns this into a dplyr tbl_df.
#'
#' @param filename A string with the name of the csv to be loaded
#'
#' @note function will return an error if filename does not exist
#'
#' @importFrom dplyr tbl_df
#'
#' @return This function returns a dplyr tbl_df corresponding to the csv.
#'
#' @examples
#' acc13 <- fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
    if(!grepl("/",filename)){
        filename <- system.file("extdata", filename, package="FarsExample")
    }


    if(!file.exists(filename))
            stop("file '", filename, "' does not exist")
    data <- suppressMessages({
            readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#------------------------------------------------------------------
#' print filename
#'
#' This is a function takes a year integer \code{year} (eg 2013)
#' and returns the name of the csv
#'
#' @param year An integer year (2013)
#'
#' @return This function returns a string that
#' corresponds to the name of a csv
#'
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
        year <- as.integer(year)
        file <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata", file, package="FarsExample")
}

#------------------------------------------------------------------
#' Read in csv for eah year in \code{years} and returns the
#' months and year in that csv
#'
#' @param years A list of year integers
#'
#' @note function will produce a warning if there is no filename
#' corresponding to year in years
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @return This function returns a list of tibbles
#'
#' @examples
#' fars_read_years(c(2013,2014))
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#------------------------------------------------------------------
#' summarises the number of entries in the csv files for each
#' year and month in \code{years}
#'
#' @param years A list of year integers
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#' @importFrom dplyr "%>%"
#'
#' @return This function returns a tibble that summarises each of the csv
#' files corresponding to the \code{years}
#'
#' @examples
#' fars_summarize_years(c(2013,2014))
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#------------------------------------------------------------------
#' Read in csv for  \code{year} and plot all the accidents in \code{state}
#' on a map.
#'
#' @param year An integer year
#' @param state An integer state number
#'
#' @note function will produce an error if there is no state data for
#' the \code{state} selected.
#' @note function will produce a warning if there are no accidents in
#' state, year combination in data
#'
#' @importFrom graphics points
#' @importFrom maps map
#'
#' @return This function returns a list of tibbles
#'
#' @examples
#' fars_map_state(1, 2013)
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
