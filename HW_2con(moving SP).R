library(ncdf4)
library(purrr)
library(dplyr)
library(lubridate)
library(slider)
library(future.apply)
plan(multisession)

Tmax_arr <- nc_open("E:/CHN_Tmax_DAY_GRID_0.5_196101-201908.nc") %>%
  ncvar_get("Tmax")
Tmin_arr <- nc_open("E:/CHN_Tmin_DAY_GRID_0.5_196101-201908.nc") %>%
  ncvar_get("Tmin")
time <- ncvar_get(ncfile, "time")
date <- time + ymd(ncfile[["dim"]][["time"]][["units"]])
nc_close(ncfile)

contpass_SPL <- function(data, prob, k = 1) {
  if (all(is.na(data))  == TRUE) {
    data[is.na(data)] <- FALSE
    return(data)
  } else {
    window <- slide(data, ~.x, .before = 7, .after = 7, .complete = TRUE) %>%
      set_names(format(date, '%m-%d'))
    merge <- map(unique(names(window)), ~ flatten_dbl(window[names(window) == .x])) %>%
      set_names(unique(names(window))) %>%
      map(function(x) quantile(x, prob, na.rm = TRUE))
    data_named <- set_names(data, format(date, '%m-%d'))

    if (k != 1) {
      satisfy <- which(data_named > merge[names(data_named)]) %>%
        split(., cumsum(c(TRUE, diff(.) != 1L))) %>%
        keep(function(x) length(x) >= k) %>%
        unlist()
      data_named[data_named != 0] <- FALSE
      data_named[satisfy] <- TRUE
      data_named
    } else {
      whether <- data_named > merge[names(data_named)]
      whether[is.na(whether)] <- FALSE
      whether
    }
  }
}

HW8 <- future_apply(Tmax_arr, c(1, 2), function(x) ifhw(x, 0.95, 2)) %>%
  aperm(c(2,3,1))
HW9 <- future_apply(Tmin_arr, c(1, 2), function(x) ifhw(x, 0.95, 2)) %>%
  aperm(c(2,3,1))

