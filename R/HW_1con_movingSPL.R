library(ncdf4)
library(dplyr)
library(lubridate)
library(slider)
library(future.apply)
plan(multisession)

#-----------------------------------------------------------------
ncfile <- nc_open("E:/CHN_Tmax_DAY_GRID_0.5_196101-201908.nc")
Tmax_arr <- ncvar_get(ncfile, "Tmax")
time <- ncvar_get(ncfile, "time")
date <- time + ymd(ncfile[["dim"]][["time"]][["units"]])
nc_close(ncfile)
#-----------------------------------------------------------------
ncfile <- nc_open("E:/CHN_Tmin_DAY_GRID_0.5_196101-201908.nc")
Tmin_arr <- ncvar_get(ncfile, "Tmin")
time <- ncvar_get(ncfile, "time")
date <- time + ymd(ncfile[["dim"]][["time"]][["units"]])
nc_close(ncfile)
#-----------------------------------------------------------------

# At least k consecutive days > or ≥ T1, default is the former
# 15-day moving sampling by year
HW_1con_movingSPL <- function(data, percentile, k = 1, equate = FALSE) {
  if (all(is.na(data)) == TRUE) {
    data[is.na(data)] <- FALSE
    return(data)
  }
  date_md <- format(date, '%m-%d')
  window <- slide(data, ~.x, .before = 7, .after = 7) %>%
    set_names(date_md) %>%
    stack() %>%
    group_by(ind) %>%
    summarise(THR = quantile(values, 0.9, na.rm = TRUE), .groups = 'drop')
  df <- data.table(date = date_md, T = x) %>%
    merge(window, by.x = 'date', by.y = 'ind', sort = FALSE)
  ifelse(equate, df$compare <- df$T >= df$THR, df$compare <- df$T > df$THR)
  if (k == 1) return(df$compare)

  df <- group_by(df, grp = cumsum(c(1, diff(compare) != 0))) %>%
    summarise(compare, last = length(grp), .groups = 'drop')
  df$last > k & df$compare == TRUE
}


HW8 <- future_apply(Tmax_arr, c(1, 2), function(x) HW_1con_movingSPL(x, 0.95, 2))
HW9 <- future_apply(Tmin_arr, c(1, 2), function(x) HW_1con_movingSPL(x, 0.95, 2))


