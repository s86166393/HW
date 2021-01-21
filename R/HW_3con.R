library(ncdf4)
library(furrr)
library(purrr)
library(dplyr)


ncfile <- nc_open("E:/CHN_Tmin_DAY_GRID_0.5_196101-201908.nc")
Tmin_arr <- ncvar_get(ncfile, "Tmin")
nc_close(ncfile)


# Everyday > THR_low
# Average > THR_high
# At least k consecutive days > THR_high
HW_3con <- function(x, THR_low, THR_high, percentile_low = FALSE, percentile_high = FALSE, k){
  if (percentile_low == TRUE) {
    THR_low <- quantile(x, THR_low, na.rm = TRUE)    #低阈值
  }
  if (percentile_high == TRUE) {
    THR_high <- quantile(x, THR_high, na.rm = TRUE)  #高阈值
  }
  x[x < THR_low] <- NA
  grp <- split(x, cumsum(c(TRUE, diff(is.na(x)) != 0))) #分组
  corr <- map_if(grp, function(x) all(is.na(x)) == FALSE, function(x) HW_2con(unlist(x), THR_high, k)) %>%  #HW_2con links to HW_2con.cpp
    unlist()
  corr[is.na(corr)] <- FALSE
  corr
}


HW10 <- apply(Tmax_arr, c(1, 2), function(x) HW_3con(x, 0.81, 0.975, percentile_low = TRUE, percentile_high = TRUE, 3)) %》%
  aperm(c(2, 3, 1))
HW11 <- apply(Tmax_arr, c(1, 2), function(x) HW_3con(x, 0.75, 0.9, percentile_low = TRUE, percentile_high = TRUE, 3)) %》%
  aperm(c(2, 3, 1))
HW12 <- apply(Tmin_arr, c(1, 2), function(x) HW_3con(x, 0.81, 0.975, percentile_low = TRUE, percentile_high = TRUE, 3)) %>%
  aperm(c(2, 3, 1))
HW13 <- apply(Tmin_arr, c(1, 2), function(x) HW_3con(x, 0.75, 0.9, percentile_low = TRUE, percentile_high = TRUE, 3)) %>%
  aperm(c(2, 3, 1))

