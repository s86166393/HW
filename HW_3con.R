library(ncdf4)
library(furrr)
library(purrr)
library(dplyr)


ncfile <- nc_open("E:/CHN_Tmax_DAY_GRID_0.5_196101-201908.nc")
Tmax_arr <- ncvar_get(ncfile, "Tmax")
nc_close(ncfile)


#对于大区间，切成多个小区间,分为不大于低阈值区间和大于低阈值区间,z为高阈值,k为持续天数
contprd <- function(x, THR_low, THR_high, percentile_low = FALSE, percentile_high = FALSE, k){
  if (percentile_low == TRUE) {
    THR_low <- quantile(x, THR_low, na.rm = TRUE)    #低阈值
  }
  if (percentile_high == TRUE) {
    THR_high <- quantile(x, THR_high, na.rm = TRUE)  #高阈值
  }
  x[x < THR_low] <- NA
  grp <- split(x, cumsum(c(TRUE, diff(is.na(x)) != 0))) #分组
  corr <- map_if(grp, function(x) all(is.na(x)) == FALSE, function(x) findprd(unlist(x), THR_high, k)) %>%
    unlist()
  corr[is.na(corr)] <- FALSE
  corr
}



last <- apply(Tmax_arr, c(1, 2), function(x) contprd(x, 0.81, 0.975, percentile_low = TRUE, percentile_high = TRUE, 3))


