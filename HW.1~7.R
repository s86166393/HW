library(ncdf4)
library(purrr)
library(dplyr)

Tmean_arr <- nc_open("E:/CHN_Tmean_DAY_GRID_0.5_196101-201908.nc") %>%
  ncvar_get("Tmean")
Tmax_arr <- nc_open("E:/CHN_Tmax_DAY_GRID_0.5_196101-201908.nc") %>%
  ncvar_get("Tmax")
Tmin_arr <- nc_open("E:/CHN_Tmin_DAY_GRID_0.5_196101-201908.nc") %>%
  ncvar_get("Tmin")
nc_close(ncfile)

# At least k consecutive days > T1
# T1 = prob percentile
# input a vector, return a vector, including True or False
contpass_prob <- function(data, prob, k){
  THR <- quantile(data, prob, na.rm = TRUE)
  data[is.na(data)] <- FALSE
  if (all(data == 0)){
    return (data)
  } else {
    satisfy <- which(data > THR) %>%
      split(., cumsum(c(TRUE, diff(.) != 1L))) %>%
      keep(function(x) length(x) >= k) %>%
      unlist()
    data[data != 0] <- FALSE
    data[satisfy] <- TRUE
    return (data)
  }
}

# At least k consecutive days > THR
# input a vector, return a vector, including True or False
contpass_THR <- function(data, THR, k){
  data[is.na(data)] <- FALSE
  if (all(data == 0)){
    return (data)
  } else {
    satisfy <- which(data > THR) %>%
      split(., cumsum(c(TRUE, diff(.) != 1L))) %>%
      keep(function(x) length(x) >= k) %>%
      unlist()
    data[data != 0] <- FALSE
    data[satisfy] <- TRUE
    return (data)
  }
}


HW.1 <- apply(Tmean_arr, c(1, 2), function(x) contpass_prob(x, 0.9, 2)) %>%
  aperm(c(2,3,1))
HW.2 <- apply(Tmean_arr, c(1, 2), function(x) contpass_prob(x, 0.95, 2)) %>%
  aperm(c(2,3,1))
HW.3 <- apply(Tmax_arr, c(1, 2), function(x) contpass_prob(x, 0.95, 2)) %>%
  aperm(c(2,3,1))
HW.4 <- apply(Tmin_arr, c(1, 2), function(x) contpass_prob(x, 0.95, 2)) %>%
  aperm(c(2,3,1))
HW.5 <- apply(Tmean_arr, c(1, 2), function(x) contpass_prob(x, 0.98, 2)) %>%
  aperm(c(2,3,1))
HW.6 <- apply(Tmean_arr, c(1, 2), function(x) contpass_prob(x, 0.99, 2)) %>%
  aperm(c(2,3,1))
HW.7 <- apply(Tmax_arr, c(1, 2), function(x) contpass_THR(x, 35, 3)) %>%
  aperm(c(2,3,1))
