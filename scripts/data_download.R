library(dplyr)
library(tidyr)


# after accessing data 

file_nms <- list.files(path="/Users/anjanamanjunath/DLMGageHeights/data",
                       full.names = TRUE, recursive = TRUE)

for (i in file_nms){
  name = sub(pattern="(.*)\\..*$", replacement="\\1",
             basename(i))
  assign(name, read.table(i), envir=globalenv())
}

# data cleaning and organizing data frames 

#clean data from buoys and extract what we need 
buoydf <- mget(ls(pattern = "^46092h20[0-9]+"))

buoydf_complete <- lapply(buoydf, function(x){replace(x, x==99, NA) 
  replace(x, x==999, NA) 
  replace(x, x==9999, NA)})

buoydf_complete[["46092h2005"]] <- buoydf_complete[["46092h2005"]][-1,]
buoydf_complete[["46092h2006"]] <- buoydf_complete[["46092h2006"]][-1,]

buoydf_complete[["46092h2005"]] <- mutate_all(buoydf_complete[["46092h2005"]], 
                                              function(x) as.numeric(as.character(x)))

buoydf_complete[["46092h2006"]] <- mutate_all(buoydf_complete[["46092h2006"]], 
                                              function(x) as.numeric(as.character(x)))


# take monthly means of covariates that will be used for prediction
buoydf_means <- lapply(buoydf_complete, 
                          function(x) {x = x %>% group_by(V2) %>% 
                            summarise(Y = mean(V1), WSPD = mean(V7), PRES = mean(V13), 
                                      ATMP = mean(V14), WTMP = mean(V15))})

all_buoy_data_X <- bind_rows(buoydf_means[1:16])

# doing the same data cleaning for our Y

df_list_Y <- list(climate_data_f_2021, climate_data_f_2022)

df_list_Y_complete <- lapply(df_list_Y, function(x){replace(x, x==99, NA) 
  replace(x, x==999, NA) 
  replace(x, x==9999, NA)})

df_list_Y_means <- lapply(df_list_Y_complete, function(x) {x = x %>% group_by(V2) %>% 
  summarise(Y = mean(V1), WSPD = mean(V7), PRES = mean(V13), 
            ATMP = mean(V14), WTMP = mean(V15))})

all_buoy_data_Y <- bind_rows(df_list_Y_means)

# prepare discharge volume data 

river_level_trans <- function(x){
  uv <- separate(x, V3, c('year', 'month', 'day'), sep = "-", remove = FALSE)
  discharge_amount = uv %>% group_by(year, month) %>% summarise(discharge_avg = mean(V5))
  discharge_amount$month <- as.numeric(discharge_amount$month)
  discharge_amount$year <- as.numeric(discharge_amount$year)
  discharge_amount <- transform(discharge_amount, YM = 
                                  as.yearmon(paste(year, month, sep = "-")))
  
  return(discharge_amount)
}

uv_X <- river_level_trans(uv)
ux_Y <- river_level_trans(uv_forecast)

# prepare precipitation data

sc_precip <- t(sapply(SC_county_precipitation$V1, function(x) 
  substring(x, first = c(1,5), last = c(4,6))))
sc_precip <- cbind(sc_precip, SC_county_precipitation$V2)
sc_precip <- data.frame(sc_precip)
sc_precip$X1 <- as.numeric(as.character((sc_precip$X1)))
sc_precip$X2 <- as.numeric(as.character((sc_precip$X2)))
sc_precip$X3 <- as.numeric(as.character((sc_precip$X3)))
colnames(sc_precip)[1] <- 'V1'
colnames(sc_precip)[2] <- 'V2'
colnames(all_buoy_data_X)[2] <- 'V1'

observations <- merge(all_buoy_data_X, sc_precip, by = c('V1', 'V2'), all.x = TRUE)
colnames(observations)[7] <- 'PRECIP'

observations_Y <- cbind(all_data_Y, sc_precip_f)
observations_Y[is.na(observations_Y)] <- 0
observations_Y <- transform(observations_Y, YM = as.yearmon(paste(Y, V2, sep = "-")))
observations_Y <- merge(observations_Y, future.discharge, by = 'YM', all.x = TRUE)
observations_Y <- observations_Y[, c(1, 4, 5, 6, 7, 8, 11)]

observations <- transform(observations, YM = as.yearmon(paste(V1, V2, sep = "-")))

observations <- observations[, c(8, 2, 1, 3, 4, 5, 6, 7)]
observations <- merge(observations, discharge_amount, by = 'YM', all.x = TRUE)
observations <- observations[, c(1, 4, 5, 6, 7, 8, 11)]
observations = observations[-168,]
observations[is.na(observations)] <- 0
