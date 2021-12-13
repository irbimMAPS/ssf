library(reticulate)
library(lubridate)
library(rjson)
library(plyr)
library(ggplot2)
library(sf)
library(chron)
library(dplyr)
library(NbClust)
library(ggmap)
library(caret)
library(nnet)
library(utiml)
library(kohonen)
library(reshape2)
library(RMySQL)  
library(stringr)
library(RPostgreSQL)
library(rpostgis)
library(lwgeom)
library(data.table)
library(ggpubr)

# Sys.setenv(RETICULATE_PYTHON = "python/bin/python")
# reticulate::py_config()

wgs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

"%ni%"=Negate('%in%')

sf::sf_use_s2(FALSE)
options(scipen = 10000)
Sys.setlocale("LC_TIME", "en_US")

st_fun = function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))

# formatting string ####
formatting_string <- function(x){
  xx = iconv(x, "UTF-8", 'ASCII//TRANSLIT',sub='x')
  xx = toupper(xx)
  xx = gsub('[^[:alnum:]]',"", xx)
  return(xx)
}

# check rate ####
check_data_raw <- function(vessel_data, rate_mins = 1, gaps_mins = 30){
  
  xpp = st_as_sf(vessel_data, coords = c("longitude", "latitude"))
  xrate = rate_mins/60
  xgaps = gaps_mins/60
  # spikes
  spikes = numeric(nrow(xpp))
  gaps = numeric(nrow(xpp))
  for(i in 2:nrow(xpp)){
    xlength = as.numeric(st_distance(xpp[i-1,], xpp[i,]))
    xtime = as.numeric(difftime(vessel_data[i,"deviceTime"], vessel_data[i-1,"deviceTime"], units = "hours"))
    xspeed = vessel_data$speed[i-1]
    xlength_estimate = xspeed/xtime
    
    if(is.nan(xlength_estimate)){
      xlength_estimate = 0
    }
    
    if(xtime > xrate + xrate * 0.15){
      if(xlength_estimate >= xlength - xlength*0.10 & xlength_estimate <= xlength - xlength*0.10){
        next()
      }else{
        if(xtime > xgaps){
          next()
        }else{
          gaps[i] = 1
        }
      }
    }else{
      if(xlength < 333){
        next()
      }else{
        spikes[i] = 1
      }
    }
  }
  vessel_data$spikes = spikes
  vessel_data$gaps = gaps
  
  vessel_data_quality = vessel_data %>%
    group_by(deviceId) %>%
    dplyr:::summarise(n_record = length(deviceId),
                      ndays = length(unique(as.Date(deviceTime))),
                      speed_err = length(which(speed > 20)),
                      course_err = length(which(course < 0 | course > 360)),
                      gaps = length(which(gaps == 1)),
                      spikes = length(which(spikes == 1)),
                      point_on_land = 0)
  
  vessel_data_quality = vessel_data_quality %>% 
    melt(1) %>%
    select(-deviceId)
  
  return(vessel_data_quality)
}

# check positions ####
check_pp_onland <- function(vessel_data, land){
  xpp = st_as_sf(vessel_data, coords = c("longitude", "latitude"))
  st_crs(xpp) = wgs
  onland = as.numeric(st_intersects(xpp, land, sparse = T))
  if(length(which(is.na(onland) == T)) == length(onland)){
    
  }else{
    
  }
}


# define in harbour position - require an harbour location ####
pp_harb = function(vessel_data, location){
  xpp = st_as_sf(vessel_data, coords = c("longitude", "latitude")) 
  xpp$in_harb = as.numeric(st_intersects(xpp, harb_buff, sparse = F))
  xpp$in_harb_stop = ifelse(xpp$in_harb == 1 & xpp$speed == 0, 1, 0)
  
  xpp_in_harb_ref = xpp%>%
    st_set_geometry(NULL) %>%
    distinct(id, in_harb, in_harb_stop)
  vessel_data = vessel_data %>%
    left_join(xpp_in_harb_ref, by = "id")
  return(vessel_data)
}

# defne fishing trips timetable using the event (geofunc) #####
fishing_trips_table <- function(vessel_data){
  
  xdat = vessel_data %>%
    filter(sat > 0 & power > 0) %>%
    dplyr:::select(id, longitude, latitude, deviceTime, totalDistance, speed, power,sat)
  
  stop = numeric(nrow(xdat))
  start = numeric(nrow(xdat))
  movement = numeric(nrow(xdat))
  if(xdat$speed[1] == 0 & xdat$power[1] < 1){
    movement[1] = 0
  }else{
    movement[1] = 1
  }
  start[1] = 1
  
  i = 2
  while(i <= nrow(xdat)){
    # cat("\n", i)
    diff_time = as.numeric(difftime(xdat$deviceTime[i], xdat$deviceTime[i-1], units = "hours"))
    
    if(xdat$speed[i] > 0 & xdat$sat[i] > 1){
      movement[i] = 1
    }
    if(diff_time >= 0.75){
      stop[i-1] = max(start)
      start[i] = max(start)+1
    }else{
      if(xdat$power[i] > xdat$power[i-1] & xdat$power[i-1] < 1 & movement[i-1] == 0){
        start[i] = max(start) + 1
      }
    }
    if(i == nrow(xdat)){
      stop[i] = max(start)
    }
    
    i = i+1
  }
  
  xdat = cbind(xdat, start, stop, movement)
  starts = xdat %>% filter(start != 0) %>% 
    distinct(id, start, trip_start = deviceTime) %>% 
    dplyr:::rename("trip" = "start", "id_start" = "id") 
  stops = xdat %>% filter(stop != 0) %>% 
    distinct(id, stop, trip_end = deviceTime) %>%
    dplyr:::rename("trip" = "stop", "id_end" = "id") 
  
  event_table = starts %>%
    inner_join(stops, by = "trip") %>%
    mutate(deviceId = unique(vessel_data$deviceId)) %>%
    mutate(week_day_start = weekdays(trip_start), 
           week_day_end = weekdays(trip_end),
           duration = as.numeric(difftime(trip_end, trip_start, units = "hours"))) %>%
    filter(duration > 0.5)
  event_table$trip = 1:nrow(event_table)
  return(event_table)
}

# assign fishing trips to raw ping #####
fishing_trips_pp <- function(vessel_data, trips_table){
  vessel_data$trip = numeric(nrow(vessel_data))
  vessel_data$trip_status = NA
  
  for(i in 1:nrow(trips_table)){
    xstart = trips_table$id_start[i]
    xend = trips_table$id_end[i]
    session_seq = xstart:xend
    xtrip = trips_table$trip[i]
    vessel_data$trip[which(vessel_data$id >= xstart & vessel_data$id <= xend)] = xtrip
    vessel_data$trip_status[which(vessel_data$id == xstart)] = "start"
    vessel_data$trip_status[which(vessel_data$id == xend)] = "stop"
  }
  return(vessel_data)
}
