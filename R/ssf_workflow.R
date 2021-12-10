source("R/ssf_functions.R")


input_dir = "data"
out_dir = "results"

# load data ####
vessel_dat = read.csv(file.path(input_dir, "gps_data.csv")) %>%
  mutate(deviceTime = as.POSIXct(deviceTime))

# harbour location ####
harb = c(13.503044, 43.616199)
harb_pp = st_point(x = harb)
harb_buff = st_buffer(harb_pp, 1/60*0.25)

from = min(as.Date(vessel_dat$deviceTime))
to = max(as.Date(vessel_dat$deviceTime))

# data quality ####
vessel_data_quality = check_data_raw(vessel_dat)
write.csv(vessel_data_quality, file.path(out_dir, "quality_data.csv"), row.names = F)

# Calculate trip table ####
vessel_trips_table = fishing_trips_table(vessel_dat) %>%
  mutate(trip_start = as.character(trip_start), trip_end = as.character(trip_end))
write.csv(vessel_trips_table, file.path(out_dir, "vessel_trips_table.csv"), row.names = F)

# assign position to session ####
vessel_dat = fishing_trips_pp(vessel_dat, vessel_trips_table)

#### plots ####
# define spatial extension e download the map 
xrange = range(vessel_dat$longitude)
yrange = range(vessel_dat$latitude)
bb = c(xrange[1]-0.01, yrange[1]-0.01, xrange[2]+0.01, yrange[2]+0.01)
map = get_map(bb, source = "osm")

# arrange input dataset
xdat = vessel_dat %>%
  mutate(trip = as.factor(trip),
         date = paste(min(deviceTime), max(deviceTime), sep = "-"))

# plot trip & sensor ####
p2 = ggmap(map) +
  geom_point(data = xdat %>%
               filter(di2 == 1), 
             aes(longitude, latitude), size = 0.1, show.legend = F) +
  geom_point(data = xdat %>%
               filter(di2 == 0), 
             aes(longitude, latitude), size = 0.5, colour = "red", shape = 19, show.legend = F) +
  theme_bw() +
  theme(legend.position = "bottom") + 
  facet_wrap(~trip, ncol = 6) + xlab("") + ylab("") + 
  # scale_color_manual(values = cols) +
  ggtitle(paste("From", as.Date(from), "to", as.Date(to)))
p2
ggsave(file.path(out_dir, "trips_sensor.pdf"), p2, device = cairo_pdf, width = 10.5, height = 10)
knitr::plot_crop(file.path(out_dir, "trips_sensor.pdf"), quiet = TRUE)
bitmap <- pdftools::pdf_render_page(file.path(out_dir, "trips_sensor.pdf"), dpi = 600)
png::writePNG(bitmap, file.path(out_dir, "trips_sensor.png"))
rm(bitmap); rm(p2); gc()


# session stats ####
trips_table = read.csv(file.path(out_dir, "vessel_trips_table.csv"))
track_stats = data.frame(xdat %>%
                           group_by(trip) %>%
                           dplyr:::summarise(duration_hrs = round(as.numeric(difftime(max(deviceTime), min(deviceTime)), units = "hours"),2),
                                             speed_kmh_avg = round(mean(speed),2),
                                             distance_km = round((max(totalDistance) - min(totalDistance))/1000,2),
                                             sensor_hrs = round(length(which(di2 == 0))/60, 2),
                                             event_entry = length(which(type == "geofenceEnter")),
                                             event_exit = length(which(type == "geofenceExit")))) %>%
  filter(trip != 0)

trips_table_stat = trips_table %>%
  dplyr:::select(-duration) %>%
  inner_join(track_stats %>%
               mutate(trip = as.numeric(as.character(trip))), by = c("trip")) %>%
  mutate(deviceId = 1)
write.csv(trips_table_stat, file.path(out_dir, "vessel_table_trips_stats.csv"), row.names = F)































