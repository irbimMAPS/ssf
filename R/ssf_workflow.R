#HEADER ------------------------------------------------
#
#Author: Jacopo Pulcinella, Anna Nora Tassatti, Alessandro Galdelli, Adriano Mancini, Luca Bolognini
#Email: jacopo.pulcinella@irbim.cnr.it
#
#Date: 2021-12-15
#
#Script Name: ssf_functions
#
#Script Description: The script below was developed to analysed the data used in: "Addressing gaps in small-scale fisheries: a low-cost tracking system"
#
#
#Notes:
#
#

source("R/ssf_functions.R")

input_dir = "data"
out_dir = "results"

##### Processing ######
# harbour location ####
harb = c(13.503044, 43.616199)
harb_pp = st_point(x = harb)
harb_buff = st_buffer(harb_pp, 1/60*0.25)

# geofence polygon ###
geofence_poly_str = data.frame(name = "Ancona", 
                               geometry = "POLYGON((13.504753065744099 43.610748955110495, 13.476144452350454 43.6236429604059, 13.490869473950122 43.63897003003305, 13.512606410597256 43.62262778468241, 13.504753065744099 43.610748955110495))")
geofence_poly = st_as_sf(geofence_poly_str, wkt = "geometry")
st_crs(geofence_poly) = wgs

geofence_poly_proj = st_transform(geofence_poly, 3857)

# load data ####
vessel_dat = read.csv(file.path(input_dir, "gps_data.csv")) %>%
  mutate(deviceTime = as.POSIXct(deviceTime))

# define period ###
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
vessel_dat = fishing_trips_pp(vessel_dat, vessel_trips_table) %>%
  filter(trip != 0)

# assign in harbour position ####
vessel_dat = pp_harb(vessel_dat, harb_buff) %>%
  dplyr:::select(-in_harb_stop)

# estimate navigation speed
navigation_speed = as.numeric(quantile(vessel_dat$speed, 0.75))

##### Analysis #####
# hours density ####
day_hours_ref = data.frame(day_hours = 0:23)
# hours density sensor off
day_hours_raw = hours(vessel_dat$deviceTime[-which(vessel_dat$sensor == 0 | vessel_dat$in_harb == 1 | vessel_dat$speed >= navigation_speed)])
day_hours_raw = data.frame(table(day_hours_raw)) %>%
  dplyr:::rename("day_hours" = "day_hours_raw")  %>%
  mutate(day_hours = as.numeric(as.character(day_hours))) %>%
  right_join(day_hours_ref) %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>%
  arrange(day_hours)%>%
  mutate(sensor = "off")
# hours density sensor on
day_hours_sensor = hours(vessel_dat$deviceTime[which(vessel_dat$sensor == 0  & vessel_dat$in_harb == 0 & vessel_dat$speed <= navigation_speed)])
day_hours_sensor = data.frame(table(day_hours_sensor)) %>%
  dplyr:::rename("day_hours" = "day_hours_sensor")  %>%
  mutate(day_hours = as.numeric(as.character(day_hours))) %>%
  right_join(day_hours_ref) %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>%
  arrange(day_hours)%>%
  mutate(sensor = "on")
#combine
day_hours_sensor_raw = rbind(day_hours_raw,
                             day_hours_sensor) %>%
  mutate(sensor = as.factor(sensor))
# plot
p1 = ggplot() +
  geom_bar(data = day_hours_sensor_raw, aes(as.factor(day_hours), Freq, fill = sensor), stat = "identity") +
  theme_bw() +
  ylab("Minutes") + xlab("Time of the day") +
  theme(legend.position = "bottom")
p1
p1.2 = ggplot() +
  geom_violin(data = vessel_dat %>% 
                 filter(vessel_dat$speed <= navigation_speed & in_harb == 0) %>%
                mutate(sensor = ifelse(sensor == 1, "off", "on")), aes(x = as.factor(sensor), y = speed, fill = as.factor(sensor)), show.legend = F) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + xlab("")

p1combo = ggpubr:::ggarrange(plotlist = list(p1, p1.2), align = "hv", common.legend = T, legend = "bottom", labels = c("a", "b"))

ggsave(file.path(out_dir, "hours_density.pdf"), p1combo, device = cairo_pdf, width = 10)
knitr::plot_crop(file.path(out_dir, "hours_density.pdf"), quiet = TRUE)
bitmap <- pdftools::pdf_render_page(file.path(out_dir, "hours_density.pdf"), dpi = 600)
png::writePNG(bitmap, file.path(out_dir, "hours_density.png"))
rm(bitmap); rm(p1); gc()

# trip & sensor ####
# define spatial extension e download the map 
xrange = range(vessel_dat$longitude)
yrange = range(vessel_dat$latitude)
bb = c(xrange[1]-0.01, yrange[1]-0.01, xrange[2]+0.01, yrange[2]+0.01)
map = get_map(bb, source = "osm", zoom = 12)
# arrange input dataset
xdat = vessel_dat %>%
  filter(trip != 0) %>%
  mutate(trip = as.factor(trip)) 

xdat_trip = xdat %>% group_by(trip) %>%
  dplyr:::summarise(date = paste(min(deviceTime), max(deviceTime), sep = " - "))

xdat = xdat %>%
  inner_join(xdat_trip)

p2 = ggmap(map) +
  geom_sf(data = geofence_poly,  inherit.aes = FALSE, fill = NA) +
  geom_point(data = xdat %>%
               filter(sensor == 1), 
             aes(longitude, latitude), size = 0.1, show.legend = F) +
  geom_point(data = xdat %>%
               filter(sensor == 0), 
             aes(longitude, latitude), size = 0.5, colour = "red", shape = 19, show.legend = F) +
  theme_bw() +
  theme(legend.position = "bottom") + 
  facet_wrap(~trip) + xlab("") + ylab("") + 
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
                                             sensor_hrs = round(length(which(sensor == 0))/60, 2),
                                             event_entry = length(which(type == "geofenceEnter")),
                                             event_exit = length(which(type == "geofenceExit")))) %>%
  filter(trip != 0)

trips_table_stat = trips_table %>%
  dplyr:::select(-duration) %>%
  inner_join(track_stats %>%
               mutate(trip = as.numeric(as.character(trip))), by = c("trip")) %>%
  mutate(deviceId = 1)
write.csv(trips_table_stat, file.path(out_dir, "vessel_table_trips_stats.csv"), row.names = F)

# working days ####
length(unique(c(as.Date(trips_table_stat$trip_start), as.Date(trips_table_stat$trip_end))))
























