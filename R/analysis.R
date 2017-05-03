rm(list = ls(all = TRUE))
source("R/packages.R")
source("R/functions.R")
source("R/generic_functions.R")

# station locations
stations <- read.csv("Data/stations_ed.csv", na.strings = c("..", ".....")) %>% 
  rename(station = Site) %>% 
  mutate(station = as.character(station), 
         Site.name = gsub(" .*", "", Site.name))


# load rainfall data frmo weather stations
filenames <- dir("Data/weather_station/", full.names = TRUE)
rain_raw <- ldply(filenames, function(x){
  read.csv(x) %>% 
    select(-Period.over.which.rainfall.was.measured..days.) %>% 
    rename(code     = Product.code,
           station  = Bureau.of.Meteorology.station.number,
           rain     = Rainfall.amount..millimetres.) %>% 
    mutate(station  = as.character(station),
           date     = as.Date(paste(Year, Month, Day, sep = "-")),
           rain     = ifelse(is.na(rain), 0, rain)) %>% 
    filter(date >= as.Date("1917-1-1") & date <= as.Date("2016-12-31"))
}, .progress = "text")
  
rain_raw <- rain_raw %>% 
  left_join(stations[, c("station", "Site.name")])
rain_raw$season <- mapvalues(rain_raw$Month, c(1:12), rep(c("summer", "autumn", "winter", "spring"), each = 3))


# stations with annual rainfall 800 ± 20%
ann_rain_800 <- rain_raw %>% 
  group_by(Site.name, Year) %>% 
  summarise(rain = sum(rain)) %>% 
  group_by(Site.name) %>% 
  summarise(ann_rain_avg = mean(rain)) %>% 
  ungroup() %>% 
  filter(ann_rain_avg <= 800 * 1.2 & ann_rain_avg >= 800 * .8) %>% 
  left_join(rain_raw) %>% 
  mutate(rain_class = cut(rain, breaks = c(0, 1, 5, 20, max(rain)),  # rainfall class: class0 rain <= 1, class1 rain <= 5, class2 rain <=20, class3 rain > 20                          include.lowest = TRUE, right = FALSE, 
                          labels = paste0("class", 0:3), include.lowest = TRUE,
                          right = FALSE))
  


# annual summary
ann_smmry <- ann_rain_800 %>% 
  group_by(Site.name, Year) %>% 
  summarise(rain = sum(rain)) %>% 
  group_by(Site.name) %>% 
  summarise_each(funs(avg. = mean, std. = sd), rain) %>% 
  mutate_each(funs(round(., 0)), avg., std.) %>% 
  mutate(unit = "mm yr-1") %>% 
  select(Site.name, unit, everything())
  


# station map
station_800 <- ann_rain_800 %>% 
  select(station, ann_rain_avg) %>%
  distinct() %>% 
  left_join(stations)


map <- get_map(location = 'New South Wales', zoom = 5)
mapPoints <- ggmap(map) +
  geom_point(data = station_800, aes(x = Lon, y = Lat, size = ann_rain_avg),
             alpha = .6, pch = 21, fill = "red")+
  geom_text(data = station_800, aes(x = Lon, y = Lat, label = substr(Site.name, 1, 3)),
            size = 3, hjust = -.5, vjust = .1, col = "blue")+
  scale_size_continuous(name = "Ann. rain (mm)")
mapPoints


# ann_rain_800 for combined winter and spring
ann_rain_800_sw <- ann_rain_800 %>% 
  filter(season %in% c("spring", "winter")) %>% 
  mutate(season = "W_S") %>% 
  bind_rows(ann_rain_800) %>% 
  mutate(season = factor(season, levels = c("winter", "spring", "W_S")))



# compute summary data for each station and season ------------------------


ann_rain_800_season_smmry <- dlply(filter(ann_rain_800_sw, season %in% c("spring", "winter", "W_S")), .(Site.name, season), function(x){
  
  d1 <- x
  
  # . prepare data frames ---------------------------------------------------
  
  
  # seasonal rainfall and extreme years
  d1_season_avg <- d1 %>% 
    group_by(Year) %>% 
    summarise_each(funs(seas_rain = sum(.), dry_d = sum(. == 0), wet_d = sum(. != 0), 
                        max_wet = max(.)), 
                   rain) %>% 
    mutate(seas_extreme = ifelse(seas_rain <= quantile(seas_rain, probs = .05), TRUE, FALSE))
  
  ## extreme years
  d1_extr <- d1_season_avg %>% 
    filter(seas_extreme) %>% 
    select(Year, seas_extreme)
  
  
  ## frequency of each rain class
  d1_rainclass <- d1 %>%
    group_by(Year, rain_class) %>% 
    summarise(no = n()) %>% 
    right_join(expand.grid(Year = unique(.$Year), rain_class = unique(.$rain_class))) %>%  # include non-observed rain_class as 0
    ungroup() %>% 
    mutate(no = ifelse(is.na(no), 0, no)) %>% 
    left_join(d1_extr) %>% 
    mutate(seas_extreme = ifelse(is.na(seas_extreme), FALSE, seas_extreme))
  
  
  ## Max contiguous duration of wet (dry) days
  cont_d <- vector()
  cont_d[1] <- 1
  for(i in 2:nrow(d1)){
    twoval <- c(d1$rain[i - 1], d1$rain[i])
    if(all(twoval == 0)|all(twoval > 0)){
      cont_d[i] <- cont_d[i - 1]
    } else {
      cont_d[i] <- cont_d[i - 1] + 1
    }
  }
  
  d1_contig_day <- d1 %>% 
    mutate(cont_d = cont_d,
           wetday = ifelse(rain > 0, "wet", "dry")) %>% 
    group_by(Year, cont_d, wetday) %>% 
    summarise(freq = n()) %>% 
    group_by(Year, wetday) %>% 
    summarise(max_freq = max(freq)) %>% 
    right_join(expand.grid(Year = unique(.$Year), wetday = unique(.$wetday))) %>% 
    ungroup() %>% 
    mutate(max_freq = ifelse(is.na(max_freq), 0, max_freq)) %>% 
    left_join(d1_extr) %>% 
    mutate(seas_extreme = ifelse(is.na(seas_extreme), FALSE, seas_extreme))
  
  
  
  
  # . summary ----------------------------------------------------------------
  
  
  # number of yrs
  n_ext <- sum(d1_extr$seas_extreme)
  n_all <- data.frame(variable = "No. yr",
                      unit = "yr",
                      extreme = as.character(n_ext),
                      overall = as.character(length(unique(d1_season_avg$Year))))
  
  
  # seasonal rainfall
  d1_season_avg_all <- d1_season_avg %>%
    summarise_each(funs(avg. = mean, std. = sd), seas_rain, dry_d, wet_d, max_wet) %>% 
    mutate(type = "overall")
  
  d1_season_avg_extr <- d1_season_avg %>%
    filter(seas_extreme) %>% 
    summarise_each(funs(avg. = mean, std. = sd), seas_rain, dry_d, wet_d, max_wet) %>% 
    mutate(type = "extreme")
  
  d1_season_avg_smmry <- rbind(d1_season_avg_extr, d1_season_avg_all) %>%
    mutate_each(funs(as.character(round(., 2))), -type) %>% 
    gather(key = variable, value, -type) %>% 
    spread(key = type, value) %>% 
    mutate(unit = car::recode(factor(variable), 
                              'c("dry_d_avg.", "wet_d_avg.", "dry_d_std.", "wet_d_std.") = "d yr-1";
                              c("max_wet_avg.", "max_wet_std.")                         = "mm d-1";
                              c("seas_rain_avg.", "seas_rain_std.")                     = "mm yr-1"')) %>% 
    select(variable, unit, overall, extreme) %>% 
    arrange(unit, variable)
  
  
  # rain class
  d1_rainclass_all <- d1_rainclass %>% 
    group_by(rain_class) %>% 
    summarise_each(funs(avg. = mean, std. = sd), no) %>% 
    mutate(type = "overall")
  
  d1_rainclass_extr <- d1_rainclass %>% 
    filter(seas_extreme) %>% 
    group_by(rain_class) %>% 
    summarise_each(funs(avg. = mean, std. = sd), no) %>% 
    mutate(type = "extreme")
  
  
  d1_rainclass_smmry <- rbind(d1_rainclass_all, d1_rainclass_extr) %>% 
    mutate_each(funs(as.character(round(., 0))), -rain_class, -type) %>% 
    gather(variable, value, -rain_class, -type) %>% 
    mutate(variable = paste(rain_class, variable, sep = "_")) %>% 
    select(-rain_class) %>% 
    spread(type, value) %>% 
    mutate(unit = "d yr-1")
  
  
  
  # max. contiguous days
  d1_contig_day_all <- d1_contig_day %>% 
    group_by(wetday) %>% 
    summarise_each(funs(avg. = mean, std. = sd), max_contig = max_freq) %>% 
    mutate(type = "overall")
  
  d1_contig_day_extr <- d1_contig_day %>% 
    filter(seas_extreme) %>% 
    group_by(wetday) %>% 
    summarise_each(funs(avg. = mean, std. = sd), max_contig = max_freq) %>% 
    mutate(type = "extreme")
  
  d1_contig_day_smmry <- rbind(d1_contig_day_all, d1_contig_day_extr) %>%
    mutate_each(funs(as.character(round(., 0))), starts_with("max_")) %>% 
    gather(key = variable, value, starts_with("max_")) %>% 
    mutate(variable = paste(wetday, variable, sep = "_"))%>% 
    select(-wetday) %>% 
    spread(type, value) %>% 
    mutate(unit = ifelse(grepl("yrs", variable), "yr", "d yr-1")) %>% 
    select(variable, unit, overall, extreme)
  
  
  # summary table
  d1_smmry <- rbind.fill(d1_season_avg_smmry, d1_rainclass_smmry, d1_contig_day_smmry) %>% 
    arrange(unit) %>% 
    bind_rows(n_all)
  
  d1_list <- list(summary_tbl = d1_smmry, season_rain = d1_season_avg, 
                  contig_day = d1_contig_day, rainclass = d1_rainclass)
  
  return(d1_list)
  
})




# Summary tbl -----------------------------------------------------------------


ann_rain_800_season_tbl <- ldply(ann_rain_800_season_smmry, function(x) x$summary_tbl) %>% 
  gather(type, value, overall, extreme) %>% 
  mutate(ntype = paste(season, type, sep = "_")) %>% 
  select(-season, -type) %>% 
  spread(ntype, value) %>% 
  arrange(Site.name, unit) %>% 
  select(Site.name, variable, unit, starts_with("winter"), starts_with("spring"), starts_with("W_S")) %>% 
  mutate(variable = mapvalues(variable, 
                              c("class0_avg.", "class1_avg.", "class2_avg.", "class3_avg.",
                                "class0_std.", "class1_std.", "class2_std.", "class3_std.",
                                "dry_d_avg.", "dry_max_contig_avg.", "wet_d_avg.", "wet_max_contig_avg.", 
                                "dry_d_std.", "dry_max_contig_std.", "wet_d_std.", "wet_max_contig_std.",
                                "seas_rain_avg.", "max_wet_avg.",
                                "seas_rain_std.", "max_wet_std."),
                              c("Avg. no. class0 rain days", 
                                "Avg. no. class1 rain days", 
                                "Avg. no. class2 rain days", 
                                "Avg. no. class3 rain days", 
                                "St. dev. of no. class0 rain days",
                                "St. dev. of no. class1 rain no. days",
                                "St. dev. of no. class2 rain no. days",
                                "St. dev. of no. class3 rain no. days",
                                "Avg. no. dry days",
                                "Avg. max. no. contiguous duration of dry days",
                                "Avg. no. wet days",
                                "Avg. max. no. contiguous duration of wet days",
                                "St. dev. of no. dry days",
                                "St. dev. of max. no. contiguous duration of dry days",
                                "St. dev. of no. wet days",
                                "St. dev. of max. no. contiguous duration of wet days",
                                "Avg. wet day rain",
                                "Avg. max wet day rain",
                                "St. dev. wet day rain",
                                "St. dev. max wet day rain"))) %>% 
  filter(variable %in% c("Avg. no. class0 rain days", "Avg. no. class1 rain days", 
                         "Avg. no. class2 rain days", "Avg. no. class3 rain days",
                         "Avg. no. dry days", "Avg. no. wet days",
                         "Avg. max. no. contiguous duration of dry days",
                         "Avg. max. no. contiguous duration of wet days",
                         "Avg. max wet day rain",
                         "Avg. wet day rain", "St. dev. wet day rain"))



# Figures -----------------------------------------------------------------


all_season_rain         <- ldply(ann_rain_800_season_smmry, function(x) x$season_rain)
all_season_rain_ext     <- filter(all_season_rain, seas_extreme)

all_contig_day          <- ldply(ann_rain_800_season_smmry, function(x) x$contig_day)
all_contig_day_ext      <- filter(all_contig_day, seas_extreme) 

all_rainclass           <- ldply(ann_rain_800_season_smmry, function(x) x$rainclass) %>% 
  mutate(rain_class2 = factor(rain_class, labels = c("<1mm", "1-5mm", "5-20mm", ">20mm")))
all_rainclass_ext       <- filter(all_rainclass, seas_extreme) 



# . 1. rain ---------------------------------------------------------------


p_rain <- ggplot()+
  geom_boxplot(data = all_season_rain, aes(x = Site.name, y = seas_rain))+
  geom_point(data = all_season_rain_ext, aes(x = Site.name, y = seas_rain), col = "red")+
  facet_wrap( ~ season) +
  labs(y = expression(Precipitation~(mm~yr^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


# . 2. No. of wet days ----------------------------------------------------


p_n_wetday <- ggplot()+
  geom_boxplot(data = all_season_rain, aes(x = Site.name, y = wet_d))+
  geom_point(data = all_season_rain_ext, aes(x = Site.name, y = wet_d), col = "red")+
  facet_wrap( ~ season) +
  labs(y = expression(No[.]~of~wet~days~(d~yr^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# . 3. No. of dry days ----------------------------------------------------


p_n_dryday <- ggplot()+
  geom_boxplot(data = all_season_rain, aes(x = Site.name, y = dry_d))+
  geom_point(data = all_season_rain_ext, aes(x = Site.name, y = dry_d), col = "red")+
  facet_wrap( ~ season) +
  labs(y = expression(No[.]~of~dry~days~(d~yr^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# . 4. Max No. of contiguous wet days -------------------------------------


p_n_contig_wet <- ggplot()+
  geom_boxplot(data = filter(all_contig_day, wetday == "wet"), aes(x = Site.name, y = max_freq))+
  geom_point(data = filter(all_contig_day_ext, wetday == "wet"), aes(x = Site.name, y = max_freq), col = "red")+
  facet_wrap( ~ season) +
  labs(y = expression(Max[.]~No[.]~of~contiguous~wet~days~(d~yr^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# . 5. Max No. of contiguous dry days -------------------------------------


p_n_contig_dry <- ggplot()+
  geom_boxplot(data = filter(all_contig_day, wetday == "dry"), aes(x = Site.name, y = max_freq))+
  geom_point(data = filter(all_contig_day_ext, wetday == "dry"), aes(x = Site.name, y = max_freq), col = "red")+
  facet_wrap( ~ season) +
  labs(y = expression(Max[.]~No[.]~of~contiguous~dry~days~(d~yr^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# . 6. Maximum wet day events ---------------------------------------------


p_max_rain <- ggplot()+
  geom_boxplot(data = all_season_rain, aes(x = Site.name, y = max_wet))+
  geom_point(data = all_season_rain_ext, aes(x = Site.name, y = max_wet), col = "red")+
  facet_wrap( ~ season) +
  labs(y = expression(Max~precipitation~(mm~d^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))



# . 7. No. of days for each rainfall class --------------------------------


p_rainclass <- ggplot()+
  geom_boxplot(data = all_rainclass, aes(x = Site.name, y = no))+
  geom_point(data = all_rainclass_ext, aes(x = Site.name, y = no), col = "red")+
  facet_grid(rain_class2 ~ season, scale = "free_y") +
  labs(y = expression(Frequency~(d~yr^'-1')), x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

  



# Multi-site summary ------------------------------------------------------

# ratios of variables between extreme and overall (magnitude of extreme events)
mult_site_smmry <- ann_rain_800_season_tbl %>% 
  filter(variable != "St. dev. wet day rain") %>%
  mutate_each(funs(as.numeric), starts_with("winter"), starts_with("spring"), starts_with("W_S")) %>% 
  group_by(variable) %>% 
  summarise_each(funs(Avg. = mean(., na.rm = TRUE), 
                      N = get_n), starts_with("winter"), starts_with("spring"), starts_with("W_S")) %>% 
  ungroup() %>% 
  transmute(variable, 
            mag.extrm_winter = winter_extreme_Avg. / winter_overall_Avg. - 1, 
            mag.extrm_spring = spring_extreme_Avg. / spring_overall_Avg. - 1,
            mag.extrm_w_s    = W_S_extreme_Avg. / W_S_overall_Avg. - 1,
             no_site    = winter_extreme_N) %>% 
  mutate_each(funs(round(., 2)), starts_with("mag"))



# save --------------------------------------------------------------------


save.image("Output/Data/all_image.RData")

  