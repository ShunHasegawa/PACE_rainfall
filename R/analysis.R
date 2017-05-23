rm(list = ls(all = TRUE))
source("R/packages.R")
source("R/functions.R")
source("R/generic_functions.R")



# load station and rinfall data -------------------------------------------


# station locations
stations <- read.csv("Data/stations_ed.csv", na.strings = c("..", ".....")) %>% 
  rename(station = Site) %>% 
  mutate(station = as.character(station), 
         Site.name = gsub(" .*", "", Site.name))


# load rainfall data from weather stations
filenames <- dir("Data/weather_station/", full.names = TRUE)
rain_raw <- ldply(filenames, function(x){
  read.csv(x) %>% 
    select(-Period.over.which.rainfall.was.measured..days.) %>% 
    rename(code     = Product.code,
           station  = Bureau.of.Meteorology.station.number,
           rain     = Rainfall.amount..millimetres.) %>% 
    mutate(station  = as.character(station),
           date     = as.Date(paste(Year, Month, Day, sep = "-")),
           rain     = ifelse(is.na(rain), 0, rain))
}, .progress = "text")
  
rain_raw <- rain_raw %>% 
  left_join(stations[, c("station", "Site.name")])
rain_raw$season <- mapvalues(rain_raw$Month, c(1:12), rep(c("summer", "autumn", "winter", "spring"), each = 3))

# raw rain for different years: 100, 30 and 10 yrs
startday   <- c(yr100 = "1917-1-1", yr30 = "1987-1-1", yr10 = "2007-1-1")
rain_raw_l <- llply(startday, function(x) filter(rain_raw, date >= as.Date(x) & date <= as.Date("2016-12-31")))


# stations with annual rainfall 800 ± 20%
ann_rain_800_l <- llply(rain_raw_l, function(x){
  x %>% 
    group_by(Site.name, Year) %>% 
    summarise(rain = sum(rain)) %>% 
    group_by(Site.name) %>% 
    summarise(ann_rain_avg = mean(rain)) %>% 
    ungroup() %>% 
    filter(ann_rain_avg <= 800 * 1.2 & ann_rain_avg >= 800 * .8) %>% 
    left_join(x) %>% 
    mutate(rain_class = cut(rain, breaks = c(0, 1, 5, 20, max(rain)),             # rainfall class: class0 rain <= 1, class1 rain <= 5, class2 rain <=20, class3 rain > 20                          include.lowest = TRUE, right = FALSE, 
                            labels = paste0("class", 0:3), include.lowest = TRUE,
                            right = FALSE),
           Site.name = relevel(factor(Site.name), "RICHMOND"))
  
})


# combine different years of dataset
ann_rain_800_all <- ldply(ann_rain_800_l)

# create annual and seasonally summary tables
source("R/fun_create_ann_smmry_tbl.R")
ann_smmry_l <- llply(ann_rain_800_l, create_ann_smmry_tbl)
write.csv(ldply(ann_smmry_l), file = "Output/Tables/summary_ann_rain.csv",
          row.names = FALSE)



# . add different classification of seasons -------------------------------


# wint&spring combined data
ann_rain_800_all_sw <- ann_rain_800_all %>%                 
  filter(season %in% c("spring", "winter")) %>% 
  mutate(season = "W_S")


# May and June data
ann_rain_800_all_mj <- ann_rain_800_all %>% 
  filter(Month %in% c(5, 6)) %>% 
  mutate(season = month.abb[Month])


# merge the above
ann_rain_800_all_ssn <- ann_rain_800_all %>% 
  bind_rows(ann_rain_800_all_sw) %>% 
  bind_rows(ann_rain_800_all_mj) %>% 
  mutate(season = factor(season, c("spring", "summer", "autumn", "winter", "W_S", "May", "Jun")))




# station map -------------------------------------------------------------


# station map
station_800 <- ann_rain_800_l$yr100 %>% 
  select(station, ann_rain_avg) %>%
  distinct() %>% 
  left_join(stations) %>% 
  mutate(Site.name = relevel(factor(Site.name), "RICHMOND"),
         labels = as.character(as.numeric(Site.name)))


map <- get_map(location = 'New South Wales', zoom = 5)
mapPoints <- ggmap(map) +
  geom_point(data = station_800, aes(x = Lon, y = Lat),
             alpha = .6, shape = 1, fill = NA,  size = 3)+
  geom_text(data = station_800, aes(x = Lon, y = Lat, label = labels),
            size = 2, col = "red")
mapPoints




# compute summary data for each station and season ------------------------

source("R/fun_create_ssn_smmry_tbl.R")
ann_rain_800_season_smmry <- dlply(filter(ann_rain_800_all_ssn, season %in% 
                                            c("spring", "winter", "W_S", "May", "Jun")),  
                                   .(.id, Site.name, season), 
                                   function(x){
                                     print(distinct(select(x, .id, Site.name, season)))
                                     create_ssn_smmry_tbl(seasonalrain_data = x)
                                   })

# Summary tbl -----------------------------------------------------------------


ann_rain_800_season_tbl <- ldply(ann_rain_800_season_smmry, function(x) x$summary_tbl) %>% 
  gather(type, value, overall, extreme) %>% 
  mutate(ntype = paste(season, type, sep = "_")) %>% 
  select(-season, -type) %>% 
  spread(ntype, value) %>% 
  arrange(.id, Site.name, unit) %>% 
  select(.id, Site.name, variable, unit, starts_with("winter"), starts_with("spring"), starts_with("W_S"), 
         starts_with("May"), starts_with("Jun")) %>% 
  mutate(variable = mapvalues(variable, 
                              c("class0_avg.", "class1_avg.", "class2_avg.", "class3_avg.",
                                "class0_std.", "class1_std.", "class2_std.", "class3_std.",
                                "dry_d_avg.", "dry_max_contig_avg.", "wet_d_avg.", "wet_max_contig_avg.", 
                                "dry_d_std.", "dry_max_contig_std.", "wet_d_std.", "wet_max_contig_std.",
                                "wetday_rain_avg.", "seas_rain_avg.", "max_wet_avg.",
                                "wetday_rain_std." ,"seas_rain_std.", "max_wet_std."),
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
                                "Avg. seasonal rain",
                                "Avg. max wet day rain",
                                "St. dev. wet day rain",
                                "St. dev. seasonal rain",
                                "St. dev. max wet day rain"))) %>% 
  filter(variable %in% c("Avg. no. class0 rain days", "Avg. no. class1 rain days", 
                         "Avg. no. class2 rain days", "Avg. no. class3 rain days",
                         "Avg. no. dry days", "Avg. no. wet days",
                         "Avg. max. no. contiguous duration of dry days",
                         "Avg. max. no. contiguous duration of wet days",
                         "Avg. max wet day rain",
                         "Avg. wet day rain", "St. dev. wet day rain",
                         "Avg. seasonal rain",
                         "Year", "No. yr"))




# Figures -----------------------------------------------------------------


all_season_rain     <- ldply(ann_rain_800_season_smmry, function(x) x$season_rain)

all_wetday_rain     <- ldply(ann_rain_800_season_smmry, function(x) x$wetday_rain)

all_contig_day      <- ldply(ann_rain_800_season_smmry, function(x) x$contig_day)
all_contig_day_ext  <- filter(all_contig_day, seas_extreme) 

all_rainclass       <- ldply(ann_rain_800_season_smmry, function(x) x$rainclass) %>% 
  mutate(rain_class2 = factor(rain_class, labels = c("<1mm", "1-5mm", "5-20mm", ">20mm")))
all_rainclass_ext   <- filter(all_rainclass, seas_extreme) 



# . 1. seasonal rain ---------------------------------------------------------------


p_rain <- dlply(all_season_rain, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = x, aes(x = Site.name, y = seas_rain))+
    geom_point(data = filter(x, seas_extreme), aes(x = Site.name, y = seas_rain), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(Precipitation~(mm~yr^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(x$.id))
})


# . 2. wet day rain ---------------------------------------------------------------


p_wetday_rain <- dlply(all_wetday_rain, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = x, aes(x = Site.name, y = wetday_rain))+
    geom_point(data = filter(x, seas_extreme), aes(x = Site.name, y = wetday_rain), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(Wet~day~precipitation~(mm~d^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
    ggtitle(unique(x$.id))
  
})
  
  

# . 3. No. of wet days ----------------------------------------------------


p_n_wetday <- dlply(all_season_rain, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = x, aes(x = Site.name, y = wet_d))+
    geom_point(data = filter(x, seas_extreme), aes(x = Site.name, y = wet_d), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(No[.]~of~wet~days~(d~yr^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(x$.id))
  })



# . 4. No. of dry days ----------------------------------------------------


p_n_dryday <- dlply(all_season_rain, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = x, aes(x = Site.name, y = dry_d))+
    geom_point(data = filter(x, seas_extreme), aes(x = Site.name, y = dry_d), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(No[.]~of~dry~days~(d~yr^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(x$.id))
})



# . 5. Max No. of contiguous wet days -------------------------------------


p_n_contig_wet <- dlply(all_contig_day, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = filter(x, wetday == "wet"), aes(x = Site.name, y = max_freq))+
    geom_point(data = filter(x, seas_extreme & wetday == "wet"), aes(x = Site.name, y = max_freq), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(Max[.]~No[.]~of~contiguous~wet~days~(d~yr^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(x$.id))
  })



# . 6. Max No. of contiguous dry days -------------------------------------


p_n_contig_dry <- dlply(all_contig_day, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = filter(x, wetday == "dry"), aes(x = Site.name, y = max_freq))+
    geom_point(data = filter(x, seas_extreme & wetday == "dry"), aes(x = Site.name, y = max_freq), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(Max[.]~No[.]~of~contiguous~dry~days~(d~yr^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(x$.id))
})


# . 7. Maximum wet day events ---------------------------------------------


p_max_rain <- dlply(all_season_rain, .(.id), function(x){
  ggplot()+
    geom_boxplot(data = x, aes(x = Site.name, y = max_wet))+
    geom_point(data = filter(x, seas_extreme), aes(x = Site.name, y = max_wet), col = "red")+
    facet_wrap( ~ season, scales = "free_y") +
    labs(y = expression(Max~precipitation~(mm~d^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(x$.id))
})


# . 8. No. of days for each rainfall class --------------------------------
p_rainclass <- dlply(all_rainclass, .(.id), function(x){
  d <- x %>% 
    mutate(rs = rain_class2:season)
  ggplot()+
    geom_boxplot(data = d, aes(x = Site.name, y = no))+
    geom_point(data = filter(d, seas_extreme), aes(x = Site.name, y = no), col = "red")+
    facet_wrap( ~ rs, scale = "free_y") +
    labs(y = expression(Frequency~(d~yr^'-1')), x = "Site") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    ggtitle(unique(d$.id))
})
  


# Multi-site summary ------------------------------------------------------

# ratios of variables between extreme and overall (magnitude of extreme events)
mult_site_smmry <- ann_rain_800_season_tbl %>% 
  filter(!variable %in% c("St. dev. wet day rain", "No. yr", "Year")) %>%
  mutate_each(funs(as.numeric), starts_with("winter"), starts_with("spring"), 
              starts_with("W_S"), starts_with("May"), starts_with("Jun")) %>% 
  group_by(.id, variable) %>% 
  summarise_each(funs(Avg. = mean(., na.rm = TRUE), N = get_n), 
                 starts_with("winter"), starts_with("spring"), starts_with("W_S"), 
                 starts_with("May"), starts_with("Jun")) %>% 
  ungroup() %>% 
  transmute(.id, variable, 
            mag.extrm_winter = winter_extreme_Avg. / winter_overall_Avg. - 1, 
            mag.extrm_spring = spring_extreme_Avg. / spring_overall_Avg. - 1,
            mag.extrm_w_s    = W_S_extreme_Avg. / W_S_overall_Avg. - 1,
            mag.extrm_May     = May_extreme_Avg. / May_overall_Avg. - 1,
            mag.extrm_Jun     = Jun_extreme_Avg. / Jun_overall_Avg. - 1,
            no_site          = winter_extreme_N) %>% 
  mutate_each(funs(round(., 2)), starts_with("mag"))
write.csv(ann_rain_800_season_tbl, file = "Output/Tables/rain_characterisation.csv",
          row.names = FALSE)



# autumn break ------------------------------------------------------------
source("R/analysis_autumn_break.R")


# save --------------------------------------------------------------------


# save.image("Output/Data/all_image_30yr.RData") # 30yrs
save.image("Output/Data/all_image.RData") # 100yrs

  