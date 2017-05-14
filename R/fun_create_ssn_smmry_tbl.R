create_ssn_smmry_tbl <- function(seasonalrain_data){
  
  d1 <- seasonalrain_data
  
  # . prepare data frames ---------------------------------------------------
  
  
  # seasonal rainfall and extreme years
  d1_season_avg <- d1 %>% 
    group_by(Year) %>% 
    summarise_each(funs(seas_rain = sum(.), dry_d = sum(. == 0), wet_d = sum(. != 0), 
                        max_wet = max(.)), 
                   rain) %>% 
    mutate(seas_extreme = ifelse(seas_rain <= quantile(seas_rain, probs = 0), TRUE, FALSE))
  
  ## extreme years
  d1_extr <- d1_season_avg %>% 
    filter(seas_extreme) %>% 
    select(Year, seas_extreme)
  
  
  ## wet day rainfall
  d1_wetrain_avg <- d1 %>% 
    filter(rain > 0) %>% 
    group_by(Year) %>% 
    summarise(wetday_rain = mean(rain)) %>% 
    ungroup() %>% 
    left_join(d1_extr) %>% 
    mutate(seas_extreme = ifelse(is.na(seas_extreme), FALSE, seas_extreme))
  
  
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
    summarise_each(funs(avg. = mean, std. = sd, cov. = sd(.)/mean(.)), seas_rain, dry_d, wet_d, max_wet) %>% 
    mutate(type = "overall")
  
  d1_season_avg_extr <- d1_season_avg %>%
    filter(seas_extreme) %>% 
    summarise_each(funs(avg. = mean, std. = sd, cov. = sd(.)/mean(.)), seas_rain, dry_d, wet_d, max_wet) %>% 
    mutate(type = "extreme")
  
  d1_season_avg_smmry <- rbind(d1_season_avg_all, d1_season_avg_extr) %>%
    mutate_each(funs(as.character(round(., 2))), -type) %>% 
    gather(key = variable, value, -type) %>% 
    spread(key = type, value) %>% 
    mutate(unit = car::recode(factor(variable), 
                              'c("dry_d_avg.", "wet_d_avg.", "dry_d_std.", "wet_d_std.")        = "d yr-1";
                              c("max_wet_avg.", "max_wet_std.")                               = "mm d-1";
                              c("seas_rain_avg.", "seas_rain_std.")                           = "mm yr-1";
                              c("dry_d_cov.", "wet_d_cov.", "max_wet_cov.", "seas_rain_cov.") = "-"'),
           extreme = ifelse(is.na(extreme), "-", extreme)) %>% 
    select(variable, unit, overall, extreme) %>% 
    arrange(unit, variable)
  
  
  # wet day rain
  d1_wetrain_avg_all <- d1_wetrain_avg %>% 
    summarise_each(funs(avg. = mean, std. = sd), wetday_rain = wetday_rain) %>%
    mutate(type = "overall")
  
  d1_wetrain_avg_extr <- d1_wetrain_avg %>% 
    filter(seas_extreme) %>% 
    summarise_each(funs(avg. = mean, std. = sd), wetday_rain = wetday_rain) %>%
    mutate(type = "extreme")
  
  d1_wetrain_avg_smmry <- rbind(d1_wetrain_avg_all, d1_wetrain_avg_extr) %>% 
    mutate_each(funs(as.character(round(., 2))), -type) %>% 
    gather(key = variable, value, -type) %>% 
    spread(key = type, value) %>% 
    mutate(unit = "mm d-1",
           extreme = ifelse(is.na(extreme), "-", extreme))
  
  
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
    mutate(unit = "d yr-1",
           extreme = ifelse(is.na(extreme), "-", extreme))
  
  
  
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
    mutate(unit = ifelse(grepl("yrs", variable), "yr", "d yr-1"),
           extreme = ifelse(is.na(extreme), "-", extreme)) %>% 
    select(variable, unit, overall, extreme)
  
  
  # focal years
  yr_smmry <- data.frame(
    variable = "Year",
    unit     = "yr",
    overall  = paste(range(d1$Year), collapse = "-"),
    extreme = paste(d1_extr$Year, collapse = ", "))
  
  # summary table
  d1_smmry <- rbind.fill(d1_season_avg_smmry, d1_wetrain_avg_smmry, d1_rainclass_smmry, d1_contig_day_smmry, yr_smmry) %>% 
    arrange(unit) %>% 
    bind_rows(n_all)
  
  
  d1_list <- list(summary_tbl = d1_smmry, season_rain = d1_season_avg,
                  wetday_rain = d1_wetrain_avg, contig_day = d1_contig_day, 
                  rainclass = d1_rainclass, focal_yr = yr_smmry)
  
  return(d1_list)
  
}
