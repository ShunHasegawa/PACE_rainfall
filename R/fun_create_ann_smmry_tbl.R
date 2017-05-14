
# create annual and seasonally summary ------------------------------------

create_ann_smmry_tbl <- function(rain_data){
  
  # . 1. annual -------------------------------------------------------------
  
  # annual summary
  ann_smmry <- rain_data %>% 
    group_by(Site.name, Year) %>% 
    summarise(rain = sum(rain)) %>% 
    group_by(Site.name) %>%
    summarise_each(funs(avg. = mean, std. = sd), rain) %>% 
    mutate(unit = "mm yr-1", cov. = round(std./avg., 2), season = "annual") %>% 
    mutate_each(funs(round(., 0)), avg., std.) %>% 
    select(Site.name, unit, everything())
  
  # annual dry summary  
  ann_smmry_dry <- rain_data %>% 
    group_by(Site.name, Year) %>% 
    summarise(rain = sum(rain)) %>% 
    arrange(Site.name, rain) %>% 
    group_by(Site.name) %>% 
    mutate(rain_rank = 1:n(),
           driest_yr = paste0("driest_yr", rain_rank)) %>% 
    filter(rain_rank <= 5) %>% 
    ungroup() %>% 
    left_join(ann_smmry) %>% 
    mutate(dry_rate = round((rain/avg. - 1) * 100, 0),
           dryrain_yr = paste0(Year, "(",round(rain, 0), "mm,", dry_rate, "%)")) %>% 
    select(Site.name, driest_yr, dryrain_yr) %>% 
    spread(driest_yr, dryrain_yr) %>% 
    left_join(ann_smmry) %>%
    select(season, Site.name, unit, avg., std., cov., everything())
  
  
  # . 2. seasonal summary ---------------------------------------------------
  
  
  # seasonal summary
  ssn_smmry <- rain_data %>% 
    group_by(Site.name, Year, season) %>% 
    summarise(rain = sum(rain)) %>% 
    group_by(Site.name, season) %>%
    summarise_each(funs(avg. = mean, std. = sd), rain) %>% 
    mutate(unit = "mm yr-1", cov. = round(std./avg., 2)) %>% 
    mutate_each(funs(round(., 0)), avg., std.)
  
  
  # seasonal dry summary
  ssn_smmry_dry <- rain_data %>% 
    group_by(Site.name, Year, season) %>% 
    summarise(rain = sum(rain)) %>% 
    arrange(season, Site.name, rain) %>% 
    group_by(season, Site.name) %>% 
    mutate(rain_rank = 1:n(),
           driest_yr = paste0("driest_yr", rain_rank)) %>% 
    filter(rain_rank <= 5) %>% 
    ungroup() %>% 
    left_join(ssn_smmry) %>% 
    mutate(dry_rate = round((rain/avg. - 1) * 100, 0),
           dryrain_yr = paste0(Year, "(",round(rain, 0), "mm,", dry_rate, "%)")) %>% 
    select(season,Site.name, driest_yr, dryrain_yr) %>% 
    spread(driest_yr, dryrain_yr) %>% 
    left_join(ssn_smmry) %>%
    select(season, Site.name, unit, avg., std., cov., everything())
  
  
  
  
  # . 3. May/June summary ---------------------------------------------------
  
  
  # May/June summary 
  mj_smmry <- rain_data %>%
    filter(Month %in% c(5, 6)) %>%
    mutate(Month = month.abb[Month]) %>% 
    group_by(Site.name, Year, Month) %>% 
    summarise(rain = sum(rain)) %>% 
    group_by(Site.name, Month) %>%
    summarise_each(funs(avg. = mean, std. = sd), rain) %>% 
    mutate(unit = "mm yr-1", cov. = round(std./avg., 2)) %>% 
    mutate_each(funs(round(., 0)), avg., std.)
  
  
  
  # May/June dry summary
  mj_smmry_dry <- rain_data %>%
    filter(Month %in% c(5, 6)) %>%
    mutate(Month = month.abb[Month]) %>% 
    group_by(Site.name, Year, Month) %>% 
    summarise(rain = sum(rain)) %>% 
    arrange(Month, Site.name, rain) %>% 
    group_by(Month, Site.name) %>% 
    mutate(rain_rank = 1:n(),
           driest_yr = paste0("driest_yr", rain_rank)) %>% 
    filter(rain_rank <= 5) %>% 
    ungroup() %>% 
    left_join(mj_smmry) %>% 
    mutate(dry_rate = round((rain/avg. - 1) * 100, 0),
           dryrain_yr = paste0(Year, "(",round(rain, 0), "mm,", dry_rate, "%)")) %>% 
    select(Month,Site.name, driest_yr, dryrain_yr) %>% 
    spread(driest_yr, dryrain_yr) %>% 
    left_join(mj_smmry) %>%
    mutate(season = Month) %>%
    select(-Month) %>% 
    select(season, Site.name, unit, avg., std., cov., everything())
  
  
  # . 4. merge the above ----------------------------------------------------
  
  
  ann_snn_smmry <- bind_rows(ann_smmry_dry, ssn_smmry_dry, mj_smmry_dry) %>% 
    mutate(season = factor(season, levels = c("annual", "spring", "summer", "autumn", "winter", "May", "Jun"))) %>% 
    arrange(season, Site.name)
  
  return(ann_snn_smmry)
  
}
