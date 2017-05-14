

# prepare data frame ------------------------------------------------------


# driest years  
driest_yr_d <- ann_rain_800_all %>% 
  group_by(.id, Site.name, Year) %>% 
  summarise(rain = sum(rain)) %>% 
  arrange(.id, Site.name, rain) %>% 
  group_by(.id, Site.name) %>% 
  mutate(rain_rank = 1:n(),
         driest_yr = paste0("driest_yr", rain_rank)) %>% 
  filter(rain_rank <= 5) %>% 
  ungroup() %>% 
  select(-rain_rank, -rain)


# autumn break
autbreak <- ddply(ann_rain_800_all, .(.id, Site.name), function(y){
  d <- y[order(y$date), ]                                       # re-order by date
  d$trigger_5d <- get_rd_sum(d$rain, nday = 5)                  # triggering: get 5-day sum of rainfall
  d$topup_30d  <- get_rd_sum(d$rain, nday = 35) - d$trigger_5d  # top-up:     get 30-day sum of rainfall after the triggering 5-day rain
  return(d)
}) 



autbreak_ed <- autbreak %>%
  filter(Month %in% c(5:6)) %>% 
  mutate(trigger = rain > 0 & trigger_5d >= 30 & topup_30d >= 20) %>%  # condition for triggering day (Rd5 > 30 and Rd30 > 20)
  filter(trigger) %>% 
  group_by(.id, Site.name, Year) %>% 
  summarise(trigger_1st = date[which.min(date)]) %>%  # identify the 1st day of triggering
  ungroup() %>% 
  mutate(day_n = yday(trigger_1st),
         day_n = day_n - min(day_n) + 1) %>% 
  left_join(driest_yr_d) %>% 
  mutate(driest_yr = factor(ifelse(is.na(driest_yr), "non-dry", driest_yr)),
         .id = factor(.id, levels = c("yr100", "yr30", "yr10")))
  


# figure ------------------------------------------------------------------


fig_autbreak_scatter <- dlply(autbreak_ed, .(.id), function(x){
  ggplot(autbreak_ed, aes(x = Year, y = day_n))+
    geom_smooth(method = "lm")+
    geom_point(aes(col = driest_yr))+
    facet_wrap(~ Site.name)+
    scale_color_manual("Dry-year", values = c(2, 3, 4, 5, 6, "gray50"))+
    labs(y = "No. of days from May to June")+
    ggtitle(as.character(unique(x$.id)))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
})


fig_autbreak_box <- ggplot(autbreak_ed, aes(x = .id, y = day_n))+
  geom_boxplot(outlier.shape = 21)+
  geom_jitter(data = filter(autbreak_ed, driest_yr != "non-dry"), aes(col = driest_yr))+
  facet_wrap(~ Site.name)+
  scale_color_manual("Dry-year", values = c(2, 3, 4, 5, 6, "gray"))+
  labs(y = "No. of days from May to June")




# summary quantile of the triggering date ---------------------------------


probs <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
autbreak_smmry <- autbreak_ed %>% 
  group_by(.id, Site.name) %>% 
  do(data.frame(day_n = round(quantile(.$day_n, prob = probs), 0),
                quant = paste0(probs * 100, "%"))) %>% 
  spread(quant, day_n) %>% 
  arrange(Site.name, .id) %>% 
  select(Site.name, .id, `1%`, `5%`, `10%`,`25%`, `50%`, `75%`, `90%`, `95%`, `99%`)
  
write.csv(autbreak_smmry, file = "Output/Tables/summary_autumnbreak_quntiles.csv", 
          row.names = FALSE)
