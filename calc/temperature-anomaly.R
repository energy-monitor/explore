source("_shared.r")

theme_set(theme_bw(base_size=18))

d.temp = loadFromStorage(id = "temperature-hdd")[, `:=`(
  date = as.Date(date)
)]

temp_vars <- d.temp %>% 
  mutate(day=yday(date),
         year=year(date))

max <- temp_vars %>% 
  filter(date==max(date)) %>% 
  dplyr::select(date) %>% 
  mutate(max_day=day(date), max_month=month(date))

temp_vars %>% 
  mutate(is_2023=ifelse(year==2023, " 2023", "!=2023")) %>% 
  ggplot(aes(x=day, y=temp)) +
  geom_point(aes(col=is_2023), size=1.2) +
  scale_color_manual(values=rev(COLORS3)) +
  xlab("Tag des Jahres") +
  ylab("Tagesdurchschnittstemperatur") +
  labs(caption=glue("Source: ERA5, 1.1.1950-{max$max_day[1]}.{max$max_month[1]}.2023"))
  
temp_vars %>% 
  mutate(month=month(date)) %>% 
  mutate(Periode=ifelse(year==2023, "2023", "1950-1990")) %>% 
  mutate(Periode=ifelse(year<=2022&year>=1990, "1990-2022", Periode)) %>% 
  ggplot(aes(x=factor(month), y=temp)) +
  geom_boxplot(aes(col=Periode)) +
  scale_color_manual(values=rev(COLORS3)) +
  xlab("Monat des Jahres")+
  ylab("Tagesdurchschnittstemperatur\n(°C)") +
  theme_bw()  +
  labs(caption=glue("Source: ERA5, 1.1.1950-{max$max_day[1]}.{max$max_month[1]}.2023"))
  
period_start <- 1950
period_end <- 2022

year_2023 <- temp_vars %>% 
  filter(year(date) == 2023) 

years <- rep(period_start:period_end, each=nrow(year_2023))

year_2023_replicate <- year_2023 %>% 
   slice(rep(1:n(), length(period_start:period_end))) %>% 
  mutate(year=years)

temp_vars <- temp_vars %>% 
  mutate(ind=1:n()) %>% 
  mutate(ind_back=n():1)

temp_inc <- summary(lm(temp~(ind),data=temp_vars))$coefficients[2, 1]
temp_vars_climate_change <- temp_vars %>% 
  mutate(temp_cc=temp + temp_inc*ind_back)

temp_vars_climate_change <- temp_vars_climate_change %>% 
  mutate(temp=temp_cc) %>% 
  dplyr::select(date, temp, hdd, temp.vienna, hdd.vienna, day, year)

stitch <- temp_vars_climate_change %>%
  filter(year(date) < 2023) %>% 
  filter(year(date)>=period_start) %>% 
  filter((month(date) > max$max_month[1])|((month(date)==max$max_month[1]) & day(date)>max$max_day[1]))

stitched <- bind_rows(year_2023_replicate, stitch) %>% 
  group_by(year) %>% 
  summarize(mean_temp=mean(temp)) %>% 
  mutate(year=2023)

yearly_means <- temp_vars %>% 
  filter(year < 2023) %>% 
  group_by(year=year(date)) %>% 
  summarize(mean_temp=mean(temp)) %>% 
  ungroup() %>% 
  mutate(ind=1:n())

yearly_means %>% 
  ggplot(aes(x=year, y=mean_temp)) +
           geom_point() + 
  geom_boxplot(data=stitched, aes(x=year, y=mean_temp)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(caption=glue("Source: ERA5, 1.1.1950-{max$max_day[1]}.{max$max_month[1]}.2023"))+
  xlab("Year") +
  ylab("Austrian average annual temperature (°C)")


