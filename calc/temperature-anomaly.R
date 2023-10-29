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
  xlab("Tag des Jahres") +
  ylab("Tagesdurchschnittstemperatur") +
  labs(caption=glue("Source: ERA5, 1.1.1950-{max$max_day[1]}.{max$max_month[1]}.2023"))

temp_vars %>%
  mutate(month=month(date), year=year(date), day=yday(date)) %>%
  #filter((month > max$max_month[1])|((month==max$max_month[1]) & day>max$max_day[1])) %>%
  group_by(year) %>%
  arrange(day) %>%
  mutate(Jahr=ifelse(year==2018,"2018","anderes Jahr")) %>%
  mutate(Jahr=ifelse(year==2023,"2023",Jahr)) %>%
  ggplot(aes(x=day, y=temp)) +
  geom_line(aes(col=Jahr, fill=as.character(year), alpha=Jahr), linewidth=1) +
  scale_alpha_manual(values=c(1,1,0.02)) +
  #scale_color_manual(values=(COLORS3)) +
  xlab("Tag des Jahres") +
  ylab("Tagesdurchschnittstemperatur (°C)")

temp_vars %>%
  mutate(month=month(date), year=year(date), day=yday(date)) %>%
  #filter((month > max$max_month[1])|((month==max$max_month[1]) & day>max$max_day[1])) %>%
  group_by(year) %>%
  arrange(day) %>%
  mutate(temp=cumsum(temp)) %>%
  mutate(Jahr=ifelse(year==2018,"2018","anderes Jahr")) %>%
  mutate(Jahr=ifelse(year==2023,"2023",Jahr)) %>%
  ggplot(aes(x=day, y=temp)) +
  geom_line(aes(col=Jahr, fill=as.character(year), alpha=Jahr), linewidth=1) +
  scale_alpha_manual(values=c(1,1,0.2)) +
  #scale_color_manual(values=(COLORS3)) +
  xlab("Tag des Jahres") +
  ylab("Kumulative Tagesdurchschnittstemperatur\n(°C)") +
  theme_bw()  +
  labs(caption=glue("Source: ERA5, 1.1.1950-{max$max_day[1]}.{max$max_month[1]}.2023"))


temp_vars %>%
  mutate(month=month(date)) %>%
  mutate(Periode=ifelse(year==2023, "2023", "1950-1990")) %>%
  mutate(Periode=ifelse(year<=2022&year>=1990, "1990-2022", Periode)) %>%
  ggplot(aes(x=factor(month), y=temp)) +
  geom_boxplot(aes(col=Periode)) +
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

max_temp <- temp_vars %>%
  filter(year < 2023) %>%
  group_by(year=year(date)) %>%
  summarize(mean_temp=mean(temp)) %>%
  summarize(max=max(mean_temp)) %>%
  unlist()

nmb_above <- stitched %>%
  filter(mean_temp > max_temp) %>%
  nrow()

nmb_above/nrow(stitched)

plot(ecdf(stitched$mean_temp))

##################################


temp_reduced <- temp_vars %>%
  mutate(month=month(date)) %>%
  filter(month==max$max_month[1]) %>%
  filter(day(date)<=max$max_day[1])

mean_temps <- temp_reduced %>%
  group_by(year) %>%
  summarize(mean=mean(temp)) %>%
  arrange(desc(mean)) %>%
  head(10)

right_join(temp_vars, mean_temps, by=c("year"="year")) %>%
  arrange(desc(mean)) %>%
  ggplot(aes(x=reorder(as.character(year), mean), y =temp)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Österreichische Durchschnittstemperatur (°C)\n
       1.-3.Oktober") +
  xlab("Jahr")

mean_temps %>%
  ggplot(aes(x=as.character(year), y =mean)) +
  geom_point() +
  theme_bw() +
  ylab("Österreichische Durchschnittstemperatur (°C)") +
  xlab("Jahr") +
  ylim(10,20) +
  theme_bw()


temp_mean_2023 <- temp_reduced %>%
  group_by(year) %>%
  summarize(mean=mean(temp)) %>%
  mutate(Periode="1950-2023")

temp_mean_2023 %>%
  arrange((mean)) %>%
  mutate(diff=mean-lag(mean)) %>%
  ggplot(aes(x=reorder(year, mean), y=mean)) +
  geom_point() +
  theme_bw() +
  ylab("Österreichische Durchschnittstemperatur im Oktober(°C)") +
  xlab("Jahr") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(caption="Quelle: ERA5, 1.1.1950-30.9.2023")


temp_mean_up_to_now <- temp_vars %>%
  mutate(month=month(date), day=day(date)) %>%
  filter(month < max$max_month[1] | (month == max$max_month[1] & day <= max$max_day[1])) %>%
  group_by(year=year(date)) %>%
  summarize(mean_temp=mean(temp)) %>%
  mutate(Periode="Bis Oktober")

temp_mean_up_to_last_month <- temp_vars %>%
  mutate(month=month(date), day=day(date)) %>%
  filter(month < max$max_month[1]) %>%
  group_by(year=year(date)) %>%
  summarize(mean_temp=mean(temp)) %>%
  mutate(Periode="Bis September")

temp_mean_up_to_now %>%
  arrange(desc(mean_temp))

temp_mean_up_to_now %>%
  arrange(desc(mean_temp)) %>%
  dplyr::select(mean_temp) %>%
  unlist() %>%
  head(5)


temp_mean_up_to_last_month %>%
  arrange(desc(mean_temp))















