require(rlms)
require(dplyr)
require(lubridate)
require(ggplot2)

raw_data <- rlms_read('r26iall26b.sav')
d_data <- haven::read_sav('r26iall26b.sav')

sub_men <- raw_data %>% 
  select(vh7.1, vh7.2, region, status, popul,vj69.9c, vm1, vm2, vh5, v_diplom, v_marst, 
          vj72.171, vj72.172, vj72.19, vj72.18,  vm3) %>% 
  transmute(date = dmy(paste0(vh7.1,".", vh7.2,".","2017")),
            region = region, 
            type_of_hometown = status,
            population = popul,
            age = 2017 - vj69.9c,
            weight = vm1,
            height = vm2,
            health = vm3,
            sex = vh5, 
            education = v_diplom,
            married_status = v_marst,
            childrens = ifelse(is.na(vj72.172), 0, vj72.172),
            religion = vj72.19,
            religion_opinion = vj72.18)


na.exclude()
summary(sub_men)

# В какие дни будние или выходные проводился опрос
sub_men$type_of_weekday <- ifelse(weekdays(sub_men$date) == 'Saturday' |  weekdays(sub_men$date) == 'Sunday', 
                                  'Weekend', 'Workday')
sub_men$weekday <- weekdays(sub_men$date)

weekends_distrubtion <- table(sub_men$type_of_weekday) 
max(sub_men$date) - min(sub_men$date) 
weekends_distrubtion[1] <- weekends_distrubtion[1]/2/363
weekends_distrubtion[2] <- weekends_distrubtion[2]/5/363
weekends_distrubtion
# плотность социологическких опросов составляла ...

names(sort(table(sub_men$religion), decreasing = T))

ifelse(is.na(raw_data$vj72.172), 0, raw_data$vj72.172)
# work 
# vi1, vj1.1.1, vj1.1.3, vj2cod08,
# vj4.1, vj6, vj6.1a, vj6.1b, vj10, vj10.3, vj11.1, vj13.2, vj14, vj18.2, vj19, vj21a,
# vj23, vj29c.1, vj31, vj32, vj33cod08, vj36.1a, vj40, vj41.1, vj49,

# случайный заработок
# vj56.11c08, vj56.12c08, vj56.13c08, vj58, vj80.1, vm418, vj197, 
# самооценка
# vj62, vj64, vj65, vj66.1, vj72.5a, vj72.5b, vj72.5c, vj72.5j, vj72.5a2, vj72.5a3б vj72.5a4
# профессионализм, доп образование
# vj69.1, vj60.1, vj72.11, vj72121c, vj72121b, vj72121d, vj260 
# соцсети
# vj401.1a, vj401.2a, vj401.3a, vj401.4a, vj401.5a, 
# 
# здоровье
# vl5.0, vl83, vl5, vl66, # 