library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(jsonlite)

lab_sodra <- read.csv("C:/Users/Aiste/OneDrive/Desktop/vizualizacija_LD2/KTU-duomenu-vizualizacija/laboratorinis/data/lab_sodra.csv")
summary(lab_sodra)

#######   uzd 1   #######

kodukas_data <- lab_sodra %>% filter(ecoActCode==561000)
summary(kodukas_data)

histo <- ggplot(kodukas_data,aes(avgWage))+geom_histogram(bins=200)+
  labs( x="56100 average salary", y="frequency")

ggsave(histo,file="C:/Users/Aiste/OneDrive/Desktop/vizualizacija_LD2/KTU-duomenu-vizualizacija/laboratorinis/img/Rplot_1.png")

#######   uzd 2   #######  TOP5

top5 <- lab_sodra %>% filter(ecoActCode==561000) %>% group_by(name,code) %>%
  summarise(wage=max(avgWage)) %>% arrange(desc(wage)) %>%
  head(5)

gr2 <- lab_sodra %>% 
  filter(code %in% top5$code)

gr2 <- mutate (gr2,month1=parse_date_time(month, "ym"))

graph2 <- ggplot(gr2, aes(x=month1, y=avgWage, group=name, color=name)) +
  geom_line(linewidth = 0.9, linetype = 4, alpha=0.4)+geom_point(color="orchid3")+theme_bw()+
  labs(colour="COMPANY", y="Average salary", x="Month")+
  scale_x_datetime(date_labels="%b %y",date_breaks  ="1 month")

graph2

ggsave(graph2,file="C:/Users/Aiste/OneDrive/Desktop/vizualizacija_LD2/KTU-duomenu-vizualizacija/laboratorinis/img/Rplot_2.png")

#######   uzd 3   #######

gr3 <- gr2 %>% group_by(name,code) %>% summarise(max_insured=max(numInsured)) %>% na.omit() %>% arrange (desc(max_insured))

graph3 <- gr3 %>% ggplot(aes(x=reorder(code, -max_insured), max_insured,group=code, fill=name))+geom_col()+ 
           scale_y_continuous(labels = scales::number_format())+theme_light()+ 
           labs(fill= "COMPANY", y="Maximum amount of insured people", x="Comapny Code")

ggsave(graph3,file="C:/Users/Aiste/OneDrive/Desktop/vizualizacija_LD2/KTU-duomenu-vizualizacija/laboratorinis/img/Rplot_3.JPG")










