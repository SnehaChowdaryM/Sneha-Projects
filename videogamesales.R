vg<-read.csv('vgsales.csv')
vg
colnames(vg)
dim(vg)
library(ggplot2)
library(tidyverse)
library(viridisLite)
library(reshape2)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(reshape2)
library(scales)
library(grid)



summary(vg)
ggplot(data=vg)+geom_histogram(binwidth=2.5,aes(x=vg$Global_Sales))+ggtitle("videogames sales Distribution")+xlab("global sales")+ylab("Frequency")+theme_minimal()
mean(vg$Global_Sales)
median(vg$Global_Sales)
sum(vg$Global_Sales<40)
sum(vg$Global_Sales>50)
sum(vg$Global_Sales>20)
ggplot(data=vg)+geom_histogram(binwidth=1,aes(x=vg$Global_Sales))+ggtitle("videogames sales Distribution")+xlab("global sales-Binwidth 0.5")+ylab("Frequency")+theme_minimal()+xlim(0,5)
ggplot(data=vg)+geom_histogram(binwidth=0.5,aes(x=vg$Global_Sales))+ggtitle("videogames sales Distribution")+xlab("global sales-Binwidth 0.25")+ylab("Frequency")+theme_minimal()+xlim(0,3)
ggplot(data=vg)+geom_histogram(binwidth=2,aes(x=vg$Global_Sales))+ggtitle("video games sales distribution by year")+xlab("global sales")+ylab("frequency")+theme_minimal()+facet_wrap(~Year)
subset(vg,Global_Sales==max(Global_Sales))
subset(vg,Global_Sales==min(Global_Sales))

vg %>%
  group_by(Year) %>%
  summarize(Number_of_Games = n()) %>%
  ggplot(aes(x = Year, y = Number_of_Games)) +
  geom_col(fill = "magenta4") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Games released per Year", x = "Year", y = "Sales (units)")   #games released per year


vg %>%
  group_by(Year, Publisher) %>%
  summarize(Sales = sum(Global_Sales)) %>%
  top_n(n = 1) %>%
  kable()           #selecting by sales



ggplot(vg, aes(factor(Genre), Global_Sales, fill=Genre)) + geom_boxplot() + ggtitle("video games sales according to genre") + xlab("Type of genre") + ylab("sales") + coord_cartesian(ylim=c(0,5))
ggplot(vg, aes(factor(Year), Global_Sales, fill=Year)) + geom_boxplot() + ggtitle("video games sales according to year") + xlab("year") + ylab("sales") + coord_cartesian(ylim=c(0,5))
ggplot(vg, aes(factor(Publisher), Global_Sales, fill=Publisher)) + geom_boxplot() + ggtitle("video games sales according to publisher") + xlab("Type of publisher") + ylab("sales") + coord_cartesian(ylim=c(0,5))
ggplot(vg, aes(factor(Genre), NA_Sales, fill=Genre)) + geom_boxplot() + ggtitle("NA sales according to genre") + xlab("Type of genre") + ylab("NA sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Genre), EU_Sales, fill=Genre)) + geom_boxplot() + ggtitle("EU sales according to genre") + xlab("Type of genre") + ylab("EU sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Genre), JP_Sales, fill=Genre)) + geom_boxplot() + ggtitle("JP sales according to genre") + xlab("Type of genre") + ylab("JP sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Year), NA_Sales, fill=Year)) + geom_boxplot() + ggtitle("NA sales according to year") + xlab("year") + ylab("NA sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Year), EU_Sales, fill=Year)) + geom_boxplot() + ggtitle("EU sales according to year") + xlab("year") + ylab("EU sales") + coord_cartesian(ylim=c(0,2))
ggplot(vg, aes(factor(Year), JP_Sales, fill=Year)) + geom_boxplot() + ggtitle("JP sales according to year") + xlab("year") + ylab("JP sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Name), NA_Sales, fill=Name)) + geom_boxplot() + ggtitle("NA sales according to name") + xlab("name") + ylab("NA sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Name), EU_Sales, fill=Name)) + geom_boxplot() + ggtitle("EU sales according to name") + xlab("name") + ylab("EU sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Name), JP_Sales, fill=Name)) + geom_boxplot() + ggtitle("JP sales according to name") + xlab("name") + ylab("JP sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Year), Other_Sales, fill=Year)) + geom_boxplot() + ggtitle("other sales according to year") + xlab("year") + ylab("other sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Publisher), Other_Sales, fill=Publisher)) + geom_boxplot() + ggtitle("other sales according to publisher") + xlab("publisher") + ylab("other sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Name), Other_Sales, fill=Name)) + geom_boxplot() + ggtitle("other sales according to name") + xlab("name") + ylab("other sales") + coord_cartesian(ylim=c(0,3))
ggplot(vg, aes(factor(Genre), Other_Sales, fill=Genre)) + geom_boxplot() + ggtitle("other sales according to Genre") + xlab("genre") + ylab("other sales") + coord_cartesian(ylim=c(0,3))



