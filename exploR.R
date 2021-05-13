library("tidyverse")
library(gridExtra)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#merge the two tables
df <- merge(NEI, SCC, by = "SCC")
df$IsCoal <- str_detect(tolower(df$Short.Name), "coal")


#1 Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

q1 <- df %>% group_by(year) %>% summarise(total = sum(Emissions))

png("plot1.png", width = 480 , height = 480)
with(q1,plot(year,total, col = "red", pch = 15))
with(q1,lines(year,total))
title("Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?")
dev.off()

#2 Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

q2 <- df %>% filter(fips == "24510") %>% group_by(year) %>% summarise(total = sum(Emissions))

png("plot2.png", width = 480 , height = 480)
with(q2,plot(year,total, col = "red", pch = 15))
with(q2,lines(year,total))
title("Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008?")
dev.off()


#3 Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


q3 <- df %>% filter(fips == "24510" & Data.Category %in% c("Point", "Nonpoint", "Onroad", "Nonroad")) %>% group_by(year,Data.Category) %>% summarise(total = sum(Emissions))

png("plot3.png", width = 480 , height = 480)
ggplot(data = q3) + geom_line(aes(x = year, y = total, color = Data.Category)) +
                  geom_point(aes(x = year, y = total, color = Data.Category, shape = Data.Category)) +
                  ggtitle("Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008 by type?") +
                  theme_minimal()
dev.off()

#4 Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

q4 <- df %>% filter(IsCoal == TRUE) %>% group_by(year) %>% summarise(total = sum(Emissions))

png("plot4.png", width = 480 , height = 480)
ggplot(data = q4) + geom_line(aes(x = year, y = total)) +
  geom_point(aes(x = year, y = total)) +
  ggtitle("Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?") +
  theme_minimal()
dev.off()


#5 How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
q5 <- df %>% filter(Data.Category == "Onroad" & fips == "24510") %>% group_by(year) %>% summarise(total = sum(Emissions))

png("plot5.png", width = 480 , height = 480)
ggplot(data = q5) + geom_line(aes(x = year, y = total)) +
  geom_point(aes(x = year, y = total)) +
  ggtitle("How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?") +
  theme_minimal()
dev.off()


#6 Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"
#fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

q6 <- df %>% filter(Data.Category == "Onroad" & (fips == "24510" | fips == "06037")) %>% mutate(county = case_when(fips == "24510"~"BA", fips == "06037"~"LA")) %>%
  group_by(county,year) %>% summarise(total = sum(Emissions))
q6$pchg <- q6$total
pct <- function(x) {(x - lag(x))/lag(x)*100}
sub <- q6%>% filter(row_number()==1) %>% select(county, total) %>% rename(base = total)
q6 <- merge(q6,sub, by = "county")
q6$pcfb <- ((q6$total - q6$base) / q6$base) * 100

q6 <- q6 %>% group_by(county) %>% mutate_each(funs(pct), c(pchg)) 
q6$pchg[is.na(q6$pchg)] = 0




p1 <- ggplot(data = q6) + geom_line(aes(x = year, y = total, color = county)) +
  geom_point(aes(x = year, y = total, color = county, shape = county)) +
  ggtitle("Which city has seen greater changes over time in motor vehicle emissions") +
  theme_minimal()


p2 <- ggplot(data = q6) + geom_line(aes(x = year, y = pchg, color = county)) +
  geom_point(aes(x = year, y = pchg, color = county, shape = county)) +
  ggtitle("Which city has seen greater changes over time in motor vehicle emissions % change from previous year") +
  geom_hline(yintercept = 0)+
  theme_minimal()

p3 <- ggplot(data = q6) + geom_line(aes(x = year, y = pcfb, color = county)) +
  geom_point(aes(x = year, y = pcfb, color = county, shape = county)) +
  ggtitle("Which city has seen greater changes over time in motor vehicle emissions % change from 1999") +
  geom_hline(yintercept = 0)+
  theme_minimal()
png("plot6.png", width = 480 , height = 480)
grid.arrange(p1,p2,p3)
dev.off()
