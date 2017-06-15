## https://www.r-bloggers.com/crime-analysis-denver-part-1/
# 
# ## v1
# library(dplyr)  
# library(lubridate)  
# library(ggplot2)
# 
# CWD = getwd()  
# data = read.csv(paste(CWD,'/data/SF_Crime_2015_2016.csv',sep=''),stringsAsFactors = TRUE)  
# head(data)
# data$date = as.Date(data$Date,format="%m/%d/%Y")
# 
# #Create new columns for grouping
# data$year = year(data$date)  
# data$month = month(data$date)  
# data$day = day(data$date)  
# # data$hour = hour(data$Time)
# 
# print(colnames(data))  
# 
# #Sum up all incidents IS_CRIME AND IS_TRAFFIC
# maxYear = max(data$year)  
# maxMonthYTD = max(data$month[data$year==maxYear])
# 
# df = data %>%  
#   group_by(year,month) %>%
#   filter(month < maxMonthYTD) %>%
#   summarise(incidents = sum(ROBBERY) + sum(ASSAULT)) %>%
#   arrange(month)
# 
# p = ggplot(df)  
# p + geom_bar(aes(x = factor(year), weight = incidents)) + ggtitle('Incidents Reported by Year') + xlab('Year') + ylab('Incidents') + theme(plot.title = element_text(hjust = 0.5))
# 
# 
# 

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}




## v2
library(readr)
# path <- "data/SF_Crime_2015_2016.csv"
path <- "D:\\GitHub\\Crime-Analysis\\SF_Crime_2007_2016.csv"
df <- read_csv(path)

proper_case <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl=TRUE))
}

library(dplyr)
df <- df %>% mutate(Category = proper_case(Category),
                    Descript = proper_case(Descript),
                    PdDistrict = proper_case(PdDistrict),
                    Resolution = proper_case(Resolution))

df_arrest <- df %>% filter(grepl("Arrest", Resolution))

df_arrest_daily <- df_arrest %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  group_by(Date) %>%
  summarize(count = n()) %>%
  arrange(Date)

library(ggplot2)
library(scales)
plot <- ggplot(df_arrest_daily, aes(x = Date, y = count)) +
  geom_line(color = "#F2CA27", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  # fte_theme() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Date of Arrest", y = "# of Police Arrests", title = "Daily Police Arrests in San Francisco from 2007 – 2016")
plot


get_hour <- function(x) {
  return (as.numeric(strsplit(x,":")[[1]][1]))
}

df_arrest$Time <- as.character(df_arrest$Time)

df_arrest_time <- df_arrest %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(DayOfWeek, Hour) %>%
  summarize(count = n())
df_arrest_time %>% head(10)

dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))

df_arrest_time$DayOfWeek <- factor(df_arrest_time$DayOfWeek, level = rev(dow_format))
df_arrest_time$Hour <- factor(df_arrest_time$Hour, level = 0:23, label = hour_format)

df_arrest_time %>% head(10)


plot <- ggplot(df_arrest_time, aes(x = Hour, y = DayOfWeek, fill = count)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "# of Police Arrests in San Francisco from 2007 – 2016, by Time of Arrest") +
  scale_fill_gradient(low = "white", high = "#27AE60", labels = comma)
plot



df_top_crimes <- df_arrest %>%
  group_by(Category) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

df_top_crimes %>% head(20)
plot(df_top_crimes$count)


df_arrest_time_crime <- df_arrest %>%
  filter(Category %in% df_top_crimes$Category[2:19]) %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(Category, DayOfWeek, Hour) %>% 
  summarize(count = n())

df_arrest_time_crime$DayOfWeek <- factor(df_arrest_time_crime$DayOfWeek, level = rev(dow_format))
df_arrest_time_crime$Hour <- factor(df_arrest_time_crime$Hour, level = 0:23, label = hour_format)

df_arrest_time_crime %>% head(10)


plot <- ggplot(df_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = count)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "# of Police Arrests in San Francisco from 2003 – 2015, by Category and Time of Arrest") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ Category, nrow = 6)
plot


df_arrest_time_crime <- df_arrest_time_crime %>%
  group_by(Category) %>%
  mutate(norm = count/sum(count))

df_arrest_time_crime %>% head(10)



plot <- ggplot(df_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = norm)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Police Arrests in San Francisco from 2003 – 2015 by Time of Arrest, Normalized by Type of Crime") +
  scale_fill_gradient(low = "white", high = "#2980B9") +
  facet_wrap(~ Category, nrow = 6)
plot



df_arrest_time_district <- df_arrest %>%
  mutate(Hour = sapply(Time, get_hour)) %>%
  group_by(PdDistrict, DayOfWeek, Hour) %>% 
  summarize(count = n()) %>%
  group_by(PdDistrict) %>%
  mutate(norm = count/sum(count))

df_arrest_time_district$DayOfWeek <- factor(df_arrest_time_district$DayOfWeek, level = rev(dow_format))
df_arrest_time_district$Hour <- factor(df_arrest_time_district$Hour, level = 0:23, label = hour_format)

df_arrest_time_district %>% head(10)


plot <- ggplot(df_arrest_time_district, aes(x = Hour, y = DayOfWeek, fill = norm)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Police Arrests in San Francisco from 20073 – 2016 by Time of Arrest, Normalized by Station") +
  scale_fill_gradient(low = "white", high = "#8E44AD") +
  facet_wrap(~ PdDistrict, nrow = 5)
plot



df_arrest_time_month <- df_arrest %>%
  mutate(Month = format(as.Date(Date, "%m/%d/%Y"), "%B"), Hour = sapply(Time, get_hour)) %>%
  group_by(Month, DayOfWeek, Hour) %>% 
  summarize(count = n()) %>%
  group_by(Month) %>%
  mutate(norm = count/sum(count))

df_arrest_time_month$DayOfWeek <- factor(df_arrest_time_month$DayOfWeek, level = rev(dow_format))
df_arrest_time_month$Hour <- factor(df_arrest_time_month$Hour, level = 0:23, label = hour_format)

df_arrest_time_month %>% head(10)



# Set order of month facets by chronological order instead of alphabetical
df_arrest_time_month$Month <- factor(df_arrest_time_month$Month,
                                     level = c("January","February","March","April","May","June","July","August","September","October","November","December"))

plot <- ggplot(df_arrest_time_month, aes(x = Hour, y = DayOfWeek, fill = norm)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Police Arrests in San Francisco from 2007 – 2016 by Time of Arrest, Normalized by Month") +
  scale_fill_gradient(low = "white", high = "#E74C3C") +
  facet_wrap(~ Month, nrow = 4)
plot


df_arrest_time_year <- df_arrest %>%
  mutate(Year = format(as.Date(Date, "%m/%d/%Y"), "%Y"), Hour = sapply(Time, get_hour)) %>%
  group_by(Year, DayOfWeek, Hour) %>% 
  summarize(count = n()) %>%
  group_by(Year) %>%
  mutate(norm = count/sum(count))

df_arrest_time_year$DayOfWeek <- factor(df_arrest_time_year$DayOfWeek, level = rev(dow_format))
df_arrest_time_year$Hour <- factor(df_arrest_time_year$Hour, level = 0:23, label = hour_format)

df_arrest_time_year %>% head(10)


plot <- ggplot(df_arrest_time_year, aes(x = Hour, y = DayOfWeek, fill = norm)) +
  geom_tile() +
  # fte_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Police Arrests in San Francisco from 2007 – 2016 by Time of Arrest, Normalized by Year") +
  scale_fill_gradient(low = "white", high = "#E67E22") +
  facet_wrap(~ Year, nrow = 6)
plot

library(ggmap)
bbox = c(-122.516441,37.702072,-122.37276,37.811818)
# credit to /u/all_genes_considered for map setting suggestion
map <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")

png("sf-arrest-where-0.png", w=900, h=900, res=300)
ggmap(map)
dev.off()


plot <- ggmap(map) +
  geom_point(data = df_arrest, aes(x=X, y=Y), color = "#27AE60", size = 0.5, alpha = 0.01) +
  # fte_theme() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.margin = unit(c(0.3, 0.3, -0.25, 0), "cm")) +
  labs(title = "Locations of Police Arrests Made in San Francisco from 2007 – 2016")
plot

