library(dplyr)
library(ggplot2)

data <-  df
names(data)
str(data)
head(data, 10)
data[sample(x = nrow(data), size = 10), ]
unique(data$Category)


data$Category <- factor(x = data$Category)
data$Category <- reorder(x = data$Category, X = data$Category, FUN = length)
table(data$Category)[order(table(data$Category), decreasing = T)]


# Set up custom theme
my_theme <- theme_bw() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        axis.title = element_text(colour = "grey40"),
        plot.title = element_text(vjust = 2.0))

# Plot crime count by month
ggplot(data = data, aes(x = monthRC)) + 
  my_theme +
  geom_bar(stat = "bin", fill = "lightblue", width = 0.75) +
  ggtitle("Total Crimes by Month") +
  xlab(NULL) +
  ylab("Total Crime Count")