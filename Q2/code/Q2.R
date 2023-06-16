London<-read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/London/london_weather.csv")
colnames(London)
London$date <- ymd(London$date)
UK<- read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/London/UKMonthly_Detailed.csv")
colnames(UK)



# Line chart for maximum temperature
MT <- ggplot(London, aes(x = date, y = max_temp)) +
    geom_line(color = "#FF5733", size = 1.5) +
    geom_smooth(method = "loess", se = FALSE, color = "#3366FF", linetype = "dashed") +
    labs(x = "Date", y = "Maximum Temperature", title = "Maximum Temperature Trend") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid = element_blank()) +
    annotate("text", x = as.Date("1980-01-01"), y = 25, label = "Record High", color = "red", size = 3) +
    annotate("text", x = as.Date("1985-01-01"), y = -5, label = "Record Low", color = "blue", size = 3) +
    annotate("text", x = as.Date("1995-01-01"), y = 30, label = "Heatwave", color = "orange", size = 3) +
    annotate("text", x = as.Date("1990-01-01"), y = -10, label = "Cold Spell", color = "purple", size = 3) +
    theme(plot.background = element_rect(fill = "lightgray"))

#print(MT)


# Bar chart for precipitation levels
PL<- ggplot(London, aes(x = date, y = precipitation)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Date", y = "Precipitation", title = "Precipitation Levels")

PL <- ggplot(London, aes(x = date, y = precipitation)) +
    geom_bar(stat = "identity", fill = "#0099CC", alpha = 0.8) +
    labs(x = "Date", y = "Precipitation (mm)", title = "Precipitation Levels") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "lightgray"))

#print(PL)



# Area plot for sunshine duration
SD <- ggplot(London, aes(x = date, y = sunshine)) +
    geom_area(fill = "orange", alpha = 0.7) +
    labs(x = "Date", y = "Sunshine Duration", title = "Distribution of Sunshine Duration") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 10))

#print(SD)

# Scatter plot for temperature vs. precipitation

TemP <- ggplot(London, aes(x = mean_temp, y = precipitation)) +
    geom_point(color = "blue", alpha = 0.7, size = 3) +
    labs(x = "Mean Temperature (°C)", y = "Precipitation (mm)", title = "Temperature vs. Precipitation") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "lightgray"))

#print(TemP)


#box plot
Box_plot_year <- function(df) {
    london_box_y <- df %>%
        mutate(date = ymd(date)) %>%
        mutate(Year = year(date)) %>%
        group_by(Year) %>%
        summarise(max_temp = max(max_temp),
                  mean_temp = mean(mean_temp),
                  min_temp = min(min_temp)) %>%
        ungroup() %>%
        select(Year, max_temp, mean_temp, min_temp)

    boxplot_data <- gather(london_box_y, key = "Variable", value = "Temperature", -Year)

    boxplot <- ggplot(boxplot_data, aes(x = Year, y = Temperature, fill = Variable)) +
        geom_boxplot() +
        labs(x = "Year", y = "Temperature (°C)", title = "Temperature Comparison across Years") +
        scale_fill_manual(values = c("#FF5733", "#FFC300", "#C70039")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 12))

    return(boxplot)
}

Box_plot_year(London)




