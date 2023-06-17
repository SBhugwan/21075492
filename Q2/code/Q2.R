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



# using UK data

# Number of Days max Temp less than 21.1 degrees Celsius

temperature_data <- UK[, c("DATE", "TMAX", "TMIN", "DX70", "DX90")]
temperature_data$DATE <- as.Date(paste0(temperature_data$DATE, "-01"))
average_temperature_monthly <- aggregate(cbind(TMAX, TMIN) ~ format(DATE, "%Y-%m"), data = temperature_data, FUN = mean)
maximum_temperature_monthly <- aggregate(TMAX ~ format(DATE, "%Y-%m"), data = temperature_data, FUN = max)
minimum_temperature_monthly <- aggregate(TMIN ~ format(DATE, "%Y-%m"), data = temperature_data, FUN = min)
filtered_data2 <- temperature_data[format(temperature_data$DATE, "%Y") >= "1990", ]


L21 <- ggplot(filtered_data2, aes(x = format(DATE, "%Y"), y = DX70)) +
    geom_bar(stat = "identity", color = "blue", fill = "blue", width = 0.5) +
    labs(x = "Year", y = "Number of Days", title = "Number of Days with Max Temperature < 21.1°C") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())


#L21

M32<- ggplot(filtered_data2, aes(x = format(DATE, "%Y"), y = DX90)) +
    geom_bar(stat = "identity", color = "red", fill = "red", width = 0.5) +
    labs(x = "Year", y = "Number of Days", title = "Number of days with maximum temperature > 32.2 degrees Celsius") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
#M32



