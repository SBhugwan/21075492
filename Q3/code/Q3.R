CP<-read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/Coldplay_vs_Metallica/Coldplay.csv")
metallica<- read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/Coldplay_vs_Metallica/metallica.csv")
Spotify<- read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/Coldplay_vs_Metallica/Broader_Spotify_Info.csv")
colnames(CP)


# Scatter plot: Popularity vs. Release Date
scatter_plot <- function(data) {
    ggplot(data, aes(x = release_date, y = popularity)) +
        geom_point() +
        labs(x = "Release Date", y = "Popularity", title = "Popularity of Songs over Time")
}

#scatter_plot(metallica)
#scatter_pot(CP)

#Function of Popular songs by rating and album

PS <- function(df, release_date_filter = NULL, popularity_filter = NULL) {
    cold_pop <- df %>%
        mutate(release_date = as.Date(release_date)) %>%
        group_by(album_name) %>%
        mutate(album_release = min(release_date)) %>%
        ungroup()

    arranger <- cold_pop %>%
        group_by(album_name) %>%
        summarise(average = mean(popularity)) %>%
        arrange(desc(average))

    order <- arranger$album_name


    if (!is.null(release_date_filter)) {
        cold_pop <- cold_pop %>%
            filter(release_date >= as.Date(release_date_filter))
    }

    if (!is.null(popularity_filter)) {
        cold_pop <- cold_pop %>%
            filter(popularity >= popularity_filter)
    }


    top_songs <- cold_pop %>%
        group_by(name) %>%
        summarise(total_popularity = sum(popularity)) %>%
        arrange(desc(total_popularity)) %>%
        top_n(5)

    cold_pop <- cold_pop %>%
        semi_join(top_songs, by = "name")


    plot <- ggplot(cold_pop, aes(x = release_date, y = popularity, color = album_name, label = name)) +
        geom_line() +
        geom_point() +
        geom_text(nudge_y = 5, check_overlap = TRUE) +
        labs(x = "Release Date", y = "Popularity", title = "Top Songs Popularity Over Time") +
        scale_color_discrete(name = "Album") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))

    print(plot)

    list(cold_pop = cold_pop, order = order)
}

#PS(CP)
#PS(metallica)


#coldplay Correlation matrix
library(corrplot)
CMCP<- CP %>%
    select(danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo) %>%
    cor()

corrplot(CMCP, method = "color")

#Mettallica correlation matrix

CMM<- metallica%>%
    select(danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo) %>%
    cor()
corrplot::corrplot(CMM, method = "color", col = colorRampPalette(c("blue", "white", "red"))(10))

