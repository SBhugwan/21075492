credits<-read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/netflix/credits.csv")
colnames(credits)

Titles<-read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/netflix/titles.csv")
colnames(Titles)


merge_datasets <- function(path_credits, path_titles) {

    credits <- read_csv(path_credits)
    titles <- read_csv(path_titles)


    merged_data <- merge(credits, titles, by = "id")

    return(merged_data)
}

merged_data <- merge_datasets("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/netflix/credits.csv",
                              "/Users/sahilbhugwan/Downloads/DatsciPractical23/data/netflix/titles.csv")

colnames(merged_data)

# 1 # number on content over time
analyze_titles <- function(data) {

    titles_per_year <- data %>%
        group_by(release_year) %>%
        summarize(num_titles = n())


    ggplot(titles_per_year, aes(x = release_year, y = num_titles)) +
        geom_line() +
        labs(x = "Year", y = "Number of Titles", title = "Trends in Number of Titles on Netflix")
}

Trends <- analyze_titles(merged_data)

print(Trends) #use to call graph


# 2 # View distribution of content



# Step 2: Define a function to analyze the distribution of content types and plot the proportions
analyze_content_types <- function(data) {

    content_type_counts <- data %>%
        count(type)


    pie_chart <- ggplot(content_type_counts, aes(x = "", y = n, fill = type)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(fill = "Content Type", x = NULL, y = NULL, title = "Distribution of Content Types on Netflix") +
        theme_void()


    bar_chart <- ggplot(content_type_counts, aes(x = type, y = n, fill = type)) +
        geom_bar(stat = "identity", width = 0.5) +
        labs(x = "Content Type", y = "Count", title = "Distribution of Content Types on Netflix") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


    list(pie_chart = pie_chart, bar_chart = bar_chart)
}


plots <- analyze_content_types(merged_data)

Pie<- print(plots$pie_chart)
Pie  #use to call graph
Bar<- print(plots$bar_chart)
Bar#use to call graph

# 3 average rating between movie and show



filtered_data <- merged_data[, c("type", "imdb_score")]


average_ratings <- aggregate(imdb_score ~ type, filtered_data, FUN = mean)


sorted_ratings <- average_ratings[order(average_ratings$imdb_score, decreasing = TRUE), ]

plot_average_ratings <- function(ratings_data) {
    barplot(ratings_data$imdb_score,
            names.arg = ratings_data$type,
            xlab = "Content Type",
            ylab = "Average IMDb Rating",
            main = "Average IMDb Ratings by Content Type",
            col = "steelblue",
            ylim = c(0, 10))
}


plot_average_ratings(sorted_ratings) #use to call graph


# Names with the most content on netflix
library(viridis)

plot_top_names <- function(data) {
    name_counts <- data %>%
        group_by(name) %>%
        summarise(content_count = n()) %>%
        arrange(desc(content_count)) %>%
        top_n(5)

    color_palette <- viridis(nrow(name_counts))

    ggplot(name_counts, aes(x = reorder(name, -content_count), y = content_count, fill = name)) +
        geom_bar(stat = "identity") +
        xlab("Name") +
        ylab("Content Count") +
        ggtitle("Top 5 Names with the Most Content on Netflix") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = color_palette)
}

plot_top_names(merged_data)  # Use to call graph

#correlation

plot_popularity <- function(data, actor_name) {
    filtered_data <- filter(data, name == actor_name)

    ggplot(filtered_data, aes(x = tmdb_popularity, y = imdb_score, color = tmdb_popularity)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_viridis() +
        xlab("TMDB Popularity") +
        ylab("IMDb Score") +
        ggtitle(paste("Correlation between TMDB Popularity and IMDb Score for", actor_name)) +
        theme_minimal() +
        theme(legend.position = "none")
}

plot_popularity <- purrr::partial(plot_popularity, data = merged_data)

plot_popularity("Shah Rukh Khan")


# counrties that produce the most content
library(viridis)

plot_title_count_by_country <- function(data) {
    data %>%
        mutate(production_countries = strsplit(production_countries, ", ")) %>%
        unnest(production_countries) %>%
        count(production_countries) %>%
        arrange(desc(n)) %>%
        top_n(10) %>%
        ggplot(aes(x = reorder(production_countries, n), y = n, fill = production_countries)) +
        geom_bar(stat = "identity") +
        xlab("Country") +
        ylab("Title Count") +
        ggtitle("Title Count by Production Country (Top 10)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d(option = "A", alpha = 0.8)
}

plot_title_count_by_country(merged_data)


#Analysis genres
library(wordcloud)
library(tidytext)
# Extract genres from merged_data
genres <- merged_data %>%
    select(genres) %>%
    unnest_tokens(output = "genre", input = genres, token = "words") %>%
    filter(!is.na(genre))

# Count the occurrences of each genre
genre_counts <- genres %>%
    count(genre) %>%
    arrange(desc(n))

# Plot the bar chart of genre distribution
barplot <- ggplot(genre_counts, aes(x = reorder(genre, n), y = n, fill = genre)) +
    geom_bar(stat = "identity") +
    xlab("Genre") +
    ylab("Count") +
    ggtitle("Genre Distribution") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set3")

# Display the bar chart
print(barplot)

# Create a word cloud of the dominant genres
generate_wordcloud <- function(data) {
    wordcloud(data$genre, data$n, scale = c(5, 1),
              random.order = FALSE, rot.per = 0.3,
              colors = brewer.pal(8, "Dark2"))
}

generate_wordcloud(genre_counts) # use to view





