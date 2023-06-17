UR<-read.csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/googleplay/googleplaystore_user_reviews.csv")

GPS<- read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/googleplay/googleplaystore.csv")

GPS <- GPS %>%
    mutate(Last_Updated_Date = as.Date(`Last Updated`, format = "%B %d, %Y"))


#APP rating
APR<- ggplot(GPS, aes(x = Rating)) +
    geom_histogram(fill = "#69b3a2", color = "#333333", bins = 20) +
    labs(x = "App Rating", y = "Count", title = "Distribution of App Ratings") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())

#APR



# Count of apps in each content rating category
content_rating_counts <- GPS %>%
    group_by(`Content Rating`) %>%
    summarise(count = n()) %>%
    arrange(desc(count))


Ratings<- ggplot(content_rating_counts, aes(x = `Content Rating`, y = count, fill = `Content Rating`)) +
    geom_col() +
    labs(x = "Content Rating", y = "Count", title = "App Content Rating Categories") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Ratings

#app rattings vs last update
GPS$Last_Updated <- as.Date(GPS$`Last Updated`, format = "%B %d, %Y")

# Scatter plot of last updated date vs. ratings
ARLU<- ggplot(GPS, aes(x = Last_Updated, y = Rating)) +
    geom_point(color = "#0072B2") +
    labs(x = "Last Updated Date", y = "App Rating", title = "App Ratings vs. Last Updated Date") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())


#ARLU

#app size and user rating
GPS$Size <- gsub("[^0-9.]", "", GPS$Size)
GPS$Size <- as.numeric(GPS$Size)

# Scatter plot of app size vs. ratings
ARS<-ggplot(GPS, aes(x = Size, y = Rating)) +
    geom_point(color = "#4CAF50") +
    labs(x = "App Size", y = "App Rating", title = "App Ratings vs. App Size") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.major = element_line(color = "gray", linetype = "dashed"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
#ARS


#user reviews into a cloud
library(tidytext)
library(wordcloud)
library(RColorBrewer)
corpus <- UR %>%
    select(Translated_Review) %>%
    mutate(Translated_Review = tolower(Translated_Review)) %>%
    unnest_tokens(word, Translated_Review)

corpus <- corpus %>%
    anti_join(stop_words)

term_frequency <- corpus %>%
    count(word) %>%
    arrange(desc(n))

#wordcloud(term_frequency$word, term_frequency$n, scale = c(5, 0.5), max.words = 100, random.order = FALSE,color = brewer.pal(8, "Set2"))



# APP sentiment
library(tidytext)
library(stringr)

corpus <- UR %>%
    select(App, Translated_Review, Sentiment) %>%
    mutate(Translated_Review = tolower(Translated_Review))


sentiment <- get_sentiments("bing")

corpus_sentiment <- corpus %>%
    mutate(Sentiment_Score = ifelse(Sentiment == "positive", 1, -1))


average_sentiment <- corpus_sentiment %>%
    group_by(App, Sentiment) %>%
    summarize(Average_Sentiment = mean(Sentiment_Score))

filtered_sentiment <- average_sentiment %>%
    group_by(Sentiment) %>%
    sample_n(min(n(), 5))

SDA<-ggplot(filtered_sentiment, aes(x = App, y = Average_Sentiment, fill = Sentiment)) +
    geom_col(position = "stack", width = 0.6) +
    labs(title = "Sentiment Distribution of Certain Apps",
         x = "App",
         y = "Average Sentiment") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10))

#SDA

