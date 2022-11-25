## ---------------------------
##
## Script name: Exploring Potentials of Computational Text Analysis in the Study of Radicalism
##
## Purpose of script: Script for the analysis and its reproducibility
##
## Author: Nader Hotait
##
## Date Created: 20-11-2022 (dd/mm/yyyy)
##
## Copyright (c) Nader Hotait, 2022
## Email: nader.hotait@hu-berlin.de
##
## ---------------------------
##
## Notes: Script, Code, and Data are available at following repository:
##   
##
## ---------------------------
  
## Packages, Data, and Figure Path---------------------------
# Packages
pacman::p_load(tidyverse, readxl, ggrepel, lubridate, ggpubr, quanteda,
               quanteda.textplots, quanteda.textstats, quanteda.textmodels,
               grid, gridExtra, Cairo, ldatuning, stm, tidytext, seededlda,
               caret, glmnet, tibble, kableExtra, caretEnsemble, ranger)

# Create figure/table path within your working directory
dir.create(file.path(getwd(), "figs"), showWarnings = FALSE)
dir.create(file.path(getwd(), "tables"), showWarnings = FALSE)

# Data (assuming that the data is in your directory)
## Scopus
scopus <- bind_rows(
  read_excel("Scopus-125886-Analyze-Year.xlsx"),
  read_excel("Scopus-43368-Analyze-Year.xlsx"),
  read_excel("Scopus-85-Analyze-Year.xlsx"),
  read_excel("Scopus-20678-Analyze-Year.xlsx")
)

scopus$query <- factor(scopus$query, levels = c("CTA", "CTA in SOCI", "Radicalism", "CTA+Radicalism"))

## Generation Islam
genislam <- readRDS("genislam.RDS")

## ZMD
zmd <- readRDS("zmd")

## All Islam Actors
all_islam <- bind_rows(genislam,
                       zmd,
                       readRDS("alhm"),
                       readRDS("ditib"),
                       readRDS("igmggenclik"),
                       readRDS("islamratbrd"))


## Introduction: Scopus Query---------------------------

scopus_table <- scopus %>%
  filter(year==2001 | year==2021) %>%
  pivot_wider(names_from = year, values_from = n)

names(scopus_table) <- c("Query", "2021", "2001")
scopus_table <- scopus_table[c(1,4,2,3), c(1,3,2)]
scopus_table[4,2] <- 0

graph_table <- ggtexttable(scopus_table, rows = NULL, theme = ttheme("light"))

# Creating figure to see at files and plots column
# "Publications listed on Scopus between 2001-2021: (a) CTA - Publications relating to
# Computational Text Analysis. (b) CTA in SOCI - Publications relating to Computational Text Analysis
# in the subject area Social Sciences. (c) Radicalism - Publications relating to the study of radicalism. 
# (d) CTA+Radicalism - Publications relating to the study of radicalism and Computational Text Analysis."

scopus_grph <- scopus %>%
  filter(year>=2001) %>%
  ggplot(aes(x=year, y=n, fill=query)) +
  geom_point(aes(shape=query), size = 1.5) +
  geom_line(aes(linetype=query)) +
  theme_minimal() +
  xlab("Year") +
  ylab("Number of Publications") +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(title="Search Query:"),
         shape=guide_legend(title="Search Query:"),
         linetype=guide_legend(title="Search Query:"))

scopus_grph + annotation_custom(ggplotGrob(graph_table),
                                xmin = 2002.5, ymin = 8100,
                                xmax = 2007.5)
# Saving figure
ggsave("scopus_1.pdf", path = file.path(getwd(), "figs"), device="pdf", dpi=800)

# **Query Method:** For the query of the subject fields, 
# terms were searched for within the titles, abstracts, and keywords of
# publications listed on Scopus. For the query related to 
# Computational Text Analysis in social sciences (CTA in SOCI) and Radicalism, 
# the search was limited to the Scopus subject category "social sciences". 
# This is also due to the fact that terms related to the root word "radical" 
# often have a different meaning in the natural sciences. For the search query 
# related to CTA, related domains (e.g. Quantitative Text Analysis and 
# Natural Language Processing), as well as common methods also searched for.
# 
# Search Terms:
#   
# Computational Text Analysis (CTA): ((TITLE-ABS-KEY ("quantitative text" OR "computational text" 
# OR "computational text" OR "natural language processing" OR "quantitative content analysis" 
# OR "quantitative content analyses") OR TITLE-ABS-KEY("sentiment analysis" 
# OR "sentiment analyses" OR "named entity recognition" OR "topic model" 
# OR "topic modeling" OR "topic models") OR TITLE-ABS-KEY("text classification"
# OR "key word extraction" OR "lemmatization" OR "word embedding")))
#
# Computational Text Analysis in Social Sciences (CTA IN SOCI): 
# CTA AND ( LIMIT-TO ( SUBJAREA,"SOCI" ) ) )
# 
# Radicalism: ((TITLE-ABS-KEY (radicali OR terrorist OR terrorism OR "political violence" 
# OR extremi OR "far left" OR "far right" OR islamist OR islamism OR fundamentalis 
# OR jihad) ) AND ( LIMIT-TO ( SUBJAREA,"SOCI" ) ) )
# 
# CTA+Radicalism: Intersection of the CTA and Radicalism query

## Tweet Frequency of @genislam1---------------------------
# Setting up the data
Sys.setlocale("LC_TIME", "English") # Setting time form to English
ts <- genislam
ts$tweet <- 1L

ts <- ts %>%
  mutate(weekly = str_c(
    formatC(isoweek(created_at), format = "f", digits = 0, width = 2, flag = "0"), 
    "/", 
    str_sub(isoyear(created_at), 3, 4))) %>%
  group_by(screen_name) %>%
  arrange(created_at, .by_group = TRUE) %>%
  mutate("cum" = cumsum(tweet))

ts <- ts %>%
  group_by(screen_name, weekly) %>%
  mutate("week_cum" = cumsum(tweet))

cum_tweet <- ggplot(ts) +
  geom_line(aes(x=as.Date(created_at), y=cum)) +
  xlab("Date") +
  ylab("Tweets (cumulative)") +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month") +
  theme_minimal() +
  labs(title = "A") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Creating figure
# "Account Activity of Generation Islam (@genislam1) between August 2019 and April 2022: 
# (a) Cumulative tweets over time. (b) Weekly tweet frequency with generalized additive mode smoothing."

week_tweet <- ggplot(ts) +
  geom_point(aes(x=as.Date(created_at), y=week_cum), alpha = 0.1) +
  geom_smooth(aes(x=as.Date(created_at), y=week_cum), se = FALSE, color = "black", 
              method = "gam", formula = y ~ s(x)) +
  xlab("Date") +
  ylab("Tweets (weekly)") +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month") +
  theme_minimal() +
  labs(title = "B") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(cum_tweet, week_tweet, ncol = 1, nrow = 2, align = "v")

# Saving figure
ggsave("tweets_genislam.pdf", path = file.path(getwd(), "figs"), device="pdf", dpi=800)


## Preprocessing---------------------------
# Tokenization without preprocessing
dfm_pre <- corpus(genislam) %>%
  dfm() %>%
  dfm_group(groups = screen_name)

# With preprocessing
post_genislam <- genislam
post_genislam$text <- gsub("🇵🇸", "",
                           gsub("🏽","",
                                gsub("🏼","",
                                     gsub("✊","", 
                                          gsub("🏻", "", 
                                               gsub("🤦","", 
                                                    gsub("♂️","",
                                                         gsub("👨","",
                                                              gsub("👏", "", 
                                                                   gsub("🤲🏾", "", 
                                                                        gsub("🤲", "", 
                                                                             gsub("💔", "", 
                                                                                  gsub(" 👇", "", post_genislam$text)))))))))))))
post_genislam$text <- gsub("🤷","", 
                           gsub("🤷‍","", 
                                gsub("♀", "",
                                     gsub("🤷‍♀", "", 
                                          gsub("🇨🇳","", post_genislam$text)))))

dfm_post <- corpus(post_genislam) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T, 
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english", source = "marimo")) %>%
  tokens_remove(stopwords("de", source = "marimo")) %>%
  tokens_remove(stopwords("ar", source = "stopwords-iso")) %>%
  tokens_remove(c("à", "un", "la", "le", "en", "et", "été", "les", "avec")) %>%
  tokens_remove(pattern = "^[\\p{script=Arab}]+$", valuetype = "regex") %>%
  dfm() %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T) %>%
  dfm_wordstem()

# Setting up data for figure
pre_pp <- textstat_frequency(dfm_pre, n = 50)

pre_pp$feature <- with(pre_pp, reorder(feature, -frequency))

pp_1 <- ggplot(pre_pp, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Frequency") +
  xlab("Text elements (without preprocessing)") +
  labs(title = "A")

post_pp <- textstat_frequency(dfm_post, n = 50)

post_pp$feature <- with(post_pp, reorder(feature, -frequency))

# Creating figure
# "Text elements: (a) Before and (b) after preprocessing"
pp_2 <- ggplot(post_pp, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Frequency") +
  xlab("Text elements (with preprocessing)") +
  labs(title = "B")

ggarrange(pp_1, pp_2, ncol = 1, nrow = 2, align = "v")

# Saving Figure
ggsave("preprocessing.pdf", path = file.path(getwd(), "figs"), device="pdf", dpi=800)


## Exploring Content: Wordclouds---------------------------
# Creating function to automatize the analysis
create_wordclouds <- function(data, ngram=1, seed=123, color = "gray20", 
                              max_words=100, name = "1"){
  # Setting up the data and preprocessing
  df <- data
  df$text <- gsub("&amp;", "and", df$text, ignore.case = TRUE)
  df$text <- gsub("&", "and", df$text, ignore.case = TRUE)
  df$text <- gsub("Ben and Jerry", "Ben_and_Jerry", df$text, ignore.case = TRUE)
  df$text <- gsub("🇵🇸", "",
                             gsub("🏽","",
                                  gsub("🏼","",
                                       gsub("✊","", 
                                            gsub("🏻", "", 
                                                 gsub("🤦","", 
                                                      gsub("♂️","",
                                                           gsub("👨","",
                                                                gsub("👏", "", 
                                                                     gsub("🤲🏾", "", 
                                                                          gsub("🤲", "", 
                                                                               gsub("💔", "", 
                                                                                    gsub(" 👇", "", df$text)))))))))))))
  df$text <- gsub("🤷","", 
                  gsub("🤷‍","", 
                       gsub("♀", "",
                            gsub("🤷‍♀", "", 
                                 gsub("🇨🇳","", df$text)))))
  wordcloud <- corpus(df) %>%
    tokens(remove_punct=T,
           remove_numbers = T,
           remove_url = T, 
           split_hyphens = T,
           remove_symbols = T) %>%
    tokens_remove(stopwords("english")) %>%
    tokens_remove(stopwords("de", source = "marimo")) %>%
    tokens_remove(pattern = "^[\\p{script=Arab}]+$", valuetype = "regex") %>%
    tokens_ngrams(n=ngram) %>% # uni-, bi-, or trigram
    dfm() %>%
    dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de"))
  set.seed(seed)
  # Saving figure
  cairo_pdf(paste0(file.path(getwd(), "figs"), "/","wc-", name, ".pdf"), fallback_resolution=800)
  wordcloud %>%
    textplot_wordcloud(max_words = max_words, color = color)
  dev.off()
  set.seed(seed)
  # Display figure
  wordcloud %>%
    textplot_wordcloud(max_words = max_words, color = color)
}

# Creating and saving figures (Word cloud (Unigram))
create_wordclouds(genislam, seed = 100)

# Creating and saving figures (Word cloud (Bigram))
create_wordclouds(genislam, seed = 100, ngram = 2, name = "2")

# Creating and saving figures (Word cloud (Trigram))
create_wordclouds(genislam, seed = 100, ngram = 3, name = "3")


## Exploring Content: Feature-occurrence matrix---------------------------
# 50 most used hashtags by @genislam1
# Create data
tweet_dfm <- tokens(genislam$text, remove_punct = TRUE) %>%
  dfm() %>%
  dfm_select(pattern = "#*")

# display 50 Most used Hashtags
toptag <- names(topfeatures(tweet_dfm, 50))
toptag_table <- bind_cols("1-10" = toptag[1:10],
                          "11-20" = toptag[11:20],
                          "21-30" = toptag[21:30],
                          "31-40" = toptag[31:40],
                          "41-50" = toptag[41:50])

# Saving table
sink(file = paste0(file.path(getwd(), "tables"), "/", "top_50_hashtags.txt"))
toptag_table
sink(file = NULL)

# Printing table
toptag_table

# Feature co-occurrence of 50 most used hashtags by @genislam1
# Creating figure
tag_fcm <- fcm(tweet_dfm)

topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
set.seed(123)
textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5,
                 edge_color="grey60")

# Saving saving figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "feature_co_occur.pdf"), 
          fallback_resolution=800, height = 7, width = 10)
set.seed(123)
textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5,
                 edge_color="grey60")
dev.off()


## Exploring Content: Username co-occurrance----------------------------
# Creating figure
user_dfm <- tweet_dfm <- tokens(genislam$text, remove_punct = TRUE) %>%
  dfm() %>%
  dfm_select(pattern = "@*")

topuser <- names(topfeatures(user_dfm, 80))
user_fcm <- fcm(user_dfm)
user_fcm <- fcm_select(user_fcm, pattern = topuser)
set.seed(123)
textplot_network(user_fcm, min_freq = 0.1, edge_color = "grey60", 
                 edge_alpha = 0.5, edge_size = 5)

# Saving figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "user_co_occur.pdf"), 
          fallback_resolution=800, height = 7, width = 10)
set.seed(123)
textplot_network(user_fcm, min_freq = 0.1, edge_color = "grey60", 
                 edge_alpha = 0.5, edge_size = 5)
dev.off()


## Exploring Content: Topic Modelling (Ldatuning)---------------------------
# Ldatuning: Metrics to determine optimal number of topics

# Creating new Document-Feature-Matrix with no empty cells
dfm_post.new <- dfm_subset(dfm_post, ntoken(dfm_post) > 0)

# Apply ldatuning
result <- FindTopicsNumber(
  dfm_post.new,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed =1234),
  mc.cores = NA,
  verbose = FALSE)

# Modify function to plot figure
lda_plot <- function (values)
{
  if ("LDA_model" %in% names(values)) {
    values <- values[!names(values) %in% c("LDA_model")]
  }
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(values["topics"], base::apply(columns,
                                                           2, function(column) {
                                                             scales::rescale(column, to = c(0, 1), from = range(column))
                                                           }))
  values <- reshape2::melt(values, id.vars = "topics",
                           na.rm = TRUE)
  values$group <- values$variable %in% c("Griffiths2004",
                                         "Deveaud2014")
  values$group <- base::factor(values$group, levels = c(FALSE,
                                                        TRUE), labels = c("minimize", "maximize"))
  p <- ggplot(values, aes_string(x = "topics", y = "value",
                                 group = "variable"))
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"),
                      size = 3)
  p <- p + guides(size = FALSE, shape = guide_legend(title = "Metrics:"))
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "Number of topics", y = NULL)
  p <- p + facet_grid(group ~ .)
  p <- p + theme_bw() %+replace% theme(panel.grid.major.y = element_blank(),
                                       panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(colour = "grey60"),
                                       panel.grid.minor.x = element_blank(), legend.key = element_blank(),
                                       strip.text.y = element_text(angle = 90))
  p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g <- ggplotGrob(p)
  g$layout[g$layout$name == "strip-right", c("l",
                                             "r")] <- 3
  grid::grid.newpage()
  grid::grid.draw(g)
}

# Plot results
lda_plot(result)

# Saving figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "find_topics.pdf"),
          fallback_resolution=800, height = 7, width = 10)
lda_plot(result)
dev.off()


## Exploring Content: Topic Modelling (searchK)---------------------------
#"``Stm'': Metrics to determine an optimal number of topics"
dfm2stm <- convert(dfm_post, to = "stm")

# Searching for optimal topic number between 6 and 18 in steps of 2
set.seed(1234)
searchk <- searchK(dfm2stm$documents, dfm2stm$vocab, K = seq(6,18,2))

# Creating figure
plot(searchk)

#Saving figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "searchk.pdf"), 
          fallback_resolution=800, height =5, width = 7)
plot(searchk, main = "")
dev.off()


## Exploring Content: Performing topic modelling on 10 topics
# STM model
topic.count <- 10
set.seed(1234)
model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, 
                 K = topic.count, data = dfm2stm$meta, 
                 init.type = "Spectral", verbose = FALSE)

# Top 20  words per topics
# Printing table
labelTopics(model.stm, n=20)

# Saving table
sink(file = paste0(file.path(getwd(), "tables"), "/", "top20_words_topics.txt"))
labelTopics(model.stm, n=20)
sink(file = NULL)


# Plotting expected topic proportions
par(bty="n",col="grey40",lwd=5)
plot(model.stm, type = "summary", text.cex = 1, main = "")

# Save figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "topic-model.pdf"), 
          fallback_resolution=800, height =5, width = 7)
par(bty="n",col="grey40",lwd=5)
plot(model.stm, type = "summary", text.cex = 1, main = "")
dev.off()


# Topics, expected topic proportion, and highest probability words
# Creating table data
proportions_table <- make.dt(model.stm)  
topic_table <- summarize_all(proportions_table, mean)
topic_long <- topic_table[,c(2:11)] %>%
  pivot_longer(everything(), names_to = "Topic #", values_to = "Topic Proportions")

topics_label <- labelTopics(model.stm, n=10)
prob_word <- topics_label[["prob"]]

topic_long$`Top Words` <- ""
for (i in 1:10) {
  topic_long[i,3] <- paste(prob_word[i,], collapse = ", ")
}

# Changing names to be intuitive
topic_long$`Topic #` <- gsub("Topic", "", topic_long$`Topic #`)
topic_long$`Topic Proportions` <- round(topic_long$`Topic Proportions`*100)
topic_long$`Topic Proportions` <- paste0(topic_long$`Topic Proportions`, "%")

# Printing out table
topic_long

# Saving table
sink(file = paste0(file.path(getwd(), "tables"), "/", "topic_proportions.txt"))
topic_long
sink(file = NULL)

## Exploring Content: Semi-Supervised Latent Dirichlet Allocation---------------------------
# Creating Dictionary with seed words
dict <- dictionary(list(middle_east = c("israel*", "palest*", "paläs*", 
                                        "jerusalem", "aqsa", "west bank", 
                                        "west-bank", "gaza", "aparth*", "antisemit*"),
                        other_grievances = c("china", "chines*", "uigur*", "uyghur*",
                                             "chines*", "camp*", "hindu*", "myanm*",
                                             "rohing*", "bosni*", "srebrenica", 
                                             "hindutv*", "bomb*", "attack", "strike"),
                        defence = c("verteid*", "resist*", "defen*", 
                                    "widerstand*", "battl*", "wehren*", 
                                    "abwehr*", "oppos*"),
                        hijab = c("kopftuch*", "scarf*", "niqab*", "jilbab*",
                                  "hijab*", "shador", "schador"),
                        racism_islamophobia = c("rassis*", "race*", "racis*", 
                                                "discrim*", "diskrim*", "nazi*", "*phob*"),
                        islam_general = c("dua", "quran", "koran", "hadith", "narration", "prophet*", "gesandter", 
                                          "rasul*", "sahaba*", "salaf", "kalif*", "mubarak", "eid", "deen")))

# Create table data to print dictionary
table_dict <- as.data.frame(matrix(ncol = 6, nrow = 14))
names(table_dict) <- c("middle_east", "other_grievances", "defence", "hijab", 
                       "racism_islamophobia", "islam_general")

table_dict$middle_east <- c(dict[["middle_east"]], rep("", 14-length(dict[["middle_east"]])))
table_dict$other_grievances <- c(dict[["other_grievances"]], rep("", 14-length(dict[["other_grievances"]])))
table_dict$defence <- c(dict[["defence"]], rep("", 14-length(dict[["defence"]])))
table_dict$hijab <- c(dict[["hijab"]], rep("", 14-length(dict[["hijab"]])))
table_dict$racism_islamophobia <- c(dict[["racism_islamophobia"]], rep("", 14-length(dict[["racism_islamophobia"]])))
table_dict$islam_general <- c(dict[["islam_general"]], rep("", 14-length(dict[["islam_general"]])))

# Printing dictionary
table_dict

# Saving dictionary
sink(file = paste0(file.path(getwd(), "tables"), "/", "dictionary.txt"))
table_dict
sink(file = NULL)

# 30 most used terms per topic (seed)
# Create Seeded LDA
set.seed(1234)
tmod_slda <- textmodel_seededlda(dfm_post, dictionary = dict)

# Print terms per seed
terms(tmod_slda, 30)

# Saving terms per seed
sink(file = paste0(file.path(getwd(), "tables"), "/", "terms_slda.txt"))
terms(tmod_slda, 30)
sink(file = NULL)

# Number of documents that most likely correspond respective topic (seed)

dfm_seed <- dfm_post
dfm_seed$topic2 <- topics(tmod_slda)

# Printing
table(dfm_seed$topic2)

# Saving
sink(file = paste0(file.path(getwd(), "tables"), "/", "sdla_topic_prev.txt"))
table(dfm_seed$topic2)
sink(file = NULL)


## Classification: Keyness
# Analysis of relative frequency (keyness): 
# Generation Islam (@genislam1) and Central Council of Muslims in Germany (@der_zmd)"

# Preparing data
key_df <- bind_rows(genislam, zmd)
key_df$screen_name <- factor(key_df$screen_name, levels = c("der_zmd", "genislam1"))

key_corp <- corpus(key_df)
key_dfm <- tokens(key_corp, remove_punct=T,
                  remove_numbers = T,
                  remove_url = T,
                  split_hyphens = T,
                  remove_symbols = T) %>%
  tokens_remove(stopwords("english", source = "marimo")) %>%
  tokens_remove(stopwords("de", source = "marimo")) %>%
  tokens_remove(stopwords("ar", source = "stopwords-iso")) %>%
  dfm() %>%
  dfm_group(groups = screen_name) %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T)

# Save reults
result_keyness <- textstat_keyness(key_dfm, target = "genislam1")

# Creating figure
textplot_keyness(result_keyness, color = c("black", "gray"))

# Saving figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "keyness.pdf"), 
          fallback_resolution=800, height =7, width = 9)
textplot_keyness(result_keyness, color = c("black", "gray"))
dev.off()


## Classification: Naive Bayes---------------------------

# Preparing data
key_df_small<- key_df
key_df_small<- key_df_small%>%
  select(text, screen_name)

data_corpus <- corpus(key_df_small, text_field = "text")

# Randomly select half of tweets for test and training data sets
set.seed(1234)
training_id <- sample(1:4666, 2333, replace = FALSE)

# Create docvar with ID
docvars(data_corpus, "id_numeric") <- 1:ndoc(data_corpus)

# Training data set
dfmat_training <- corpus_subset(data_corpus, id_numeric %in% training_id) %>%
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T,
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english", source = "marimo")) %>%
  tokens_remove(stopwords("de", source = "marimo")) %>%
  tokens_remove(stopwords("ar", source = "stopwords-iso")) %>%
  dfm() %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T)

# Test data set
dfmat_test <-
  corpus_subset(data_corpus,!id_numeric %in% training_id) %>% # not (!) test data
  tokens(remove_punct=T,
         remove_numbers = T,
         remove_url = T,
         split_hyphens = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english", source = "marimo")) %>%
  tokens_remove(stopwords("de", source = "marimo")) %>%
  tokens_remove(stopwords("ar", source = "stopwords-iso")) %>%
  dfm() %>%
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*", ".de")) %>%
  dfm_trim(max_termfreq = .99,termfreq_type = "quantile",verbose = T) %>%
  dfm_trim(min_termfreq = .7,termfreq_type = "quantile",verbose = T)


# Training naive bayes
set.seed(1234)
model_nb <- textmodel_nb(dfmat_training, docvars(dfmat_training, "screen_name"), prior = "docfreq")

# Applying on test_data
dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

# Create data for confusion matrix
actual_class <- docvars(dfmat_matched, "screen_name")
predicted_class <- predict(model_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)

confusion_nb <- confusionMatrix(tab_class, mode = "everything", positive = "genislam1")

# Printing confusion matrix
confusion_nb

# Saving confusion matrix
sink(file = paste0(file.path(getwd(), "tables"), "/", "confusion_nb.txt"))
confusion_nb
sink(file = NULL)

# Displaying predicted and actually observed accounts from naive bayes model 
confusion_df <- as.data.frame(confusion_nb[["table"]])

# Creating figure
level_order_y <-
  factor(confusion_df$actual_class,
         level = c("genislam1", "der_zmd"))

ggplot(confusion_df,
       aes(x = predicted_class, y = level_order_y, fill = Freq, label = Freq)) +
  xlab("Predicted") +
  ylab("Observed") +
  geom_tile() + theme_minimal() + coord_equal() +
  geom_label(fill = "white") +
  scale_fill_distiller(palette = "Greys", direction = 1, name = "Tweets")

# Saving figure
ggsave("naive_bayes.pdf", path = file.path(getwd(), "figs"), device="pdf", dpi=800)


# Naive Bayes: Text elements with the most predicting power for Generation Islam----------------------------

# Extract coefficients
model_parm <- rownames_to_column(as.data.frame(model_nb[["param"]]), "screen_name")

# Convert to long
model_parm_long <- model_parm %>%
  pivot_longer(-screen_name)

model_parm_long$value <- format(model_parm_long$value , scientific = FALSE)

# Words with 10 highest probabilty
top_ten_parameters <- model_parm_long %>%
  filter(screen_name == "genislam1") %>%
  slice_max(value, n=10)

top_ten_parameters$value <- as.numeric(top_ten_parameters$value)

# Creating figure
ggplot(top_ten_parameters, aes(y = value, x = reorder(name,value))) +
  geom_col() +
  coord_flip() +
  xlab("Text elements") +
  ylab("Coefficient") +
  theme_minimal()

# Saving figure
ggsave("nb_coef.pdf", path = file.path(getwd(), "figs"), device="pdf", dpi=800)


## Classification: Glmnet and Random Forest---------------------------

# Descriptive Data: "Number of Tweets corresponding with grievances"
# Create character vector inspired by Seeded-LDA
grievance <- c("israel", "palest", "paläs", "jerusalem", "aqsa", "west bank",
               "west-bank", "gaza", "aparth", "antisemit","uigur", "uyghur",
               "hindut", "myanm", "rohing", "srebrenica", "bomb", "attack", 
               "strike", "verteid", "resist", "defen", "widerstand", "battl",
               "wehren", "abwehr", "oppos", "kopftuch", "scarf", "niqab",
               "jilbab","hijab", "shador", "schador", "rassis", "race", "racis",
               "discrim", "diskrim", "nazi", "phob")

# How often do one of these words/stems occur in the data?
# Finding them
all_islam$grievance <- grepl(paste(grievance,collapse="|"), all_islam$text, ignore.case = TRUE)

# Dividing the accounts in Generation Islam and Other
all_islam <- all_islam %>%
  mutate(account = case_when(screen_name == "genislam1" ~ "genislam",
                             TRUE ~ "other"))

# Creating and printing tables with distribution
# Relative and absolute frequency of tweets corresponding with grievances
all_islam %>%
  group_by(account, grievance) %>%
  summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n)*100), "%"))

# Saving table
sink(file = paste0(file.path(getwd(), "tables"), "/", "grievance_table.txt"))
all_islam %>%
  group_by(account, grievance) %>%
  summarise(n = n()) %>%
  mutate(freq = paste0(round(n / sum(n)*100), "%"))
sink(file = NULL)


## Elastic-Net Regularized Generalized Linear Models (glmnet)---------------------------
# Fitting a glmnet model with more variables
account_y <- factor(all_islam$account, levels = c("other", "genislam"))
account_x <- select(all_islam, grievance,display_text_width, 
                    retweet_count, favorite_count)

# Create custom indices: myFolds (for a 10-fold CV)
set.seed(1234)
myFolds <- createFolds(account_y, k = 10)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

train <- data.frame(account_x, account_y) 

# Training the acutal glmnet model
set.seed(1234)
model_glmnet <- train(account_y ~ ., data = train,
                      metric = "ROC",
                      method = "glmnet",
                      trControl = myControl,
                      tuneGrid = expand.grid(
                        alpha=seq(0,1,0.25), # 5 values of alpha
                        lambda=seq(0.0001, 1, length=20) # 20 values of lambda
                        )
                      )

# Displaying and saving the best AUC (mean)
max(model_glmnet$results[["ROC"]])

sink(file = paste0(file.path(getwd(), "tables"), "/", "glmnet_max_auc.txt"))
max(model_glmnet$results[["ROC"]])
sink(file = NULL)


# Displaying coefficients
coef(model_glmnet$finalModel, s = -1.008148)

# Saving coefficients
sink(file = paste0(file.path(getwd(), "tables"), "/", "glmnet_coef.txt"))
coef(model_glmnet$finalModel, s = -1.008148)
sink(file = NULL)

## Model Comparison: Glmnet and Random Forest-------------------------
# Optimizing predictions with random forest
set.seed(1234)
model_rf <- train(account_y ~ ., data = train,
                  metric = "ROC",
                  method = "ranger",
                  trControl = myControl)

# Create model_list
model_list <- list(glmnet = model_glmnet, rf = model_rf)

# Pass model_list to resamples()
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Saving the results
sink(file = paste0(file.path(getwd(), "tables"), "/", "rf_glmnet_auc.txt"))
summary(resamples)
sink(file = NULL)

# RF performs best, Creating model comparison
bwtheme <- standard.theme("pdf", color=FALSE)
bwtheme <- bwtheme
bwtheme$par.main.text <- list(font = 2,
                              just = "left", 
                              x = grid::unit(5, "mm"))
glm_plot1 <- bwplot(resamples, metric="ROC", par.settings=bwtheme, main="A")
glm_plot1[["xlab"]] <- "AUC"
glm_plot2 <- dotplot(resamples, metric = "ROC", par.settings=bwtheme, main="B")
glm_plot2[["xlab"]] <- "AUC"
glm_plot3 <- xyplot(resamples, metric="ROC", par.settings=bwtheme)
glm_plot3[["xlab"]] <- "rf\nAUC"
glm_plot3[["main"]] <- "C"
glm_plot4 <- densityplot(resamples, metric="ROC", ylab="Density", par.settings=bwtheme, main="D")
glm_plot4[["xlab"]] <- "AUC"

# Creating figure as arrangements of model comparisons
grid.arrange(glm_plot1,glm_plot2, glm_plot3, glm_plot4, ncol=2, bottom ="All values display AUC (ROC)")

# Saving figure
cairo_pdf(paste0(file.path(getwd(), "figs"), "/", "model_stack.pdf"), 
          fallback_resolution=800, height =7, width = 9)
grid.arrange(glm_plot1,glm_plot2, glm_plot3, glm_plot4, ncol=2, bottom ="All values display AUC (ROC)")
dev.off()


# Stacking Glmnet and Random Forest to improve predictions-------------------------
set.seed(1234)
training_id_new <- sample(1:11425, 5712, replace = FALSE)
train_new <- train[training_id_new,]
test_new <- train[-training_id_new,]

myFolds <- createFolds(train_new$account_y, k = 10)

myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

models <- caretList(account_y ~ ., data=train_new, trControl = myControl, methodList=c("glmnet", "ranger"))

# Create ensemble model: stack
stack <- caretStack(models, method="glm")

# Printing summary
summary(stack)

# Saving summary
sink(file = paste0(file.path(getwd(), "tables"), "/", "summary_rf_glmnet.txt"))
summary(stack)
sink(file = NULL)


# Displaying predicted and actual classes from stacking Glment and Random Forest
test_new$pred <- predict(stack, newdata=test_new, level = 0.95)
confusionMatrix(data = test_new$pred, reference = test_new$account_y, positive = "genislam")
confusion_new <- confusionMatrix(data = test_new$pred, reference = test_new$account_y, positive = "genislam")

confusion.data.new <- as.data.frame(confusion_new[["table"]])

# Creating figure
level_order_y <-
  factor(confusion.data.new$Reference,
         level = c("genislam", "other"))

ggplot(confusion.data.new,
       aes(x = Prediction, y = level_order_y, fill = Freq, label = Freq)) +
  xlab("Predicted") +
  ylab("Observed") +
  geom_tile() + theme_minimal() + coord_equal() +
  geom_label(fill = "white") +
  scale_fill_distiller(palette = "Greys", direction = 1, name = "Tweets")

# Saving saving figure
ggsave("caret_stack.pdf", path = file.path(getwd(), "figs"), device="pdf", dpi=800)

