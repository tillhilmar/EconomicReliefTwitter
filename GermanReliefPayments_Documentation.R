## This is the R code documentation for the manuscript 
## "Who deserves economic relief? Examining Twitter/X debates about Covid-19 economic relief for small businesses and the self-employed in Germany"
## Authored by Till Hilmar, University of Vienna, till.hilmar@univie.ac.at
## Forthcoming in the Journal of Social Policy (2024)

library("httr")
library("acedmictwitteR")
library("jsonlite")
library("magrittr")
library("purrr")
library("tibble")
library("tidyverse")
library("plyr")
library("dplyr")
library("data.table")
library("quanteda")
library("quanteda.textstats")
library("quanteda.textplots")
library("ggthemes")
library("tidytext")
library("scales")
library("lubridate")
library("ggplot2")
library("ggpubr")
library("kableExtra")
library("readr")
library("readxl")
library("writexl")
library("spacyr")

# Twitter Data for this project was fetched on Dec 02 and Dec 03, 2021
# examples for how the Twitter Data was downloaded using the Academic Research API:
bearer_token <- "x"
Coronahilfen <- get_all_tweets("Coronahilfen", "2020-01-01T00:00:00Z", "2021-08-31T23:59:59Z", n = 500000, bearer_token)
Coronahilfe <- get_all_tweets("Coronahilfe", "2020-01-01T00:00:00Z", "2021-08-31T23:59:59Z", n = 500000, bearer_token)
query1 <- build_query('Corona-Hilfen')
Corona_Hilfen <- get_all_tweets(query1, "2020-01-01T00:00:00Z", "2021-08-31T23:59:59Z", n = 500000, bearer_token)
query2 <- build_query('Corona-Hilfe')
Corona_Hilfe <- get_all_tweets(query2, "2020-01-01T00:00:00Z", "2021-08-31T23:59:59Z", n = 500000, bearer_token)

# loading the data
ALL_Coronahilfen <- readRDS("TwitterData/ALL_Coronahilfen.rds")
ALL_Dezemberhilfen <- readRDS("TwitterData/ALL_Dezemberhilfen.rds")
ALL_Novemberhilfen <- readRDS("TwitterData/ALL_Novemberhilfen.rds")
ALL_Soforthilfen <- readRDS("TwitterData/ALL_Soforthilfen.rds")
ALL_Überbrückungshilfen <- readRDS("TwitterData/ALL_Überbrückungshilfen.rds")
ALL_Neustarthilfen <- readRDS("TwitterData/ALL_Neustarthilfen.rds")

# pre-cleaning Soforthilfen data: removing "sofort Hilfe"
Soforthilfe_grepl <- c("sofort\\s+Hilfe", "sofort\\s+hilfe")
ALL_Soforthilfen_corp <- corpus(ALL_Soforthilfen)
ALL_Soforthilfen_out <- corpus_subset(ALL_Soforthilfen_corp, grepl(paste(Soforthilfen_grepl,collapse="|"), texts(ALL_Soforthilfen_corp)))
ALL_Soforthilfen_out_df <- convert(ALL_Soforthilfen_out, to = c("data.frame"), pretty = TRUE) 
ALL_Soforthilfen_out_list <- ALL_Soforthilfen_out_df$unique_tweet_id
ALL_Soforthilfen_clean <- subset (ALL_Soforthilfen, !(unique_tweet_id %in% ALL_Soforthilfen_out_list))
# 1469 tweets were removed

# adding the respective keyword to each of the dataframes
ALL_Coronahilfen$keyword <- "Coronahilfen"
ALL_Soforthilfen_clean$keyword <- "Soforthilfen"
ALL_Überbrückungshilfen$keyword <- "Überbrückungshilfen"
ALL_Novemberhilfen$keyword <- "Novemberhilfen"
ALL_Dezemberhilfen$keyword <- "Dezemberhilfen"
ALL_Neustarthilfen$keyword <- "Neustarthilfen"

# formatting the date
ALL_Coronahilfen$created_at <- as_datetime(ALL_Coronahilfen$created_at)
ALL_Soforthilfen_clean$created_at <- as_datetime(ALL_Soforthilfen_clean$created_at)
ALL_Überbrückungshilfen$created_at <- as_datetime(ALL_Überbrückungshilfen$created_at)
ALL_Novemberhilfen$created_at <- as_datetime(ALL_Novemberhilfen$created_at)
ALL_Dezemberhilfen$created_at <- as_datetime(ALL_Dezemberhilfen$created_at)
ALL_Neustarthilfen$created_at <- as_datetime(ALL_Neustarthilfen$created_at)

# create a master dataframe.
df_ALL_duplicates <- rbind(ALL_Coronahilfen, ALL_Soforthilfen_clean, ALL_Überbrückungshilfen, ALL_Novemberhilfen, ALL_Dezemberhilfen, ALL_Neustarthilfen)
#saveRDS(df_ALL_duplicates, file = "TwitterData/df_ALL_duplicates.rds")
#df_ALL_duplicates <- readRDS("TwitterData/df_ALL_duplicates.rds")
df_ALL <- df_ALL_duplicates[!duplicated(df_ALL_duplicates[ , c("unique_tweet_id")]), ]

# beginng data preprocessing

# remove tweets outside of timeframe between March 01, 2020 and June 30, 2021
df_ALL_duplicates <- subset(df_ALL_duplicates, created_at >= '2020-03-01 00:00:00' & created_at <= '2021-06-30 23:59:59')

# define stopwords to remove
ger_stopwords <- read_lines("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")
#length(ger_stopwords) # check how long this list is
#c("dass", "beim") %in% ger_stopwords
custom_stopwords <- setdiff(ger_stopwords, stopwords("german"))
custom_stopwords <- c(custom_stopwords, "amp", "rt", "+", "via", "de", "<U+304C>", "<U+0001F926><U+200D><U+2642>", "<U+0001F926><U+200D><U+2640>", "<U+0001F1E9><U+0001F1EA>", "<U+306E>", "aldigi") #"Euro", "Milliarden", "Millionen", "Geld")

# remove Austrian and Swiss user accounts
location_out <- c("Österreich", "Austria", "Oesterreich", "Wien", "Vienna", "Salzburg", "Graz", "Innsbruck", "Linz", "Klagenfurt", "St.Pölten", "St. Poelten", "Bregenz", "Eisenstadt","Lienz","Schweiz", "Switzerland", "Suisse", "Zürich", "Zurich", "Zuerich", "Bern", "Luzern", "Genf", "Lausanne", "Basel", "St.Gallen")

df_ALL_duplicates_corp <- quanteda::corpus(df_ALL_duplicates)
df_ALL_duplicates_corp_out <- corpus_subset(df_ALL_duplicates_corp, grepl(paste(location_out,collapse="|"), texts(df_ALL_duplicates_corp$location)))
df_ALL_duplicates_out <- convert(df_ALL_duplicates_corp_out, to = c("data.frame"), pretty = TRUE) 
df_ALL_duplicates_out_list <- df_ALL_duplicates_out$unique_tweet_id
df_ALL_duplicates_cleanedlocation <- subset (df_ALL_duplicates, !(unique_tweet_id %in% df_ALL_duplicates_out_list))

# remove languages that are not German, English, or undefined
df_ALL_duplicates_cleanedlanguage <- subset(df_ALL_duplicates_cleanedlocation, lang %in% c("de", "en", "und"))

# create main dataframe

ALL_duplicates <- df_ALL_duplicates_cleanedlanguage

# remove additional foreign use accounts identified at a later stage (in the manual coding of user accounts)
additional_foreignaccounts <- c("973936198367444994", "55313370", "2510548399", "896104281169956864", "14368274", "752847116565897216","28523101", "1244300971464953858", "2152574316", "588743019", "896104281169956864", "2510548399", "343947963", "547173846", "14368274", "303319566")
ALL_duplicates <- subset(ALL_duplicates, !(author_id %in% additional_foreignaccounts))

#saveRDS(ALL_duplicates, file = "TwitterData/ALL_duplicates.rds")
#ALL_duplicates <- readRDS("TwitterData/ALL_duplicates.rds")

ALL_duplicates_noRT <- subset(ALL_duplicates, !(in_reference == 'retweeted'))

ALL <- ALL_duplicates[!duplicated(ALL_duplicates[ , c("unique_tweet_id")]), ]
ALL_noRT <- subset(ALL, !(in_reference == 'retweeted'))
#saveRDS(ALL_noRT, file = "TwitterData/ALL_noRT.rds")
#ALL_noRT <- readRDS("TwitterData/ALL_noRT.rds")

sapply(ALL, function(x) length(unique(x)))
sapply(ALL_noRT, function(x) length(unique(x)))

# BEGIN ANALYSIS ----

# Plot distribution over time----
# This includes duplicates because I want to represent the distribution of keywords over time, not tweets.
# It excludes retweets because this would skew the distribution

ALL_duplicates_noRT$created_at <- as_date(ALL_duplicates_noRT$created_at)

# modification: generating a new version that I can use for an English labelling of the aids
ALL_duplicates_noRT$created_at <- as_date(ALL_duplicates_noRT$created_at)

ALL_duplicates_noRT_modified <- ALL_duplicates_noRT

ALL_duplicates_noRT_modified$keyword[ALL_duplicates_noRT_modified$keyword == "Coronahilfen"] <- "Corona Assistance"
ALL_duplicates_noRT_modified$keyword[ALL_duplicates_noRT_modified$keyword == "Soforthilfen"] <- "Emergency Assistance"
ALL_duplicates_noRT_modified$keyword[ALL_duplicates_noRT_modified$keyword == "Novemberhilfen"] <- "November Assistance"
ALL_duplicates_noRT_modified$keyword[ALL_duplicates_noRT_modified$keyword == "Dezemberhilfen"] <- "December Assistance"
ALL_duplicates_noRT_modified$keyword[ALL_duplicates_noRT_modified$keyword == "Überbrückungshilfen"] <- "Bridging Assistance"
ALL_duplicates_noRT_modified$keyword[ALL_duplicates_noRT_modified$keyword == "Neustarthilfen"] <- "New Start Assistance"

keyword_counts <- ALL_duplicates_noRT_modified %>%
  group_by(created_at, keyword) %>%
  tally()

wsjPal <- c('#1C366B','#C4CFD0','#1DACE8','#F24D29','#76A08A','#9A872D')

ordered_keywords <- c("Corona Assistance", "December Assistance", "Emergency Assistance", 
                      "Bridging Assistance", "November Assistance", "New Start Assistance")

keyword_counts$keyword <- factor(keyword_counts$keyword, levels = ordered_keywords)

Sys.setlocale("LC_TIME", "C")

# Plot with ordered small multiples
gb <- keyword_counts %>%
  ggplot(aes(x = created_at, y = n, group = keyword, color = keyword)) +
  geom_line(size = 0.5) +
  labs(x = "Date", y = "Count", color = "Keyword", fill = "Keyword", 
       title = "Number of times German Covid-19 direct subsidies are mentioned in tweets",
       subtitle = "Mar 1, 2020 - Jun 30, 2021 n = 142.585 (excl. retweets)") +
  scale_color_manual(values = wsjPal, breaks = ordered_keywords) +  # Ensure legend order matches
  scale_x_date(breaks = "2 month", date_labels = "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "none", # removed the legend in this plot
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~keyword, scales = "free_y") 

# Display the plot
print(gb)

ggsave("distribution_new.png", gb, width = 10, height = 6, dpi = 900)

# calculating means

mean(ALL_noRT$retweet_count, na.rm= T) 
median(ALL_noRT$retweet_count, na.rm= T) 
mean(ALL_noRT$like_count, na.rm=T)
median(ALL_noRT$like_count, na.rm=T)

summary(ALL_noRT$like_count)

# Identifying authors ----
x <- ALL[c("author_id", "username", "name", "followers_count", "following_count", "tweet_count", "user_description")]
y <- count(ALL, 'author_id')
ALL_all_authors <- merge(x,y, by = "author_id")
ALL_unique_authors <- ALL_all_authors[!duplicated(ALL_all_authors[ , c("author_id")]), ]
ALL_unique_authors_ord <- arrange(ALL_unique_authors, desc(ALL_unique_authors$freq))

ALL_unique_authors_ord_table <- ALL_unique_authors_ord[c("username", "name", "freq", "user_description")]
ALL_unique_authors_ord_table[1:500, 1:4] %>%
  kbl(caption = "Most Frequently Tweeting Authors") %>%
  kable_classic(full_width = F, html_font = "Cambria")  %>%
  save_kable(file = "Most_Freq_Tweeting_Authors.html", self_contained = T)

# most prolific tweeters excluding RT
x <- ALL_noRT[c("author_id", "username", "name", "followers_count", "following_count", "tweet_count", "user_description")]
y <- count(ALL_noRT, 'author_id')
ALL_noRT_all_authors <- merge(x,y, by = "author_id")
ALL_noRT_unique_authors <- ALL_noRT_all_authors[!duplicated(ALL_noRT_all_authors[ , c("author_id")]), ]
ALL_noRT_unique_authors_ord <- arrange(ALL_noRT_unique_authors, desc(ALL_noRT_unique_authors$freq))
ALL_noRT_unique_authors_ord_table <- ALL_noRT_unique_authors_ord[c("username", "name", "freq", "user_description", "author_id")]
ALL_noRT_unique_authors_ord_table <- subset(ALL_noRT_unique_authors_ord_table, freq >= 20)
ALL_noRT_unique_authors_ord_table[1:868, 1:5] %>%
  kbl(caption = "Most Frequently Tweeting Authors") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "Most_Freq_Tweeting_Authors_NoRT.html", self_contained = T)

sum(ALL_noRT_unique_authors_ord_table$freq)

#another way to create a frequency score for single authors in the data
author_counts <- ALL %>%
  group_by(author_id) %>%
  tally

# Creating a score for: author activity * freq mentioned
df_mentions <- ALL %>% select(mentions)
df_mentions$mentions = as.character(df_mentions$mentions)

df_mentions_1 <- df_mentions %>% 
  rownames_to_column() %>% 
  mutate(string = strsplit(mentions, ",")) %>% 
  unnest %>%
  group_by(string) %>%
  dplyr::summarise(count = n(),
                   rows = paste(rowname, collapse=","))
df_mentions_1$string <- str_replace_all(string = df_mentions_1$string, pattern = "@", replacement = "")
df_mentions_1$string <- str_trim(df_mentions_1$string, side="both")
df_mentions_2 <- df_mentions_1[!df_mentions_1$string == 'NA',]
df_mentions_2$rows <- NULL
names(df_mentions_2)[names(df_mentions_2) == "string"] <- "username"
df_mentions_3 <- ddply(df_mentions_2,"username",numcolwise(sum))
names(df_mentions_3)[names(df_mentions_2) == "count"] <- "freq_mentioned"

ALL_authors <- merge(ALL_unique_authors, df_mentions_3, by = "username")
ALL_authors$score <- ALL_authors$freq*ALL_authors$freq_mentioned
score1 <- test$frequency*test$freq
# results: Who are the most active authors who also get mentioned the most?

ALL_authors_plot <- ALL_authors %>% select(username, score)
ALL_authors_plot$ID <- seq.int(nrow(ALL_authors_plot))
ALL_authors_plot_SUB <- ALL_authors_plot[ which(ALL_authors_plot$score >= 1000 | ALL_authors_plot), ]

ALL_authors_table <- ALL_authors %>% select(username, name, freq, freq_mentioned, score)
ALL_authors_table <- arrange(ALL_authors_table, desc(ALL_authors_table$score))
names(ALL_authors_table)[names(ALL_authors_table) == "freq"] <- "Tweeted"
names(ALL_authors_table)[names(ALL_authors_table) == "freq_mentioned"] <- "Mentioned"
names(ALL_authors_table)[names(ALL_authors_table) == "username"] <- "Username"
names(ALL_authors_table)[names(ALL_authors_table) == "name"] <- "Name"
names(ALL_authors_table)[names(ALL_authors_table) == "score"] <- "Tweeted*Mentioned"
ALL_authors_table[1:15, 1:5] %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "ALL_authors_table.html", self_contained = T)

# Identifying the language around "small business" and "self employed" actors ----
# I do so using two text statistical tools: Part of Speech Tagging (including nounphrases), and Quanteda's own textstat_freq and textstate_colloq

# using POS on the whole corpus #-----
corp_ALL <- quanteda::corpus(ALL_noRT)

spacy_initialize(model = "de_core_news_sm")
#spacy_finalize()

pos_ALL <- spacy_parse(corp_ALL, lemma = F, entity = T, dependency = T, nounphrase = TRUE)
#saveRDS(pos_ALL, file = "pos_ALL.rds")

subjekte_ALL_full <- pos_ALL %>% 
  filter(pos == "NOUN") %>% 
  mutate(Wort = str_to_lower(token)) %>% 
  group_by(Wort) %>% 
  dplyr::summarise(Frequenz = n()) %>% 
  dplyr::arrange(desc(Frequenz)) %>% 
  dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 500)
subjekte_ALL_full[1:500,1:3] %>% kbl(caption = "POS Subjekte First 500") %>% kable_classic(full_width = F, html_font = "Cambria") %>% save_kable(file = "POS_Subjekte.html", self_contained = T)

pos_ALL_extract <- nounphrase_extract(pos_ALL)
nounphrases_ALL_full <- as.data.frame(table(grep("_", pos_ALL_extract$nounphrase, value = TRUE)) %>%
                                        sort(decreasing = TRUE))
nounphrases_ALL_full[1:500,1:2] %>% kbl(caption = "POS Nounphrases First 500") %>% kable_classic(full_width = F, html_font = "Cambria") %>% save_kable(file = "POS_Nounphrases.html", self_contained = T)

tstat_freq <- textstat_frequency(dfm_ALL, n = 500)
tstat_freq[1:500,1:5] %>% kbl(caption = "Tstat_Freq First 500") %>% kable_classic(full_width = F, html_font = "Cambria") %>% save_kable(file = "Tstat_Freq_First500.html", self_contained = T)

tstat_coll <- textstat_collocations(tokens_ALL, size = 2, tolower = FALSE)
tstat_coll <- tstat_coll %>% arrange(desc(count))
tstat_coll[1:500,1:6] %>% kbl(caption = "Tstat_Coll First 500") %>% kable_classic(full_width = F, html_font = "Cambria") %>% save_kable(file = "Tstat_Coll_First500.html", self_contained = T)

tstat_coll_3 <- textstat_collocations(tokens_ALL, size = 3, tolower = FALSE)
tstat_coll_3 <- tstat_coll_3 %>% arrange(desc(count))
tstat_coll_3[1:500,1:6] %>% kbl(caption = "Tstat_Coll_3 First 500") %>% kable_classic(full_width = F, html_font = "Cambria") %>% save_kable(file = "Tstat_Coll_3_First500.html", self_contained = T)

selected_terms1 <- c('selbstständige', 'künstler', 'freiberufler', 'soloselbständige', 'künstler*innen', 'soloselbstständige', 'solo-selbstständige', 'mittelstand', 'selbständige', 'gastronomie', 'kleinunternehmer', 'gastronomen', 'soloselbststaendige', 'kleinunternehmen', 'einzelhandel', 'hotels', 'solo-selbständige', 'startups', 'kulturschaffende', '\\<Handel\\>', '\\<handel\\>', 'selbständigen', 'kleinstunternehmen', 'restaurants', 'soloselbständigen', 'soloselbstständigen', '\\<Kreative\\>','\\<kreative\\>', 'geschäftsleute', 'kleine unternehmen', 'kleinen unternehmen', 'freie berufe', 'kleine betriebe', 'kleine und mittelständische unternehmen', 'mittlere unternehmen', 'mittelständische unternehmen', 'freie berufe', 'kleine mittlere unternehmen', 'kleine mittelständische unternehmen')

selected_subset_all <- corpus_subset(corp_ALL, grepl(paste(selected_terms1,collapse="|"), texts(corp_ALL)))
selected_subset_all_df <- convert(selected_subset_all, to = c("data.frame"), pretty = TRUE)
#saveRDS(selected_subset_all_df, file = "selected_subset_all_df.rds")

# Creating the file for coding author type -----
sapply(selected_subset_all_df, function(x) length(unique(x))) 
# I'd have to code about 4424 authors first! create a df with them as unique
subset_unique_authors <- selected_subset_all_df[!duplicated(selected_subset_all_df[ , c("author_id")]), ]
# I'd like to add the info on how many times an author has tweeted in my overall data
author_counts_subset <- subset(author_counts, (author_id %in% subset_unique_authors$author_id))
names(author_counts_subset)[names(author_counts_subset) == "n"] <- "freq_tweeted"
subset_unique_authors <- merge(subset_unique_authors, author_counts_subset, by = "author_id")
subset_unique_authors_short <- subset_unique_authors[c("username", "name", "user_description", "author_id", "freq_tweeted")]

# NOTE: groups are differently designated here than in the manuscript - group 1 is politicians here, whereas group 1 is private/non-institutional accounts in the manuscript

# write_xlsx(subset_unique_authors_short,"Authors_Coding_new.xlsx")
authors_coding <- read_excel("Authors_Coding_Final.xlsx")

authors_coding_modified <- authors_coding[c("author_id", "author_type")]
selected_subset_all_df_authortypes <- merge(selected_subset_all_df, authors_coding_modified, by = "author_id")
table(selected_subset_all_df_authortypes$author_type)
selected_subset_all_df_authortypes_OLD_includingBadTweets <- selected_subset_all_df_authortypes 

# Further data cleaning: Removing some tweets that behave like retweets ----
# when coding tweets manually, I noticed that there was one tweet not flagged but as a RT but it was clearly a retweet.
# this is the tweet: '.@peteraltmaier @OlafScholz Lassen Sie uns nicht im Regen stehen! Bund und Länder müssen #Soforthilfen für #Soloselbständige und #Künstler*innen ändern: Sie sollen auch für den eigenen Lebensunterhalt eingesetzt werden dürfen! @a_pinkwart @KristinaVogt_HB https://t.co/trjfC2LOOk'
# so I return to the data cleaning part and exclude this tweet manually
bad_tweet <- ".@peteraltmaier @OlafScholz Lassen Sie uns nicht im Regen stehen!"

ALL_noRT_bad_tweets <- corpus_subset(corp_ALL, grepl(paste(bad_tweet,collapse="|"), texts(corp_ALL)))
ALL_noRT_bad_tweets <- convert(ALL_noRT_bad_tweets, to = c("data.frame"), pretty = TRUE) 
list_bad_tweets <- ALL_noRT_bad_tweets$unique_tweet_id

selected_subset_all_df_authortypes_bad_tweets_out<- subset(selected_subset_all_df_authortypes, !(unique_tweet_id %in% list_bad_tweets))
selected_subset_all_df_authortypes <- selected_subset_all_df_authortypes_bad_tweets_out

table(selected_subset_all_df_authortypes$author_type)

sapply(selected_subset_all_df_authortypes, function(x) length(unique(x)))

# Analyzing corpus segments by author groups ----

selected_subset_all_df_authortypes %>%
  subset(author_type == 4) %>%
  sapply(function(x) length(unique(x)))

selected_subset_all_df_authortypes %>%
  subset(author_type == 2) %>%
  tally(like_count)
selected_subset_all_df_authortypes %>%
  subset(author_type == 5) %>%
  dplyr::summarize(Mean = mean(like_count, na.rm=TRUE))
selected_subset_all_df_authortypes %>%
  subset(author_type == 5) %>%
  dplyr::summarize(Median = median(like_count, na.rm=TRUE))
selected_subset_all_df_authortypes %>%
  subset(author_type == 4) %>%
  tally(retweet_count)
selected_subset_all_df_authortypes %>%
  subset(author_type == 5) %>%
  dplyr::summarize(Mean = mean(retweet_count, na.rm=TRUE))
selected_subset_all_df_authortypes %>%
  subset(author_type == 5) %>%
  dplyr::summarize(Median = median(retweet_count, na.rm=TRUE))

corp_ALL_sample <- quanteda::corpus(selected_subset_all_df_authortypes)
#saveRDS(corp_ALL_sample, file = "corp_ALL_sample.rds")

ALL_sample_tokens <- tokens(corp_ALL_sample, 
                            remove_punct = TRUE,   
                            remove_numbers = TRUE,
                            remove_symbols = TRUE, 
                            remove_url = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(stopwords("german")) %>%
  tokens_remove("custom_stopwords")

tstat_coll <- textstat_collocations(ALL_sample_tokens, size = 2, tolower = FALSE)
head(tstat_coll, 10)

dfm_ALL_sample <- dfm(ALL_sample_tokens)

dfm_ALL_sample_clean <- dfm_remove(dfm_ALL_sample, pattern = c('Corona*', 'Covid*', 'corona*', 'coronahilfen', 'corona-hilfen', 'Hilfen', 'Hilfe'), valuetype = "glob")
dfm_ALL_sample_clean <- dfm_remove(dfm_ALL_sample_clean, pattern = "@*")
dfm_ALL_sample_clean <- dfm_remove(dfm_ALL_sample_clean, pattern = c("ja", "via", "mal", "u", "amp"))
#dfm_ALL_sample_clean_keyness <- dfm_remove(dfm_ALL_sample_clean, pattern = "#*")

ALL_sample_grouped <- dfm_group(dfm_ALL_sample_clean, groups = author_type)

textstat_keyness(ALL_sample_grouped, target = "1") %>% 
  textplot_keyness(n = 40, color = c("darkblue", "lightblue"))
textstat_keyness(ALL_sample_grouped, target = "2") %>% 
  textplot_keyness(n = 40, color = c("darkblue", "lightblue"))
textstat_keyness(ALL_sample_grouped, target = "3") %>% 
  textplot_keyness(n = 40, color = c("darkblue", "lightblue"))
textstat_keyness(ALL_sample_grouped, target = "4") %>% 
  textplot_keyness(n = 40, color = c("darkblue", "lightblue"))
textstat_keyness(ALL_sample_grouped, target = "4") %>% 
  textplot_keyness(n = 30, color = c("darkblue", "lightblue"))
textstat_keyness(ALL_sample_grouped, target = "5") %>% 
  textplot_keyness(n = 40, color = c("darkblue", "lightblue"))

# checking hashtags in different author groups
dfm_ALL_sample_select_clean <- dfm_remove(dfm_ALL_sample, pattern = c("#corona*", "#Corona*"), valuetype = "glob")
select <-  dfm_subset(dfm_ALL_sample_select_clean, author_type == 2)
hashtags <- dfm_select(select, ('#*'), selection = "keep")
tstat_freq <- textstat_frequency(hashtags, n = 40)
head(tstat_freq, 40)

# look at some basic textstat in groups
type1_dfm <- dfm_subset(dfm_ALL_sample_select_clean, author_type == 1)
tstat1_freq <- textstat_frequency(type1_dfm, n = 100)
type2_dfm <- dfm_subset(dfm_ALL_sample_select_clean, author_type == 2)
tstat2_freq <- textstat_frequency(type2_dfm, n = 100)
type3_dfm <- dfm_subset(dfm_ALL_sample_select_clean, author_type == 3)
tstat3_freq <- textstat_frequency(type3_dfm, n = 100)
type4_dfm <- dfm_subset(dfm_ALL_sample_select_clean, author_type == 4)
tstat4_freq <- textstat_frequency(type4_dfm, n = 100)

# POS by author groups ----

spacy_initialize(model = "de_core_news_sm")
spacy_finalize()

author_type_sub <- corpus_subset(corp_ALL_sample, author_type == 4)
pos_type4 <- spacy_parse(author_type_sub, lemma = F, entity = T, dependency = T)

subjekte_type1 <- pos_type1 %>% filter(pos == "NOUN") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
adjektive_type1 <- pos_type1 %>% filter(pos == "ADJ") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
verbs_type1 <- pos_type1 %>% filter(pos == "VERB") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)

subjekte_type2 <- pos_type2 %>% filter(pos == "NOUN") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
adjektive_type2 <- pos_type2 %>% filter(pos == "ADJ") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
verbs_type2 <- pos_type2 %>% filter(pos == "VERB") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)

subjekte_type3 <- pos_type3 %>% filter(pos == "NOUN") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
adjektive_type3 <- pos_type3 %>% filter(pos == "ADJ") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
verbs_type3 <- pos_type3 %>% filter(pos == "VERB") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)

subjekte_type4 <- pos_type4 %>% filter(pos == "NOUN") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
adjektive_type4 <- pos_type4 %>% filter(pos == "ADJ") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)
verbs_type4 <- pos_type4 %>% filter(pos == "VERB") %>% mutate(Wort = str_to_lower(token)) %>% group_by(Wort) %>% dplyr::summarise(Frequenz = n()) %>% dplyr::arrange(desc(Frequenz)) %>% dplyr::mutate(Rang = row_number()) %>% 
  filter(Rang <= 400)

# creating the corpus of different author types for random samples

final_corpus_1 <- subset(selected_subset_all_df_authortypes, author_type == 1)
random_sample_final_corpus_1 <- corpus_sample(quanteda::corpus(final_corpus_1), size = 50)
random_sample_final_corpus_1_df <- convert(random_sample_final_corpus_1, to = "data.frame")
#saveRDS(random_sample_final_corpus_1_df, file = "random_sample_final_corpus_1_df.rds")

final_corpus_2 <- subset(selected_subset_all_df_authortypes, author_type == 2)
random_sample_final_corpus_2 <- corpus_sample(quanteda::corpus(final_corpus_2), size = 50)
random_sample_final_corpus_2_df <- convert(random_sample_final_corpus_2, to = "data.frame")
#saveRDS(random_sample_final_corpus_2_df, file = "random_sample_final_corpus_2_df.rds")

final_corpus_3 <- subset(selected_subset_all_df_authortypes, author_type == 3)
random_sample_final_corpus_3 <- corpus_sample(quanteda::corpus(final_corpus_3), size = 50)
random_sample_final_corpus_3_df <- convert(random_sample_final_corpus_3, to = "data.frame")
#saveRDS(random_sample_final_corpus_3_df, file = "random_sample_final_corpus_3_df.rds")

# creating the final corpus for the qualitative analysis -----

final_corpus_4 <- subset(selected_subset_all_df_authortypes, author_type == 4)

write_xlsx(final_corpus_4,"Final_Corpus_Coding_4_new_new.xlsx")

final_corpus_coded_type4 <- read_excel("Final_Corpus_Coding_4_final.xlsx")

#this step is needed to delete the "Regen" tweets
final_corpus_4_reduced <- final_corpus_coded_type4[c("unique_tweet_id", "main_code", "sub_codes")]
final_corpus_merged <- merge(final_corpus_4,final_corpus_4_reduced, by = "unique_tweet_id", all.x= TRUE)

main_codes <- as.data.frame(table(final_corpus_merged$main_code))

#creating a random sample of the final corpus for a second coder:
random_sample_finalcorpus <- corpus_sample(quanteda::corpus(final_corpus_merged), size = 200)
random_sample_finalcorpus_df <- convert(random_sample_finalcorpus, to = "data.frame")
write_xlsx(random_sample_finalcorpus_df,"random_sample_finalcorpus_df.xlsx")

main_codes_modified <- main_codes
main_codes_modified$Freq <- NULL

main_codes_modified <- convert(main_codes_modified, to = "data.frame")
write_xlsx(main_codes_modified,"main_codes_modified1.xlsx")

##
main_codes <- main_codes %>%
  arrange(desc(Freq))

main_codes[1:15, 1:2] %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "main_codes1.html", self_contained = T)

summary(final_corpus_merged$like_count)
summary(final_corpus_merged$retweet_count)

subc_codes <- as.data.frame(table(final_corpus_merged$sub_codes))

tx <- final_corpus_merged %>% 
  subset(main_code == 'criticizing exclusionary nature of aids') %>%
  subset(,sub_codes)%>%
  count()

# drawing a random sample for additional validation

random_sample <- corpus_sample(quanteda::corpus(ALL_noRT), size = 100)
#saveRDS(random_sample, file = "random_sample.rds")

random_sample_df <- convert(random_sample, to = "data.frame")
write_xlsx(random_sample_df,"random_sample.xlsx")

