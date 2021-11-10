## Script to merge the annotations file with semeval file


DIR_working <- "C:/Users/s3642603/Documents/myPapers/sos/"


library(tidyr)
library(dplyr)
library(ggplot2)
set.seed(1234)


## read the annotations
df <- read.csv(paste0(DIR_working,"/data/opinion_similarity_annotation.csv"),
               header=TRUE,
               stringsAsFactors = FALSE)


## i need to allocate a pair_id
df_pair <- df %>%
    select(sent1_id, sent2_id) %>%
    unique()
df_pair$pair_id <- seq(1:nrow(df_pair))
df_pair$pair_id <- paste0("pair_", df_pair$pair_id)


## read the semeval labels
df_restaurant <-read.csv(paste0(DIR_working,"/data/EN_REST_SB1_TEST.csv"),
                         header=TRUE,
                         stringsAsFactors = FALSE)
df_restaurant <- df_restaurant %>%
    select(-target,-index) %>%
    unique()
df_laptop  <-read.csv(paste0(DIR_working,"/data/EN_LAPT_SB1_TEST_.csv"),
                      header=TRUE,
                      stringsAsFactors = FALSE)
df_laptop <- df_laptop  %>%
    select(-index) %>%
    unique()
df_semeval <- rbind(df_restaurant, df_laptop)
rm(df_restaurant)
rm(df_laptop)
df_semeval <- df_semeval %>%
    select(sent_id, category, polarity, text)


## combine the semeval labels with the annotations
df_pair <- merge(df_pair, df_semeval, by.x = c("sent1_id"), by.y = c("sent_id"))
names(df_pair)[names(df_pair) == 'category'] <- 'sent1_aspect'
names(df_pair)[names(df_pair) == 'polarity'] <- 'sent1_polarity'
names(df_pair)[names(df_pair) == 'text'] <- 'sent1_text'
df_pair <- merge(df_pair, df_semeval, by.x = c("sent2_id"), by.y = c("sent_id"))
names(df_pair)[names(df_pair) == 'category'] <- 'sent2_aspect'
names(df_pair)[names(df_pair) == 'polarity'] <- 'sent2_polarity'
names(df_pair)[names(df_pair) == 'text'] <- 'sent2_text'

## select only sentences with same aspect
df_pair <- df_pair[df_pair$sent1_aspect==df_pair$sent2_aspect,]
## and I only want those from this list of aspects
list_aspect <- c("FOOD#QUALITY",
                 "RESTAURANT#GENERAL",
                 "SERVICE#GENERAL",
                 "LAPTOP#GENERAL",
                 "LAPTOP#OPERATION_PERFORMANCE",
                 "SUPPORT#QUALITY")
df_pair <- df_pair %>%
    filter(sent1_aspect %in% list_aspect)


## I want only same polarity and opposite polarity sentence pairs
# df <- df %>%
#   filter(sent1_polarity!="neutral",
#           sent2_polarity!="neutral")
df_pair$group <- rep(NA, nrow(df_pair))
df_pair$group[df_pair$sent1_polarity==df_pair$sent2_polarity] <- "Same"
df_pair$group[df_pair$sent1_polarity!=df_pair$sent2_polarity] <- "Different"

df_pair <- df_pair %>%
    arrange(group)

## as sentence pairs can be associated with more than one aspect
## I am taking just one aspect
df_pair <- df_pair[!duplicated(df_pair$pair_id),]


## merge it back
df <- merge(df, df_pair)
rm(df_pair)

## i need a domain 
df$domain <- rep("laptop",nrow(df))
df$domain[df$sent1_aspect %in% c("FOOD#QUALITY",
                                 "RESTAURANT#GENERAL",
                                 "SERVICE#GENERAL")] <- "restaurant"


df <- df %>%
    group_by(pair_id, sent1_id, sent2_id, group, domain) %>%
    summarise(gold = mean(annotation))

## write out the file
write.csv(df,
          paste0(DIR_working,"/result/sentence_pair.csv"),
          row.names = FALSE)


## end


