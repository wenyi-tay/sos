## Script to get the result in Table 1
## compute the Krippendorff's alpha

DIR_working <- "C:/Users/s3642603/Documents/myPapers/sos"

library(tidyr)
library(dplyr)
library(irr)


getAgreement <- function(df_annotation){
    ## df contains pair_id, annotator_id, sent1_aspect, annotation, domain
    df_result <- data.frame()
    
    ## df_annotation <- df
    
    ## get list of domain
    list_domain <- unique(df_annotation$domain)
    for(d in list_domain){
        # d <- list_domain[1]
        df_domain <- df_annotation %>%
            filter(domain == d)
        print(d)
        print(nrow(df_domain))
        current_aspect <- d ## I kept it the same as later. Because I am using the same df

        df_alpha <- df_domain %>%
            select(pair_id,annotation,annotator_id) %>%
            unique()
        df_alpha <- spread(df_alpha, pair_id, annotation)
        df_alpha <- df_alpha %>%
            select(-annotator_id)
        df_alpha <- as.matrix(df_alpha)
        kripp <- kripp.alpha(df_alpha ,"ordinal")
        alpha_value <- kripp$value
        alpha_raters <- kripp$raters
        alpha_subjects <- kripp$subjects
        df_result_more <- data.frame(current_aspect ,
                                     alpha_value,
                                     alpha_raters,
                                     alpha_subjects)
        df_result <- rbind(df_result, df_result_more)
        
    }
    
    
    
    ## get list of aspects
    list_aspect <- unique(df_annotation$sent1_aspect)
    

    ## loop through all the subcategories
    for(a in list_aspect){
        # a <- list_aspect[5]
        df_aspect <- df_annotation %>%
            filter(sent1_aspect == a)
        print(a)
        print(nrow(df_aspect))
        current_aspect <- a
        
        if(nrow(df_aspect)>0){ ## check that there is at least some annotations for that aspect

            df_alpha <- df_aspect

            # print(table(df_alpha$adjustAnnotation))
            df_alpha <- df_alpha %>%
                select(pair_id,annotation,annotator_id) %>%
                unique()
            df_alpha <- spread(df_alpha, pair_id, annotation)
            df_alpha <- df_alpha %>%
                select(-annotator_id)
            df_alpha <- as.matrix(df_alpha)
            kripp <- kripp.alpha(df_alpha ,"ordinal")
            alpha_value <- kripp$value
            alpha_raters <- kripp$raters
            alpha_subjects <- kripp$subjects
            df_result_more <- data.frame(current_aspect ,
                                         alpha_value,
                                         alpha_raters,
                                         alpha_subjects)
            df_result <- rbind(df_result, df_result_more)

        }
    }
    return(df_result)
}


## read the annotations
df <- read.csv(paste0(DIR_working,"/result/sentence_pair.csv"),
               header=TRUE,
               stringsAsFactors = FALSE)
df_annotation <- read.csv(paste0(DIR_working,"/data/opinion_similarity_annotation.csv"),
                          header=TRUE,
                          stringsAsFactors = FALSE)
df <- merge(df, df_annotation)
df$annotation <- factor(df$annotation,
                        ordered = TRUE,
                        levels = c(0,1,2,3,4))


## get the agreement
df_result <- getAgreement(df)
df_result <- df_result %>%
    filter(current_aspect %in% c("laptop","restaurant"))


## get the average number of annotations by aspect
df_avg <- df %>%
    group_by(domain, pair_id) %>%
    summarise(count = n())
df_avg <- df_avg %>%
    group_by(domain) %>%
    summarise(avg = mean(count))
df_result <- merge(df_result, df_avg, by.x = c("current_aspect"), by.y = c("domain"))
rm(df_avg)


## to get the average variance
df_temp <- df
df_temp$annotation <- as.integer(df_temp$annotation)
df_temp <- df_temp %>%
    group_by(domain, pair_id) %>%
    summarise(var = var(annotation))
df_temp <- df_temp %>%
    group_by(domain) %>%
    summarise(mean_var = mean(var))
df_result <- merge(df_result, df_temp, by.x = c("current_aspect"), by.y= c("domain"))


## formatting the results for paper
df_result$current_aspect <- gsub("[#]","\\\\#",df_result$current_aspect)
df_result$current_aspect <- gsub("[_]","\\\\_",df_result$current_aspect)
df_result <- df_result %>%
    select(-alpha_raters)
df_result$current_aspect <- ordered(df_result$current_aspect,
                                    levels = c("laptop", "restaurant"),
                                    labels = c("Laptop","Restaurant"))
n_pairs <- sum(df_result$alpha_subjects)
df_last_line <- data.frame("Total Pairs",n_pairs,"","","")
colnames(df_last_line) <- c("current_aspect","alpha_subjects","alpha_value","avg","mean_var")
df_result <- rbind(df_result, df_last_line)
df_result <- df_result %>%
    select("current_aspect", "alpha_subjects", "alpha_value", "avg", "mean_var" )
colnames(df_result) <- c("domain","pairs","alpha","avg",
                         "meanVar")

## write out the table for paper
write.csv(df_result,
          paste0(DIR_working,"/result/table1.csv"),
          row.names = FALSE,
          quote=FALSE)
## end
