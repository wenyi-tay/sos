## Script to generate Table 5 and Table 6



DIR_working <- "C:/Users/s3642603/Documents/myPapers/sos"


# load the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)


mymodels <- c("sbert_amazon_1000_entirereview_triplet_margin1_run1",
              "sbert_yelp_3000_entirereview_triplet_margin7_run1",
              "sbert",
              "wmd_sswe",
              "sbert_yelp_8000_entirereview_pair_run1",
              "sbert_amazon_8000_entirereview_pair_run1")

df_results <- data.frame()

for(model in mymodels){
    # model <- "sbert"
    print(model)
    if(model == "sbert" | model == "wmd_sswe"){
        df <- read.csv(paste0(DIR_working,"/result/sentence_pair_",model,".csv"),
                       header=TRUE,
                       stringsAsFactors=FALSE)
        df_gold <- read.csv(paste0(DIR_working,"/result/sentence_pair.csv"),
                            header=TRUE,
                            stringsAsFactors=FALSE)
        df <- merge(df, df_gold)
        names(df)[names(df) == 'gold'] <- 'avg'
    }else{
        df <- read.csv(paste0(DIR_working,"/result/sentence_pair_",model,".csv"),
                       header=TRUE,
                       stringsAsFactors=FALSE)
    }
    df$sim_group <- rep("more similar", nrow(df))
    df$sim_group[df$avg <= 1.5] <- "more different"
    
    ## get the correlation for the domain
    list_domain <- unique(df$domain)
    list_sim_group <- unique(df$sim_group)
    
    

    df <- df %>%
        select(pair_id, model_similarity, avg, domain, sim_group)
    df <- distinct(df, pair_id,.keep_all = TRUE)
    
    
    
    for(a in list_domain){
        # a <- list_domain[1]
        df_temp <- df %>%
            filter(domain == a)
        n_points <- nrow(df_temp)
        

        for(t in c("pearson","spearman","kendall")){
            s <- "all"
            cor_type <- t
            cor_value <- cor(df_temp$model_similarity, df_temp$avg, method=t)
            df_results_line <- data.frame(a,model,s,cor_type,cor_value,n_points)
            df_results <- rbind(df_results, df_results_line)
        }
        
        for(s in list_sim_group){
            df_temp2 <- df_temp %>%
                filter(sim_group == s)
            for(t in c("pearson","spearman","kendall")){
                cor_type <- t
                cor_value <- cor(df_temp2$model_similarity, df_temp2$avg, method=t)
                df_results_line <- data.frame(a,model,s,cor_type,cor_value,n_points)
                df_results <- rbind(df_results, df_results_line)
            }
        }
        
        print(a)
        print(model)
        print(mean(df_temp$model_similarity))

    }

}
print(df_results)




## layout 2
df_results$a <- tolower(df_results$a)
df_laptop <- df_results %>%
    filter(a == "laptop",
           s=="all")
df_laptop <- spread(df_laptop, cor_type, cor_value)
df_laptop <- df_laptop %>%
    select(model, pearson, kendall)
colnames(df_laptop) <- c("model", "laptop-pearson","laptop-kendall")

df_restaurant<- df_results %>%
    filter(a == "restaurant",
           s=="all")
df_restaurant <- spread(df_restaurant, cor_type, cor_value)
df_restaurant <- df_restaurant %>%
    select(model, pearson, kendall)
colnames(df_restaurant) <- c("model", "restaurant-pearson","restaurant-kendall")


df_table_2 <- merge(df_laptop, df_restaurant)

## format it for the paper
df_table_2$model <- ordered(df_table_2$model,
                            levels = c(
                                       "sbert",
                                       "wmd_sswe",
                                       "sbert_amazon_8000_entirereview_pair_run1",
                                       "sbert_yelp_8000_entirereview_pair_run1",
                                       "sbert_amazon_1000_entirereview_triplet_margin1_run1",
                                       "sbert_yelp_3000_entirereview_triplet_margin7_run1"),
                            labels = c("SBERT",
                                       "WMD-SSWE",
                                       "SOS^S_{PC}",
                                       "SOS^S_{Yelp}",
                                       "SOS^T_{PC}",
                                       "SOS^T_{Yelp}"))

df_table_2 <- df_table_2 %>%
    arrange(model)

## output the file
write.csv(df_table_2,
          paste0(DIR_working,"/result/table5.csv"),
          row.names = FALSE,
          quote = FALSE)



## I need the table for breaking down by sim_group
df_table_3a_laptop <- df_results %>%
    filter(s == "more similar", a == "laptop")
df_table_3a_laptop  <- spread(df_table_3a_laptop , cor_type, cor_value)
df_table_3a_laptop  <- df_table_3a_laptop %>%
    select(model, pearson, kendall)
colnames(df_table_3a_laptop ) <- c("model", "laptop-pearson","laptop-kendall")


df_table_3a_rest <- df_results %>%
    filter(s == "more similar", a == "restaurant")
df_table_3a_rest  <- spread(df_table_3a_rest , cor_type, cor_value)
df_table_3a_rest <- df_table_3a_rest %>%
    select(model, pearson, kendall)
colnames(df_table_3a_rest ) <- c("model", "restaurant-pearson","restaurant-kendall")


df_table_3a <- merge(df_table_3a_laptop, df_table_3a_rest)

## format it for the paper
df_table_3a$model <- ordered(df_table_3a$model,
                             levels = c(
                                 "sbert",
                                 "wmd_sswe",
                                 "sbert_amazon_8000_entirereview_pair_run1",
                                 "sbert_yelp_8000_entirereview_pair_run1",
                                 
                                 "sbert_amazon_1000_entirereview_triplet_margin1_run1",
                                 "sbert_yelp_3000_entirereview_triplet_margin7_run1"),
                             labels = c("SBERT",
                                        "WMD-SSWE",
                                        "SOS^S_{PC}",
                                        "SOS^S_{Yelp}",
                                        "SOS^T_{PC}",
                                        "SOS^T_{Yelp}"))

df_table_3a <- df_table_3a %>%
    arrange(model)

## more different

df_table_3b_laptop <- df_results %>%
    filter(s == "more different", a == "laptop")
df_table_3b_laptop  <- spread(df_table_3b_laptop , cor_type, cor_value)
df_table_3b_laptop  <- df_table_3b_laptop %>%
    select(model, pearson, kendall)
colnames(df_table_3b_laptop ) <- c("model", "laptop-pearson","laptop-kendall")


df_table_3b_rest <- df_results %>%
    filter(s == "more different", a == "restaurant")
df_table_3b_rest  <- spread(df_table_3b_rest , cor_type, cor_value)
df_table_3b_rest <- df_table_3b_rest %>%
    select(model, pearson, kendall)
colnames(df_table_3b_rest ) <- c("model", "restaurant-pearson","restaurant-kendall")

df_table_3b <- merge(df_table_3b_laptop, df_table_3b_rest)

## format it for the paper
df_table_3b$model <- ordered(df_table_3b$model,
                             levels = c(
                                 "sbert",
                                 "wmd_sswe",
                                 "sbert_amazon_8000_entirereview_pair_run1",
                                 "sbert_yelp_8000_entirereview_pair_run1",
                                 
                                 "sbert_amazon_1000_entirereview_triplet_margin1_run1",
                                 "sbert_yelp_3000_entirereview_triplet_margin7_run1"),
                             labels = c("SBERT",
                                        "WMD-SSWE",
                                        "SOS^S_{PC}",
                                        "SOS^S_{Yelp}",
                                        "SOS^T_{PC}",
                                        "SOS^T_{Yelp}"))

df_table_3b <- df_table_3b %>%
    arrange(model)


df_break_3a <- setNames(data.frame(t(c("Broadly Similar", "","","",""))),
                        c("model","laptop-pearson", "laptop-kendall",    
                          "restaurant-pearson","restaurant-kendall"))
df_break_3b <- setNames(data.frame(t(c("Broadly Different", "","","",""))),
                        c("model","laptop-pearson", "laptop-kendall",    
                          "restaurant-pearson","restaurant-kendall"))



df_table_3 <- rbind(df_break_3b, df_table_3b, df_break_3a, df_table_3a)

write.csv(df_table_3,
          paste0(DIR_working,"/result/table6.csv"),
          row.names = FALSE,
          quote = FALSE)

## end

