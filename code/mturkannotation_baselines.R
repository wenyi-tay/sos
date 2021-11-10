## Script to get the results in Table 3 and 4


## set working directory
DIR_working <- "C:/Users/s3642603/Documents/myPapers/sos/result"


## load the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)


mymodels <- c("wmd",
              "sbert",
              "moverscore")
df_results <- data.frame()

    
for(model in mymodels){
    # model <- "wmd"
    print(model)
    if(model == "sbertFT"){
        df <- read.csv(paste0(DIR_working,"/sentence_pair_",model,".csv"),
                       header=TRUE,
                       stringsAsFactors=FALSE)
    }else{
        df <- read.csv(paste0(DIR_working,"/sentence_pair_",model,".csv"),
                       header=TRUE,
                       stringsAsFactors=FALSE)
    }
    ## get the gold standard scores
    df_gold <- read.csv(paste0(DIR_working,"/sentence_pair.csv"),
                        header=TRUE,
                        stringsAsFactors = FALSE)
    df <- merge(df, df_gold)
    
    
    df$sim_group <- rep("more similar", nrow(df))
    df$sim_group[df$gold <= 1.5] <- "more different"
    
    ## get the correlation for the domain
    list_domain <- unique(df$domain)
    list_sim_group <- unique(df$sim_group)
    

    df <- df %>%
        select(pair_id, model_similarity, gold, domain, sim_group)
    df <- distinct(df, pair_id,.keep_all = TRUE)
    
    
    
    for(a in list_domain){
        # a <- list_domain[1]
        df_temp <- df %>%
            filter(domain == a)
        n_points <- nrow(df_temp)
        

        for(t in c("pearson","spearman","kendall")){
            s <- "all"
            cor_type <- t
            cor_value <- cor(df_temp$model_similarity, df_temp$gold, method=t)
            df_results_line <- data.frame(a,model,s,cor_type,cor_value,n_points)
            df_results <- rbind(df_results, df_results_line)
        }
        
        for(s in list_sim_group){
            df_temp2 <- df_temp %>%
                filter(sim_group == s)
            for(t in c("pearson","spearman","kendall")){
                cor_type <- t
                cor_value <- cor(df_temp2$model_similarity, df_temp2$gold, method=t)
                df_results_line <- data.frame(a,model,s,cor_type,cor_value,n_points)
                df_results <- rbind(df_results, df_results_line)
            }
        }
        
        print(a)
        print(model)
        print(mean(df_temp$model_similarity))
     }

}

# Add in the SPICE scores

## read the sentence pairs 
df <- read.csv(paste0(DIR_working,"/sentence_pair_unique.csv"),
               header=TRUE,
               stringsAsFactors = FALSE)
## read the spice scores
df_spice <- read.csv(paste0(DIR_working,"/sentence_pair_spice.csv"), 
                     header=TRUE, 
                     stringsAsFactors = FALSE)
colnames(df_spice) <- c("pair_id","precision","recall","fscore")
model <- "spice"


## merge the df
df <- merge(df, df_spice, by=c("pair_id"))


## get the correlation 
## get the correlation for the domain
df$sim_group <- rep("more similar", nrow(df))
df$sim_group[df$avg <= 1.5] <- "more different"
list_domain <- unique(df$domain)
list_sim_group <- unique(df$sim_group)


for(a in list_domain){
    # a <- list_domain[1]
    df_temp <- df %>%
        filter(domain == a)
    n_points <- nrow(df_temp)
    
    
    for(t in c("pearson","spearman","kendall")){
        s <- "all"
        cor_type <- t
        cor_value <- cor(df_temp$fscore, df_temp$avg, method=t)
        df_results_line <- data.frame(a,model,s,cor_type,cor_value,n_points)
        df_results <- rbind(df_results, df_results_line)
    }
    
    for(s in list_sim_group){
        df_temp2 <- df_temp %>%
            filter(sim_group == s)
        for(t in c("pearson","spearman","kendall")){
            cor_type <- t
            cor_value <- cor(df_temp2$fscore, df_temp2$avg, method=t)
            df_results_line <- data.frame(a,model,s,cor_type,cor_value,n_points)
            df_results <- rbind(df_results, df_results_line)
        }
    }
    
    print(a)
    print(model)
}


## adding in the ROUGE Scores
df_results_rouge <- data.frame()
list_domain <- c("RESTAURANT","LAPTOP")
list_metric <- c("ROUGE-1","ROUGE-2","ROUGE-L")


df <- read.csv(paste0(DIR_working,"/sentence_pair_rouge.csv"),
               header=TRUE,
               stringsAsFactors=FALSE)
## get the gold standard scores
df_gold <- read.csv(paste0(DIR_working,"/sentence_pair.csv"),
               header=TRUE,
               stringsAsFactors = FALSE)


for(domain in list_domain){
    for(m in list_metric){
        df_temp <- df %>%
            filter(ROUGE.Type == m)
        df_temp <- df_temp[grepl(domain, df_temp$Task.Name),]
        df_temp$pair_id <- tolower(df_temp$Task.Name)
        df_temp$pair_id <- gsub(tolower(domain),"",df_temp$pair_id)
        df_temp$pair_id <- gsub("pair","pair_",df_temp$pair_id)

        
        df_temp <- merge(df_temp, df_gold, by=c("pair_id"))
        df_temp$sim_group <- rep("more similar", nrow(df_temp))
        df_temp$sim_group[df_temp$gold <= 1.5] <- "more different"
        n_points <- nrow(df_temp)
        
        p1 <- ggplot(data=df_temp, aes(x =Avg_F.Score, y = gold))
        p1 + geom_point(alpha =0.2)
        for(t in c("pearson","spearman","kendall")){
            s <- "all"
            cor_type <- t
            cor_value <- cor(df_temp$Avg_F.Score, df_temp$gold, method=t)
            df_results_line <- data.frame(domain,m,s,cor_type,cor_value,n_points)
            df_results_rouge <- rbind(df_results_rouge, df_results_line)
        }
        
        for(s in list_sim_group){
            # s <- list_sim_group[1]
            df_temp2 <- df_temp %>%
                filter(sim_group == s)
            for(t in c("pearson","spearman","kendall")){
                cor_type <- t
                cor_value <- cor(df_temp2$Avg_F.Score, df_temp2$gold, method=t)
                df_results_line <- data.frame(domain,m,s,cor_type,cor_value,n_points)
                df_results_rouge <- rbind(df_results_rouge, df_results_line)
            }
        }
        print(domain)
        print(m)

        
    }
}

colnames(df_results_rouge) <- c("a",
                                "model",
                                "s",
                                "cor_type",
                                "cor_value",
                                "n_points")
df_results_rouge$a <- factor(df_results_rouge$a,
                                 levels= c("RESTAURANT","LAPTOP"),
                                 labels=c("Restaurant","Laptop"))
df_results <- rbind(df_results, df_results_rouge)
df_results$a <- tolower(df_results$a)



## format it to Table 2

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
                            levels = c("ROUGE-1","ROUGE-2","ROUGE-L","spice","wmd","wmd_sswe","sbert","moverscore"),
                            labels = c("ROUGE-1","ROUGE-2","ROUGE-L","SPICE","WMD","WMD-SSWE","SBERT","MoverScore"))

df_table_2 <- df_table_2 %>%
    arrange(model)

## output the file
write.csv(df_table_2,
          paste0(DIR_working,"/table3.csv"),
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
                             levels = c("ROUGE-1","ROUGE-2","ROUGE-L","spice","wmd","wmd_sswe","sbert","moverscore","bertscore",
                                        "sbertFT"),
                             labels = c("ROUGE-1","ROUGE-2","ROUGE-L","SPICE","WMD","WMD-SSWE","SBERT","MoverScore","BERTScore",
                                        "Our models"))

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
                             levels = c("ROUGE-1","ROUGE-2","ROUGE-L","spice","wmd","wmd_sswe","sbert","moverscore"),
                             labels = c("ROUGE-1","ROUGE-2","ROUGE-L","SPICE","WMD","WMD-SSWE","SBERT","MoverScore"))

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
          paste0(DIR_working,"/table4.csv"),
          row.names = FALSE,
          quote = FALSE)

## end

