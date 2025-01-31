###########################################
# R commands to process the Yelp database #
###########################################

#############################################
# Part 1:  Setup and initial data wrangling #
#############################################

# Load library

library(dplyr)

# Read in csv files
setwd('C:\\Users\\p14te\\Desktop\\Yelp\\springboardCapstone_YelpAnalysis')

reviews    <- read.csv("yelp_academic_dataset_review.csv",   header = FALSE)
users      <- read.csv("yelp_academic_dataset_user.csv",     header = FALSE)
businesses <- read.csv("yelp_academic_dataset_business.csv", header = FALSE)

# Add names to the fields
colnames(reviews)[1] = "user_id" 
colnames(reviews)[2] = "business_id"
colnames(reviews)[3] = "stars"
colnames(reviews)[4] = "word_num"
colnames(users)[1] = "user_id"
colnames(users)[2] = "user_name"
colnames(businesses)[1] = "business_id"
colnames(businesses)[2] = "city"
colnames(businesses)[3] = "business_name"
colnames(businesses)[4] = "categories"
colnames(businesses)[5] = "review_count"
colnames(businesses)[6] = "avg_stars"

# Join the files
ru  <- inner_join(reviews, users)
rub <- inner_join(ru, businesses)


#################################################################
# Part 2b:  Analysis of Method 1 -- Extension to Other Cuisines #
#################################################################

c_list <- list("Indian"= c(c("Indian")),"Japanese"= c("Japanese","Sushi"),"Chinese" = c("Chinese"),"Italian"=c("Italian"), "greek"=c("greek"),"french"=c("french"),"thai"=c("thai"),"spanish"=c("tapas","spanish"),"Mediterranean"=c("Mediterranean"))
print(names(c_list))
num_reviews_by_country <- list()
Indian <- NULL
is_Indian<- NULL
#loop starts
for(curr_c_name in names(c_list))
{
search_words <- c_list[curr_c_name]
rub$curr_c <- grepl(paste(search_words[[1]], collapse="|"), rub$categories) == TRUE

rub$is_indian <- grepl("Indian", rub$categories) == TRUE
indian <- subset(rub, is_indian == TRUE)

curr_c_df <- subset(rub, curr_c == TRUE)
if (curr_c_name=="Indian")
{
  Indian <- curr_c_df
  
}
print(nrow(curr_c_df))
num_reviews_by_country[[curr_c_name]]<- curr_c_df %>% select(user_id, user_name, curr_c) %>%
  group_by(user_id) %>%
  summarise(tot_rev = sum(curr_c))
print(table(num_reviews_by_country[[curr_c_name]]$tot_rev))
print(count(num_reviews_by_country[[curr_c_name]]))
print(mean(num_reviews_by_country[[curr_c_name]]$tot_rev))
}


#####################################################################
# Part 2c:  Analysis of Method 1 -- Apply new weight and see effect #
#####################################################################

# Combine num_reviews information with original data frame of indian restaurant reviews
cin <- inner_join(Indian, num_reviews_by_country[["Indian"]])

# Generate "weighted_stars" for later calculation
cin$weighted_stars <- cin$stars * cin$tot_rev

# Use "summarise" to generate a new rating for each restaurant
new_rating_Indian <- cin %>% select(city, business_name, avg_stars, stars, 
                                    tot_rev, weighted_stars) %>%
  group_by(city, business_name, avg_stars) %>%
  summarise(cnt = n(),
            avg = sum(stars) / cnt,
            new = sum(weighted_stars) / sum(tot_rev),
            dif = new - avg)

# Print summary data of the effect this new rating has
summary(new_rating_Indian$dif)

# Limit to those with at least 5 ratings and redo summary
nri5 <- subset(new_rating_Indian, cnt > 5)
summary(nri5$dif)
                                        

################################################################
# Part 3:  Analysis of Method 2 -- Generate "immigrant" rating #
################################################################
indian <- Indian
# Read Indian names into a list
inames <- scan("indian_names.txt", what = character())

# Add field "reviewer_indian_name" to indian reviews if user name is in the list
indian$reviewer_indian_name <- indian$user_name %in% inames

# Generate "istars" for internal calculation later
indian$istars <- indian$stars * indian$reviewer_indian_name

# Find out # of reviewers with a uniquely Indian name
table(indian$reviewer_indian_name)
1274/(1274 + 11872)    # .096

# Generate new "immigrant" rating
avg_rating_Indian <- indian %>% select(business_id, business_name, city, stars, 
                                       avg_stars, reviewer_indian_name, 
                                       is_indian, istars) %>%
                                group_by(city, business_name, avg_stars) %>%
                                summarise(count = n(),
                                          nin = sum(reviewer_indian_name),
                                          pin = sum(reviewer_indian_name) / n(),
                                          avg = sum(stars) / count,
                                          ias = sum(istars) / nin,
                                          dif = ias - avg)

# Find out extent of effect of new rating
summary(avg_rating_Indian)
summary(avg_rating_Indian$dif)

# Limit to those restaurants with at least 5 "immigrant" reviews and look at effect again
ari5 <- subset (avg_rating_Indian, nin > 5)                                        
summary(ari5$dif)
summary(ari5)

#######################################################################
# Part 3b:  Analysis of Method 3 -- Generate rating based on word count
#######################################################################
cin$newrate <- c()
mean<- mean(cin$word_num)
for(i in 1:68325)
{
  print(i)
if(cin$word_num[i] > mean(cin$word_num)){
  if(cin$stars[i]>=3){
      cin$newrate[i] <- min(cin$stars[i]*(1+((cin$word_num[i]- mean)/cin$word_num[i])),5)
  }
  else{
      cin$newrate[[i]] <- max(cin$stars[[i]]*(1-((cin$word_num[[i]]- mean)/cin$word_num[[i]])),1)
  }
    summary(cin$newrate)  
}
else{
  cin$newrate[i]<-cin$stars[i]
}
}
