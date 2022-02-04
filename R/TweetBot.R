library(rtweet)
library(googledrive)
library(dplyr)

token <- create_token(
  app = Sys.getenv("TWEET_BOT"),
  consumer_key = Sys.getenv("TWEET_BOT_CONSUMER_KEY"),
  consumer_secret = Sys.getenv("TWEET_BOT_CONSUMER_KEY_SECRET"),
  access_token = Sys.getenv("TWEET_BOT_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWEET_BOT_ACCESS_SECRET")
)


drive_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))
raw_dat <- drive_ls(as_id(Sys.getenv('MEME_FOLDER_1')))

#Feed memes to the second folder on drive
for (i in 1:nrow(raw_dat)) {
  drive_cp(raw_dat$name[i],path = 'memes/', overwrite = TRUE) 
}

data <- drive_ls(as_id(Sys.getenv('MEME_FOLDER_2')))
data$name <- gsub('Copy of ', '',data$name)


#store all memes to the AllMemesFolder
for (meme in 1:nrow(data)) {
  
  drive_download(file = data$name[meme], path = paste0('./Memes/AllMemesFolder/',data$name[meme]),overwrite = TRUE)
}

#Reading and cleaning up tweeted memes to be excluded in the next tweet
TweetedMemes_png = list.files(path = "./Memes/TweetedMemesFolder", pattern = '*.png', full.names = TRUE, ignore.case = TRUE)
TweetedMemes_jpg = list.files(path = "./Memes/TweetedMemesFolder", pattern = '*jpg', full.names = TRUE, ignore.case = TRUE)
TweetedMemes <- c(TweetedMemes_jpg,TweetedMemes_png)
TweetedMemes <- gsub('./Memes/TweetedMemesFolder/','',TweetedMemes)

#remove already tweeted memes from the new downloaded meme before
meme_for_tweet <- data %>% filter(!name%in%TweetedMemes)
meme_for_tweet <- distinct(meme_for_tweet,id,.keep_all = TRUE)

#previous meme
previous_meme <- readRDS('./R/previous_meme.RDS')
previous_meme <- gsub('./Memes/JustNowMemesFolder/','',previous_meme)

#store new memes to the JustNowMemesFolder
for (meme in 1:nrow(meme_for_tweet)) {
  
  drive_download(file = meme_for_tweet$name[meme], path = paste0('./Memes/JustNowMemesFolder/',meme_for_tweet$name[meme]),overwrite = TRUE)
}

#get meme to tweet
JustNowMemes_jpg = list.files(path = "./Memes/JustNowMemesFolder", pattern = "*.jpg", full.names = TRUE, ignore.case = TRUE)
JustNowMemes_png = list.files(path = "./Memes/JustNowMemesFolder", pattern = "*.PNG", full.names = TRUE, ignore.case = TRUE)
JustNowMemes <- c(JustNowMemes_jpg,JustNowMemes_png)



tweet_meme <- function(){
  
  if(length(JustNowMemes)>0){
    
    current_meme = sample(JustNowMemes, size = 1,replace = FALSE)
    
    new_meme <- gsub('./Memes/JustNowMemesFolder/','',current_meme)
    
    if(previous_meme==new_meme){
      current_meme = sample(JustNowMemes, size = 1,replace = FALSE) 
    }
    
    file.copy(current_meme,'./Memes/TweetedMemesFolder')
    
    
    
    
    repeat{
      
      if(new_meme != previous_meme){
        
        print('Todays tweet')
        tweeting_meme <- post_tweet(status = '#meme #memeforeveryone #memes #ilovememes #memesdaily',media = current_meme,token = token)
        unlink(current_meme)
        saveRDS(current_meme, './R/previous_meme.RDS')
        break
        
      }else{
        
        JustNowMemes_jpg = list.files(path = "./Memes/JustNowMemesFolder", pattern = "*.jpg", full.names = TRUE, ignore.case = TRUE)
        JustNowMemes_png = list.files(path = "./Memes/JustNowMemesFolder", pattern = "*.PNG", full.names = TRUE, ignore.case = TRUE)
        JustNowMemes <- c(JustNowMemes_jpg,JustNowMemes_png)
        
        current_meme = sample(JustNowMemes, size = 1,replace = FALSE)
        saveRDS(current_meme, './R/previous_meme.RDS')
      }
      
    }
    
    
    
  }else{
    
    AllMemesTweeted_jpg = list.files(path = "./Memes/AllMemesFolder", pattern = "*.jpg", full.names = TRUE,ignore.case = TRUE)
    AllMemesTweeted_png = list.files(path = "./Memes/AllMemesFolder", pattern = "*.png", full.names = TRUE,ignore.case = TRUE)
    AllMemesTweeted <- c(AllMemesTweeted_jpg,AllMemesTweeted_png)
    
    current_meme = sample(AllMemesTweeted, size = 1,replace = FALSE)
    
    new_meme <- gsub('./Memes/AllMemesFolder/','',current_meme)
    old_meme <- gsub('./Memes/AllMemesFolder/','',previous_meme)
    
    repeat{
      if(new_meme != old_meme){
        
        print('from the all tweet folder')
        tweeting_meme <- post_tweet(status = '#meme #memeforeveryone #memes #ilovememes #memesdaily',media = current_meme,token = token)
        saveRDS(current_meme, './R/previous_meme.RDS')
        break
        
      }else{
        
        AllMemesTweeted_jpg = list.files(path = "./Memes/AllMemesFolder", pattern = "*.jpg", full.names = TRUE,ignore.case = TRUE)
        AllMemesTweeted_png = list.files(path = "./Memes/AllMemesFolder", pattern = "*.png", full.names = TRUE,ignore.case = TRUE)
        AllMemesTweeted <- c(AllMemesTweeted_jpg,AllMemesTweeted_png)
        
        current_meme = sample(AllMemesTweeted, size = 1,replace = FALSE)
        saveRDS(current_meme, './R/previous_meme.RDS')
      }
      
    }
    
    
    
  }
  
  return(tweeting_meme)
}


tweet_meme()


