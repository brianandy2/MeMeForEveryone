library(rtweet)
library(googledrive)
library(forcats)

token <- create_token(
  app = Sys.getenv("TWEET_BOT"),
  consumer_key = Sys.getenv("TWEET_BOT_CONSUMER_KEY"),
  consumer_secret = Sys.getenv("TWEET_BOT_CONSUMER_KEY_SECRET"),
  access_token = Sys.getenv("TWEET_BOT_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWEET_BOT_ACCESS_SECRET")
)



drive_auth(path = Sys.getenv("GOOGLE_AUTHENTICATION_CREDENTIALS"))
raw_dat <- drive_ls(as_id(Sys.getenv('MEME_FOLDER_1')))

for (i in 1:nrow(raw_dat)) {
  drive_cp(raw_dat$name[i],path = 'memes/', overwrite = TRUE) 
}


data <- drive_ls(as_id(Sys.getenv('MEME_FOLDER_2')))

data$name <- gsub('Copy of ', '',data$name)


duplicates <- fct_count(as.factor(data$name))

duplicates <- duplicates[duplicates$n>1,]

dp_name <- as.character(duplicates$f)

for (dp_names in dp_name) {
  
  to_clean <- data[data$name==dp_names,]
  num_time_to_clean <- nrow(to_clean)-1
  
  for (i in 1:num_time_to_clean) {
    
    drive_rm(to_clean$id[i])
    
  }
  
}

data <- drive_ls(as_id(Sys.getenv('MEME_FOLDER_2')))

data$name <- gsub('Copy of ', '',data$name)



for (meme in 1:nrow(raw_dat)) {
  
  drive_download(file = data$name[meme], path = paste0('./Memes/TodaysMemesFolder/',data$name[meme]),overwrite = TRUE)
}

previous_meme <- readRDS('./R/previous_meme.RDS')

#Get the list of all memes
AllMemes = list.files(path = "./Memes/AllMemesFolder", pattern = ".jpg", full.names = TRUE)

TodaysMemes = list.files(path = "./Memes/TodaysMemesFolder", pattern = ".jpg", full.names = TRUE)
minus <- which(TodaysMemes == previous_meme)

TodaysMemes = TodaysMemes[-minus]


tweet_meme <- function(){
  
if(length(TodaysMemes)>0){
  
  current_meme = sample(TodaysMemes, size = 1,replace = FALSE)
  
  
  new_meme <- gsub('./Memes/TodaysMemesFolder/','',current_meme)
  old_meme <- gsub('./Memes/TodaysMemesFolder/','',previous_meme)
  
  repeat{
    
    if(new_meme != old_meme){
      
      print('Todays tweet')
      tweeting_meme <- post_tweet(status = '#meme #memeforeveryone #memes #ilovememes #memesdaily',media = current_meme,token = token)
      file.copy(current_meme, "./Memes/AllMemesFolder")
      file.copy(TodaysMemes, "./Memes/TodaysMemesFolder")
      
      break
      
    }else{
      
      TodaysMemes = list.files(path = "./Memes/TodaysMemesFolder", pattern = ".jpg", full.names = TRUE)
      current_meme = sample(TodaysMemes, size = 1,replace = FALSE)
      
    }
   
  }
  saveRDS(current_meme, './R/previous_meme.RDS') 
  delete_meme <- data[data$name==new_meme,][2]
  drive_rm(delete_meme$id[1])
  
}else{
 
  current_meme = sample(AllMemes, size = 1,replace = FALSE)
  AllMemes = list.files(path = "./Memes/AllMemesFolder", pattern = ".jpg", full.names = TRUE)
  previous_meme <- readRDS('./R/previous_meme.RDS')
  
  new_meme <- gsub('./Memes/AllMemesFolder/','',current_meme)
  old_meme <- gsub('./Memes/TodaysMemesFolder/','',previous_meme)
  
  repeat{
    if(new_meme != old_meme){
      
      print('all tweet')
      tweeting_meme <- post_tweet(status = '#meme #memeforeveryone #memes #ilovememes #memesdaily',media = current_meme,token = token)
       delete_meme <- data[data$name==new_meme,][2]
       drive_rm(delete_meme$id[1])
      break
      
    }else{
      
      AllMemes = list.files(path = "./Memes/AllMemesFolder", pattern = ".jpg", full.names = TRUE)
      current_meme = sample(AllMemes, size = 1,replace = FALSE)
    }
    
  }
  saveRDS(current_meme, './R/previous_meme.RDS') 
  delete_meme <- data[data$name==new_meme,][2]
  drive_rm(delete_meme$id[1])
}
  
  return(tweeting_meme)
}


tweet_meme()


