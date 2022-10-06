#Salvador Grover
#Octover 5, 2022
#Script to try out USC keyword search to find courses that are related to sustainability and UN's SDG
library(dplyr)
library(tidyverse)
#library(devtools)
#devtools::install_github("USC-Office-of-Sustainability/USC-SDGmap")

fall <- read.csv("1212.csv", header = T)
spring <- read.csv("1214.csv", header = T)

courses <- rbind(fall, spring) 

#filtering the courses by unique course ID, takes the first subject and department and tosses the second
uw_courses <- courses[unique(courses[, c("Course.Id")]), ]

uw_courses["year"] <- "AY2020"
uw_courses <- uw_courses %>% 
  rename(course_title = Course.Title, course_desc = Course.Description) 
uw_courses["clean_course_desc"] <- NA


for (i in 1:nrow(uw_courses)){
  desc = uw_courses$course_desc[i]
  desc_clean = gsub("[^[:alnum:][:space:]']", " ", desc)
  desc_spaces = str_squish(desc_clean)
  uw_courses$clean_course_desc[i] = desc_spaces
}



# exclude words if followed by "environment"
words = c("business", "Business", "reporting", "immersive", "learning", "Learning", "visualization", "Visualisation",
          "outpatient", "hospital", "clinical", "dental", "lab", "media", "network", 
          "digital", "professional", "legal", "news", "focused",
          "virtual", "workshop", "health care", "emotionally rich", "corporate", 
          "market", "structured", 
          "built", "business", "classroom", "clinical", "commmunication", "consulting",
          "dental", "digital", "economic", "education", "educational", "focused",
          "future", "hospital", "inclusive", "institutional", "lab", "learning", 
          "legal", "liveable", "market", "marketing", "media", "network", "news",
          "orgnanizational", "outpatient", "peoples", "policy", "politics",
          "professional", "regulatory", "reporting", "reporting", "social", 
          "sonic", "strategic", "structured", "tax", "technology", "urban", 
          "various", "virtual", "voting", "workshop") 

# change "environment" to "domain" if preceded by an exclude word
# change "ecology" to "domain" if preceded by classroom
for (i in 1:nrow(uw_courses)){
  desc = uw_courses$clean_course_desc[i]
  x = unlist(strsplit(desc, " "))
  for (j in 2:length(x)){
    if (is.na(x[j])){
      next
    }
    if (x[j] == "environment" | x[j] == "environments" | x[j] == "Environment" | x[j] == "Environments"){
       #print(i)
      if (tolower(x[j-1]) %in% words){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "ecology"){ 
      if (tolower(x[j-1]) == "classroom"){
        print(i)
        x[j] = "domain"
      }
    }
  }
  uw_courses$clean_course_desc[i] = paste(x, collapse=" ")
}


# fixing all of other  phrases before environment or ecology
for (i in 1:nrow(uw_courses)){
  desc = uw_courses$clean_course_desc[i]
  x = unlist(strsplit(desc, " "))
  if (length(x) < 6){ # start at the 6th word so no out of bounds errors 
    next
  }
  for (j in 6:length(x)){
    if (tolower(x[j]) == "environment" | tolower(x[j]) == "environments"){
      if (tolower(x[j-3]) == "design" && tolower(x[j-5]) == "consequences"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "rich" && tolower(x[j-2]) == "emotionally"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "care" && tolower(x[j-2]) == "health"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-2]) == "characters" && tolower(x[j-3]) == "creating"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "of" && tolower(x[j-2]) == "development"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "global" && tolower(x[j-2]) == "changing"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environment"){
      if (tolower(x[j-4]) == "international" && tolower(x[j-3]) == "trade"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "data" && tolower(x[j-2]) == "large"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environment"){
      if (tolower(x[j-1]) == "today's" && tolower(x[j-3]) == "faced"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "changing" && tolower(x[j-2]) == "under"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "time" && tolower(x[j-2]) == "real"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environment"){
      if (tolower(x[j-1]) == "physical" && tolower(x[j-2]) == "satisfying"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-1]) == "and" && tolower(x[j-2]) == "systems"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "environments"){
      if (tolower(x[j-2]) == "cross" && tolower(x[j-3]) == "platform"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "ecology"){
      if (tolower(x[j-1]) == "classroom"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "ecology"){
      if (tolower(x[j-2]) == "within" && tolower(x[j-3]) == "entertainment"){
        print(i)
        x[j] = "domain"
      }
    }
    
    if (tolower(x[j]) == "restoration"){
      if (tolower(x[j-2]) == "filtering" && tolower(x[j-3]) == "image"){
        print(i)
        x[j] = "repair"
      }
    }
    
  }
  uw_courses$clean_course_desc[i] = paste(x, collapse=" ")
}

#write new cleaned table as csv
write.csv(uw_courses, "uw_courses_cleaned.csv", row.names = F)

##mapping courses to SDG
classes <- read.csv("uw_courses_cleaned.csv")
cmu_usc_keywords <- read.csv("cmu_usc_pwg_mapped.csv")

all_sdg_keywords <- data.frame()
for (goal_num in 1:17) {
  print(goal_num) #useful for seeing how far you are in the code run
  classes %>%
    mutate(goal = goal_num, #run on clean_course_desc column w no punctuation and accuracy edits made
           keyword = tabulate_sdg_keywords(classes$clean_course_desc, goal_num, keywords = "cmu_usc")) %>%
    unnest(keyword) -> cur_sdg_keywords
  
  all_sdg_keywords <- rbind(all_sdg_keywords, cur_sdg_keywords) 
}

