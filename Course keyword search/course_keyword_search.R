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
uw_courses <- courses[!duplicated(courses[, c("Course.Id")]), ]

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

##RUN FUNCTION IN tabulate_sdg_keywords.R FIRST 
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

all_sdg_keywords_copy = all_sdg_keywords

all_sdg_keywords_copy <- all_sdg_keywords_copy %>%
  left_join(cmu_usc_keywords, by = c("goal", "keyword")) %>%
  select(Course.Id, course_title, Subject, Catalog.Number, Department, School.College, Sustainability, year, course_desc,keyword, goal, weight, color) %>%
  arrange(Course.Id)

write.csv(all_sdg_keywords_copy, "master_course_sdg_data.csv", row.names = F)
save(all_sdg_keywords_copy, file="all_sdg_keywords.Rda")


# function to filter data to data only with weight above certain threshold
filter = function(data, threshold){
  mini_df = data[data$weight > threshold, ]
  return(mini_df)
}

master_data = read.csv("master_course_sdg_data.csv")
filtered_data = filter(master_data, 0.2)
write.csv(filtered_data, "master_course_sdg_data_filtered.csv", row.names=F)

##Classifying which classes are sustainability related

# grab the unique class titles
classes = unique(uw_courses$course_title)
sustainability = data.frame(classes)
# create column to store goals that class maps to 
sustainability = sustainability %>% add_column(goals = NA)
# create column to store sustainability-relatedness
sustainability = sustainability %>% add_column(related = NA)
# criteria lists
social_economic_goals = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 16, 17)
environment_goals = c(13, 14, 15)


index = 1
for (class in sustainability$classes){
  # subset master data to just the rows for that class and grab the unique goals
  mini_df = unique(master_data[master_data$course_title == class, "goal"])
  # combine all the goals into a string to be added to the goals column in df
  goals = paste(mini_df, collapse=",")
  #update the goals column of df to be this string "goals"
  sustainability$goals[index] = goals
  index = index + 1
}


# now need to go through and check criteria and update the related column accordingly
for (i in 1:nrow(sustainability)){
  # first check if it is null
  if (sustainability$goals[i] == ""){
    sustainability$related[i] = "Not Related"
    next
  }
  # grab the goals in each row
  goals = as.list(strsplit(sustainability$goals[i], ",")[[1]])
  # set these booleans to false for each row to start
  is_social_economic = FALSE
  is_environment = FALSE
  for (j in 1:length(goals)){
    if (goals[j] %in% social_economic_goals){
      is_social_economic = TRUE
    }
    if (goals[j] %in% environment_goals){
      is_environment = TRUE
    }
  }
  # now we should know if there was at least one of the criteria present
  if (is_social_economic & is_environment){
    sustainability$related[i] = "Focused"
  }
  if (is_social_economic & !is_environment){
    sustainability$related[i] = "Inclusive"
  }
  if (!is_social_economic & is_environment){
    sustainability$related[i] = "Inclusive"
  }
}

# need to rename the columns
names(sustainability)[names(sustainability) == 'classes'] <- "course_title"
names(sustainability)[names(sustainability) == 'related'] <- "sustainability_classification"
names(sustainability)[names(sustainability) == 'goals'] <- "all_goals"

sum(sustainability$sustainability_classification == "Focused")
sum(sustainability$sustainability_classification == "Inclusive")
sum(sustainability$sustainability_classification == "Not Related")

write.csv(sustainability, "sustainability_related_courses_filtered.csv", row.names = F)

sum(uw_courses$Sustainability == "SUST")
