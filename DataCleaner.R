library(readxl) 
library(dplyr)
library(magrittr)
library(lubridate)
library(stringr)
library(pastecs)
library(glmnet)
library(randomForest)
library(tree)
library(ggplot2)
library(reshape2)

#function that will break down a string date
break_down_string_date = function(date){
  months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  day = "0"
  month = "0"
  year = "0"
  time = ""
  sub_date = date
  
  #find month
  for(i in 1:length(months)){
    if (grepl(months[i], date, ignore.case = TRUE)){
      month = i
      sub_date = gsub(months[i], "", sub_date)
    } 
  }
  if(month == "0"){
    month = month(now())
  }
  
  #find time
  time = str_extract(sub_date, "[0-9]{1,2}:[0-9]{1,2} [A|P]M")
  if(!is.na(time)){
    sub_date = gsub(time, "", sub_date)
  } else {
    time = ""
  }
  
  #find year
  year = str_extract(sub_date, "20[0-2][0-9]")
  if(!is.na(year)){
    sub_date = gsub(year, "", sub_date)
  } else {
    year = "2022"
  }
  
  #find day
  day = str_extract(sub_date, "[0-3]{0,1}[0-9]")
  if(!is.na(day)){
    sub_date = gsub(day, "", sub_date)
  } else{
    today = grepl("Today", sub_date, ignore.case = TRUE)
    yesertday = grepl("Yesterday", sub_date, ignore.case = TRUE)
    if (yesertday){
      day = day(now()) - 1
      
    } else {
      day = day(now())
    }
  }
  build = paste(year, "-", month, "-", day, sep = "")
  
  return(build)
}

#days in shelter
days_diff = function(d1, d2){
  return (difftime(date(d2), date(d1), units="days"))
}

#find the direct post
pet_spotlit = function(a_name, a_org, a_in, a_out, posts){
  #remove undesired posts
  posts_sub = subset(posts, org_name == a_org & a_in < formatted_date & formatted_date <  a_out )

  found = posts_sub %>% filter(grepl(a_name, text))
  return (found)
}

#units abbreviated
units_abbreviated = function(value){
  ret = value
  k = grepl("[0-9]K$", value, ignore.case = FALSE)
  if(k){
    value = gsub("K$", "", value, ignore.case = FALSE)
    base = str_extract(value, "[0-9]*.[0-9]{0,4}")
    ret = as.double(base) * 1000
    return (ret)
  }
  
  return (value)
}

#CCHS data cleaning
ClarkCountyHS = read_excel("CCHS/CCHS 2021 Data for analysis.xlsx")
ClarkCountyHS$org_name = "Clark County Humane Society"
ClarkCountyHS <- subset(ClarkCountyHS, OUTCOME != "Died")
ClarkCountyHS$month = month(ClarkCountyHS$`DATE EXIT`)
ClarkCountyHS$year = year(ClarkCountyHS$`DATE EXIT`)
ClarkCountyHS$`DAYS AT SHELTER` = difftime(ClarkCountyHS$`DATE EXIT`,ClarkCountyHS$`DATE BROUGHT IN`, units = "days")

#ECCHA data cleaning
EauClaireHA = read_excel("ECHA/AnimalIntakeWithResults_12.2019-12.2021.xlsx")
EauClaireHA$`Outcome Date` = as.POSIXlt(EauClaireHA$`Outcome Date`,format = "%m/%d/%Y")
EauClaireHA$`Intake Date` = as.POSIXlt(EauClaireHA$`Intake Date`,format = "%m/%d/%Y")
EauClaireHA$month = month(EauClaireHA$`Outcome Date`)
EauClaireHA$year = year(EauClaireHA$`Outcome Date`)
EauClaireHA$org_name = "Eau Claire County Humane Association"
EauClaireHA$stay_length = (difftime(date(EauClaireHA$`Outcome Date`)
                                    , date(EauClaireHA$`Intake Date`), units="days"))

#CHA data cleaning
ChequamegonHA_2019 = read_excel("CHA/2019 Data.xlsx")
colnames(ChequamegonHA_2019) <- c('THEDATE', 'REASON', 'SHELTERCODE', 'ANIMALNAME', 'ANIMALTYPENAME', 'SPECIESNAME', 'ANIMALAGE', 'SEXNAME', 'LAST LOCATION', 'CATEGORYNAME', 'OUTORIN')
cha_2019_in = ChequamegonHA_2019[ChequamegonHA_2019$OUTORIN == "Animals In",]
cha_2019_out = ChequamegonHA_2019[ChequamegonHA_2019$OUTORIN == "Animals Out",]
ChequamegonHA_2020 = read_excel("CHA/2020 Data.xlsx")
cha_2020_in = ChequamegonHA_2020[ChequamegonHA_2020$OUTORIN == "Animals In",]
cha_2020_out = ChequamegonHA_2020[ChequamegonHA_2020$OUTORIN == "Animals Out",]
ChequamegonHA_2021 = read_excel("CHA/2021 Data.xlsx")
cha_2021_in = ChequamegonHA_2021[ChequamegonHA_2021$OUTORIN == "Animals In",]
cha_2021_out = ChequamegonHA_2021[ChequamegonHA_2021$OUTORIN == "Animals Out",]

cha_in = rbind(cha_2019_in, cha_2020_in, cha_2021_in)
cha_in <- subset (cha_in, select = -OUTORIN)

cha_out = rbind(cha_2019_out, cha_2020_out, cha_2021_out)
cha_out <- subset (cha_out, select = -OUTORIN)

cha_adopt = merge(cha_in, cha_out, by="SHELTERCODE")
cha_adopt = subset(cha_adopt, THEDATE.x < THEDATE.y)

identify_ids = subset(cha_adopt, CATEGORYNAME.y == "Animals Euthanised")
remove_ids = unique(identify_ids$SHELTERCODE)
cha_adopt = subset(cha_adopt, !(SHELTERCODE %in% remove_ids) )

cha_adopt = subset(cha_adopt, CATEGORYNAME.y == "Animals Adopted" | CATEGORYNAME.y == "Animals Reclaimed")
cha_adopt$month = month(cha_adopt$THEDATE.y)
cha_adopt$year = year(cha_adopt$THEDATE.y)
cha_adopt$org_name = "Chequamegon Humane Association"
cha_adopt$stay_length = (difftime(cha_adopt$THEDATE.y, cha_adopt$THEDATE.x, units="days"))

#pet finder data cleaning
petfinder_All = read.csv("petfinder_results.csv")

#name petfinder orgs
petfinder_All$org_name = ""

petfinder_All$org_name[which(petfinder_All$organization_id == "WI325")] = "Jackson County Animal Shelter"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI335")] = "Taylor County Humane Society"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI03")] = "Rusk County Animal Shelter"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI07")] = "Chippewa Humane Association"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI46")] = "Dunn County Humane Society"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI25")] = "Washburn County Area Humane Society"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI61")] = "Humane Society of Douglas County"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI469")] = "St Croix Animal Friends"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI96")] = "Buffalo County Humane Association"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI246")] = "Trempealeau County Humane Society"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI128")] = "Northwoods Humane Society"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI09")] = "Peppin County Humane Society"
petfinder_All$org_name[which(petfinder_All$organization_id == "WI22")] = "Humane Society of Baron County"

#find the animals over the past 3 years
desired_petfinder = petfinder_All[date(petfinder_All$published_at) >= date("2020-01-01") & date(petfinder_All$published_at) <= date("2021-12-31"),]

#build time at shelter
desired_petfinder$status_changed_at = as.POSIXlt(desired_petfinder$status_changed_at)
desired_petfinder$published_at = as.POSIXlt(desired_petfinder$published_at)

desired_petfinder$stay_length = (difftime(date(desired_petfinder$status_changed_at), date(desired_petfinder$published_at), units="days"))

#remove pets entered as adopted
desired_petfinder = subset(desired_petfinder, (stay_length > 0 & status == "adopted") | status == "adoptable")

#indicate month and year in variables
desired_petfinder$month = month(desired_petfinder$status_changed_at)
desired_petfinder$year = year(desired_petfinder$status_changed_at)

desired_petfinder %>% 
  group_by(org_name, status) %>% 
  summarise(X = length(unique(X))) %>%
  arrange(org_name, status)

#pull in facebook posts and clean them
bcha_fb = read.csv("facebook data/Buffalo County Humane Association.csv")
bcha_fb$formatted_date = apply(bcha_fb[c(2)], 1, break_down_string_date)
bcha_fb$Image = ""
colnames(bcha_fb) <- c('org_name', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date', 'image')
bcha_fb$org_name = "Buffalo County Humane Association"

cha_fb = read.csv("facebook data/Chequamegon Humane Association - Home.csv")
cha_fb$formatted_date = apply(cha_fb[c(2)], 1, break_down_string_date)
cha_fb$Image = ""
colnames(cha_fb) <- c('org_name', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date', 'image')
cha_fb$org_name = "Chequamegon Humane Association"

cch_fb = read.csv("facebook data/Chippewa Humane - Home.csv")
cch_fb$formatted_date = apply(cch_fb[c(3)], 1, break_down_string_date)
colnames(cch_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
cch_fb$org_name = "Chippewa Humane Association"

cchs_fb = read.csv("facebook data/Clark County Humane Society.csv")
cchs_fb$formatted_date = apply(cchs_fb[c(2)], 1, break_down_string_date)
colnames(cchs_fb) <- c('org_name', 'post_date', 'image', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
cchs_fb$org_name = "Clark County Humane Society"

dchs_fb = read.csv("facebook data/Dunn County Humane Society.csv")
dchs_fb$formatted_date = apply(dchs_fb[c(3)], 1, break_down_string_date)
dchs_fb <- subset (dchs_fb, select = -Text_Copy)
colnames(dchs_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
dchs_fb$org_name = "Dunn County Humane Society"

eccha_fb = read.csv("facebook data/Eau Claire County Humane Association - ECCHA.csv")
eccha_fb$formatted_date = apply(eccha_fb[c(8)], 1, break_down_string_date)
colnames(eccha_fb) <- c('org_name', 'title', 'image', 'text', 'likes', 'comments', 'shares', 'post_date', 'formatted_date')
eccha_fb$org_name = "Eau Claire County Humane Association"

hsbc_fb = read.csv("facebook data/Humane Society of Baron County.csv")
hsbc_fb$formatted_date = apply(hsbc_fb[c(2)], 1, break_down_string_date)
colnames(hsbc_fb) <- c('org_name', 'post_date', 'image', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
hsbc_fb$org_name = "Humane Society of Baron County"

hsdc_fb = read.csv("facebook data/Humane Society of Douglas County Wisconsin.csv")
hsdc_fb$formatted_date = apply(hsdc_fb[c(3)], 1, break_down_string_date)
hsdc_fb <- subset (hsdc_fb, select = -Text_Copy)
colnames(hsdc_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
hsdc_fb$org_name = "Humane Society of Douglas County"

jcas_fb = read.csv("facebook data/Jackson County Animal Shelter.csv")
jcas_fb$formatted_date = apply(jcas_fb[c(3)], 1, break_down_string_date)
jcas_fb <- subset (jcas_fb, select = -Text_Copy)
colnames(jcas_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
jcas_fb$org_name = "Jackson County Animal Shelter"

nwhs_fb = read.csv("facebook data/Northwoods Humane Society - Sawyer County.csv")
nwhs_fb$formatted_date = apply(nwhs_fb[c(3)], 1, break_down_string_date)
nwhs_fb <- subset (nwhs_fb, select = -Text_Copy)
colnames(nwhs_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
nwhs_fb$org_name = "Northwoods Humane Society"

pchs_fb = read.csv("facebook data/Pepin County Humane Society.csv")
pchs_fb$formatted_date = apply(pchs_fb[c(3)], 1, break_down_string_date)
pchs_fb <- subset (pchs_fb, select = -Text_Copy)
colnames(pchs_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
pchs_fb$org_name = "Peppin County Humane Society"

rcas_fb = read.csv("facebook data/Rusk County Animal Shelter.csv")
rcas_fb$formatted_date = apply(rcas_fb[c(3)], 1, break_down_string_date)
rcas_fb <- subset (rcas_fb, select = -Text_Copy)
colnames(rcas_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
rcas_fb$org_name = "Rusk County Animal Shelter"

tachs_fb = read.csv("facebook data/Taylor County Humane Society.csv")
tachs_fb$formatted_date = apply(tachs_fb[c(3)], 1, break_down_string_date)
colnames(tachs_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
tachs_fb$org_name = "Taylor County Humane Society"

trchs_fb = read.csv("facebook data/Trempealeau County Humane Society.csv")
trchs_fb$formatted_date = apply(trchs_fb[c(3)], 1, break_down_string_date)
colnames(trchs_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
trchs_fb$org_name = "Trempealeau County Humane Society"

wcahs_fb= read.csv("facebook data/Washburn County Area Humane Society.csv")
wcahs_fb$formatted_date = apply(wcahs_fb[c(3)], 1, break_down_string_date)
colnames(wcahs_fb) <- c('org_name', 'image', 'post_date', 'title', 'text', 'likes', 'comments', 'shares', 'formatted_date')
wcahs_fb$org_name = "Washburn County Area Humane Society"

#single facebook dataframe
facebook_posts = rbind(bcha_fb, cch_fb, cchs_fb, cha_fb, dchs_fb, eccha_fb, hsbc_fb, hsdc_fb, jcas_fb, nwhs_fb, pchs_fb, rcas_fb, tachs_fb, trchs_fb, wcahs_fb)

#only between 2020 and 2022
desired_fb = facebook_posts[date(facebook_posts$formatted_date) >= date("2020-01-01") & date(facebook_posts$formatted_date) <= date("2021-12-31") ,]

#date and month for summary
desired_fb$year = year(date(desired_fb$formatted_date))
desired_fb$month = month(date(desired_fb$formatted_date))

#remove words from some fields
desired_fb$comments = gsub(" Comment.?", "", desired_fb$comments)
desired_fb$shares = gsub(" Share.?", "", desired_fb$shares)

#k to 1000's
desired_fb$likes = apply(desired_fb[c(5)], 1, units_abbreviated)
desired_fb$comments = apply(desired_fb[c(6)], 1, units_abbreviated)
desired_fb$shares = apply(desired_fb[c(7)], 1, units_abbreviated)

#change text to int
desired_fb$likes[which(desired_fb$likes == '' | is.na(desired_fb$likes))] = 0
desired_fb$comments[which(desired_fb$comments == '' | is.na(desired_fb$comments))] = 0
desired_fb$shares[which(desired_fb$shares == '' | is.na(desired_fb$shares))] = 0

desired_fb$likes = as.integer(desired_fb$likes)
desired_fb$comments = as.integer(desired_fb$comments)
desired_fb$shares = as.integer(desired_fb$shares)

#add index
desired_fb$index = seq.int(nrow(desired_fb))

##all pets
#posts by month/year
post_agg <- desired_fb %>% 
  group_by(year, month) %>% 
  summarise(posts = n(),
            likes = sum(likes, na.rm = TRUE),
            comments = sum(comments, na.rm = TRUE),
            shares = sum(shares, na.rm = TRUE)
            ) %>%
  arrange(year, month)

# adoptions by month/year
adopted_desired_petfinder = desired_petfinder[desired_petfinder$status == 'adopted', c("org_name", "month", "year", "stay_length", "name", "published_at", "status_changed_at", "species")]
adopted_desired_clark = ClarkCountyHS[,c("org_name", "month", "year", "DAYS AT SHELTER", "ANIMAL NAME", "DATE BROUGHT IN", "DATE EXIT", "SPECIES")]
adopted_desired_eauclaire = EauClaireHA [,c("org_name", "month", "year", "stay_length", "Animal Name", "Intake Date", "Outcome Date", "Species")]
adopted_desired_Chequamegon = cha_adopt[,c("org_name", "month", "year", "stay_length", "ANIMALNAME.x", "THEDATE.x", "THEDATE.y", "SPECIESNAME.x")]

colnames(adopted_desired_petfinder) <- c("org_name", "month", "year", "stay_length", "name", "in date", "out date", "species")
colnames(adopted_desired_clark) <- c("org_name", "month", "year", "stay_length", "name", "in date", "out date", "species")
colnames(adopted_desired_eauclaire) <- c("org_name", "month", "year", "stay_length", "name", "in date", "out date", "species")
colnames(adopted_desired_Chequamegon) <- c("org_name", "month", "year", "stay_length", "name", "in date", "out date", "species")

adopted_desired = dplyr::bind_rows(adopted_desired_petfinder, adopted_desired_clark, adopted_desired_eauclaire, adopted_desired_Chequamegon)
adopted_desired = subset(adopted_desired, (year == 2020 | year == 2021) & stay_length < 500)
adopted_desired$index = seq.int(nrow(adopted_desired))
adopted_agg <- adopted_desired %>%
  group_by(year, month) %>%
  summarise(adoptions = n(),
            average_stay = mean(as.integer(stay_length), na.rm = TRUE),
            stay_deviation = sd(as.integer(stay_length), na.rm = TRUE)
            ) %>%
  arrange(year, month)

all_agg = merge(post_agg, adopted_agg, by=c("year", "month"))

#posts by shelter
post_shelters <- desired_fb %>%
  group_by(org_name) %>%
  summarise(posts = n(),
            first_post = min(date(formatted_date)),
            last_post = max(date(formatted_date))
            ) %>%
  arrange(org_name)

#adoptions by shelter
adoption_shelters <- adopted_desired %>%
  group_by(org_name) %>%
  summarise(posts = n()) %>%
  arrange(org_name)

###direct_adoption results
##ClarkCountyHS
#date and month for summary
cchs_fb_cleaned = subset(desired_fb, org_name == "Clark County Humane Society")

#posts by month/year
cchs_fb_summarised <- cchs_fb_cleaned %>% 
  group_by(year, month) %>% 
  summarise(posts = n(),
            likes = sum(likes, na.rm = TRUE),
            comments = sum(comments, na.rm = TRUE),
            shares = sum(shares, na.rm = TRUE)
  ) %>%
  arrange(year, month)

#pet adoptions
adopted_desired_clark = adopted_desired_clark[adopted_desired_clark$year == 2020 | adopted_desired_clark$year == 2021,]
cchs_adopted_ag <- adopted_desired_clark %>%
  group_by(year, month) %>%
  summarise(adoptions = n(),
            average_stay = mean(as.integer(stay_length), na.rm = TRUE),
            stay_deviation = sd(as.integer(stay_length), na.rm = TRUE)
  ) %>%
  arrange(year, month)

cchs_agg = merge(cchs_fb_summarised, cchs_adopted_ag, by=c("year", "month"))

##Eau Claire HA
eccha_fb_cleaned = subset(desired_fb, org_name == "Eau Claire County Humane Association")

#posts by month/year
eccha_fb_summarised <- eccha_fb_cleaned %>% 
  group_by(year, month) %>% 
  summarise(posts = n(),
            likes = sum(likes, na.rm = TRUE),
            comments = sum(comments, na.rm = TRUE),
            shares = sum(shares, na.rm = TRUE)
  ) %>%
  arrange(year, month)

#pet adoptions
adopted_desired_eauclaire = adopted_desired_eauclaire[adopted_desired_eauclaire$year == 2020 | adopted_desired_eauclaire$year == 2021,]
eccha_adopted_ag <- adopted_desired_eauclaire %>%
  group_by(year, month) %>%
  summarise(adoptions = n(),
            average_stay = mean(as.integer(stay_length), na.rm = TRUE),
            stay_deviation = sd(as.integer(stay_length), na.rm = TRUE)
  ) %>%
  arrange(year, month)

eccha_agg = merge(eccha_fb_summarised, eccha_adopted_ag, by=c("year", "month"))

##Chequamegon HA
cha_fb_cleaned = subset(desired_fb, org_name == "Chequamegon Humane Association")

#posts by month/year
cha_fb_summarised <- cha_fb_cleaned %>% 
  group_by(year, month) %>% 
  summarise(posts = n(),
            likes = sum(likes, na.rm = TRUE),
            comments = sum(comments, na.rm = TRUE),
            shares = sum(shares, na.rm = TRUE)
  ) %>%
  arrange(year, month)

#pet adoptions
adopted_desired_Chequamegon = adopted_desired_Chequamegon[adopted_desired_Chequamegon$year == 2020 | adopted_desired_Chequamegon$year == 2021,]
cha_adopted_ag <- adopted_desired_Chequamegon %>%
  group_by(year, month) %>%
  summarise(adoptions = n(),
            average_stay = mean(as.integer(stay_length), na.rm = TRUE),
            stay_deviation = sd(as.integer(stay_length), na.rm = TRUE)
  ) %>%
  arrange(year, month)

cha_agg = merge(cha_fb_summarised, cha_adopted_ag, by=c("year", "month"))

###output all data set for agg sets

write.csv(all_agg,"output/aggregated_results.csv")

write.csv(cchs_agg,"output/clark_county_aggregated_results.csv")

write.csv(eccha_agg,"output/eau_claire_county_aggregated_results.csv")

write.csv(cha_agg,"output/chequamegon_county_aggregated_results.csv")

## output tables for steps

write.csv(adoption_shelters, "output/adoptions_by_shelters.csv")

write.csv(post_shelters, "output/posts_by_shelter.csv")

###simple least squares regression over different groupings

all_lm = lm(adoptions ~ likes + comments + shares, data = all_agg)
summary(all_lm)

cchs_lm = lm(adoptions ~ likes + comments + shares, data = cchs_agg)
summary(cchs_lm)

eccha_lm = lm(adoptions ~ likes + comments + shares, data = eccha_agg)
summary(eccha_lm)

cha_lm = lm(adoptions ~ likes + comments + shares, data = cha_agg)
summary(cha_lm)

### identify spotlights

#adoption updates
adopted_desired$spotlit = FALSE
adopted_desired$spotlights = 0
adopted_desired$post_to_exit_length = 0
adopted_desired$`out date` = as.POSIXlt(adopted_desired$`out date`, format = "%Y-%m-%d")
adopted_desired$`in date` = as.POSIXlt(adopted_desired$`in date`, format = "%Y-%m-%d")
adopted_desired$name = toupper(adopted_desired$name)
adopted_desired$species = toupper(adopted_desired$species)

other_species = rep(FALSE, length(adopted_desired$species))
other_species[which(adopted_desired$species == "DOG" | adopted_desired$species == "CAT")] = TRUE
adopted_desired$species[which(!other_species)] = "OTHER"

adopted_desired$species = as.factor(adopted_desired$species)

#fb post updates
desired_fb$text = toupper(desired_fb$text)
desired_fb$spotlight = FALSE
desired_fb$spotlit_id = 0
desired_fb$spotlight_species = ""
desired_fb$image = desired_fb$image != ''

for(i in 1:nrow(adopted_desired)){
  c_pet = adopted_desired[i,]
  
  spots = pet_spotlit(c_pet$name, c_pet$org_name, c_pet$`in date`, c_pet$`out date`, desired_fb)
  
  if (nrow(spots) >= 1){
    adopted_desired$spotlit[i] = TRUE
    adopted_desired$spotlights[i] = paste(spots[,12], collapse=',')
    for (n in spots[,12]){
      desired_fb$spotlight[n] = TRUE
      desired_fb$spotlit_id[n] = i
      desired_fb$spotlight_species[n] = c_pet$species
    }
  }
}

# post by post type
post_any <- desired_fb %>% 
  summarize(posts = n(),
            likes_mean = mean(likes, na.rm = TRUE),
            likes_sd = sd(likes, na.rm = TRUE),
            comments_mean = mean(comments, na.rm = TRUE),
            comments_sd = sd(comments, na.rm = TRUE),
            shares_mean = mean(shares, na.rm = TRUE),
            shares_sd = sd(shares, na.rm = TRUE),
            images = sum(image, na.rm = TRUE)
  )

post_spotlit <- desired_fb %>%
  group_by(spotlight) %>% 
  summarize(posts = n(),
            likes_mean = mean(likes, na.rm = TRUE),
            likes_sd = sd(likes, na.rm = TRUE),
            comments_mean = mean(comments, na.rm = TRUE),
            comments_sd = sd(comments, na.rm = TRUE),
            shares_mean = mean(shares, na.rm = TRUE),
            shares_sd = sd(shares, na.rm = TRUE)
  )

post_spotlights_images <- desired_fb %>% 
  group_by(spotlight, image) %>% 
  summarize(posts = n(),
            likes_mean = mean(likes, na.rm = TRUE),
            likes_sd = sd(likes, na.rm = TRUE),
            comments_mean = mean(comments, na.rm = TRUE),
            comments_sd = sd(comments, na.rm = TRUE),
            shares_mean = mean(shares, na.rm = TRUE),
            shares_sd = sd(shares, na.rm = TRUE)
  ) 

post_spotlights_species <- desired_fb %>% 
  group_by(spotlight, spotlight_species) %>% 
  summarize(posts = n(),
            likes_mean = mean(likes, na.rm = TRUE),
            likes_sd = sd(likes, na.rm = TRUE),
            comments_mean = mean(comments, na.rm = TRUE),
            comments_sd = sd(comments, na.rm = TRUE),
            shares_mean = mean(shares, na.rm = TRUE),
            shares_sd = sd(shares, na.rm = TRUE),
            images = sum(image, na.rm = TRUE)
  ) 



#pets to post
only_spotlit = subset(desired_fb, spotlight = TRUE)
only_spotlit = merge(only_spotlit, adopted_desired, by.x = "spotlit_id", by.y = "index")

only_spotlit$post_to_exit_length = difftime(only_spotlit$`out date`, only_spotlit$formatted_date, units = "days")

#all posts

adopted_days <- only_spotlit %>%
  group_by(spotlit, species) %>%
  summarize(animals = n(),
            stay_mean = mean(stay_length, na.rm = TRUE),
            stay_sd = sd(stay_length, na.rm = TRUE),
            spotlight_to_exit_mean = mean(post_to_exit_length, na.rm = TRUE),
            spotlight_to_exit_sd = sd(post_to_exit_length, na.rm = TRUE),
            unique_animals = n_distinct(spotlit_id)
  )

adopted_days_all <- adopted_desired %>%
  group_by(spotlit, species) %>%
  summarize(animals = n(),
            stay_mean = mean(stay_length, na.rm = TRUE),
            stay_sd = sd(stay_length, na.rm = TRUE),
  )

adopted_org <- only_spotlit %>%
  group_by(spotlit, org_name.x) %>%
  summarize(posts = n(),
            stay_mean = mean(stay_length, na.rm = TRUE),
            stay_sd = sd(stay_length, na.rm = TRUE),
            spotlight_to_exit_mean = mean(post_to_exit_length, na.rm = TRUE),
            spotlight_to_exit_sd = sd(post_to_exit_length, na.rm = TRUE),
            unique_animals = n_distinct(spotlit_id)
  )

adopted_org_all <- adopted_desired %>%
  group_by(spotlit, org_name) %>%
  summarize(animals = n(),
            stay_mean = mean(stay_length, na.rm = TRUE),
            stay_sd = sd(stay_length, na.rm = TRUE),
  )

options(scipen=100)
options(digits=5)
stat.desc(only_spotlit$stay_length)

#export descriptive
write.csv(post_any,"output/descriptive_any_post.csv")
write.csv(post_spotlit,"output/descriptive_spotlight_post.csv")
write.csv(post_spotlights_images, "output/descriptive_image_post.csv")
write.csv(post_spotlights_species, "output/descriptive_species_post.csv")
write.csv(adopted_days, "output/descriptive_adoptions.csv")
write.csv(adopted_days_all, "output/descriptive_adoptions_all.csv")
write.csv(adopted_org, "output/descriptive_adoptions_org.csv")
write.csv(adopted_org_all, "output/descriptive_adoptions_org_all.csv")

#random forest
set.seed(400, sample.kind = "Rounding")
spotlight_tree = randomForest(post_to_exit_length~likes + comments + shares + image + species, data=only_spotlit) 

importance(spotlight_tree)
varImpPlot(spotlight_tree)

min_nodes = which.min(spotlight_tree$mse) * 2

plot(spotlight_tree)

#training data
set.seed(13, sample.kind = "Rounding")
train = sample(1:dim(only_spotlit)[1], min_nodes, replace=F)

#tree
aacTree = tree(post_to_exit_length~likes + comments + shares + as.factor(image) + species, data=only_spotlit[train,])
summary(aacTree)

plot(aacTree)
text(aacTree, pretty=0)

#predict
std_post_to_exit = sd(only_spotlit$post_to_exit_length)
predict_results = only_spotlit[-train,c(1,26)]

predict_results$predicted_days = predict(spotlight_tree, new_data=only_spotlit[-train,])[-train]

predict_results$predicted_diff = abs(predict_results$predicted_days - predict_results$post_to_exit_length)

predict_results$succesful = FALSE
predict_results$succesful[predict_results$predicted_diff < std_post_to_exit] = TRUE

sum(predict_results$succesful, na.rm = FALSE) / dim(predict_results)[1]

predict_results$succesful_hard = FALSE
predict_results$succesful_hard[predict_results$predicted_diff < (std_post_to_exit/2)] = TRUE

sum(predict_results$succesful_hard, na.rm = FALSE) / dim(predict_results)[1]

#days saved
std_spotlit_animal_cat = sd(only_spotlit$post_to_exit_length[only_spotlit$species == "CAT"])
std_spotlit_animal_dog = sd(only_spotlit$post_to_exit_length[only_spotlit$species == "DOG"])
std_spotlit_animal_other = sd(only_spotlit$post_to_exit_length[only_spotlit$species == "OTHER"])

std_adopted_animal_cat = sd(only_spotlit$stay_length[only_spotlit$species == "CAT"])
std_adopted_animal_dog = sd(only_spotlit$stay_length[only_spotlit$species == "DOG"])
std_adopted_animal_other = sd(only_spotlit$stay_length[only_spotlit$species == "OTHER"])

only_spotlit$predicted_savings = 0
only_spotlit$predicted_savings[only_spotlit$species=="CAT"] = (std_adopted_animal_cat - std_spotlit_animal_cat) - only_spotlit$post_to_exit_length
only_spotlit$predicted_savings[only_spotlit$species=="DOG"] = (std_adopted_animal_dog - std_spotlit_animal_dog) - only_spotlit$post_to_exit_length
only_spotlit$predicted_savings[only_spotlit$species=="OTHER"] = (std_adopted_animal_other - std_spotlit_animal_other) - only_spotlit$post_to_exit_length

ggplot(data = only_spotlit, aes(x=predicted_savings, fill=species)) +
  geom_histogram(color="#e9ecef", position = 'identity', bins = 25)  + 
  geom_vline(aes(xintercept = median(only_spotlit$predicted_savings), colour = "red"), size = 1) +
  scale_fill_manual(values=c("#090934", "#2E2FE3", "#FCBA12")) +
  labs(title = "Histogram of Estimated Days Reduced Before Predicted Adoption Date", x = "Estimated Days Reduced in Shelter", y = "Number of Spotlight Posts", color = "median") +
  xlim(-100, NA)

median(only_spotlit$predicted_savings)
median(only_spotlit$predicted_savings[only_spotlit$species == "CAT"])
median(only_spotlit$predicted_savings[only_spotlit$species == "DOG"])
median(only_spotlit$predicted_savings[only_spotlit$species == "OTHER"])

#spotlights correlated to adoptions

post_correlation <- desired_fb %>% 
  group_by(year, month) %>% 
  summarise(posts = n(),
            spotlights = sum(spotlight, na.rm = TRUE)
  ) %>%
  arrange(year, month)

adoption_correlation <- adopted_desired %>%
  group_by(year, month) %>%
  summarise(adoptions = n(),
            average_stay = mean(as.integer(stay_length), na.rm = TRUE),
            stay_deviation = sd(as.integer(stay_length), na.rm = TRUE)
  ) %>%
  arrange(year, month)

correlation_agg = merge(post_correlation, adoption_correlation, by=c("year", "month"))

write.csv(correlation_agg, "output/correlation_agg.csv")

posts_lm = lm(average_stay ~ posts, data = correlation_agg)
summary(posts_lm)

spotlights_lm = lm(average_stay ~ spotlights, data = correlation_agg)
summary(spotlights_lm)