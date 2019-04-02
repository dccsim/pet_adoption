# Pet adoption speed prediction
#
# Goal: Predict how quickly a pet listed online will be adopted or at all

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tictoc)) install.packages("tictoc", repos = "http://cran.us.r-project.org")

# Source of data set: Kaggle - PetFinder.my Adoption Prediction
# https://www.kaggle.com/c/petfinder-adoption-prediction

#############################################################
#
# Set environment and load data
#
#############################################################

# set working dir to same folder the script:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# place data files in same folder as script: 
# breed_labels.csv, state_labels.csv, color_labels.csv, train.csv 

# breed_labels:   BreedID	Type	BreedName
df_breed <- read.csv(file='./breed_labels.csv', header=TRUE, sep=",")

# color_labels:   ColorID	ColorName
df_color <- read.csv(file='./color_labels.csv', header=TRUE, sep=",")  

# state_labels:   StateID	StateName
df_state <- read.csv(file='./state_labels.csv', header=TRUE, sep=",")  

# data set:   Type	Name	Age	Breed1	Breed2	Gender	Color1	Color2	Color3	MaturitySize	FurLength	Vaccinated	Dewormed	
#             Sterilized	Health	Quantity	Fee	State	RescuerID	VideoAmt	Description	PetID	PhotoAmt	AdoptionSpeed
df_pets <- read.csv(file='./pets.csv', header=TRUE, sep=",") 

# flip the adoption speed
# 0 --> 4, 1 --> 3, 2 (unchanged), 3 --> 1, 4 --> 0
df_pets <- df_pets %>% 
  mutate(AdoptionSpeed = max(AdoptionSpeed)-AdoptionSpeed)

#############################################################
#
# Clean data and new data dimension
#
#############################################################

# Clean up: Breed data
#
# Notation: New data column names have underscore instead of camelcase
# e.g. Breed_Mix, Breed_Combine
#
# df data set contains information about pet breed in columns Breed1 and Breed2 
# df_breeds data set contains BreedLabels dictionary 
#
# There are 307 breed types for all dogs and cats including "Mixed Breed"
tail(df_breed %>% arrange(BreedID))
head(df_pets %>% select(PetID, Breed1, Breed2), n=10)

# Mixed Breed is denoted by BreedID=307 
df_breed %>% filter(grepl('mix', BreedName, ignore.case = TRUE))

# Mixed breed can be listed in either Breed1 or Breed2, or both
head(df_pets %>% filter(Breed1==307) %>% select(PetID, Breed1, Breed2))
head(df_pets %>% filter(Breed2==307) %>% select(PetID, Breed1, Breed2))

# There is no record of both Breed1 == 0 and Breed2 == 0. Hence, data set is clean in this aspect
# Breed1 == 0 & Breed2 == 0 : 0 rows
df_pets %>% filter(Breed1 == 0 & Breed2 == 0) %>% select(PetID, Breed1, Breed2)

# Pets listed with Breed1 != 0 and Breed2 ==0 indicate pure breed, unless Breed1==307 (mixed)
# Breed1 != 0 & Breed2 == 0 : 10512 rows
df_pets %>% filter(Breed1 != 0 & Breed2 == 0) %>% select(PetID, Breed1, Breed2) %>% head()

# There are pets listed with BreedID==0 
# Breed1 == 0 & Breed2 != 0 : 5 rows
df_pets %>% filter(Breed1 == 0 & Breed2 != 0) %>% select(PetID, Breed1, Breed2) %>% head()

# data cleaning - there are 5 rows with Breed1 (primary breed) marked as 0 (unknown),
# and Breed2 (secondary breed) != 0 denoting a particular breed in the data set.

# this has to be corrected by flipping the values of Breed1 and Breed2 
df_pets <- df_pets %>% 
  mutate(Breed1_temp=ifelse((Breed1==0),Breed2,Breed1),
         Breed2_temp=ifelse((Breed1==0),0,Breed2),
         Breed1=Breed1_temp,
         Breed2=Breed2_temp)
# check: no more rows with Breed1==0
df_pets %>% filter(Breed1==0) %>% select(PetID, Breed1, Breed2)
# drop temporary columns
df_pets <- within(df_pets, rm('Breed1_temp','Breed2_temp'))

# there are also pets listed as having Breed1 (primary breed) == Breed2 (secondary breed)
# in such case, where Breed1 == Breed2, set Breed2 == 0
# after that, set Breed_Mix = Mixed if Breed2 != 0 or Breed1/Breed2 == 307; else Pure
df_pets %>% filter(Breed1==Breed2) %>% select(PetID, Breed1, Breed2) %>% head(n=10)
df_pets <- df_pets %>%
  mutate(Breed2=ifelse((Breed1==Breed2),0,Breed2),
         Breed_Mix = ifelse((Breed2!=0 | Breed1==307 | Breed2==307), 'Mixed', 'Pure'))
# check: no rows with Breed1==Breed2
df_pets %>% filter(Breed1==Breed2) %>% select(PetID, Breed1, Breed2)

# define breed labels
label_breed <- as.character(df_breed$BreedName)

# combine Breed1 and Breed2 with name
df_pets <- df_pets %>%
  mutate(Breed1_Name = ifelse((Breed1!=0),label_breed[Breed1],NA),
         Breed2_Name = ifelse((Breed2!=0),label_breed[Breed2],NA)) %>%
  rowwise() %>%
  mutate(Breed_Combined = toString(sort(c(unique(c(Breed1_Name, Breed2_Name))))))
df_pets %>% select(Breed1, Breed2, Breed1_Name, Breed2_Name, Breed_Combined) %>% head(n=10)

# df_state: set StateID from int to character
# str(df_state)
# df_state <- df_state %>% mutate(StateID=as.character(StateID))

# add word count of Description
df_pets <- df_pets %>% 
  mutate(Description = as.character(Description),
         Desc_WordCount = sapply(strsplit(Description, " "), length))

# df_pets data structure
str(df_pets)
ncol(df_pets)
nrow(df_pets)

# Data Clean - completed

#############################################################
#
# Explore data
#
#############################################################

# quick look at the data
as_tibble(head(df_pets))
head(df_breed)
head(df_color)
head(df_state)

# plot parameters
alpha_bar <- 0.8
alpha_point <- 0.5

# plot of adoption speed
#
# Pet adoption speed is defined by how quickly, if at all, a pet is adopted
# The values are determined in the following way: 
# 4 - Pet was adopted on the same day as it was listed. 
# 3 - Pet was adopted between 1 and 7 days (1st week) after being listed. 
# 2 - Pet was adopted between 8 and 30 days (1st month) after being listed. 
# 1 - Pet was adopted between 31 and 90 days (2nd & 3rd month) after being listed. 
# 0 - No adoption after 100 days of being listed. (There are no pets in this dataset that waited between 90 and 100 days).
adoption_speed_desc <- c('0'='No adoption\n after 100 days',
                         '1'='31-90 Days',
                         '2'='8-30 Days',
                         '3'='1-7 Days', 
                         '4'='Same Day')

# plot adoption speed
adoption_speed_count <- df_pets %>%
  group_by(AdoptionSpeed) %>%
  summarise(Count = n()) %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed),
         Rate=Count/nrow(df_pets)) %>%
  select(AdoptionSpeed, Count, Rate) %>%
  arrange(desc(AdoptionSpeed))
adoption_speed_count
adoption_speed_count %>%
  ggplot(aes(x=AdoptionSpeed, y=Count)) +
  geom_bar(stat = "identity", alpha=.8, fill=rainbow(n=length(adoption_speed_count$AdoptionSpeed))) +
  ggtitle('Adoption Speed') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() 

# define species label: 1=Dog, 2=Cat
label_species <- c('Dog', 'Cat')

# there are slightly less cats listed for adoption compared to dogs
species_count <- df_pets %>%
  group_by(Type) %>%
  summarise(Count = n()) %>%
  mutate(Species = label_species[Type]) %>%
  select(Species, Count) 
species_count
dog_cat_ratio <- species_count[species_count$Species=='Dog',]$Count/species_count[species_count$Species=='Cat',]$Count
cat("dog to cat ratio: ", dog_cat_ratio)
dog_cat_pct <- round((dog_cat_ratio - 1) * 100, 1)
caption_dog_cat <- paste('There are ', dog_cat_pct, '% more dogs than cats', sep='')
caption_dog_cat

# plot of species count: 1=Dog, 2=Cat
pet_species <- df_pets %>% 
  mutate(Species = label_species[Type]) 
pet_species %>%
  ggplot(aes(x=Species, fill=Species)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Species') +
  labs(caption = caption_dog_cat) +
  xlab('Species') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        legend.position = 'hide') 

# adoption speed by species
# >> cats appear to have a higher adoption speed than dogs on the first 7 days
# >> there are more dogs than cats that are not adopted after 100 days
pet_species %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Species)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Adoption Speed by Species') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

avg_speed_species <- pet_species %>%
  group_by(Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_species)

# pets with name
# pets with no name has column Name = "" or sometimes contain string 'no name' 

# pets with Name column mentioning the pet has no name or empty "" is about 9% 
# >> this proportion of pets with no name is negligible
# >> some people filled this Name field with pet description like colour, gender and other information 
num_no_name <- df_pets %>% 
  filter(grepl('no name',Name, ignore.case = TRUE) | Name == "") %>% 
  select(Name) 
top_n(num_no_name, n=10)
nrow(df_pets)
ratio_no_name <- nrow(num_no_name)/nrow(df_pets)
ratio_no_name

label_named <- c('Named', 'No name')

pet_named <- df_pets %>% 
  mutate(HasName = ifelse((grepl('no name',Name, ignore.case = TRUE) | Name == ""),
                          'No name', 'Named')) 

pet_named %>%
  ggplot(aes(x=HasName, fill=HasName)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet with and without name') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot named/unnamed pets with adoption speed 
# >> named pets appear to have a higher chance of being adopted on the same day it is listed 
# >> after that, the name does not appear to have effect on adoption speed
pet_named %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=HasName)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed for pets with name / without name') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed of pets listed with / without name
avg_speed_name <- pet_named %>%
  group_by(HasName) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(HasName, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_name)

# define gender labels: 1 = Male, 2 = Female, 3 = Mixed, if profile represents group of pets
label_gender <- c('Male', 'Female', 'Mixed group of pets')

# Gender 
# (1 = Male, 2 = Female, 3 = Mixed, if profile represents group of pets)
# There is slightly more female pets in the population.

# plot of gender count
# >> more females in the population 
pet_gender <- df_pets %>% 
  mutate(Gender=label_gender[Gender]) 
pet_gender %>%
  ggplot(aes(x=Gender, fill=Gender)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Gender') +
  xlab('Gender') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide')

# gender count - to use for calculating adoption rate by gender in each adoption_speed class 
gender_count <- df_pets %>%
  group_by(Gender) %>%
  summarise(Count=n()) %>%
  mutate(PetGender=label_gender[Gender]) %>%
  arrange(Gender)
gender_count

# example: to get the total 1=Male; 2=Female; 3=Mixed
# gender_count[gender_count$Gender==1,]$Count
# gender_count[gender_count$Gender==2,]$Count
# gender_count[gender_count$Gender==3,]$Count
# Mixed Gender make up ~ 14% of df_pets 
gender_mixed <- gender_count[gender_count$Gender==3,]$Count/sum(gender_count$Count)
gender_mixed

# >> Female pets appear to be adopted faster, or is it really the case ? 
pet_gender %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Gender)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed by pet gender') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# calculate average adoption speed by gender for each adoption_speed class
# >> each Gender appear to have average adoption speed despite the previous plot
avg_speed_gender <- pet_gender %>%
  group_by(Gender) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Gender, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_gender)
# plot adoption rate by gender for each adoption_speed class
avg_speed_gender %>% 
  ggplot() + 
  geom_point(aes(x=Gender,y=AvgSpeed,size=Count), alpha=0.5) +
  ggtitle("Average adoption speed across different genders") +
  xlab("") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# distribution of age
# >> most pets listed for adoption are young, mostly between 0-4 months
# there's a good portion of pets older than 12 months that are in multiples of 12 
# (i.e. 12, 24, 36 months) which suggests the person who list the pets for adoption
# do not bother to describe the pet's age to the month, and rather make a direct year to
# month conversion
pet_age <- df_pets %>%
  mutate(Species = label_species[Type]) %>% 
  group_by(Species, Age) %>%
  summarise(Count=n()) %>%
  arrange(desc(Count))
head(pet_age, n=20)

pet_age %>% 
  ggplot() + 
  geom_point(aes(x=Age, y=Count, color=Species), alpha=0.5) + 
  ggtitle("Pets for adoption across different ages") + 
  scale_y_log10() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Ages when listed in months") +
  ylab("Number of pets") 

# average adopt_speed (continuous scale) by age (age when pets listed in months)
avg_speed_age <- df_pets %>%
  mutate(Species = label_species[Type]) %>% 
  group_by(Species, Age) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Species, Age, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed),Count)
top_n(avg_speed_age, n=20)
avg_speed_age %>% 
  ggplot() + 
  geom_point(alpha=0.5, aes(x=Age,y=AvgSpeed,size=Count,color=Species)) +
  ggtitle("Average adoption speed across ages") +
  xlab("Ages when listed in months") +
  ylab("Average adoption speed") + 
  scale_x_sqrt(breaks=c(seq(0,12,2),seq(12,max(avg_speed_age$Age),12))) +
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'bottom',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# pets older than 24 months that are adopted within 1 week
pets_older_2yr_adopted_1wk <- df_pets %>%
  filter(Age > 24) %>% 
  filter(AdoptionSpeed %in% c(3,4))
nrow(pets_older_2yr_adopted_1wk)/nrow(df_pets)

# plot count of adoption speed by pure breed vs. mixed breed

# distribution of pure and mixed breed (6651 are mixed breed)
# m1 <- nrow(df_pets %>% filter(Breed1==307))
# m2 <- nrow(df_pets %>% filter(Breed2==307))
# m3 <- nrow(df_pets %>% filter(Breed1==307 & Breed2==307))
# cat(m1,m2,m3,m1+m2+m3,m1+m2-m3)

breeds <- df_pets %>% 
  mutate(Species = label_species[Type]) %>%
  group_by(Breed_Mix, Species) %>%
  summarise(Count=n()) %>%
  select(Breed_Mix, Species, Count)
breeds

# total pets: nrow(df_pets) = 14993
# validation check
sum(breeds$Count)
nrow(df_pets)

# half of the pets are pure breed
# with cats being mostly pure breeds (81%), while dogs are only 14% pure breeds
pure_breed_pet <- sum(breeds[(breeds$Breed_Mix=='Pure'),]$Count)/
  sum(breeds$Count)
pure_breed_pet

pure_breed_cat <- breeds[(breeds$Species=='Cat' & breeds$Breed_Mix=='Pure'),]$Count/
  sum(breeds[(breeds$Species=='Cat'),]$Count)
pure_breed_cat

pure_breed_dog <- breeds[(breeds$Species=='Dog' & breeds$Breed_Mix=='Pure'),]$Count/
  sum(breeds[(breeds$Species=='Dog'),]$Count)
pure_breed_dog

# average adopt_speed (continuous scale) by breed1, breed2 
# >> adopters appear to prefer pure breed (dog) to mixed breed (dog) with more pure breed (dog)
# being mostly adopted within 30 days
# > most cats are pure breed, and pure/mixed breed of cats do not have much effect on the
# adoption_speed for cat
df_pets %>%
  mutate(Species = label_species[Type], 
         PetGender=label_gender[Gender], 
         Gender=as.character(Gender),
         AdoptionSpeed=as.character(AdoptionSpeed)) %>% 
  ggplot(aes(x=AdoptionSpeed, fill=Species)) +
  geom_bar(position = 'stack', alpha=alpha_bar) +
  facet_grid(~Breed_Mix) +
  ggtitle('Adoption speed by breed') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# calculate average adoption speed by pure/mixed breed for each adoption_speed class
pet_breed_mix <- df_pets %>% 
  mutate(Species = label_species[Type],
         Breed_Species = paste(Breed_Mix, 'breed -', Species)) 

pet_breed_mix %>%
  ggplot(aes(x=Breed_Species, fill=Breed_Species)) +
  geom_bar(alpha=alpha_bar) +
  ggtitle('Breed mix') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank()) +
  coord_flip()

pet_breed_mix %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Breed_Species)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed by mixed/pure breed and species') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# calculate average adoption speed by gender for each adoption_speed class
# >> each Gender appear to have average adoption speed despite the previous plot
avg_speed_breed_mix <- pet_breed_mix %>%
  group_by(Breed_Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Breed_Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed), desc(Count))
head(avg_speed_breed_mix, n=20)

avg_speed_breed_mix %>% 
  ggplot() + 
  geom_point(aes(x=Breed_Species,y=AvgSpeed,size=Count), alpha=0.5) +
  ggtitle("Average adoption speed across pure / mixed breeds") +
  xlab("") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# see if there's popular breed with fast adoption speed

# different type of pure/mixed breeds
length(unique(df_pets$Breed_Combined))

# distribution of pet breeds with count > x
df_pets %>%
  group_by(Breed_Combined) %>% 
  summarise(Count=n()) %>%
  mutate(Breed_Combined=fct_reorder(Breed_Combined,desc(Count))) %>% 
  filter(Count>30) %>%
  ggplot(aes(x=Breed_Combined, y=Count, fill=Breed_Combined)) +
  geom_bar(position='dodge', stat='identity', alpha=alpha_bar) +
  ggtitle('Breed distribution') +
  xlab('Breeds') +
  ylab('Count') + 
  # scale_y_log10() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') +
  coord_flip()

# average adoption speed by breed
avg_speed_breed <- df_pets %>%
  mutate(Species=label_species[Type]) %>%
  group_by(Breed_Combined, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Breed_Combined, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_breed, n=20)

# plot average adoption speed by breed for count > x
avg_speed_breed %>%
  filter(Count>10) %>%
  ggplot() + 
  geom_point(aes(y=AvgSpeed,x=Breed_Combined,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across breeds") +
  xlab("Breeds") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits=c(0,4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), # element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "grey"), 
        panel.grid.minor.y = element_line(size = 0.1, linetype = 'solid',
                                          colour = "white")) 

# colours
# > there are 7 color types (ColorID=1:7 with respective color labels stated in df_color
# > pets can be listed with a maximum of three colors types
sum(df_pets$Color1 == 0)
sum(df_pets$Color2 == 0)
sum(df_pets$Color3 == 0)
# > some pets are listed with ColorID == 0 in Color2 and Color3 
# and this can be cleaned and changed to NA or filter out where == 0

# define colour labels
label_color <- as.character(df_color$ColorName)
label_color

# single / mixed color
color_single <- df_pets %>%
  filter(Color2 == 0 & Color3 == 0)

color_mixed <- df_pets %>%
  filter(Color2 != 0 | Color3 != 0)

# extract different color combinations
pet_color <- df_pets %>%
  mutate(Color1 = ifelse((Color1!=0),label_color[Color1],NA),
         Color2 = ifelse((Color2!=0),label_color[Color2],NA),
         Color3 = ifelse((Color3!=0),label_color[Color3],NA),
         Species = label_species[Type]) %>%
  rowwise() %>%
  mutate(Color = toString(sort(c(unique(c(Color1, Color2, Color3))))),
         Color_Mix = ifelse((is.na(Color2) & is.na(Color3)),'Single','Mixed')) %>%
  select(PetID, Color, Color_Mix, Color1, Color2, Color3, Species, Gender, AdoptionSpeed)

# number of unique color combinations
length(unique(pet_color$Color))

# colour distribution
# > most pets listed are Brown, and Black & White; 
# followed by Black and Black & Brown
pet_color %>% 
  group_by(Color) %>% 
  summarise(Count=n()) %>%
  mutate(Color=fct_reorder(Color,desc(Count))) %>% 
  top_n(25) %>%
  ggplot(aes(x=Color, y=Count, fill=Color)) +
  geom_bar(position='dodge', stat='identity', alpha=alpha_bar) +
  ggtitle('Color combination distribution') +
  xlab('Color combinations') +
  ylab('Count') + 
  # scale_y_log10() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') +
  coord_flip()

# distribution of pets by single/mixed colors
pet_color %>%
  ggplot(aes(x=Color_Mix, fill=Color_Mix)) +
  geom_bar(alpha=alpha_bar) +
  ggtitle('Color mix') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'right',
        legend.title = element_blank()) +
  coord_flip()

# average adoption speed by single/mixed color
avg_speed_color_mix <- pet_color %>%
  group_by(Color_Mix, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Color_Mix, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_color_mix, n=20)
avg_speed_color_mix %>% 
  ggplot() + 
  geom_point(aes(x=Color_Mix,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across single / mixed color pets") +
  xlab("") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# distribution of pets by main color (Color1) shows Black and Brown being the dominant colors
# df_pets %>% 
#   mutate(Species = label_species[Type], Color1=label_color[Color1]) %>
pet_color %>%
  filter(!is.na(Color1)) %>%
  group_by(Color1, Species) %>% 
  summarise(Count=n()) %>%
  ggplot(aes(x=Color1, y=Count, fill=Species)) +
  geom_bar(position='dodge', stat='identity', alpha=alpha_bar) +
  ggtitle('Color1 combination distribution by main color (Color1)') +
  xlab('Color1') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom') +
  coord_flip()

# average adoption speed by main color (Color1)
# > Most pets are listed with Black and Brown as main color
# however, all main colors (Color1) appear to have similar average adoption speed 
avg_speed_color1 <- pet_color %>%
  filter(!is.na(Color1)) %>%
  group_by(Color1, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  filter(!is.na(Color1)) %>%
  select(Color1, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_color1, n=20)
avg_speed_color1 %>% 
  ggplot() + 
  geom_point(aes(x=Color1,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across main color (Color1)") +
  xlab("Main color (Color1)") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# average adoption speed by second color (Color2)
# > all second colors (Color2) appear to have similar average adoption speed 
avg_speed_color2 <- pet_color %>%
  filter(!is.na(Color2)) %>%
  group_by(Color2, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  filter(!is.na(Color2)) %>%
  select(Color2, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_color2, n=20)
avg_speed_color2 %>% 
  ggplot() + 
  geom_point(aes(x=Color2,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across second color (Color2)") +
  xlab("Second color (Color2)") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# average adoption speed by third color (Color3)
# > all third colors (Color3) appear to have similar average adoption speed 
avg_speed_color3 <- pet_color %>%
  filter(!is.na(Color3)) %>%
  group_by(Color3, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  filter(!is.na(Color3)) %>%
  select(Color3, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_color3, n=20)
avg_speed_color3 %>% 
  ggplot() + 
  geom_point(aes(x=Color3,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across third color (Color3)") +
  xlab("Third color (Color3)") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white")) 

# average adoption speed for all colors combined
avg_speed_color_all <- pet_color %>%
  rowwise() %>%
  mutate(Color_All = toString(sort(c(unique(c(Color1, Color2, Color3)))))) %>%
  group_by(Color_All, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  filter(!is.na(Color_All)) %>%
  select(Color_All, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_color_all, n=20)
avg_speed_color_all %>% 
  ggplot() + 
  geom_point(aes(x=Color_All,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across all color combination") +
  xlab("All color combinations") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "grey"), 
        panel.grid.minor.y = element_line(size = 0.1, linetype = 'solid',
                                          colour = "white"))

# maturity size
label_maturity_size = c("1"="Small", "2"="Medium", "3"="Large", "4"="Extra Large", "0"="Not Specified")
# data set is clean - all pets listed has maturity size specified
nrow(df_pets %>% filter(MaturitySize==0))

pet_size <- df_pets %>%
  mutate(MaturitySize = label_maturity_size[MaturitySize],
         Species = label_species[Type]) %>%
  select(PetID, Species, MaturitySize, AdoptionSpeed)

pet_size %>%
  ggplot(aes(x=MaturitySize, fill=MaturitySize)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet maturity size') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot pet maturity size with adoption speed 
pet_size %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=MaturitySize)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed across pet maturity sizes') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed across pet maturity sizes
avg_speed_size <- pet_size %>%
  group_by(MaturitySize, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(MaturitySize, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_size)
avg_speed_size %>% 
  ggplot() + 
  geom_point(aes(x=MaturitySize,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across maturity sizes") +
  xlab("Maturity Size") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# fur length
label_fur = c("1"="Short", "2"="Medium", "3"="Long", "0"="Not Specified")
# data set is clean - all pets listed has fur length specified
nrow(df_pets %>% filter(FurLength==0))

pet_fur <- df_pets %>%
  mutate(FurLength = label_fur[FurLength],
         Species = label_species[Type]) %>%
  select(PetID, Species, FurLength, AdoptionSpeed)

pet_fur %>%
  ggplot(aes(x=FurLength, fill=FurLength)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet fur length') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot pet fur length with adoption speed 
pet_fur %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=FurLength)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed across pet fur length') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed across pet fur lengths
avg_speed_fur <- pet_fur %>%
  group_by(FurLength, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(FurLength, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_fur)
avg_speed_fur %>% 
  ggplot() + 
  geom_point(aes(x=FurLength,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across fur lengths") +
  xlab("Fur Length") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))


# vaccinated
label_vaccinated = c("1"="Yes", "2"="No", "3"="Not Sure")

pet_vaccinated <- df_pets %>%
  mutate(Vaccinated = label_vaccinated[Vaccinated],
         Species = label_species[Type]) %>%
  select(PetID, Species, Vaccinated, AdoptionSpeed)

pet_vaccinated %>%
  ggplot(aes(x=Vaccinated, fill=Vaccinated)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet vaccinated') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot pet vaccination status with adoption speed 
pet_vaccinated %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Vaccinated)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed across pet vaccination status') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed across pet vaccination status 
avg_speed_vaccinated <- pet_vaccinated %>%
  group_by(Vaccinated, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Vaccinated, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_vaccinated)
avg_speed_vaccinated %>% 
  ggplot() + 
  geom_point(aes(x=Vaccinated,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across vaccination status") +
  xlab("Vaccination Status") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# dewormed
label_dewormed = c("1"="Yes", "2"="No", "3"="Not Sure")

pet_dewormed <- df_pets %>%
  mutate(Dewormed = label_dewormed[Dewormed],
         Species = label_species[Type]) %>%
  select(PetID, Species, Dewormed, AdoptionSpeed)

pet_dewormed %>%
  ggplot(aes(x=Dewormed, fill=Dewormed)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet dewormed') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot pet dewormed status with adoption speed 
pet_dewormed %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Dewormed)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed across pet dewormed status') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed across pet dewormed status 
avg_speed_dewormed <- pet_dewormed %>%
  group_by(Dewormed, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Dewormed, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_dewormed)
avg_speed_dewormed %>% 
  ggplot() + 
  geom_point(aes(x=Dewormed,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across dewormed status") +
  xlab("Dewormed Status") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# sterilized
label_sterilized = c("1"="Yes", "2"="No", "3"="Not Sure")

pet_sterilized <- df_pets %>%
  mutate(Sterilized = label_sterilized[Sterilized],
         Species = label_species[Type]) %>%
  select(PetID, Species, Sterilized, AdoptionSpeed)

pet_sterilized %>%
  ggplot(aes(x=Sterilized, fill=Sterilized)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet sterilized') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot pet sterilized status with adoption speed 
pet_sterilized %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Sterilized)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed across pet sterilized status') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed across pet dewormed status 
avg_speed_sterilized <- pet_sterilized %>%
  group_by(Sterilized, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Sterilized, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_sterilized)
avg_speed_sterilized %>% 
  ggplot() + 
  geom_point(aes(x=Sterilized,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across sterilized status") +
  xlab("Sterilized Status") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# health
label_health = c("1"="Healthy", "2"="Minor Injury", "3"="Serious Injury", "0"="Not Specified")
# data set is clean - all pets listed has health status specified
nrow(df_pets %>% filter(Health==0))

pet_health <- df_pets %>%
  mutate(Health = label_health[Health],
         Species = label_species[Type]) %>%
  select(PetID, Species, Health, AdoptionSpeed)

pet_health %>%
  ggplot(aes(x=Health, fill=Health)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Pet health') +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# plot pet health status with adoption speed 
pet_health %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed)) %>%
  ggplot(aes(x=AdoptionSpeed, fill=Health)) +
  geom_bar(position = 'dodge', alpha=alpha_bar) +
  ggtitle('Adoption speed across pet health status') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt')) +
  coord_flip() 

# average adoption speed across pet health status 
avg_speed_health <- pet_health %>%
  group_by(Health, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Health, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_health)
avg_speed_health %>% 
  ggplot() + 
  geom_point(aes(x=Health,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across health status") +
  xlab("Health Status") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# number of pets represented in profile
# data set is clean - all pets listed has quantity > 0
nrow(df_pets %>% filter(Quantity<=0))

pet_quantity <- df_pets %>%
  mutate(Species = label_species[Type],
         Quantity = as.factor(Quantity)) %>%
  select(PetID, Species, Quantity, AdoptionSpeed)

pet_quantity %>%
  group_by(Quantity) %>%
  summarise(Count=n())

pet_quantity %>%
  ggplot(aes(x=as.numeric(Quantity), fill=Quantity)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('Number of pets in each profile listed') +
  xlab('') +
  ylab('Count') + 
  # scale_x_sqrt(breaks=c(1:5,seq(6,max(pet_quantity$Quantity),5))) +
  scale_y_log10() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'hide') 

# average adoption speed across number of pets in each profile
avg_speed_quantity <- pet_quantity %>%
  group_by(Quantity, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Quantity, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
top_n(avg_speed_quantity, n=20)
avg_speed_quantity %>% 
  ggplot() + 
  geom_point(aes(x=as.numeric(Quantity),y=AvgSpeed,size=Count,color=Species), 
             alpha=0.5) +
  ggtitle("Average adoption speed across profiles with different number of pets") +
  xlab("Number of pets in each profile") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))


# Adoption fee
# data set is clean - all pets listed has fee >= 0 (with 0 as free)
nrow(df_pets %>% filter(Fee <0))

pet_fee <- df_pets %>%
  mutate(Species = label_species[Type],
         Fee = as.numeric(Fee)) %>%
  select(PetID, Species, Fee, AdoptionSpeed)

pet_fee_summary <- pet_fee %>%
  group_by(Fee) %>%
  summarise(Count=n(), Average=n()/nrow(df_pets))

max_pet_fee <- max(pet_fee$Fee)
max_pet_fee

# free pets
free_pet_ratio <- pet_fee_summary[(pet_fee_summary$Fee==0),]$Average

# average adoption speed across number of pets in each profile
avg_speed_fee <- pet_fee %>%
  group_by(Fee, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(Fee, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
top_n(avg_speed_fee, n=20)
avg_speed_fee %>% 
  ggplot() + 
  geom_point(aes(x=Fee,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across adoption fees") +
  xlab("Adoption fee") +
  ylab("Average adoption speed") + 
  scale_x_sqrt(breaks=c(seq(0,200,100),seq(400,1000,200),seq(2000,3000,1000))) +
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))


# state location in Malaysia
label_state = df_state$StateName
# data set is clean - all pets listed has valid state specified
nrow(df_pets %>% filter(State==0))

pet_state <- df_pets %>%
  mutate(
    # State = as.factor(State),
    Species = label_species[Type]) %>%
  select(PetID, Species, State, AdoptionSpeed)

pet_state %>%
  # mutate(State=as.character(State)) %>%
  left_join(x=., y=df_state, by=c('State'='StateID')) %>%
  ggplot(aes(x=StateName, fill=StateName)) +
  geom_bar(position='dodge', alpha=alpha_bar) +
  ggtitle('State location where pet is listed') +
  scale_y_sqrt(breaks=c(100,500,seq(1000,9000,1000))) +
  xlab('') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'hide') 

# average adoption speed across pet different state locations
avg_speed_state <- pet_state %>%
  # mutate(State=as.character(State)) %>%
  left_join(x=., y=df_state, by=c('State'='StateID')) %>%
  group_by(StateName, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(StateName, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_state)
avg_speed_state %>% 
  ggplot() + 
  geom_point(aes(x=StateName,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across different state locations") +
  xlab("State locations where pet is listed") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# Photo Amount
# data set is clean - all pets listed has PhotoAmt >= 0 
nrow(df_pets %>% filter(PhotoAmt <0))

pet_photo <- df_pets %>%
  mutate(Species = label_species[Type],
         PhotoAmt = as.numeric(PhotoAmt)) %>%
  select(PetID, Species, PhotoAmt, AdoptionSpeed)

pet_photo %>%
  group_by(PhotoAmt) %>%
  summarise(Count=n(), Average=n()/nrow(df_pets))
max(pet_photo$PhotoAmt)

# average adoption speed across number of photos in each profile
avg_speed_photo <- pet_photo %>%
  group_by(PhotoAmt, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(PhotoAmt, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
top_n(avg_speed_photo, n=20)
avg_speed_photo %>% 
  ggplot() + 
  geom_point(aes(x=PhotoAmt,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across pet photos") +
  xlab("Number of photos") +
  ylab("Average adoption speed") + 
  scale_x_sqrt() +
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# Video Amount
# data set is clean - all pets listed has VideoAmt >= 0 
nrow(df_pets %>% filter(VideoAmt <0))

pet_video <- df_pets %>%
  mutate(Species = label_species[Type],
         VideoAmt = as.numeric(VideoAmt)) %>%
  select(PetID, Species, VideoAmt, AdoptionSpeed)

pet_video %>%
  group_by(VideoAmt) %>%
  summarise(Count=n(), Average=n()/nrow(df_pets))
max(pet_video$VideoAmt)

# average adoption speed across number of videos in each profile
avg_speed_video <- pet_video %>%
  group_by(VideoAmt, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(VideoAmt, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
top_n(avg_speed_video, n=20)
avg_speed_video %>% 
  ggplot() + 
  geom_point(aes(x=VideoAmt,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across pet videos") +
  xlab("Number of videos") +
  ylab("Average adoption speed") + 
  scale_x_sqrt() +
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# RescuerID
# > interesting to note there are some dedicated kind souls who are out there rescuing pets
df_pets %>% 
  group_by(RescuerID) %>% 
  summarise(n=n()) %>% 
  select(RescuerID, n) %>% 
  arrange(desc(n))

pet_rescuer <- df_pets %>%
  mutate(RescuerID = as.factor(RescuerID),
         Species = label_species[Type]) %>%
  select(PetID, Species, RescuerID, AdoptionSpeed)

# rescuers who rescue pets with count > x
pet_rescuer %>%
  group_by(RescuerID) %>%
  summarise(Count=n()) %>%
  filter(Count > 10) %>%
  mutate(RescuerID=fct_reorder(RescuerID, desc(Count))) %>%
  ggplot(aes(x=RescuerID, y=Count)) +
  geom_col(position='dodge', alpha=alpha_bar) +
  ggtitle('Pets by rescuers') +
  xlab('Rescuers') +
  ylab('Count') + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'hide') 

# average adoption speed across pet different rescuers who are listed in more than x profiles
avg_speed_rescuer <- pet_rescuer %>%
  group_by(RescuerID, Species) %>%
  summarise(AvgSpeed=mean(AdoptionSpeed),Count=n()) %>%
  select(RescuerID, Species, AvgSpeed, Count) %>%
  arrange(desc(AvgSpeed))
head(avg_speed_rescuer)
avg_speed_rescuer %>%
  filter(Count > 10) %>%
  ggplot() + 
  geom_point(aes(x=RescuerID,y=AvgSpeed,size=Count,color=Species), alpha=0.5) +
  ggtitle("Average adoption speed across different rescuers") +
  xlab("Rescuers") +
  ylab("Average adoption speed") + 
  scale_y_continuous(limits = c(0, 4), labels=adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.spacing.x = unit(5, 'pt'),
        legend.key.size = unit(10,'pt'),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                          colour = "grey"), 
        panel.grid.minor.y = element_line(size = 0.1, linetype = 'solid',
                                          colour = "white"))

# Clean up memory: unused data frames 
rm('pet_age', 'pet_breed_mix', 'pet_color', 'pet_dewormed', 'pet_fee', 'pet_fur', 'pet_gender',
   'pet_health', 'pet_named', 'pet_photo', 'pet_quantity', 'pet_rescuer', 'pet_size',
   'pet_species', 'pet_state', 'pet_sterilized', 'pet_vaccinated', 'pets_older_2yr_adopted_1wk')

##########################################################
#
# Prepare data set to experiment with models
#
##########################################################

# backup: df_pets
df_pets_backup <- df_pets

# set categorical variables to factors for knncat()
df_pets$Type <- as.factor(df_pets$Type)
df_pets$Breed1 <- as.factor(df_pets$Breed1)
df_pets$Breed2 <- as.factor(df_pets$Breed2)
df_pets$Gender <- as.factor(df_pets$Gender)
df_pets$Color1 <- as.factor(df_pets$Color1)
df_pets$Color2 <- as.factor(df_pets$Color2)
df_pets$Color3 <- as.factor(df_pets$Color3)
df_pets$MaturitySize <- as.factor(df_pets$MaturitySize)
df_pets$FurLength <- as.factor(df_pets$FurLength)
df_pets$Vaccinated <- as.factor(df_pets$Vaccinated)
df_pets$Dewormed <- as.factor(df_pets$Dewormed)
df_pets$Sterilized <- as.factor(df_pets$Sterilized)
df_pets$Health <- as.factor(df_pets$Health)
df_pets$State <- as.factor(df_pets$State)
df_pets$AdoptionSpeed <- as.factor(df_pets$AdoptionSpeed)

# drop unused columns
df_pets <- within(df_pets, rm('Breed_Combined','Breed1_Name','Breed2_Name',
                              'PetID','RescuerID','Name','Description','Breed_Mix'))

# df_pets <- within(df_pets, rm('Desc_WordCount'))

# move AdoptionSpeed to last column
df_pets <- df_pets %>% select(-AdoptionSpeed,everything())

# df_pets$AdoptionSpeed <- as.factor(df_pets$AdoptionSpeed)

# check again: no NAs in data set
sapply(df_pets, function(x) sum(is.na(x)))

# ensure results are repeatable
seed <- 1
set.seed(seed)

# split data set to train/test
ratio_test <- 0.1
val_index <- createDataPartition(y = df_pets$AdoptionSpeed, times = 1, 
                                  p = ratio_test, list = FALSE)
df_train <- df_pets[-val_index,]
temp <- df_pets[val_index,]

# Make sure features in df_validation set are also in df_train set
df_validation <- temp %>% 
  semi_join(df_train, by = "Type") %>%
  semi_join(df_train, by = "Age") %>%
  semi_join(df_train, by = "Breed1") %>%
  semi_join(df_train, by = "Breed2") %>%
  semi_join(df_train, by = "Gender") %>%
  semi_join(df_train, by = "Color1") %>%
  semi_join(df_train, by = "Color2") %>%
  semi_join(df_train, by = "Color3") %>%
  semi_join(df_train, by = "MaturitySize") %>%
  semi_join(df_train, by = "FurLength") %>%  
  semi_join(df_train, by = "Vaccinated") %>%
  semi_join(df_train, by = "Dewormed") %>%
  semi_join(df_train, by = "Sterilized") %>%
  semi_join(df_train, by = "Health") %>%
  semi_join(df_train, by = "State") %>%
  semi_join(df_train, by = "AdoptionSpeed") 

# Add rows removed from df_validation set back into df_train set
removed <- anti_join(temp, df_validation)
nrow(removed)
df_train <- rbind(df_train, removed)
nrow(df_validation)/nrow(df_train)

# remove unsed data
rm(val_index, temp, removed)

cat("ratio df_train/df_pets: ", nrow(df_train)/nrow(df_pets))
cat("ratio df_validation/df_pets: ", nrow(df_validation)/nrow(df_pets))

# df_train data structure
str(df_train)
ncol(df_train)
nrow(df_train)

# df_validation data structure
str(df_validation)
ncol(df_validation)
nrow(df_validation)


# complete df_train/df_validation preparation

##########################################################
#
# try: Random forest
#
##########################################################

# mtry: Number of variables randomly sampled as candidates at each split
# ntree: Number of trees to grow
# tuneLength: tells the algorithm to try different default values for the main parameter
# number: number of folds in cross-validation - 10-fold CV mean dividing your training dataset randomly into 10 parts and then using each of 10 parts as testing dataset for the model trained on other 9.
# repeat: number of times the number of folds in cross validation is repeated 

num_fold <- 10
num_repeat <- 3
seed <- 1
       
# set metric
metric <- ifelse(is.factor(df_train$AdoptionSpeed), 'Accuracy', 'RMSE')
metric

# model 1: 
# Rank features by importance - and also build model using caret: Random Search
tic('model 1: random forest, cv=10')
set.seed(seed)
formula <- AdoptionSpeed ~ .
control <- trainControl(method='cv', number=num_fold, search='random', verboseIter=TRUE)
model_1 <- train(formula, method='rf', data=df_train, metric=metric, 
                 trControl=control, importance=TRUE)
toc()
print(model_1)
plot(model_1)
print(model_1$results)
# rank importance
imp_1 <- varImp(model_1)
print(imp_1)
# predict
prediction_1 <- predict(model_1, df_validation)
table(prediction_1)
df_prediction_1 <- as.data.frame(prediction_1)
cm_1 <- confusionMatrix(df_prediction_1$prediction, df_validation$AdoptionSpeed)
print(cm_1)
# save the model-  commented out to avoid overwriting
# saveRDS(model_1, './model_1.rds')

# create a table to store the results for each model, starting with the first simple model
results <- data_frame(model = 'model 1: random forest, cv=10', 
                      accuracy = max(model_1$results$Accuracy),
                      kappa = max(model_1$results$Kappa))
kp <- 5
results %>% knitr::kable(padding=kp)

# note that: there is a small proportion of Adoption speed of class 4
# compared the other classes
df_pets %>%
  group_by(AdoptionSpeed) %>%
  summarise(n=n(), ratio=n()/nrow(df_pets))

# restore the df_pets data frame
df_pets <- df_pets_backup

# select all the rows with AdoptionSpeed == 4
df_4 <- df_pets[(df_pets$AdoptionSpeed=="4"),]
nrow(df_4)
df_4 %>% select(PetID, AdoptionSpeed, Breed1) %>% head()

# replicate data rows with AdoptionSpeed == 4 for this many times
df_4_times <- 7

# replicate the samples with AdoptionSpeed == 4, so that it has 
# a somewhat even distribution across the different AdoptionSpeed classes
for (i in 1:df_4_times) {
  cat('add df_4: ', i, 'x \n')
  df_pets <- rbind(df_pets, df_4)
}

df_pets %>%
  group_by(AdoptionSpeed) %>%
  summarise(n=n(), ratio=n()/nrow(df_pets))

# plot adoption speed
adoption_speed_count <- df_pets %>%
  group_by(AdoptionSpeed) %>%
  summarise(Count = n()) %>%
  mutate(AdoptionSpeed=as.character(AdoptionSpeed),
         Rate=Count/nrow(df_pets)) %>%
  select(AdoptionSpeed, Count, Rate) %>%
  arrange(desc(AdoptionSpeed))
adoption_speed_count
adoption_speed_count %>%
  ggplot(aes(x=AdoptionSpeed, y=Count)) +
  geom_bar(stat = "identity", alpha=.8, fill=rainbow(n=length(adoption_speed_count$AdoptionSpeed))) +
  ggtitle('Adoption Speed') +
  xlab('Adoption Speed') +
  ylab('Count') + 
  scale_x_discrete(labels = adoption_speed_desc) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# re-split the df_pets into df_train and df_validation
#
# set categorical variables to factors for knncat()
df_pets$Type <- as.factor(df_pets$Type)
df_pets$Breed1 <- as.factor(df_pets$Breed1)
df_pets$Breed2 <- as.factor(df_pets$Breed2)
df_pets$Gender <- as.factor(df_pets$Gender)
df_pets$Color1 <- as.factor(df_pets$Color1)
df_pets$Color2 <- as.factor(df_pets$Color2)
df_pets$Color3 <- as.factor(df_pets$Color3)
df_pets$MaturitySize <- as.factor(df_pets$MaturitySize)
df_pets$FurLength <- as.factor(df_pets$FurLength)
df_pets$Vaccinated <- as.factor(df_pets$Vaccinated)
df_pets$Dewormed <- as.factor(df_pets$Dewormed)
df_pets$Sterilized <- as.factor(df_pets$Sterilized)
df_pets$Health <- as.factor(df_pets$Health)
df_pets$State <- as.factor(df_pets$State)
df_pets$AdoptionSpeed <- as.factor(df_pets$AdoptionSpeed)

# drop unused columns
df_pets <- within(df_pets, rm('Breed_Combined','Breed1_Name','Breed2_Name',
                              'PetID','RescuerID','Name','Description','Breed_Mix'))

# move AdoptionSpeed to last column
df_pets <- df_pets %>% select(-AdoptionSpeed,everything())

# check again: no NAs in data set
sapply(df_pets, function(x) sum(is.na(x)))

# ensure results are repeatable
seed <- 1
set.seed(seed)

# split data set to train/test
ratio_test <- 0.1
val_index <- createDataPartition(y = df_pets$AdoptionSpeed, times = 1, 
                                 p = ratio_test, list = FALSE)
df_train <- df_pets[-val_index,]
temp <- df_pets[val_index,]

# Make sure features in df_validation set are also in df_train set
df_validation <- temp %>% 
  semi_join(df_train, by = "Type") %>%
  semi_join(df_train, by = "Age") %>%
  semi_join(df_train, by = "Breed1") %>%
  semi_join(df_train, by = "Breed2") %>%
  semi_join(df_train, by = "Gender") %>%
  semi_join(df_train, by = "Color1") %>%
  semi_join(df_train, by = "Color2") %>%
  semi_join(df_train, by = "Color3") %>%
  semi_join(df_train, by = "MaturitySize") %>%
  semi_join(df_train, by = "FurLength") %>%  
  semi_join(df_train, by = "Vaccinated") %>%
  semi_join(df_train, by = "Dewormed") %>%
  semi_join(df_train, by = "Sterilized") %>%
  semi_join(df_train, by = "Health") %>%
  semi_join(df_train, by = "State") %>%
  semi_join(df_train, by = "AdoptionSpeed") 

# Add rows removed from df_validation set back into df_train set
removed <- anti_join(temp, df_validation)
nrow(removed)
df_train <- rbind(df_train, removed)
nrow(df_validation)/nrow(df_train)

# remove unsed data
rm(val_index, temp, removed)

cat("ratio df_train/df_pets: ", nrow(df_train)/nrow(df_pets), "\n")
cat("ratio df_validation/df_pets: ", nrow(df_validation)/nrow(df_pets), "\n")

# df_train data structure
str(df_train)
ncol(df_train)
nrow(df_train)

# df_validation data structure
str(df_validation)
ncol(df_validation)
nrow(df_validation)

# set metric
metric <- ifelse(is.factor(df_train$AdoptionSpeed), 'Accuracy', 'RMSE')
metric

# model 2: re-train model 1 with somewhat more even distribtion of class 4 adoption speed
# Rank features by importance - and also build model using caret: Random Search
tic('model 2: re-train model 1 using data set with balanced classes')
set.seed(seed)
formula <- AdoptionSpeed ~ .
control <- trainControl(method='cv', number=num_fold, search='random', verboseIter=TRUE)
model_2 <- train(formula, method='rf', data=df_train, metric=metric, 
                 trControl=control, importance=TRUE)
toc()
print(model_2)
plot(model_2)
print(model_2$results)
# rank importance
imp_2 <- varImp(model_2)
print(imp_2)
# predict
prediction_2 <- predict(model_2, df_validation)
table(prediction_2)
df_prediction_2 <- as.data.frame(prediction_2)
cm_2 <- confusionMatrix(df_prediction_2$prediction, df_validation$AdoptionSpeed)
print(cm_2)
# save the model -  commented out to avoid overwriting
# saveRDS(model_2, './model_2.rds')

# store model_2 to results
model_2_result <- data_frame(model = 'model 2: re-train model 1 using data set with balanced portion of classes', 
                             accuracy = max(model_2$results$Accuracy),
                             kappa = max(model_2$results$Kappa))

results <- bind_rows(results, model_2_result)
results %>% knitr::kable(padding=kp)

# model 3: search for ntree value with mtry=133
tic('model 3: tune ntree')
set.seed(seed)
control <- trainControl(method="cv", number=num_fold, verboseIter = TRUE)
tunegrid <- data.frame(.mtry = 133)
modellist <- list()
formula <- AdoptionSpeed ~ . 
for (ntree in c(100, 200, 300, 500, 1000, 1500, 2000)) {
  set.seed(seed)
  cat('ntree: ', str(ntree))
  fit <- train(formula, data=df_train, method="rf", metric=metric,
               tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
toc()
# compare results
model_3_ntrees <- resamples(modellist)
summary(model_3_ntrees)
dotplot(model_3_ntrees)
# save all the ntree models - commented out to avoid overwriting
# saveRDS(modellist, './model_3_modellist_ntrees.rds')
# saveRDS(model_3_ntrees, './model_3_ntrees.rds')
#
# examine the model with best accuracy (mtry=133, ntree=1000)
model_3 <- modellist["300"]
print(model_3)
print(model_3$`300`$results)
# validate
prediction_3 <- predict(model_3, df_validation)
table(prediction_3)
df_prediction_3 <- as.data.frame(prediction_3)
cm_3 <- confusionMatrix(df_prediction_3$X300, df_validation$AdoptionSpeed)
print(cm_3)
# save model 3 - commented out to avoid overwriting
# saveRDS(model_3, './model_3.rds')

# store model_3 to results
model_3_result <- data_frame(model = 'model 3: tune mtry=133 and ntree=300', 
                             accuracy = model_3$`300`$results$Accuracy,
                             kappa = model_3$`300`$results$Kappa )

results <- bind_rows(results, model_3_result)
results %>% knitr::kable(padding=kp)

# store results
# commented out to avoid overwriting
# saveRDS(results, './results.rds')
