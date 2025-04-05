# MATHEMATICAL STATISTICS PROJECT: Road Accidents in Milan 
# Authors: Matteo Mello, Riccardo Scibetta

# In this file we use interchangeably the terms crash and accident and incident

######  0) LIBRARIES AND SETUP  #######

# install.packages("ggplot2")  # Used for: data visualization
library(ggplot2)
#install.packages("dplyr")  # Used for: data manipulation
library(dplyr)
#install.packages("faraway")  # Regression diagnostic
library(faraway)
#install.packages('olsrr')  # Step up and step down methods in linear regression
library(olsrr)
#install.packages("caret") # Splitting data in cross validation
library(caret)
#install.packages("lattice") # Data visualization for a better level plot
library(lattice)
#install.packages("tidyr") # Reshaping data into long format 
library(tidyr)

# Import the dataset in R environment with Delimiter: Semicolon
data_linear = INCSTRAD_Microdati_2022_Excel  # Copy for linear regression

######  1) DATASET MODIFICATIONS  ######

## 1.1) Create new variables ##

data_linear$vehicle_a_driver_age1 = ifelse(data_linear$vehicle_a_driver_age == "18-29", 24, 
                              ifelse(data_linear$vehicle_a_driver_age == "30-44", 37,
                              ifelse(data_linear$vehicle_a_driver_age == "45-54", 50,
                              ifelse(data_linear$vehicle_a_driver_age == "55-64", 60,
                              ifelse(data_linear$vehicle_a_driver_age == "65+", 70,
                              ifelse(data_linear$vehicle_a_driver_age == "06-set", 8,  # actual range is 6-9
                              ifelse(data_linear$vehicle_a_driver_age == "ott-14", 12, # actual range is 10-14
                              ifelse(data_linear$vehicle_a_driver_age == "15-17", 16,
                              ifelse(data_linear$vehicle_a_driver_age == "0-5  ", 3, NA)))))))))

data_linear$vehicle_b_driver_age1 = ifelse(data_linear$vehicle_b_driver_age == "18-29", 24, 
                             ifelse(data_linear$vehicle_b_driver_age == "30-44", 37,
                             ifelse(data_linear$vehicle_b_driver_age == "45-54", 50,
                             ifelse(data_linear$vehicle_b_driver_age == "55-64", 60,
                             ifelse(data_linear$vehicle_b_driver_age == "65+", 70,
                             ifelse(data_linear$vehicle_b_driver_age == "06-set", 8,  # actual range is 6-9
                             ifelse(data_linear$vehicle_b_driver_age == "ott-14", 12, # actual range is 10-14
                             ifelse(data_linear$vehicle_b_driver_age == "15-17", 16,
                             ifelse(data_linear$vehicle_b_driver_age == "0-5  ", 3, NA)))))))))

data_linear$vehicle_a_registration_year = ifelse(data_linear$vehicle_a_registration_year == "(2002,2012]", 2007, 
                                   ifelse(data_linear$vehicle_a_registration_year == "(1983,1992]", 1988,
                                   ifelse(data_linear$vehicle_a_registration_year == "(1973,1982]", 1978,
                                   ifelse(data_linear$vehicle_a_registration_year == "2013", 2013,
                                   ifelse(data_linear$vehicle_a_registration_year == "2014", 2014,
                                   ifelse(data_linear$vehicle_a_registration_year == "2015", 2015,
                                   ifelse(data_linear$vehicle_a_registration_year == "2016", 2016,
                                   ifelse(data_linear$vehicle_a_registration_year == "2017", 2017,
                                   ifelse(data_linear$vehicle_a_registration_year == "2018", 2018,
                                   ifelse(data_linear$vehicle_a_registration_year == "2019", 2019,
                                   ifelse(data_linear$vehicle_a_registration_year == "2020", 2020,
                                   ifelse(data_linear$vehicle_a_registration_year == "2021", 2021,
                                   ifelse(data_linear$vehicle_a_registration_year == "2022", 2022,NA)))))))))))))

data_linear$vehicle_b_registration_year = ifelse(data_linear$vehicle_b_registration_year == "(2002,2012]", 2007, 
                                   ifelse(data_linear$vehicle_b_registration_year == "(1983,1992]", 1988,
                                   ifelse(data_linear$vehicle_b_registration_year == "(1973,1982]", 1978,
                                   ifelse(data_linear$vehicle_b_registration_year == "2013", 2013,
                                   ifelse(data_linear$vehicle_b_registration_year == "2014", 2014,
                                   ifelse(data_linear$vehicle_b_registration_year == "2015", 2015,
                                   ifelse(data_linear$vehicle_b_registration_year == "2016", 2016,
                                   ifelse(data_linear$vehicle_b_registration_year == "2017", 2017,
                                   ifelse(data_linear$vehicle_b_registration_year == "2018", 2018,
                                   ifelse(data_linear$vehicle_b_registration_year == "2019", 2019,
                                   ifelse(data_linear$vehicle_b_registration_year == "2020", 2020,
                                   ifelse(data_linear$vehicle_b_registration_year == "2021", 2021,
                                   ifelse(data_linear$vehicle_b_registration_year == "2022", 2022,NA)))))))))))))

data_linear$vehicle_b_release_year = ifelse(data_linear$vehicle_b_release_year == "(2002,2007]", 2005, 
                              ifelse(data_linear$vehicle_b_release_year == "(2007,2012]", 2010,
                              ifelse(data_linear$vehicle_b_release_year == "(2012,2017]", 2015,
                              ifelse(data_linear$vehicle_b_release_year == "(2017,2022]", 2020,
                              ifelse(data_linear$vehicle_b_release_year == "(1987,1992]", 1990,
                              ifelse(data_linear$vehicle_b_release_year == "(1982,1987]", 1985,
                              ifelse(data_linear$vehicle_b_release_year == "(1992,1997]", 1995,
                              ifelse(data_linear$vehicle_b_release_year == "<1983      ", 1983,NA))))))))

data_linear$vehicle_a_release_year = ifelse(data_linear$vehicle_a_release_year == "(2002,2007]", 2005, 
                              ifelse(data_linear$vehicle_a_release_year == "(2007,2012]", 2010,
                              ifelse(data_linear$vehicle_a_release_year == "(2012,2017]", 2015,
                              ifelse(data_linear$vehicle_a_release_year == "(2017,2022]", 2020,
                              ifelse(data_linear$vehicle_a_release_year == "(1987,1992]", 1990,
                              ifelse(data_linear$vehicle_a_release_year == "(1982,1987]", 1985,
                              ifelse(data_linear$vehicle_a_release_year == "(1992,1997]", 1995,
                              ifelse(data_linear$vehicle_a_release_year == "<1983      ", 1983,NA))))))))

data = data_linear # Copy  for general dataset inspection
data_logistic = data_linear # Copy for logistic regression

## 1.2) Restrict the dataset for linear regression ##

# Firstly, we exclude the outcome variables (directly related to tot_injured)
data_linear$fatality = NULL
data_linear$vehicle_a_driver_outcome = NULL
data_linear$vehicle_b_driver_outcome = NULL
data_linear$injured_pedestrians = NULL
data_linear$tot_death_within_24h = NULL

# We delete the two variables since they are not numerical
data_linear$vehicle_a_driver_age = NULL
data_linear$vehicle_b_driver_age = NULL

# Let's exclude all the variables related to vehicle C
data_linear$vehicle_c_registration_year = NULL
data_linear$vehicle_c_driver_age = NULL
data_linear$vehicle_c_driver_sex = NULL
data_linear$vehicle_c_driver_outcome = NULL
data_linear$vehicle_c_driver_licence = NULL
data_linear$vehicle_c_release_year = NULL
data_linear$type_vehicle_c = NULL
data_linear$cubic_capacity_c = NULL

# Let's exclude the second circumstances variables, since they only appear in very few cases
data_linear$vehicle_a_circumstances_2 = NULL
data_linear$vehicle_b_circumstances_2 = NULL

#We restrict the dataset only to the observations with all the values
data_linear = na.omit(data_linear) 

## 1.3) Dummy variables creation  ##

#NOTE: Whenever only one variable is commented, it is because otherwise there - 
# would be a singularity in the results

# Convert the circumstances categorical variables into dummy variables
#data_linear$unknown_circumstances = ifelse(data_linear$vehicle_a_circumstances_1 ==0 |data_linear$vehicle_b_circumstances_1 ==0 , 1, 0)
data_linear$accident_between_vehicles_intersection = ifelse(data_linear$vehicle_a_circumstances_1 >=1 & data_linear$vehicle_a_circumstances_1<=18, 1, 0)
data_linear$accident_between_vehicles_not_intersection= ifelse(data_linear$vehicle_a_circumstances_1 >=19 & data_linear$vehicle_a_circumstances_1<=39, 1, 0)
data_linear$pedestrian_impact = ifelse(data_linear$vehicle_b_circumstances_1>=40 & data_linear$vehicle_b_circumstances_1 <=55, 1, 0)
data_linear$steady_vehicle_or_obstacle = ifelse(data_linear$vehicle_b_circumstances_1>=60 & data_linear$vehicle_b_circumstances_1 <=76, 1, 0)

data_linear$regular_travel = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(1, 14, 16, 20, 37, 40, 60),1,0)
data_linear$distracted_driving = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(2, 21, 61, 71), 1, 0)
data_linear$speeding = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(11, 23, 41, 64, 72, 12, 24, 42, 65),1,0)
data_linear$no_priority = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(4, 5, 6, 49),1,0)
data_linear$wrong_direction = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(7, 26, 43, 63, 25),1,0)
data_linear$rules_violated = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(8, 46, 10, 27, 66, 68),1,0)
data_linear$irregular_surpasses = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(18, 30, 31, 33, 44, 50),1,0)
data_linear$irregular_maneuvers = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(34, 35, 36, 38, 47),1 ,0)
data_linear$accidents_with_pedestrians = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(48, 51, 52),1,0)
data_linear$bright_headlights = ifelse(data_linear$vehicle_a_circumstances_1 %in% c(13, 28),1 ,0)

data_linear$vehicle_a_circumstances_1 = NULL
data_linear$vehicle_b_circumstances_1 = NULL

# Convert the weather conditions into dummy variables
data_linear$Weather_sunny = ifelse(data_linear$meteorological_conditions == 1, 1, 0)
data_linear$Weather_fog = ifelse(data_linear$meteorological_conditions == 2, 1, 0)
data_linear$Weather_rain = ifelse(data_linear$meteorological_conditions == 3, 1, 0)
data_linear$Weather_hail = ifelse(data_linear$meteorological_conditions == 4, 1, 0)
data_linear$Weather_snow = ifelse(data_linear$meteorological_conditions == 5, 1, 0)
data_linear$Weather_strong_wind = ifelse(data_linear$meteorological_conditions == 6, 1, 0)

data_linear$meteorological_conditions = NULL

# Convert the accident locations into dummy variables
data_linear$street_inside_city = ifelse(data_linear$accident_location >= 0 & data_linear$accident_location<=3, 1, 0)
data_linear$comunal_extraurban_street = ifelse(data_linear$accident_location == 4,1, 0)
data_linear$provincial_street = ifelse(data_linear$accident_location == 5,1, 0)
data_linear$regional_street = ifelse(data_linear$accident_location == 9,1, 0)
#data_linear$highway = ifelse(data_linear$accident_location == 7,1, 0)
data_linear$state_road = ifelse(data_linear$accident_location == 6,1, 0)
data_linear$other_street = ifelse(data_linear$accident_location ==8 ,1, 0)

data_linear$accident_location = NULL

# Convert the categorical variable type_of_street into dummy variables
data_linear$one_way_carriageway = ifelse(data_linear$type_of_street==1, 1, 0)
data_linear$two_way_carriageway = ifelse(data_linear$type_of_street==2, 1, 0)
#data_linear$two_carriageways = ifelse(data_linear$type_of_street==3, 1, 0)
data_linear$more_than_two_carriageways = ifelse(data_linear$type_of_street==4, 1, 0)

data_linear$type_of_street = NULL

# Convert the categorical variable floor into dummy variables
#data_linear$paved_road = ifelse(flooring == 1, 1, 0)
data_linear$bumpy_paved_road = ifelse(data_linear$flooring == 2, 1, 0)
data_linear$non_paved_road = ifelse(data_linear$flooring ==3, 1, 0)

data_linear$flooring = NULL

# Convert the categorical variable road_surface_conditions into dummy variables
data_linear$dry_road = ifelse(data_linear$road_surface_conditions ==1, 1, 0)
data_linear$wet_road = ifelse(data_linear$road_surface_conditions ==2, 1, 0)
#data_linear$slippery_road = ifelse(data_linear$road_surface_conditions ==3, 1, 0)
data_linear$iced_road = ifelse(data_linear$road_surface_conditions ==4, 1, 0)
data_linear$snowy_road = ifelse(data_linear$road_surface_conditions ==5, 1, 0)

data_linear$road_surface_conditions = NULL

# Convert the categorical variable intersection_type into dummy variables
data_linear$intersection = ifelse(data_linear$intersection_type %in% c(1,3,4,5), 1, 0)
data_linear$gallery = ifelse(data_linear$intersection_type %in% c(11, 12), 1, 0)
data_linear$level_crossing = ifelse(data_linear$intersection_type == 6,1,0)
data_linear$straight = ifelse(data_linear$intersection_type ==7,1, 0)
data_linear$curve = ifelse(data_linear$intersection_type == 8, 1, 0)
data_linear$sloping_street = ifelse(data_linear$intersection_type ==9,1, 0)
data_linear$narrow_street = ifelse(data_linear$intersection_type == 10, 1, 0)

data_linear$intersection_type = NULL

# Convert the categorical variable nature of the accident into dummy variables
data_linear$hand_on_collision = ifelse(data_linear$nature_of_the_accident %in% c(1, 2), 1,0)
data_linear$lateral_collision = ifelse(data_linear$nature_of_the_accident == 3, 1, 0)
data_linear$back_collision = ifelse(data_linear$nature_of_the_accident == 4, 1,0)
data_linear$pedestrians_hit = ifelse(data_linear$nature_of_the_accident == 5, 1, 0)
data_linear$collision_with_train = ifelse(data_linear$nature_of_the_accident == 9, 1, 0)
data_linear$heeling = ifelse(data_linear$nature_of_the_accident == 10, 1, 0)
#data_linear$collision_with_vehicle_at_rest = ifelse(data_linear$nature_of_the_accident %in% c(6,7),1,0)
data_linear$collision_with_obstacle = ifelse(data_linear$nature_of_the_accident == 8, 1, 0)

data_linear$nature_of_the_accident = NULL

#data_linear$car = ifelse(data_linear$type_vehicle_a %in% c(1,2,3,4, 21) | type_vehicle_b %in% c(1,2,3,4, 21),1,0)
#data_linear$autobus = ifelse(data_linear$type_vehicle_a %in% c(5,6,7)|type_vehicle_b %in% c(5,6,7),1,0)
#data_linear$camion = ifelse(data_linear$type_vehicle_a %in% c(8, 9, 10, 11) | type_vehicle_b %in% c(8, 9, 10, 11) , 1, 0)
#data_linear$tractor = ifelse(data_linear$type_vehicle_a %in% c(12, 13) | type_vehicle_b %in% c(12, 13), 1, 0)
#data_linear$bicycle = ifelse(data_linear$type_vehicle_a %in% c(14, 23) | type_vehicle_b %in% c(14, 23),1,0)
#data_linear$scooter = ifelse(data_linear$type_vehicle_a %in% c(15, 16, 17,18, 22) | type_vehicle_b %in% c(15, 16, 17,18, 22),1 ,0)

# Convert the categorical variable vehicle a type into dummy variables
data_linear$vehicle_a_car = ifelse(data_linear$type_vehicle_a %in% c(1,2,3,4, 21),1,0)
data_linear$vehicle_a_autobus = ifelse(data_linear$type_vehicle_a %in% c(5,6,7), 1, 0)
#data_linear$vehicle_a_camion = ifelse(data_linear$type_vehicle_a %in% c(8, 9, 10, 11), 1, 0)
data_linear$vehicle_a_tractor = ifelse(data_linear$type_vehicle_a %in% c(12, 13), 1, 0)
data_linear$vehicle_a_bicycle = ifelse(data_linear$type_vehicle_a %in% c(14, 23), 1, 0)
data_linear$vehicle_a_scooter = ifelse(data_linear$type_vehicle_a %in% c(15, 16, 17,18, 22),1 ,0)

data_linear$type_vehicle_a = NULL

# Convert the categorical variable vehicle b type into dummy variables
data_linear$vehicle_b_car = ifelse(data_linear$type_vehicle_b %in% c(1,2,3,4, 21),1,0)
data_linear$vehicle_b_autobus = ifelse(data_linear$type_vehicle_b %in% c(5,6,7), 1, 0)
#data_linear$vehicle_b_camion = ifelse(data_linear$type_vehicle_b %in% c(8, 9, 10, 11), 1, 0)
data_linear$vehicle_b_tractor = ifelse(data_linear$type_vehicle_b %in% c(12, 13), 1, 0)
data_linear$vehicle_b_bicycle = ifelse(data_linear$type_vehicle_b %in% c(14, 23), 1, 0)
data_linear$vehicle_b_scooter = ifelse(data_linear$type_vehicle_b %in% c(15, 16, 17,18, 22),1 ,0)

data_linear$type_vehicle_b = NULL

# Convert the days into dummy variables
data_linear$Sunday = ifelse(data_linear$day == 7, 1,0)
data_linear$Monday = ifelse(data_linear$day == 1, 1,0)
data_linear$Tuesday = ifelse(data_linear$day == 2, 1,0)
data_linear$Wednesday = ifelse(data_linear$day == 3, 1,0)
#data_linear$Thursday = ifelse(data_linear$day == 4, 1,0)
data_linear$Friday = ifelse(data_linear$day == 5, 1,0)
data_linear$Saturday = ifelse(data_linear$day == 6, 1,0)

data_linear$day = NULL

#Create two variables to handle the variable hour in linear regression keeping continuity
data_linear$sin_hour = sin(2 * pi * data_linear$hour / 24)
data_linear$cos_hour = cos(2 * pi * data_linear$hour / 24)

#data_linear$morning = ifelse(hour >= 7 & hour <= 12, 1,0 )
##data_linear$afternoon = ifelse(hour >= 13 & hour <= 18, 1,0 )
#data_linear$evening = ifelse(hour %in% c(0,19,20,21,22,23), 1,0 )
#data_linear$night = ifelse(hour >= 1 & hour <= 6, 1,0 )
data_linear$hour = NULL

data_linear$vehicle_a_driver_sex = ifelse(data_linear$vehicle_a_driver_sex == 2, 1, 0)
data_linear$vehicle_b_driver_sex = ifelse(data_linear$vehicle_b_driver_sex == 2, 1, 0)
#We delete all the columns containing no information (only zeros)
# Final_dataset = data_linear[, colSums(data_linear !=0) >0]
Final_dataset = data_linear[, colSums(data_linear != 0, na.rm = TRUE) > 0]


######  2) DATASET EXPLORATION   ######

## 2.1) Accidents in time  ##

#Create dataframe for heatmap
dd1 = data.frame(data$hour, data$day)  
df = data.frame(table(dd1))

# Heatmap of total # accidents each hour of each day of the week
df$data.day = factor(df$data.day, 
                      levels = 1:7, 
                      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

gg = ggplot(df, aes(x = data.day, y = data.hour, fill = Freq)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "#edf8fb", high = "#084081", name = "Crashes") + 
  labs(title = "Heatmap of Crashes",x = "Day", y = "Hour of the Day") + 
  scale_y_discrete(breaks = as.character(seq(0, 23, by = 2)) ) +
  scale_x_discrete(drop = FALSE ) + theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15), 
    axis.text.x = element_text(angle = 0, hjust = 1, size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10) ); gg

## 2.2) Location and conditions ##

#Road classification, Info on Carriageway, Floor, Intersection type etc

rclass = table(data$accident_location)  #1 is urban road in the built-up area
barplot(rclass)

tos = table(data$type_of_street)
barplot(tos)

floor = table(data$flooring)  # 1=paved road
barplot(floor)

int = table(data$intersection_type) # 7=straight, 4=int. with traffic light, 3=signaled int., 1=int.
barplot(int)

surf = table(data$road_surface_conditions) # 1=dry, 2=wet
barplot(surf)

meteo = table(data$meteorological_conditions)  # 1=clear, 7=else, 3=rain, 2=fog
barplot(meteo)

# Nature of the accident
nat = table(data$nature_of_the_accident) 
# 2=head-on - side collision, 3=lateral collision, 4=rear end collision, -
# 5=pedestrian impact, 6=Collision with stopping or stopped vehicle, -
# 8=collision with obstacle, 10=run off road/skidding
barplot(nat)

#Types of vehicles
sum_v = c(data$type_vehicle_a, data$type_vehicle_b, data$type_vehicle_c)
type_veh = sum_v[!is.na(sum_v)]
tv = table(type_veh)/13544
barplot(tv)

# Circumstances of the accident

# We note an inconsistency in the data: even when there is no vehicle b there -
# is a value for the vehicle_b_circumstances, there is no explanation for this -
# phenomenon in the metadata. So we filter accordingly.
sum_circ = c(data$vehicle_a_circumstances_1, data$vehicle_b_circumstances_1[!is.na(data$type_vehicle_b)])
type_circ = sum_circ[(!is.na(sum_circ))&(sum_circ!="")]
#table with relative percentages
circ = table(type_circ)/length(type_circ)
circdf = as.data.frame(circ)  #Note: 2,21,61 are "Was driving with distracted behavior or an uncertain course”, so are 4,6,49 for "not giving the right of way"

updated_circdf = circdf[(circdf$Freq>0.005),] #keeping only the relevant ones

ggplot(data = updated_circdf,  aes(x = type_circ, y = Freq)) +
  geom_col()

# According to INCSTRAD_Questionario_2022 we can group the circumstances in this way:
# 0: unkown circumstances. Accident between vehicles (1-39): 1-18: Accident in a intersection
# ) 20-39: Accident not in a intersection. Pedestrian impact (40-52)
# Moving vehicle with steady vehicle or obstacle (60-76):60-68: with impact
# ) 70-76: without impact. Defects or vehicle failure (80-89).Psycho-physical state (90-97)

circ = data.frame(type=c("Unknow circ.","Multiple v. - intersection", 
                         "Multiple v. - non intersection","Pedestrian impact",
                         "Moving v. vs steady v. or obstacle", 
                         "defects or v. failure", "Psycho-physical state"), 
                  percentage = c(circdf$Freq[1],sum(circdf$Freq[2:17]),
                                 sum(circdf$Freq[18:36]),sum(circdf$Freq[37:48]),
                                 sum(circdf$Freq[49:62]),sum(circdf$Freq[63:68]),
                                 0))

ggplot(data = circ, aes(x = type, y = percentage)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size= 8))

## 2.3) Gender and age of the driver ##

# Gender analysis
init = c(data$vehicle_a_driver_sex, data$vehicle_b_driver_sex, data$vehicle_c_driver_sex)
tot_sex = init[(!is.na(init))]
table(tot_sex)  #1 is male, 2 is female

# Creating a proper dataframe
# - df for vehicle a
va_sex = data$vehicle_a_driver_sex
va_age = data$vehicle_a_driver_age
va_df_sa = data.frame(sex = va_sex, age = va_age)  #the name stands for: vehicle-a dataframe of sex and age
va_df_sa2 = va_df_sa[!is.na(va_df_sa$age)&(!va_df_sa$age=="n.i."),] #removed 364 incomplete obs

# - df for vehicle b
vb_sex = data$vehicle_b_driver_sex
vb_age = data$vehicle_b_driver_age
vb_df_sa = data.frame(sex = vb_sex, age = vb_age)
vb_df_sa2 = vb_df_sa[!is.na(vb_df_sa$age),]  #removed instances of vehicle b not present and incomplete obs
vb_df_sa3 = vb_df_sa2[(!vb_df_sa2$age=="n.i."),] #removed 291 incomplete obs

# - df for vehicle c
vc_sex = data$vehicle_c_driver_sex
vc_age = data$vehicle_c_driver_age
vc_df_sa = data.frame(sex = vc_sex, age = vc_age)
vc_df_sa2 = vc_df_sa[!is.na(vc_df_sa$age),]  #removed instances of vehicle c not present and incomplete obs
vc_df_sa3 = vc_df_sa2[(!vc_df_sa2$age=="n.i."),] #removed 79 incomplete obs

# - merging the dfs and calculating percentages
v_df_sa = data.frame(sex= c(va_df_sa2$sex, vb_df_sa3$sex, vc_df_sa3$sex), age = 
                       c(va_df_sa2$age, vb_df_sa3$age, vc_df_sa3$age))
t_sa = table(v_df_sa);t_sa  
t_sa_df = data.frame(age_group=c("0-9","10-17","18-29","30-44","45-54","55-64","65+"),
                     male_percentage=c(6,121,2468,2934,1952,1532,948)/sum(c(2,4,34,87,2468,2934,1952,1532,948)),
                     female_percentage=c(2,22,764,900,534,371,190)/sum(c(1,1,0,22,764,900,534,371,190)))

# Reshape data into long format
t_sa_df$male_percentage = -t_sa_df$male_percentage
data_long = tidyr::pivot_longer(t_sa_df, cols = c("male_percentage", "female_percentage"),
               names_to = "Gender", values_to = "Percentage")

# Plot
ggplot(data_long, aes(x = Percentage, y = age_group, fill = Gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  scale_x_continuous(
    labels = abs,name = "Percentage",expand = expansion(mult = c(0.2, 0.2))) +
  scale_y_discrete(name = "Age Group") +
  scale_fill_manual(breaks = c("female_percentage", "male_percentage"),
    values = c("male_percentage" = "#4682B4", "female_percentage" = "#F08080"),
    labels = c("Female", "Male"),
    guide = guide_legend(reverse = TRUE) ) +
  labs(title = "Population Pyramid",       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15), 
    panel.border = element_rect(fill = NA),
    legend.position = "bottom",
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

## 2.4) Cubic Capacities

cubics = na.omit(c(data$cubic_capacity_a, data$cubic_capacity_b, data$cubic_capacity_c))
cubics = cubics[cubics<3500] #remove 90 outliers
df_cubics = data.frame(x = cubics)

# Create an enhanced histogram with many breaks (bins)
ggplot(df_cubics, aes(x = x)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
    bins = 50,fill = "#008080", color = "black") +
  labs(title = "Histogram of Vehicle Cubic Capacities",x = "Cubic capacity",  
    y = "Relative Frequency" ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) + 
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15), 
    panel.border = element_rect(fill = NA),
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 13))



######### 3) TESTS  ###########

## Asymptotic t TEST ##

#Creating proper variables
data_test = data[!is.na(data$vehicle_a_circumstances_1),]
data_test$speeding = ifelse(data_test$vehicle_a_circumstances_1 %in% c(11, 23, 41, 64, 72, 12, 24, 42, 65),1,0)
data_test = subset(data_test, select = c("tot_injured", "speeding"))
data_test = na.omit(data_test)
X = data_test$tot_injured[data_test$speeding == 1]
Y = data_test$tot_injured[data_test$speeding == 0]

#Test
t_test_result = t.test(X, Y, alternative = "greater", var.equal = FALSE)
print(t_test_result$p.value)


#########  4) LINEAR REGRESSION  #########

mean(data_linear$tot_injured)

## 4.1) Building the model ##

# Initialize a model with all predictors
regression = lm(tot_injured ~ ., data = Final_dataset)

# Model selection
model1 = ols_step_backward_p(regression, p_val = 0.05) # step down method
model2 = ols_step_forward_p(regression, p_val = 0.05) # step up method
summary(model1$model) # Let's see the step down model
summary(model2$model) # Let's see the step up model
regression1 = model1$model # we use the step down model

## 4.2) Regression diagnostics ##

# Analysis of homoscedasticity

# Create a data frame with fitted values and residuals and plot it
df_plot = data.frame(fitted     = fitted(regression1),
  residuals  = residuals(regression1))

ggplot(df_plot, aes(x = fitted, y = residuals)) +
  geom_point(shape = 1,color = "black", size = 3, alpha = 0.7) +           
  geom_hline(yintercept = 0, color = "red2", size = 1) +
  labs(title = "Residuals vs. Fitted Plot", x = "Fitted Values",y = "Residuals" ) +
  theme_minimal() +                                         
  theme(plot.title = element_text(face = "bold", size = 15), 
    panel.border = element_rect(fill = NA),
    axis.text        = element_text(size = 12),              
    axis.title       = element_text(size = 14),            
    panel.grid.major = element_line(color = "grey90", linetype = "solid"),  
    panel.grid.minor = element_line(color = "grey95", linetype = "solid") )

# Normality of the residuals

# QQ-plot

# Create a data frame with residuals
df_residuals = data.frame(residuals = residuals(regression1))

# Generate the QQ plot using ggplot2
ggplot(df_residuals, aes(sample = residuals)) +
  stat_qq(shape = 1,color = "black", size = 3, alpha = 0.7) +
  stat_qq_line(color = "red2", size = 1) +                       
  labs(title = "QQ Plot",x = "Theoretical Quantiles",y = "Sample Quantiles") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 15), 
    panel.border = element_rect(fill = NA),
    axis.text        = element_text(size = 12),                    
    axis.title       = element_text(size = 14),                  
    axis.line        = element_line(color = "black"),             
    axis.ticks       = element_line(color = "black"),              
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),  
    panel.grid.minor = element_line(color = "grey90", linetype = "dashed"))

## Histogram of the residuals

# Create a data frame with residuals
df_residuals = data.frame(residuals = residuals(regression1))

# Calculate mean and standard deviation of the residuals
mean_res = mean(df_residuals$residuals)
sd_res = sd(df_residuals$residuals)

# Generate data for the Gaussian curve
x_vals = seq(min(df_residuals$residuals), max(df_residuals$residuals), length.out = 500)
gaussian_curve = data.frame(x = x_vals,y = dnorm(x_vals, mean = mean_res, sd = sd_res),
  label = "Gaussian Curve (same\nmean and variance)")

# Plot histogram with Gaussian overlay and legend
ggplot(df_residuals, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
    bins = 30,fill = "#708090",color = "black",alpha = 0.8) +
  geom_line(data = gaussian_curve, 
  aes(x = x, y = y / sum(y) * 30, color = label),size = 0.8) +
  scale_color_manual(name = NULL,
    values = c("Gaussian Curve (same\nmean and variance)" = "red2")) +
  labs(title = "Histogram of Regression Residuals",x = "Residuals",y = "Relative Frequency") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 15), 
    panel.border = element_rect(fill = NA),
    legend.position = c(0.80, 0.85),              
    legend.background = element_rect(fill = "white", color = "black"), 
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13))

# Shapiro test for normality of the residuals
shapiro.test(residuals(regression1))


########### 5) LOGISTIC REGRESSION #############

## 5.1) Dataset ##
# Creating dataset for regression with categorical variables as factors

data_logistic$vehicle_b_circumstances = data_logistic$vehicle_b_circumstances_1*(!is.na(data_logistic$type_vehicle_b))

data_logistic$vehicle_a_circumstances = ifelse(is.na(data_logistic$vehicle_a_circumstances_1),0,data_logistic$vehicle_a_circumstances_1)
data_logistic$vehicle_b_circumstances = ifelse(is.na(data_logistic$vehicle_b_circumstances_1),0,data_logistic$vehicle_b_circumstances_1)

data_logistic$vehicle_a_circumstances_gen = cut(data_logistic$vehicle_a_circumstances, 
                       breaks = c(0,1,19,40,60,80,90),  
                       labels = 1:6,                    
                       include.lowest = TRUE)          
data_logistic$vehicle_b_circumstances_gen = cut(data_logistic$vehicle_b_circumstances, breaks = c(0,1,19,40,60,80,90),labels = 1:6,include.lowest = TRUE)   

categorical_vars1 = c("day", "accident_location", "type_of_street", "flooring", 
                      "intersection_type", "road_surface_conditions", 
                      "meteorological_conditions", "nature_of_the_accident",
                      "type_vehicle_a","type_vehicle_b","vehicle_a_circumstances_gen", 
                      "vehicle_b_circumstances_gen", "vehicle_a_driver_sex",
                      "vehicle_a_driver_licence","vehicle_b_driver_sex", 
                      "vehicle_b_driver_licence", "vehicle_c_driver_sex", 
                      "vehicle_c_driver_licence","trimester","hour","type_vehicle_c", 
                      "vehicle_a_circumstances_gen", "vehicle_b_circumstances_gen")

categorical_vars2 = c("vehicle_a_driver_age","vehicle_b_driver_age",
                      "vehicle_c_driver_age")

# - Convert specified columns to factors
for (var in categorical_vars1) {
  data_logistic[[var]] = factor(data_logistic[[var]])
}
for (var in categorical_vars2) {
  data_logistic[[var]] = factor(data_logistic[[var]])
}

str(data_logistic)  # Check structure of the dataset


## 5.2) Observations  ## 

# Performing a heuristic check on categorical variables

categorical_vars3 = c("day", "accident_location", "type_of_street", "flooring", 
                       "intersection_type", "road_surface_conditions", 
                       "meteorological_conditions", "nature_of_the_accident",
                       "type_vehicle_a", "type_vehicle_b", "vehicle_a_driver_sex", 
                       "vehicle_a_driver_licence", "vehicle_b_driver_sex", 
                       "vehicle_b_driver_licence", "vehicle_c_driver_sex", 
                       "vehicle_c_driver_licence", "trimester", "hour", "type_vehicle_c",
                       "vehicle_a_driver_age", "vehicle_a_circumstances_gen",
                       "vehicle_b_circumstances_gen")

lapply(categorical_vars3, function(var) {
  table_var = table(data_logistic[[var]], data_logistic$fatality)
  cat("\nVariable:", var, "\n")
  print(table_var)
})

# List of numeric variables
numeric_vars = c("vehicle_a_registration_year", "vehicle_b_registration_year",
                  "vehicle_a_release_year","vehicle_b_release_year",
                  "vehicle_c_release_year", "vehicle_a_driver_age1", 
                  "cubic_capacity_a", "cubic_capacity_b",
                  "cubic_capacity_c") 

## Looking at variable "hour" ##

# Convert hours to period data in this copy of the dataset
data_logistic$hour_sin = sin(2 * pi * data$hour/24)
data_logistic$hour_cos = cos(2 * pi * data$hour/24)

# Comparing periodical, categorical and numerical alternatives
modelA = glm(fatality ~ hour_sin + hour_cos, data = data_logistic, family = binomial)
summary(modelA)

modelB = glm(fatality ~ hour, data = data_logistic, family = binomial)
summary(modelB)

modelC= glm(fatality ~ hour, data = data_logistic, family = binomial)
summary(modelC)

## 5.3) Model selection ##

##################################### 
# This function tests a model with specified Y and X, and returns F-beta score

test_logistic_model = function(data, dependent_var, 
                                    independent_vars, 
                                    beta = 0.5,       # parameter for the F-beta score
                                    threshold = 0.5,  # decision threshold for classification
                                    size_test = 0.5)  # how much of the data to use for training
                                  {
  
  # Create the formula dynamically
  formula = as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
  
  # Shuffle the data
  set.seed(123) # For reproducibility
  shuffled_data = data %>% sample_frac(1)
  
  # Determine the size of each fold
  total_rows = nrow(shuffled_data)
  fold_size = ceiling(total_rows*size_test)
  
  # Split data into training and testing
  train_data = shuffled_data[1:fold_size, ]
  test_data = shuffled_data[(fold_size + 1):total_rows, ]
    
  # Determine expected classes from the entire dataset
  expected_classes = unique(data[[dependent_var]])
    
  # Calculate class proportions in the training set
  class_counts = table(train_data[[dependent_var]])
  total_train = sum(class_counts)
    
  # Check for missing classes
  present_classes = names(class_counts)
  missing_classes = setdiff(expected_classes, present_classes)
  if (length(missing_classes) > 0) {
    stop(paste("Missing classes in training set for fold", paste(missing_classes, collapse = ", ")))
  }
  
  # Calculate and assign weights 
  weight_fat = total_train / (2 * class_counts["1"])
  weight_nfat = total_train / (2 * class_counts["0"])
  train_data$weight = ifelse(train_data[[dependent_var]] == 1, weight_fat, weight_nfat)
  cat("Assigned Weights:\n")
  cat(paste("Class 1 Weight:", round(weight_fat, 4), "\n"))
  cat(paste("Class 0 Weight:", round(weight_nfat, 4), "\n"))

  # Fit logistic regression model on the training set
  model = glm(formula, 
                data = train_data, 
                family = binomial, 
                weights = train_data$weight,
                control = glm.control(maxit = 50, epsilon = 1e-8))  # allow for more iterations to converge
    
  print(summary(model))
  
  # Align factor levels across train_data and test_data for each variable in
  # independent_vars if both columns are factors.
  for (var in independent_vars) {
    if (is.factor(train_data[[var]]) && is.factor(test_data[[var]])) {
      # Get the union of the factor levels
      new_levels = union(levels(train_data[[var]]), levels(test_data[[var]]))
      
      # Reset the factors with the new levels
      train_data[[var]] = factor(train_data[[var]], levels = new_levels)
      test_data[[var]] = factor(test_data[[var]], levels = new_levels)
    }
  }
  
  # Make predictions on the testing set
  test_data$predicted_prob = predict(model, newdata = test_data, type = "response")
  test_data$predicted_class = ifelse(test_data$predicted_prob > threshold, 1, 0)
  
  # Evaluate the model by calculating metrics directly
  actual = test_data[[dependent_var]]
  predicted = test_data$predicted_class

  # Removing NAs 
  complete_cases = !is.na(predicted)
  actual_clean = actual[complete_cases]
  predicted_clean = predicted[complete_cases]
  
  # Calculate True Positives, False Positives, True Negatives, False Negatives
  TP = ifelse(is.na(sum((predicted_clean == 1) & (actual_clean == 1))),0,sum((predicted_clean == 1) & (actual_clean == 1)))
  FP = ifelse(is.na(sum((predicted_clean == 1) & (actual_clean == 0))),0,sum((predicted_clean == 1) & (actual_clean == 0)))
  TN = ifelse(is.na(sum((predicted_clean == 0) & (actual_clean == 0))),0,sum((predicted_clean == 0) & (actual_clean == 0)))
  FN = ifelse(is.na(sum((predicted_clean == 0) & (actual_clean == 1))),0,sum((predicted_clean == 0) & (actual_clean == 1)))
    
  # Message with confusion matrix details
  cat(sprintf("True Positives  = %d\nFalse Positives = %d\nTrue Negatives  = %d\nFalse Negatives = %d\n\n", TP, FP, TN, FN))
  # Metrics
  precision = ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall = ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  fbeta_score = ifelse((precision + recall) == 0, 0, 
                          (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall))  

  cat(paste("Fbeta Score:", round(fbeta_score, 4), "\n"))
  return(fbeta_score)
}

#######################################

# Testing the function
results = test_logistic_model(data = data_logistic, dependent_var = "fatality", 
                               independent_vars = c("type_of_street"), beta = 0.5,
                               threshold = 0.5,size_test = 0.75)

# NOTE: the Warning about "non integer successes..." is normal when applying -
# weights to the classes.

## Step-up method ## (implemented manually). First step
independent_vars_vector = c(categorical_vars3, numeric_vars)

f1_scores = list()

for (var in independent_vars_vector) {
  # Run the logistic model using only the current independent variable
  results = test_logistic_model(data = data_logistic, dependent_var = "fatality",
                                    independent_vars = var, beta = 0.5, size_test = 0.75)
    f1_scores[[var]] = results
}

print(f1_scores)  #the fbeta score associated to "accident_location" is highest 

# 2nd STEP in Step Up

# Removing varibles that eccessively restric the number of accidents combined with accident_location
independent_vars_vector2 = independent_vars_vector[c(-2,-13,-15,-21,-24,-27,-29,-31)] 

f1_scores2 = list()

for (var in independent_vars_vector2) {
  results = test_logistic_model(data = data_logistic, dependent_var = "fatality",
                                    independent_vars = c("accident_location",var), 
                                    beta = 0.5, size_test = 0.75)
    f1_scores2[[var]] = results
}

print(f1_scores2)   #step up method tells us to stop

## 5.4) Optimizing weights ##

###################################
# This function performs k-fold cross-validation of a model and returns the mean 
# F-beta score. Additionally, it gives the option to use custom class-weights

test_logistic_model_cv = function(data, 
                                   dependent_var, 
                                   independent_vars, 
                                   beta = 0.5, 
                                   threshold = 0.5,
                                   custom_weights = NULL,  # option to run with custom weights
                                   n_folds = 5,
                                   print_summary = FALSE) {
  
  # Create the formula dynamically
  formula = as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
  
  fbeta_scores = numeric(n_folds)
  set.seed(123)
  
  # Create stratified folds to maintain class distribution in each fold
  folds = createFolds(data[[dependent_var]], k = n_folds, list = TRUE, returnTrain = FALSE)
  
  # Iterate over each fold
  for (i in 1:n_folds) {
    if (print_summary) {
      cat(sprintf("\n--- Fold %d ---\n", i))
    }
    
    # Split data into training and testing based on current fold
    test_indices = folds[[i]]
    test_data = data[test_indices, ]
    train_data = data[-test_indices, ]
    
    # Determine expected classes from the entire dataset
    expected_classes = unique(data[[dependent_var]])
    
    # Calculate class proportions in the training set
    class_counts = table(train_data[[dependent_var]])
    total_train = sum(class_counts)
    
    # Check for missing classes in the training set
    present_classes = names(class_counts)
    missing_classes = setdiff(as.character(expected_classes), present_classes)
    if (length(missing_classes) > 0) {
      stop(paste("Missing classes in training set for fold", i, ":", paste(missing_classes, collapse = ", ")))
    }
    
    # Assign weights
    if (!is.null(custom_weights)) {
      # Use custom weights provided by the user
      weight_fat = custom_weights["1"]
      weight_nfat = custom_weights["0"]
      
      if (is.na(weight_fat) || is.na(weight_nfat)) {
        stop("custom_weights must include weights for both classes '0' and '1'.")
      }
      
      train_data$weight = ifelse(train_data[[dependent_var]] == 1, weight_fat, weight_nfat)
      
      if (print_summary) {
        cat("Assigned Custom Weights:\n")
        cat(paste("Class 1 Weight:", round(weight_fat, 4), "\n"))
        cat(paste("Class 0 Weight:", round(weight_nfat, 4), "\n"))
      }
      
    } else {
      # Calculate weights based on class proportions in the training set
      weight_fat = total_train / (2 * class_counts["1"])
      weight_nfat = total_train / (2 * class_counts["0"])
      train_data$weight = ifelse(train_data[[dependent_var]] == 1, weight_fat, weight_nfat)
      
      if (print_summary) {
        cat("Assigned Weights Based on Training data_linear:\n")
        cat(paste("Class 1 Weight:", round(weight_fat, 4), "\n"))
        cat(paste("Class 0 Weight:", round(weight_nfat, 4), "\n"))
      }
    }
    
    # Fit logistic regression model on the training set
    model = glm(formula, 
                 data = train_data, 
                 family = binomial, 
                 weights = train_data$weight,
                 control = glm.control(maxit = 50, epsilon = 1e-8))
    
    if (print_summary) {
      print(summary(model))
    }
    
    # Align factor levels across train_data and test_data for each independent variable
    for (var in independent_vars) {
      if (is.factor(train_data[[var]]) && is.factor(test_data[[var]])) {
        # Get the union of the factor levels
        new_levels = union(levels(train_data[[var]]), levels(test_data[[var]]))
        
        # Reset the factors with the new levels
        train_data[[var]] = factor(train_data[[var]], levels = new_levels)
        test_data[[var]] = factor(test_data[[var]], levels = new_levels)
      }
    }
    
    # Make predictions on the testing set
    test_data$predicted_prob = predict(model, newdata = test_data, type = "response")
    test_data$predicted_class = ifelse(test_data$predicted_prob > threshold, 1, 0)
    
    # Evaluate the model by calculating metrics
    actual = test_data[[dependent_var]]
    predicted = test_data$predicted_class
    
    # Removing NAs 
    complete_cases = !is.na(predicted)
    actual_clean = actual[complete_cases]
    predicted_clean = predicted[complete_cases]
    
    # Calculate True Positives, False Positives, True Negatives, False Negatives
    TP = ifelse(is.na(sum((predicted_clean == 1) & (actual_clean == 1))),0,sum((predicted_clean == 1) & (actual_clean == 1)))
    FP = ifelse(is.na(sum((predicted_clean == 1) & (actual_clean == 0))),0,sum((predicted_clean == 1) & (actual_clean == 0)))
    TN = ifelse(is.na(sum((predicted_clean == 0) & (actual_clean == 0))),0,sum((predicted_clean == 0) & (actual_clean == 0)))
    FN = ifelse(is.na(sum((predicted_clean == 0) & (actual_clean == 1))),0,sum((predicted_clean == 0) & (actual_clean == 1)))
    
    if (print_summary) {
      cat(sprintf("True Positives  = %d\nFalse Positives = %d\nTrue Negatives  = %d\nFalse Negatives = %d\n\n", TP, FP, TN, FN))
    }
    
    # Metrics
    precision = ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    recall = ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    fbeta_score = ifelse((precision + recall) == 0, 0, 
                          (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall))  
    
    if (print_summary) {
      cat(paste("F-beta Score:", round(fbeta_score, 4), "\n"))
    }
    
    # Store the F-beta score for the current fold
    fbeta_scores[i] = fbeta_score
  }
  
  # Calculate mean F-beta score across all folds
  mean_fbeta = mean(fbeta_scores, na.rm = TRUE)
  
  cat(sprintf("\nMean F-beta Score over %d folds: %.4f\n", n_folds, mean_fbeta))
  
  return(mean_fbeta)
}
###################################

# To test the function
results = test_logistic_model_cv(data = data_logistic, dependent_var = "fatality", 
                               independent_vars = c("accident_location"), beta = 0.5,
                               threshold = 0.5)


# Test function with custom weights
custom_weights = c("0" = 1.09, "1" = 84)

results = test_logistic_model_cv(data = data_logistic, dependent_var = "fatality", 
                                   independent_vars = c("accident_location"), beta = 0.5,
                                   threshold = 0.5,custom_weights = custom_weights, print_summary = T)


##############################
# This function explores all permutations of class weights for classes "0" and 
# "1" to identify the optimal weight combination based on the mean F-beta score.

optimize_weights_fbeta_heatmap = function(data, dependent_var, independent_vars, 
                                           beta = 0.5, 
                                           threshold = 0.5,
                                           range_weight_0,  # Numeric vector of length 2 specifying the range for weight "0" 
                                           step_weight_0,   # Numeric value specifying the step size for weight "0" 
                                           range_weight_1, 
                                           step_weight_1) {

  # Generate Weight Vectors
  weight0_vector = seq(from = range_weight_0[1], to = range_weight_0[2], by = step_weight_0)
  weight1_vector = seq(from = range_weight_1[1], to = range_weight_1[2], by = step_weight_1)
  
  # Create Weight Grid with all possible combinations of weights for classes "0" and "1".
  weight_grid = expand.grid(weight0 = weight0_vector, weight1 = weight1_vector)
  
  # Create a matrix to store mean F-beta scores with rows as weight "0" and columns as weight "1".
  fbeta_scores = matrix(NA, nrow = length(weight0_vector), 
                         ncol = length(weight1_vector),
                         dimnames = list(weight0 = weight0_vector, weight1 = weight1_vector))
  
  # Iterate Over Weight Combinations
  for (i in 1:nrow(weight_grid)) {
    # Extract current weights
    w0 = weight_grid$weight0[i]
    w1 = weight_grid$weight1[i]
    
    # Define the weights as a named vector
    weights = c("0" = w0, "1" = w1)
    
    cat("Evaluating Weights: Class 0 =", w0, ", Class 1 =", w1, "\n")
    
    # Call the Logistic Regression Function
    # + use tryCatch to handle any errors during model training.
    result = tryCatch({
      test_logistic_model_cv(data = data, 
                             dependent_var = dependent_var, 
                             independent_vars = independent_vars, 
                             beta = beta, 
                             threshold = threshold,
                             custom_weights = weights)
    }, error = function(e){
      cat("Error with weights:", weights, "\n")
      return(NULL)
    })
    
    #Store the Mean F-beta Score in corresponding matrix entry
    if (!is.null(result)) {
      fbeta_scores[as.character(w0), as.character(w1)] = result
    }
  }
  
  return(list(
    weight_grid = weight_grid,
    fbeta_scores = fbeta_scores
  ))
}
##################################

##  Create the Level Plot  ##

# WARNING: running the function is computationally intensive. Adjust the ranges 
# and steps of the weights for desired granularity and computing time

optimization_results = optimize_weights_fbeta_heatmap(
  data = data_logistic, 
  dependent_var = "fatality", 
  independent_vars = c("accident_location"), 
  beta = 0.5, 
  threshold = 0.5,
  range_weight_0 = c(0.4,0.8), #c(0.1,0.9), was used for our plot
  step_weight_0 = 0.055,
  range_weight_1 = c(80,100), # c(10,120), was used for our plot
  step_weight_1 = 2
)

# Create proper df
fbeta_df = as.data.frame(as.table(optimization_results$fbeta_scores))
colnames(fbeta_df) = c("Weight0", "Weight1", "Mean_Fbeta_Score")
fbeta_df$Weight0 = as.numeric(as.character(fbeta_df$Weight0))
fbeta_df$Weight1 = as.numeric(as.character(fbeta_df$Weight1))

# Find best values
high_performers = subset(fbeta_df, Mean_Fbeta_Score >= 0.036)
print(high_performers)
proportion_highperf = high_performers$Weight0/high_performers$Weight1
summary(proportion_highperf)

# Create the Level Plot
level_plot = levelplot(Mean_Fbeta_Score ~ Weight1 * Weight0,
  data = fbeta_df, cuts = 10, xlab = list("Weight for Class 1", cex = 1.1),
  ylab = list("Weight for Class 0", cex = 1.1),
  scales = list(x = list(cex = 1.1),y = list(cex = 1.1)),
  panel = function(...) {panel.levelplot(...)  
    panel.abline(a = 0, b = 0.01415, col = "#FFA555", lwd = 1.4)
  })
print(level_plot)

# Add title
grid.text("Level Plot of Mean F-beta Scores", x = 0.4, y = 0.95,
  gp = gpar(fontsize = 14,fontface = "bold",fontfamily = "Helvetica"))




