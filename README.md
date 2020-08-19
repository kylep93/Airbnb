# Airbnb
library(readr)
main_data <- read_csv("main_data.csv")
View(main_data)
library(readxl)
name_survey_results <- read_excel("name_survey_results.xlsx")
View(name_survey_results)
library(haven)
hosts <- read_dta("hosts.dta")
View(hosts)
library(lmtest)
colnames(main_data) <- c("host_response", "response_date", "number_of_messages", "auto_code", "latitude",
                         "longitude", "bed_type", "property_type", "cancellation_policy", "number_guests",
                         "bedrooms", "bathrooms", "cleaning_fee", "price", "apt_rating", "property_setup",
                         "city", "date_sent", "listing_down", "num_of_listings", "num_of_reviews",
                         "member_since", "verified_id", "host_race", "super_host", "host_gender", "host_age",
                         "host_gender_1", "host_gender_2", "host_gender_3", "host_race_1", "host_race_2", "host_race_3",
                         "guest_first_name", "guest_last_name", "guest_race", "guest_gender","guest_id", "population", "white",
                         "blacks", "asians", "hispanics", "available_sept", "up_not_available_sept", "sept_price", "census_tract",
                         "host_id", "new_number_of_listings")
main_data[main_data == "\\N"] <- "."
main_data[main_data == "NULL"] <-  "."
main_data[main_data == "-1"] <- "."
main_data$guest_black <- main_data$guest_race == "black"
main_data$guest_white <- main_data$guest_race == "white"
main_data$guest_male <- main_data$guest_gender == "male"
main_data$guest_by_city <- paste(main_data$guest_first_name, main_data$city)
install.packages("tidyverse")
library(tidyverse)
main_data <- left_join(main_data, name_survey_results, by = c("guest_first_name","guest_first_name"))
main_data$guest_race_continuous <- main_data$guest_race_continuous - 1
main_data$host_black <- main_data$host_race == "black"
main_data$host_white <- main_data$host_race == "white"
main_data$host_asian <- main_data$host_race == "asian"
main_data$host_hisp <- main_data$host_race == "hisp"
main_data$host_male <- main_data$host_gender == "M"
main_data$host_female <- main_data$host_gender == "F"
main_data$host_gender <- trimws(main_data$host_gender)
main_data$host_same_sex <- main_data$host_gender
main_data$host_diff_sex <- main_data$host_gender
main_data <-   main_data %>% mutate(host_same_sex= case_when(host_same_sex == "MM" ~ 1,
                                                       host_same_sex == "FF" ~ 1, TRUE ~ 0))
main_data <-   main_data %>% mutate(host_diff_sex= case_when(host_diff_sex == "MM" ~ 1,
                                                             host_diff_sex == "FF" ~ 1, TRUE ~ 0))
main_data$host_age_cat <- main_data$host_age
main_data$host_age_cat[main_data$host_age_cat == "young" | main_data$host_age_cat == "young/UU" |
                         main_data$host_age_cat == "UU/young" | main_data$host_age_cat == "young/NA" |
                         main_data$host_age_cat == "NA/young"] <- 0
main_data$host_age_cat[main_data$host_age_cat == "middle/young" | main_data$host_age_cat == "young/middle"] <- 1
main_data$host_age_cat[main_data$host_age_cat == "middle" | main_data$host_age_cat == "middle/UU"|
                         main_data$host_age_cat == "UU/middle" | main_data$host_age_cat == "middle/NA"|
                         main_data$host_age_cat == "NA/middle"] <- 2
main_data$host_age_cat[main_data$host_age_cat == "middle/old" | main_data$host_age_cat == "old/middle"] <- 3
main_data$host_age_cat[main_data$host_age_cat == "old" | main_data$host_age_cat == "old/UU" |
                         main_data$host_age_cat == "UU/old" | main_data$host_age_cat == "old/NA" |
                         main_data$host_age_cat == "NA/old"] <- 4
main_data$ten_reviews <- main_data$num_of_reviews >= 10
main_data$five_star_property <- main_data$apt_rating == 5
main_data$multiple_listings <- main_data$num_of_listings > 1
main_data$shared_property <-  main_data$property_setup == "Private Room" | main_data$property_setup == "Shared Room"
main_data$shared_bathroom <- main_data$shared_property == 1 & main_data$bathrooms <1.5
main_data$strict_cancellation <- main_data$cancellation_policy == "Strict"
main_data$young <- main_data$host_age_cat == 0
main_data$middle <- main_data$host_age_cat == 1 | main_data$host_age_cat == 2
main_data$old <- main_data$host_age_cat == 3 | main_data$host_age_cat == 4
main_data$price <- as.numeric(main_data$price)
main_data <- na.omit(main_data)
pricey <- quantile(main_data$price, .9)
main_data$pricey <- main_data$price >= pricey
price_median <- quantile(main_data$price, .5)
main_data$price_median <- main_data$price > price_median
main_data$log_price <- log(main_data$price)
main_data$white_proportion <- as.numeric(main_data$white) / as.numeric(main_data$population)
main_data$black_proportion <- as.numeric(main_data$blacks) / as.numeric(main_data$population)
main_data$asian_proportion <- as.numeric(main_data$asians) / as.numeric(main_data$population)
main_data$hispanic_proportion <- as.numeric(main_data$hispanics) / as.numeric(main_data$population)
#main_data$tract_listings <- main_data %>% group_by(census_tract) %>% warnings(sum(main_data$latitude > 0))
main_data$tract_listings <- as.numeric(with(main_data, ave(census_tract, list(main_data$latitude, main_data$census_tract),
                                                FUN = seq_along)))
main_data$log_tract_listings <- log(main_data$tract_listings)
#main_data$tract_listings <- main_data %>% group_by(census_tract) %>% summarise(latitude, n())
main_data$host_response <- as.numeric(main_data$host_response)
main_data$host_response_labels <- main_data$host_response
main_data$host_response_labels[main_data$host_response_labels == 0] <- "No or unavailable"
main_data$host_response_labels[main_data$host_response_labels == 1] <- "Yes"
main_data$host_response_labels[main_data$host_response_labels == 2] <- "Request for more info (Can you verify? How many people?)"
main_data$host_response_labels[main_data$host_response_labels == 3] <- "No, unless you verify"
main_data$host_response_labels[main_data$host_response_labels == 4] <- "Yes, if you verify/give more info"
main_data$host_response_labels[main_data$host_response_labels == 5] <- "Offers a different place"
main_data$host_response_labels[main_data$host_response_labels == 6] <- "Offers Lower Price if you Book Now"
main_data$host_response_labels[main_data$host_response_labels == 7] <- "Asks for higher price"
main_data$host_response_labels[main_data$host_response_labels == 8] <- "Yes if stay is extended"
main_data$host_response_labels[main_data$host_response_labels == 9] <- "Check back later for definitive answer"
main_data$host_response_labels[main_data$host_response_labels == 10] <- "I will get back to you"
main_data$host_response_labels[main_data$host_response_labels == 11] <- "Unsure right now"
main_data$host_response_labels[main_data$host_response_labels == 12] <- "Only used for events"
main_data$host_response_labels[main_data$host_response_labels == 13] <- "Confused (our date error)"
main_data$host_response_labels[main_data$host_response_labels == 14] <- "Message not sent"
main_data$host_response_labels[main_data$host_response_labels == -1] <- "No response"

main_data$simplified_host_response <- main_data$host_response
main_data$simplified_host_response[main_data$simplified_host_response == 4] <- 2
main_data$simplified_host_response[main_data$simplified_host_response == 6] <- 3
main_data$simplified_host_response[main_data$simplified_host_response == 8] <- 4
main_data$simplified_host_response[main_data$simplified_host_response == 7] <- 6
main_data$simplified_host_response[main_data$simplified_host_response == 2] <- 7
main_data$simplified_host_response[main_data$simplified_host_response >= 9 &
                                     main_data$simplified_host_response <= 11] <- 8
main_data$simplified_host_response[main_data$simplified_host_response == -1] <- 9
main_data$simplified_host_response[main_data$simplified_host_response == 3] <- 10
main_data$simplified_host_response[main_data$simplified_host_response == 0] <- 11
main_data$simplified_host_response[main_data$simplified_host_response >= 12 &
                                     main_data$simplified_host_response <= 14] <- "."
main_data$simplified_host_response_lables <- as.numeric(main_data$simplified_host_response)
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 1] <- "Yes"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 2] <- "Yes, but requests more info" 
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 3] <- "Yes, with lower price if booked now"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 4] <- "Yes, if guest extends"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 5] <- "Yes, but in different property"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 6] <- "Yes, at a higher price"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 7] <- "Requests more information"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 8] <- "Not sure or check later"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 9] <- "No response"
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 10] <- "No, without more information" 
main_data$simplified_host_response_lables[main_data$simplified_host_response_lables == 11] <- "No"
main_data$graph_bins <- as.numeric(main_data$simplified_host_response)
main_data$graph_bins[main_data$graph_bins >= 2 & main_data$graph_bins <=6] <- 2
main_data$graph_bins[main_data$graph_bins == 9] <- 3
main_data$graph_bins[main_data$graph_bins == 7 | main_data$graph_bins ==8 |
                       main_data$graph_bins == 10] <- 4
main_data$graph_bins[main_data$graph_bins == 11] <- 5
main_data$graph_bins_labels <- main_data$graph_bins
main_data$graph_bins_labels[main_data$graph_bins_labels == 1] <- "Yes"
main_data$graph_bins_labels[main_data$graph_bins_labels == 2] <- "Conditional Yes"
main_data$graph_bins_labels[main_data$graph_bins_labels == 3] <- "No Response"
main_data$graph_bins_labels[main_data$graph_bins_labels == 4] <- "Conditional No"
main_data$graph_bins_labels[main_data$graph_bins_labels == 5] <- "No"

main_data$positive_response <- main_data$host_response
main_data$negative_response <- main_data$host_response
main_data$negative_response[main_data$positive_response == 1 | main_data$positive_response == 4|
                              main_data$positive_response == 6] <- 0
main_data$negative_response[main_data$positive_response == 0 | main_data$positive_response == -1|
                              main_data$positive_response == 2 | main_data$positive_response == 3 |
                              (main_data$positive_response >6 & main_data$positive_response <13)] <- 1
main_data$positive_response[main_data$positive_response == 1 | main_data$positive_response == 4|
                              main_data$positive_response == 6] <- 1
main_data$positive_response[main_data$positive_response == 0 | main_data$positive_response == -1|
                              main_data$positive_response == 2 | main_data$positive_response == 3 |
                              (main_data$positive_response >6 & main_data$positive_response <13)] <- 0
main_data <- main_data[!grepl("Atlanta", main_data$city, fixed = T),]
main_data <- main_data[!grepl("Tampa", main_data$city, fixed = T),]
fix(main_data)
main_data$baltimore <- main_data$city == "Baltimore"
main_data$dallas <- main_data$city == "Dallas"
main_data$los_angeles <- main_data$city == "Los-Angeles"
main_data$sl <- main_data$city == "St-Louis"
main_data$dc <- main_data$city == "Washington"

main_data <- left_join(main_data, hosts, by = c("host_id","host_id"))
main_data$past_guest_merge <- as.numeric(main_data$past_guest_merge)
main_data <- main_data %>% filter(past_guest_merge != 2)
main_data$filled_sept <- main_data$up_not_available_sept == 1

main_data <- na.omit(main_data)
mod_probit <- glm(filled_sept ~ host_black + host_asian + host_hisp + host_male + log_price + bedrooms + 
                    shared_bathroom + shared_property + num_of_reviews + young + multiple_listings +
                    white_proportion + log_tract_listings + baltimore + dallas + los_angeles + sl,
                    data = main_data, family = binomial(link = "probit"))
install.packages("clubSandwich")
suppressMessages(library(clubSandwich))
clusV <- vcovCR(mod_probit, main_data$city, "CR1")
round(coeftest(mod_probit,vcov=clusV),4)
summary(mod_probit)
probit_predictions <- predict(mod_probit)
library(survival)
library(sandwich)
#table2 results
mod1_t2 <- lm(positive_response ~ guest_black, data = main_data)
summary(mod1_t2)
clusV_mod1_t2 <- vcovCR(mod1_t2, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t2, vcov=clusV_mod1_t2),4)
mod2_t2 <- lm(positive_response ~ guest_black + host_black + host_male, data = main_data)
clusV_mod2_t2 <- vcovCR(mod2_t2, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t2, vcov=clusV_mod2_t2),4)
summary(mod2_t2)
mod3_t2 <- lm(positive_response ~ guest_black + host_black + host_male + multiple_listings + shared_property+
              ten_reviews + log_price, data = main_data)
clusV_mod3_t2 <- vcovCR(mod3_t2, main_data$guest_by_city, "CR1")
round(coeftest(mod3_t2, vcov=clusV_mod3_t2),4)
summary(mod3_t2)
#table 3 results
main_data$guest_host_black <- main_data$guest_black * main_data$host_black
mod1_t3 <- lm(positive_response~ guest_black + host_black + guest_host_black, data = main_data)
clusV_mod1_t3 <- vcovCR(mod1_t3, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t3, vcov= clusV_mod1_t3),4)
mod2_t3 <- lm(positive_response~ guest_black + host_black + guest_host_black,
              data = subset(main_data, host_male ==T))
summary(mod1_t5)
clusV_mod2_t3 <- vcovCR(mod2_t3, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t3, vcov= clusV_mod2_t3),4)
mod3_t3 <- lm(positive_response~ guest_black + host_black + guest_host_black,
              data = subset(main_data, host_female ==T))
mod4_t3 <- lm(positive_response~ guest_black + host_black + guest_host_black,
              data = subset(main_data, host_male !=T & host_female != T))
#table 5 results
mod1_t5 <- lm(positive_response~ guest_black + shared_property + I(shared_property*guest_black),
                                 data = main_data)
clusV_mod1_t5 <- vcovCR(mod1_t5, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t5, vcov= clusV_mod1_t5),4)
mod2_t5 <- lm(positive_response~ guest_black + multiple_listings + I(multiple_listings*guest_black),
              data = main_data)
clusV_mod2_t5 <- vcovCR(mod2_t5, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t5, vcov=clusV_mod2_t5),4)
mod3_t5 <- lm(positive_response~ guest_black + ten_reviews + I(ten_reviews*guest_black),
              data = main_data)
clusV_mod3_t5 <- vcovCR(mod3_t5, main_data$guest_by_city, "CR1")
round(coeftest(mod3_t5, vcov= clusV_mod3_t5),4)
mod4_t5 <- lm(positive_response~guest_black + young + I(young*guest_black), data = main_data)
clusV_mod4_t5 <- vcovCR(mod4_t5, main_data$guest_by_city, "CR1")
round(coeftest(mod4_t5, vcov= clusV_mod4_t5),4)
mod5_t5 <- lm(positive_response~ guest_black + any_black + I(any_black*guest_black), data = main_data)
clusV_mod5_t5 <- vcovCR(mod5_t5, main_data$guest_by_city, "CR1")
round(coeftest(mod5_t5, vcov= clusV_mod5_t5),4)
#table 6 results
mod1_t6 <- lm(positive_response~ guest_black + price_median + I(guest_black*price_median), data = main_data)
clusV_mod1_t6 <- vcovCR(mod1_t6, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t6, vcov= clusV_mod1_t6),4)
mod2_t6 <- lm(positive_response~ guest_black + black_proportion + I(guest_black*black_proportion),
              data = main_data)
clusV_mod2_t6 <- vcovCR(mod2_t6, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t6, vcov= clusV_mod2_t6),4)
mod3_t6 <- lm(positive_response~guest_black + tract_listings + I(guest_black*tract_listings), data = main_data)
clusV_mod3_t6 <- vcovCR(mod3_t6, main_data$guest_by_city, "CR1")
round(coeftest(mod3_t6, vcov= clusV_mod3_t6),4)
mod4_t6 <- lm(positive_response~ guest_black + probit_predictions + I(guest_black*probit_predictions),
              data = main_data)
clusV_mod4_t6 <- vcovCR(mod4_t6, main_data$guest_by_city, "CR1")
round(coeftest(mod4_t6, vcov= clusV_mod4_t6),4)

#robustness check
rob1 <- lm(positive_response~guest_black + raw_black + I(raw_black*guest_black), data = main_data)
clusV_rob1 <- vcovCR(rob1, main_data$guest_by_city, "CR1")
round(coeftest(rob1, vcov= clusV_rob1),4)
rob2 <- lm(positive_response~guest_black + prop_black + I(prop_black*guest_black),data = main_data)
clusV_rob2 <- vcovCR(rob2, main_data$guest_by_city, "CR1")
round(coeftest(rob2, vcov= clusV_rob2),4)
rob3 <- lm(positive_response~guest_black + any_black + I(guest_black*any_black), data = main_data)
clusV_rob3 <- vcovCR(rob3, main_data$guest_by_city, "CR1")
round(coeftest(rob3, vcov= clusV_rob3),4)

#######all regressions are ran again but with the response variable as a negative response instead positive
#table2 results
mod1_t2_no <- lm(negative_response ~ guest_black, data = main_data)
summary(mod1_t2)
clusV_mod1_t2_no <- vcovCR(mod1_t2_no, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t2_no, vcov=clusV_mod1_t2_no),4)
mod2_t2_no <- lm(negative_response ~ guest_black + host_black + host_male, data = main_data)
clusV_mod2_t2_no <- vcovCR(mod2_t2_no, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t2_no, vcov=clusV_mod2_t2_no),4)
mod3_t2_no <- lm(negative_response ~ guest_black + host_black + host_male + multiple_listings + shared_property+
                ten_reviews + log_price, data = main_data)
clusV_mod3_t2_no <- vcovCR(mod3_t2_no, main_data$guest_by_city, "CR1")
round(coeftest(mod3_t2_no, vcov=clusV_mod3_t2_no),4)
#table 3 results
main_data$guest_host_black <- main_data$guest_black * main_data$host_black
mod1_t3_no <- lm(negative_response~ guest_black + host_black + guest_host_black, data = main_data)
clusV_mod1_t3_no <- vcovCR(mod1_t3_no, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t3_no, vcov= clusV_mod1_t3_no),4)
mod2_t3_no <- lm(negative_response~ guest_black + host_black + guest_host_black,
              data = subset(main_data, host_male ==T))
summary(mod1_t5_no)
clusV_mod2_t3_no <- vcovCR(mod2_t3_no, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t3_no, vcov= clusV_mod2_t3_no),4)
mod3_t3_no <- lm(negative_response~ guest_black + host_black + guest_host_black,
              data = subset(main_data, host_female ==T))
mod4_t3_no <- lm(negative_response~ guest_black + host_black + guest_host_black,
              data = subset(main_data, host_male !=T & host_female != T))
#table 5 results
mod1_t5_no <- lm(negative_response~ guest_black + shared_property + I(shared_property*guest_black),
              data = main_data)
clusV_mod1_t5_no <- vcovCR(mod1_t5_no, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t5_no, vcov= clusV_mod1_t5_no),4)
mod2_t5_no <- lm(negative_response~ guest_black + multiple_listings + I(multiple_listings*guest_black),
              data = main_data)
clusV_mod2_t5_no <- vcovCR(mod2_t5_no, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t5_no, vcov=clusV_mod2_t5_no),4)
mod3_t5_no <- lm(negative_response~ guest_black + ten_reviews + I(ten_reviews*guest_black),
              data = main_data)
clusV_mod3_t5_no <- vcovCR(mod3_t5_no, main_data$guest_by_city, "CR1")
round(coeftest(mod3_t5_no, vcov= clusV_mod3_t5_no),4)
mod4_t5_no <- lm(negative_response~guest_black + young + I(young*guest_black), data = main_data)
clusV_mod4_t5_no <- vcovCR(mod4_t5_no, main_data$guest_by_city, "CR1")
round(coeftest(mod4_t5_no, vcov= clusV_mod4_t5_no),4)
mod5_t5_no <- lm(negative_response~ guest_black + any_black + I(any_black*guest_black), data = main_data)
clusV_mod5_t5_no <- vcovCR(mod5_t5_no, main_data$guest_by_city, "CR1")
round(coeftest(mod5_t5_no, vcov= clusV_mod5_t5_no),4)
#table 6 results
mod1_t6_no <- lm(negative_response~ guest_black + price_median + I(guest_black*price_median), data = main_data)
clusV_mod1_t6_no <- vcovCR(mod1_t6_no, main_data$guest_by_city, "CR1")
round(coeftest(mod1_t6_no, vcov= clusV_mod1_t6_no),4)
mod2_t6_no <- lm(negative_response~ guest_black + black_proportion + I(guest_black*black_proportion),
              data = main_data)
clusV_mod2_t6_no <- vcovCR(mod2_t6_no, main_data$guest_by_city, "CR1")
round(coeftest(mod2_t6_no, vcov= clusV_mod2_t6_no),4)
mod3_t6_no <- lm(negative_response~guest_black + tract_listings + I(guest_black*tract_listings), data = main_data)
clusV_mod3_t6_no <- vcovCR(mod3_t6_no, main_data$guest_by_city, "CR1")
round(coeftest(mod3_t6_no, vcov= clusV_mod3_t6_no),4)
mod4_t6_no <- lm(negative_response~ guest_black + probit_predictions + I(guest_black*probit_predictions),
              data = main_data)
clusV_mod4_t6_no <- vcovCR(mod4_t6_no, main_data$guest_by_city, "CR1")
round(coeftest(mod4_t6_no, vcov= clusV_mod4_t6_no),4)

#robustness check
rob1_no <- lm(negative_response~guest_black + raw_black + I(raw_black*guest_black), data = main_data)
clusV_rob1_no <- vcovCR(rob1_no, main_data$guest_by_city, "CR1")
round(coeftest(rob1_no, vcov= clusV_rob1_no),4)
rob2_no <- lm(negative_response~guest_black + prop_black + I(prop_black*guest_black),data = main_data)
clusV_rob2_no <- vcovCR(rob2_no, main_data$guest_by_city, "CR1")
round(coeftest(rob2_no, vcov= clusV_rob2_no),4)
rob3_no <- lm(negative_response~guest_black + any_black + I(guest_black*any_black), data = main_data)
clusV_rob3_no <- vcovCR(rob3_no, main_data$guest_by_city, "CR1")
round(coeftest(rob3_no, vcov= clusV_rob3_no),4)
