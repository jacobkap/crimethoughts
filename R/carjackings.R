library(dplyr)
library(ggplot2)
library(scales)
library(crimeutils)

nibrs_offense_segment_2019 <- readRDS("D:/ucr_data_storage/clean_data/nibrs/nibrs_offense_segment_2019.rds")
nibrs_offense_segment_2020 <- readRDS("D:/ucr_data_storage/clean_data/nibrs/nibrs_offense_segment_2020.rds")
nibrs_offender_segment_2019 <- readRDS("D:/ucr_data_storage/clean_data/nibrs/nibrs_offender_segment_2019.rds")
nibrs_offender_segment_2020 <- readRDS("D:/ucr_data_storage/clean_data/nibrs/nibrs_offender_segment_2020.rds")
nibrs_property_segment_2019 <- readRDS("D:/ucr_data_storage/clean_data/nibrs/nibrs_property_segment_2019.rds")
nibrs_property_segment_2020 <- readRDS("D:/ucr_data_storage/clean_data/nibrs/nibrs_property_segment_2020.rds")

robb_2019 <- nibrs_offense_segment_2019 %>%
  filter(ucr_offense_code %in% "robbery",
         ori %in% nibrs_offender_segment_2020$ori) %>%
  distinct(unique_incident_id, .keep_all = TRUE)

vehicle_stolen_2019 <- nibrs_property_segment_2019 %>%
  filter(type_of_property_loss %in%
           "stolen/etc. (includes bribed, defrauded, embezzled, extorted, ransomed, robbed, etc.)",
         property_description %in% c("automobiles",
                                     "other motor vehicles",
                                     "trucks",
                                     "recreational vehicles",
                                     "buses"),
         ori %in% nibrs_offender_segment_2020$ori) %>%
  distinct(unique_incident_id, .keep_all = TRUE) %>%
  select(unique_incident_id, property_description)

offense_and_property_2019 <- left_join(robb_2019, vehicle_stolen_2019)
carjackings_2019          <- offense_and_property_2019[!is.na(offense_and_property_2019$property_description), ]





robb <- nibrs_offense_segment_2020 %>%
  filter(ucr_offense_code %in% "robbery") %>%
  distinct(unique_incident_id, .keep_all = TRUE)

vehicle_stolen <- nibrs_property_segment_2020 %>%
  filter(type_of_property_loss %in% "stolen/etc. (includes bribed, defrauded, embezzled, extorted, ransomed, robbed, etc.)",
         property_description %in% c("automobiles",
                                     "other motor vehicles",
                                     "trucks",
                                     "recreational vehicles",
                                     "buses")) %>%
  distinct(unique_incident_id, .keep_all = TRUE) %>%
  select(unique_incident_id, property_description)


offense_and_property <- left_join(robb, vehicle_stolen) %>%
  filter(ori %in% nibrs_offense_segment_2019$ori)
carjackings <- offense_and_property[!is.na(offense_and_property$property_description), ]

carjack_offenders <- nibrs_offender_segment_2020 %>%
  filter(unique_incident_id %in% carjackings$unique_incident_id)

carjack_offenders$age_of_offender[carjack_offenders$age_of_offender %in% "unknown"] <- NA
carjack_offenders$age_of_offender[carjack_offenders$age_of_offender %in% "over 98 years old"] <- 99
carjack_offenders$age_of_offender <- as.numeric(carjack_offenders$age_of_offender)
summary(carjack_offenders$age_of_offender)


carjack_offenders_2019 <- nibrs_offender_segment_2019 %>%
  filter(unique_incident_id %in% carjackings_2019$unique_incident_id)

carjack_offenders_2019$age_of_offender[carjack_offenders_2019$age_of_offender %in% "unknown"] <- NA
carjack_offenders_2019$age_of_offender[carjack_offenders_2019$age_of_offender %in% "over 98 years old"] <- 99
carjack_offenders_2019$age_of_offender <- as.numeric(carjack_offenders_2019$age_of_offender)
summary(carjack_offenders_2019$age_of_offender)
table(carjack_offenders_2019$age_of_offender < 18) / nrow(carjack_offenders_2019[!is.na(carjack_offenders_2019$age_of_offender), ])

nrow(carjackings_2019) / nrow(offense_and_property_2019)
nrow(carjackings) / nrow(offense_and_property)

saveRDS(carjack_offenders, "data/carjacking_offenders_2020.rds")
table(is.na(carjack_offenders$age_of_offender))
make_stat_count_plots(carjack_offenders, "age_of_offender", count = FALSE, ylab = "% of Offenders",
                                  xlab = "Age of Offender", "Ages of Carjackers, N = 12,178 (2020 NIBRS)")
carjack_offenders$sex_of_offender[is.na(carjack_offenders$sex_of_offender)] <- "Unknown"
carjack_offenders$sex_of_offender <- capitalize_words(carjack_offenders$sex_of_offender)
make_barplots(carjack_offenders, "sex_of_offender", count = FALSE)








