### BBS dove data
# bbs <- BBS.tenstop::get_BBS10()

ut_routes <- dplyr::filter(bbs$routes, statenum == 85)

ut_routes <- dplyr::select(ut_routes, countrynum, statenum, Route, Active, Latitude, Longitude, BCR, RouteTypeID, routeID)

ut_weather <- dplyr::filter(bbs$weather, routeID %in% ut_routes$routeID)

ut_weather <- dplyr::select(ut_weather, RPID, Year, Month, Day, ObsN, StartTemp, EndTemp, StartWind, EndWind, StartSky, EndSky, StartTime, EndTime, Assistant, RunType, routeID)

ut_counts <- dplyr::filter(bbs$counts, routeID %in% ut_routes$routeID)

ut_counts <- dplyr::select(ut_counts, Year, aou, speciestotal, routeID)

ut_dove_counts <- dplyr::filter(ut_counts, aou == 22860)

ut_dove_counts <- dplyr::right_join(ut_dove_counts, ut_weather)

ut_dove_counts <- dplyr::select(ut_dove_counts, Year, speciestotal, routeID)

ut_dove_counts$aou <- 22860

ut_dove_counts$speciestotal[is.na(ut_dove_counts$speciestotal)] <- 0


ut_dove_counts <- dplyr::group_by(ut_dove_counts, Year)

ut_dove_counts <- dplyr::summarise(ut_dove_counts, Count = mean(speciestotal))

### All routes except Utah

all_routes <- dplyr::filter(bbs$routes, statenum != 85)

all_routes <- dplyr::select(all_routes, countrynum, statenum, Route, Active, Latitude, Longitude, BCR, RouteTypeID, routeID)

all_weather <- dplyr::filter(bbs$weather, routeID %in% all_routes$routeID)

all_weather <- dplyr::select(all_weather, RPID, Year, Month, Day, ObsN, StartTemp, EndTemp, StartWind, EndWind, StartSky, EndSky, StartTime, EndTime, Assistant, RunType, routeID)

all_counts <- dplyr::filter(bbs$counts, routeID %in% all_routes$routeID)

all_counts <- dplyr::select(all_counts, Year, aou, speciestotal, routeID)

all_dove_counts <- dplyr::filter(all_counts, aou == 22860)

all_dove_counts <- dplyr::bind_rows(data.frame(Year = seq(from = 1968, to = 1985), Count = 0),
                         all_dove_counts)

all_dove_counts <- dplyr::select(all_dove_counts, Year, speciestotal, routeID)

all_dove_counts$aou <- 22860

all_dove_counts$speciestotal[is.na(all_dove_counts$speciestotal)] <- 0


all_dove_counts <- dplyr::group_by(all_dove_counts, Year)

all_dove_counts <- dplyr::summarise(all_dove_counts, Count = mean(speciestotal))


### PVA homework 1


obs_N <- WILD3810::get_N(N0 = round(runif(1, 225, 275)), number.of.years = c(30, 8), b0 = 1,
               variance.b = 0.2, fire.prob = c(0.1, 0.25), fire.b = 0.6, fire.d = 0.8,
               d0 = 0.2, variance.d = 0.7, K = c(450, 36),
               seed = 8, d.slope = 0.0075)

### Life table data
lt <- read.csv("raw-data/life_table.csv")

lt_data <- data.frame(byear = c(lt$byear_m, lt$byear_f),
                      dyear = c(lt$dyear_m, lt$dyear_f),
                      sex = rep(c("Male", "Female"), each = length(lt$byear_m)))
lt_data <- lt_data[!is.na(lt_data$byear),]

usethis::use_data(ut_dove_counts, all_dove_counts, obs_N, lt_data, overwrite = TRUE)


