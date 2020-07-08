# setwd("/Users/gregfaletto/Dropbox/COVID-19-challenge/deliverable")

library(pglm)
library(ggplot2)
library(Metrics)
library(glmnet)

rm(list=ls())

dir.main <- getwd()

dir.dat.cases <- paste(dir.main, "/Data/Cases, tests, etc", sep="")

dir.dat.covs <- paste(dir.main, "/Data/Neighborhood covariates", sep="")


#
#
#
#
#

# Parameters of model (variables to include)

n_lags <- 4

# Covariates to include in model (either character vector or NA if you don't
# want to include any covariates)
model_covariates <- c("black_pct"
	, "pct_65_plus"
	# , "density"
	, "grocery_count"
	# , "business_count"
	# , "heart_disease_perc"
	, "diabetes_perc"
	, "Asthma_perc"
	# , "physical_activity_perc"
	# , "income_level"
	)
# model_covariates <- NA

# Assumed number of days between infection and testing
infection_test_delay <- 7

# model indicates the model to be estimated : "pooling" is just the maximum
# likelihood estimation, "between" performs the estimation on
# the individual or time means, "within" on the deviations from the individual 
# or/and time mean, and "random" perform a 
# feasible generalized least squares estimation which takes into account the 
# correlation induced by the presence of individual and/or time effects.
# (source: https://cran.r-project.org/web/packages/plm/vignettes/plmFunction.html)
model_type <- "pooling"

# Assumed multiplier between number of positive tests and number of actual 
# cases
infect_mult <- 8

# Assumed number of days between infection and recovery
recover_days <- 23

# Percent of data to go in training set vs. test set
train_pct <- 0.8

# reporting delay (how many days between most recent data and day we want
# predictions for?)
reporting_delay <- 1

# Example neighborhood for figures

# example_neighborhood <- "Los Angeles - Sherman Oaks"
example_neighborhood <- "Los Angeles - Studio City"

# Testing 

#
#
#
#
#
#

# Load data

print("Loading data...")

t0 <- Sys.time()

setwd(dir.dat.cases)

cases <- read.csv("LA_county_cases_city&community.txt", header=TRUE)
la_cases <- read.csv("date_table.csv", header=TRUE)

setwd(dir.dat.covs)

covs <- read.csv("neighborhood information, zipcode, demograpphics, density, etc.csv",
	header=TRUE)
df_case_cov <- read.csv("df_case_cov.csv", header=TRUE)

df_case_cov <- df_case_cov[, !(colnames(df_case_cov) %in% c("Total.Cases",
	"Rate", "collect_time", "Deaths", "Death_rate"))]
df_case_cov <- df_case_cov[!duplicated(df_case_cov), ]

# fix typo
colnames(df_case_cov)[colnames(df_case_cov) == "herat_disease_perc"] <- 
	"heart_disease_perc"

temps <- read.csv("temperature.csv")

setwd(dir.main)

print("Done! Time to load data:")

print(Sys.time() - t0)

############ Format/clean data


### Covariates

# Time covariates

temps$Date <- as.Date(temps$Date, "%m/%d/%y")
time_covs <- temps

# Data shows roughly exponential growth in case numbers through 3/29/20 and
# roughly linear growth afterward. This is probably due to shelter-in-place
# order (lag of 10 days due to both delay between infection and onset of
# symptoms/testing and possible lag in full compliance)
time_covs$shelter1 <- as.numeric(time_covs$Date > as.Date("2020-03-29") & 
	time_covs$Date < as.Date("2020-05-08", "%Y-%m-%d"))

# source: https://www.latimes.com/projects/california-coronavirus-cases-tracking-outbreak/reopening-across-counties/

time_covs$shelter2 <- as.numeric(time_covs$Date >= as.Date("2020-05-08"))

# restaurants reopen
# source: https://la.eater.com/2020/5/29/21274955/los-angeles-county-reopening-plan-details-restaurants-specifics

# time_covs$shelter2b <- as.numeric(time_covs$Date >= as.Date("2020-05-29"))

# Major protests (spurring curfew) started 5/30/20
time_covs$protests <- as.numeric(time_covs$Date >= as.Date("2020-05-30",
	"%Y-%m-%d"))

time_cov_names <- colnames(time_covs)[2:ncol(time_covs)]

# Get LA population (according to this data)

la_pop <- sum(covs$tot_pop)

# For names: remove characters at end(NC, CC, NDC), change to title 
# capitlization

covs$NAME <- as.character(covs$NAME)

covs$NAME <- as.character(apply(as.matrix(covs$NAME),
	1, function(x){gsub(" NC", "", x)}))

covs$NAME <- as.character(apply(as.matrix(covs$NAME),
	1, function(x){gsub(" CC", "", x)}))

covs$NAME <- as.character(apply(as.matrix(covs$NAME),
	1, function(x){gsub(" NDC", "", x)}))

covs$NAME <- as.character(apply(as.matrix(covs$NAME), 1, stringr::str_to_title))

# Transform variables

covs$white_pct <- covs$white/covs$tot_pop
covs$black_pct <- covs$black/covs$tot_pop
covs$male_pct <- covs$male/covs$tot_pop
covs$pct_65_plus <- (covs$male65to66 + covs$male67to69 + covs$male70to74 +
	covs$male75to79 + covs$male80to84 + covs$male85up + covs$fem65and66 +
	covs$fem67to69 + covs$fem70to74 + covs$fem75to79 + covs$fem80to84 +
	covs$fem85andup)/covs$tot_pop

# Transform variables

df_case_cov$white_pct <- df_case_cov$white/df_case_cov$tot_pop
df_case_cov$black_pct <- df_case_cov$black/df_case_cov$tot_pop
df_case_cov$male_pct <- df_case_cov$male/df_case_cov$tot_pop
df_case_cov$pct_65_plus <- (df_case_cov$male65to66 + df_case_cov$male67to69 + df_case_cov$male70to74 +
	df_case_cov$male75to79 + df_case_cov$male80to84 + df_case_cov$male85up + df_case_cov$fem65and66 +
	df_case_cov$fem67to69 + df_case_cov$fem70to74 + df_case_cov$fem75to79 + df_case_cov$fem80to84 +
	df_case_cov$fem85andup)/df_case_cov$tot_pop

### Neighborhood cases

# Remove deaths and death rate for now
vars_discard <- c("Deaths", "Death_rate")
keep <- !(colnames(cases) %in% vars_discard)
cases <- cases[, keep]

# For now, for locations that end in *, combine them with locations without *

cases$Locations <- as.character(apply(as.matrix(as.character(cases$Locations)),
	1, function(x){gsub("\\*", "", x)}))

# Remove dubious locations, as well as data for whole city

locations_discard <- c("Los Angeles*", "Los Angeles", "#NAME?",
	"-  Under Investigation")

cases <- cases[!(cases$Locations %in% locations_discard), ]

# Deal with date feature: unfortunately the dates are formatted two different
# ways. Dates for 5/12 - 5/21 are formatted as "%Y/%m/%d", dates for 5/22 on
# are formatted as "%Y-%m-%d".

# cases$collect_time_1 <- cases$collect_time
# cases$collect_time_2 <- cases$collect_time

cases$collect_time_1 <- as.Date(cases$collect_time, "%Y/%m/%d")
cases$collect_time_2 <- as.Date(cases$collect_time, "%Y-%m-%d")

cases$collect_time_3 <- integer(nrow(cases))
class(cases$collect_time_3) <- "Date"

cases$collect_time_3[!is.na(cases$collect_time_1)] <- cases$collect_time_1[
	!is.na(cases$collect_time_1)]

cases$collect_time_3[!is.na(cases$collect_time_2)] <- cases$collect_time_2[
	!is.na(cases$collect_time_2)]

keep <- !(colnames(cases) %in% c("collect_time", "collect_time_1",
	"collect_time_2"))
cases <- cases[, keep]

colnames(cases)[colnames(cases) == "collect_time_3"] <- "collect_time"

# Remove any entries where date is NA
cases <- cases[!is.na(cases$collect_time) ,]
print("Done!")

# Identify and remove cities with no cases on 5/12/2020--won't have enough data
locs <- unique(cases$Locations)
locs_remove <- logical(length(locs))

for(i in 1:length(locs)){
	locs_remove[i] <- any(cases[cases$Locations == locs[i], "Total.Cases"] == 0)
}

#
#
#
#
#
#
#
#
#
#
# DROP NEIGHBORHOODS HERE
#
#
#
#
#
#
#
#
#

# print("Locations that had no cases on at least one date in range:")
# print(locs[locs_remove])
# print("Removing these locations from data...")

# cases <- cases[!(cases$Locations %in% locs[locs_remove]), ]

# print("Done!")

# Plot time series--pick first 5 neighborhoods at random, look at plot
cases_small <- cases[cases$Locations %in% c(unique(cases$Locations)[1:4],
	example_neighborhood), ]
plot <- ggplot(cases_small, aes(x=collect_time, y=Total.Cases, color=Locations)) +
	geom_line()
print(plot)


# matches_0 <- 0
# matches_1 <- 0
# matches_2 <- 0

# Add a column of covs with names of strings matching from cases data.frame
covs <- data.frame(covs, character(nrow(covs)), stringsAsFactors = FALSE)
colnames(covs)[ncol(covs)] <- "Locations"

df_case_cov <- data.frame(df_case_cov, character(nrow(df_case_cov)),
	stringsAsFactors = FALSE)
colnames(df_case_cov)[ncol(df_case_cov)] <- "Locations_Match"
# Search for matches in cases$Locations
for(i in 1:length(covs$NAME)){
	# Find number of matches between strings in covariates table and strings
	# in cases table
	n_matches <- sum(grepl(covs$NAME[i], unique(cases$Locations),
		ignore.case=TRUE))
	# If there is exactly one unique match, add covariates
	if(n_matches == 1){
		# Add corresponding Location from cases to covs
		loc <- grep(covs$NAME[i], unique(cases$Locations),
		ignore.case=TRUE, value = TRUE)
		covs[i, "Locations"] <- loc
	}
	# else if(n_matches == 1){
	# 	matches_1 <- matches_1 + 1
	# } else{
	# 	matches_2 <- matches_2  + 1
	# 	print(paste("2 matches:", covs$NAME[i]))
	# }
}

# Search for matches in cases$Locations
for(i in 1:length(df_case_cov$Locations)){
	# Find number of matches between strings in covariates table and strings
	# in cases table
	n_matches <- sum(grepl(df_case_cov$Locations[i], unique(cases$Locations),
		ignore.case=TRUE))
	# If there is exactly one unique match, add covariates
	if(n_matches == 1){
		# Add corresponding Location from cases to covs
		loc <- grep(df_case_cov$Locations[i], unique(cases$Locations),
		ignore.case=TRUE, value = TRUE)
		df_case_cov[i, "Locations_Match"] <- loc
	}
	# else if(n_matches == 1){
	# 	matches_1 <- matches_1 + 1
	# } else{
	# 	matches_2 <- matches_2  + 1
	# 	print(paste("2 matches:", df_case_cov$Locations[i]))
	# }
}

colnames(df_case_cov)[colnames(df_case_cov) == "Locations"] <- "Locations_old"

colnames(df_case_cov)[colnames(df_case_cov) == "Locations_Match"] <- "Locations"
#### LA-wide cases before shelter-in-place


# Statewide shelter-in-place order: 3/19/20 (source: https://abc7news.com/health/coronavirus-what-to-know-about-california-stay-at-home-order/6032237/)
# also Los Angeles one (source: https://www.cnbc.com/2020/03/19/los-angeles-mayor-issues-safer-at-home-order-asking-residents-to-limit-non-essential-movement.html)

# Data shows roughly exponential growth in case numbers through 3/29/20 and
# roughly linear growth afterward. This is probably due to shelter-in-place
# order (lag of 10 days due to both delay between infection and onset of
# symptoms/testing and possible lag in full compliance)

# Use data from https://lahub.maps.arcgis.com/home/item.html?id=55c2f06850fd46508be313de03b3a591#data
# through 3/19/20 to estimate initial rate of outbreak (enter data manually)

Date <- c(
	# as.Date("2020-01-26", "%Y-%m-%d"),
	seq(as.Date("2020-03-04", "%Y-%m-%d"), as.Date("2020-03-29", "%Y-%m-%d"),
	by=1))

Cases <- c(
	# 1, 
	7, 11, 13, 14, 14, 16, 17, 27, 32, 40, 53, 69, 94, 144, 190, 231, 292, 351,
	409, 536, 662, 799, 1216, 1465, 1804, 2136)


# omitted first confirmed case because testing regime changed and doesn't
# match trendline for other data (different DGP)

# # Add first confirmed case: according to
# # https://lahub.maps.arcgis.com/home/item.html?id=55c2f06850fd46508be313de03b3a591#data
# # first confirmed case on 1/26/20

# first_case <- data.frame(as.Date("2020-01-26", "%Y-%m-%d"), 1)
# colnames(first_case) <- colnames(la_cases)
# la_cases <- rbind(la_cases, first_case)

la_cases_2 <- data.frame(Date, Cases)

# Add t parameter (using t=0 for first day)

la_cases_2$t <- as.integer(la_cases_2$Date) - as.integer(la_cases_2$Date[1])

# fit linear model

la_cases_2$log_cases <- log(la_cases_2$Cases)

log_model <- lm(log_cases ~ t, data=la_cases_2)

# Plot linear log model with trendline

plot_log_lin <- ggplot(la_cases_2, aes(x=t, y=log_cases)) + geom_point() +
	geom_smooth(method="lm")

print(plot_log_lin)

#### After shelter-in-place

# Format dates

la_cases$Date <- as.Date(la_cases$date_dt, "%Y-%m-%d")

la_cases <- la_cases[, c("Date", "total_cases", "new_case")]

# Sort by ascending order of date

la_cases <- la_cases[order(la_cases$Date), ]

# Remove dates before 3/30/20

la_cases <- la_cases[la_cases$Date > as.Date("2020-03-29", "%Y-%m-%d"), ]

# Add t parameter (using t=0 for first day)

la_cases$t <- as.integer(la_cases$Date) - as.integer(la_cases$Date[1])

# fit linear model

lin_model <- lm(total_cases ~ t, data=la_cases)

# Plot linear model with trendline

plot_lin <- ggplot(la_cases, aes(x=t, y=total_cases)) + geom_point() +
	geom_smooth(method="lm")

print(plot_lin)


######## Join covariates and cases

#
#
#
#
#
#
#
#
#
#
# DROP NEIGHBORHOODS HERE
#
#
#
#
#
#
#
#
#


# Now add covariates using "Locations" as match

all_locations <- unique(cases$Locations)

cases_old <- cases
cases_old <- cases_old[!duplicated(cases_old[, c("Locations", "collect_time")]), ]

# cases <- dplyr::inner_join(cases, covs, by="Locations")
cases <- dplyr::inner_join(cases, df_case_cov, by="Locations")
# # Fix column numbers
# cases <- cases[, !(colnames(cases) %in% c("Total.Cases.y", "collect_time.y",
# 	"Rate.y"))]
# colnames(cases)[colnames(cases) == "Total.Cases.x"] <- "Total.Cases"
# colnames(cases)[colnames(cases) == "collect_time.x"] <- "collect_time"
# colnames(cases)[colnames(cases) == "Rate.x"] <- "Rate"

# Remove duplicated indices

cases <- cases[!duplicated(cases[, c("Locations", "collect_time")]), ]

matched_locations <- unique(cases$Locations_old)

write.csv(matched_locations, file="matched_locations.csv")

missing_locations <- all_locations[!(all_locations %in% unique(cases$Locations))]

write.csv(missing_locations, file="missing_locations.csv")

####### Backcast case counts

# Now for each neighborhood, we backcast case counts by assuming number of cases
# added per day (from la_cases data.frame) is split among neighborhoods by
# population.

print("Backcasting case counts...")

t0 <- Sys.time()

cases_backcast <- cases

dates_to_fill <- seq(min(la_cases$Date), min(cases_backcast$collect_time) - 1, by=1)
# Keep last day we backcast for
last_backcast_date <- max(dates_to_fill)
areas <- unique(cases_backcast$Locations)

for(t in length(dates_to_fill):1){
	# this date
	today <- dates_to_fill[t]
	# We want to get the number of cases in each neighborhood on this day. To do
	# this, we'll get the number of cases that were added the following day
	# and subtract those from each number. 
	tomorrow <- today + 1
	new_cases_t <- la_cases$new_case[la_cases$Date == tomorrow]
	for(i in 1:length(areas)){
		# This location
		loc <- areas[i]
		# Get the population of this area
		pop_i <- covs[covs$Locations==loc, "tot_pop"]
		# Get the proportion of new cases attributed to this area
		new_cases_t_i <- new_cases_t*pop_i/la_pop
		# Number of total cases tomorrow
		n_cases_tomorrow <- cases_backcast[cases_backcast$Locations==loc &
			cases_backcast$collect_time ==tomorrow, "Total.Cases"]
		# Create a new data.frame row with number of cases equal to tomorrow's
		# number minus this number
		df_t_i <- cases_backcast[cases_backcast$Locations==loc &
			cases_backcast$collect_time ==tomorrow, ]
		df_t_i$collect_time <- today
		df_t_i$Total.Cases <- max(n_cases_tomorrow - new_cases_t_i, 0)
		# Add to cases data.frame
		cases_backcast <- rbind(cases_backcast, df_t_i)
	}
}


cases_backcast <- cases_backcast[order(cases_backcast$collect_time), ]
cases_backcast <- cases_backcast[order(cases_backcast$Locations), ]
rownames(cases_backcast) <- c()
cases_backcast$Total.Cases <- round(cases_backcast$Total.Cases)

print("Done! Total time:")
print(Sys.time() - t0)


# Now we assume that test result shows up 7 days after infection, so subtract
# 7 from each date to get infection levels.

cases_backcast$collect_time <- cases_backcast$collect_time -
	infection_test_delay

# We also assume that each positive test case represents 8 actual infections,
# so multiply number of test cases by 8 to get cumulative infections
cases_backcast$Infect_Cumul <- cases_backcast$Total.Cases*infect_mult



################ Compute S, I, and R levels for each neighborhood on each day

# Add Susceptible, Infected, and Recovered populations for each neighborhood
# on each day
cases_backcast <- data.frame(cases_backcast, integer(nrow(cases_backcast)), 
	integer(nrow(cases_backcast)), integer(nrow(cases_backcast)),
	integer(nrow(cases_backcast)), integer(nrow(cases_backcast)))

colnames(cases_backcast)[(ncol(cases_backcast) - 4):ncol(cases_backcast)] <- c(
	"Susceptible", "Infected", "Recovered", "New_Infected", "New_Recovered")


# # Since all areas start with 0 infections in our model, start with everyone
# # in susceptible population

# for(i in areas){
# 	# This location
# 	loc <- areas[i]
# 	cases_backcast[cases_backcast$Locations==loc &
# 		cases_backcast$collect_time==min(cases_backcast$collect_time), 
# 		"Susceptible"] <- cases_backcast[cases_backcast$Locations==loc &
# 		cases_backcast$collect_time==min(cases_backcast$collect_time), 
# 		"tot_pop"]
# }

# Now proceed through dates

print("Estimating susceptible, infected, and recovered populations in each neighborhood on each date...")

t0 <- Sys.time()

dates <- unique(cases_backcast$collect_time)

for(t in 2:length(dates)){
	# this date
	today <- dates[t]
	yesterday <- today - 1
	for(i in 1:length(areas)){
		# This location
		loc <- areas[i]
		ind <- cases_backcast$collect_time == today &
			cases_backcast$Locations==loc
		# Infected is equal to cumulative infected minus number of cases that
		# are recover_days days old
		if(t > recover_days){
			ind_rec <- cases_backcast$collect_time == (today - recover_days) &
				cases_backcast$Locations==loc
			cases_backcast[ind, "Infected"] <-
				cases_backcast[ind, "Infect_Cumul"] -
				cases_backcast[ind_rec, "Infect_Cumul"]
		} else{
			cases_backcast[ind, "Infected"] <-
				cases_backcast[ind, "Infect_Cumul"]
		}
		# Recovered is equal to cumulative infected minus currently infected
		# (also equal to cumulative number of cases that are recover_days days
		# old)
		if(t > recover_days){
			cases_backcast[ind, "Recovered"] <-
				cases_backcast[ind, "Infect_Cumul"] -
				cases_backcast[ind, "Infected"]
		}
		# New_Recovered is equal to currently recovered minus previously
		# recovered
		ind_yest <- cases_backcast$collect_time == yesterday &
			cases_backcast$Locations==loc
		cases_backcast[ind, "New_Recovered"] <-
			max(0, cases_backcast[ind, "Recovered"] -
				cases_backcast[ind_yest, "Recovered"])
		# New_Infected is equal to currently infected minus previously
		# infected plus New_Recoveries
		cases_backcast[ind, "New_Infected"] <-
			max(0, cases_backcast[ind, "Infected"] -
			cases_backcast[ind_yest, "Infected"] + 
			cases_backcast[ind, "New_Recovered"])
		# Susceptible is equal to total population minus infected minus
		# recovered
		cases_backcast[ind, "Susceptible"] <- round(
			cases_backcast[ind, "tot_pop"] - cases_backcast[ind, "Infected"] -
			cases_backcast[ind, "Recovered"])

		# cases_backcast[cases_backcast$Locations==loc &
		# 	cases_backcast$collect_time==today, "Infected"] <- 
		# 	cases_backcast[cases_backcast$Locations==loc &
		# 	cases_backcast$collect_time==yesterday, "Infected"] + new_cases
		# new_cases <- cases_backcast[cases_backcast$Locations==loc &
		# 	cases_backcast$collect_time==today, "Total.Cases"] -
		# 	cases_backcast[cases_backcast$Locations==loc &
		# 	cases_backcast$collect_time==yesterday, "Total.Cases"]
		# Susceptible is equal to previous susceptible minus number of new cases
		
	}
}

print("Done! Total time:")
print(Sys.time() - t0)


# Now for each neighborhood, we backcast case counts by assuming overall fitted
# trends for LA match trends in each neighborhood (i.e., linear with same
# coefficient (scaled by population) after 3/29, exponential with same rate
# up until 3/29).

# For now, just linear model. Get global slope (additional cases/day in LA):



############### Make forecasts of case levels

# Model
if(!any(is.na(model_covariates))){
	formula <- as.formula(paste("Total.Cases ~ lag(log(Total.Cases), 1:", n_lags,
	") + ", paste(model_covariates, collapse=" + "), " + ",
	paste(time_cov_names, collapse=" + "), sep = ""))
} else{
	formula <- as.formula(paste("Total.Cases ~ lag(log(Total.Cases), 1:", n_lags,
	")", sep = ""))
}

# Prediction function

pred.pglm <- function(model, dat, response_var, n_lags, offset = 0,
	covs=model_covariates, time_cov_true=FALSE, new_recovered = FALSE,
	date_var = "collect_time", id_var = "Locations", date = NA, loc = NA){
	# Generate predictions from a pglm model on a specified date
	# Get coefficients
	coefs <- coef(model)
	## Construct model.matrix
	units <- unique(dat[, id_var])
	n <- length(units)
	# Dates for which we will generate predictions
	dates <- unique(dat[, date_var])
	# Can't generate predictions for first offset dates
	dates <- dates[(1+offset):length(dates)]
	T <- length(dates)
	# dates over which we will fit model (predictors)
	dates_fit <- dates[1:(T - n_lags + 1)]
	dates_fit <- rep(dates_fit, n)
	if(!any(is.na(covs))){
		p <- length(covs) + as.numeric(new_recovered) + time_cov_true*length(time_cov_names)
	} else{
		p <- as.numeric(new_recovered) + time_cov_true*length(time_cov_names)
	}
	# Create desired size of design matrix--number of columns is n_lags +
	# number of coavariates + 1. number of rows is enough to have the number
	# of dates in the data.frame, minus the number of lags minus 1.
	n_rows <- n*(T - n_lags + 1)
	n_cols <- 1 + n_lags + p
	if(n_cols != length(coef(model))){
		stop("n_cols must equal length(coef(model)).")
	}
	model_mat <- matrix(0, nrow = n_rows, ncol = n_cols)
	# intercept term
	model_mat[, 1] <- 1
	# name rows of matrix
	names <- as.vector(sapply(units, function (x) rep(x, T - n_lags + 1)))
	rownames(model_mat) <- names
	colnames(model_mat) <- names(coef(model))
	for(i in 1:n){
		# Get all data points for this unit
		df_i <- dat[dat[, id_var] == units[i], ]
		# Get indices of model_mat that I will fill with these data values
		indices <- which(rownames(model_mat) == units[i])
		# Fill in lag terms (assuming lags are supposed to be log transformed)
		for(j in 1:n_lags){
			# lagged indices of response_var
			df_i_inds_lag <- (n_lags - j + 1):(T - j + 1)
			# print(df_i_inds_lag)
			model_mat[indices, 1 + j] <- log(df_i[df_i_inds_lag, response_var])
		}
		# print(model_mat)
		# Get covariates
		if(!any(is.na(covs))){
			# print(dim(as.matrix(df_i[, covs])))
			# print(length(indices))
			model_mat[indices,
				(1 + n_lags + 1):(1 + n_lags + p -
					time_cov_true*length(time_cov_names) -
					as.numeric(new_recovered))] <-
				as.matrix(df_i[1:length(indices), covs])
			rownames(model_mat) <- names
			colnames(model_mat) <- names(coef(model))
		}
		# Add time covariates
		if(time_cov_true){
			for(i in 1:length(dates_fit)){
				date_i <- dates_fit[i]
				# print(date_i)
				# print(dim(time_covs[time_covs$collect_time==date_i, 3:ncol(time_covs)]))
				# print(dim(model_mat[i,
				# 	(1 + n_lags + p - length(time_cov_names) - as.numeric(new_recovered) + 1):
				# 	(1 + n_lags + p - length(time_cov_names) - as.numeric(new_recovered) + 3)]))
				# print("i:")
				# print(i)
				# print("date_i:")
				# print(date_i)
				# print("str(model_mat):")
				# print(str(model_mat))
				# print("nrow(model_mat):")
				# print(nrow(model_mat))
				# print((1 + n_lags + p - length(time_cov_names) - as.numeric(new_recovered) + 1):(1 + n_lags + p - as.numeric(new_recovered)))
				# print(model_mat[i, (1 + n_lags + p - length(time_cov_names) - as.numeric(new_recovered) + 1):(1 + n_lags + p - as.numeric(new_recovered))])
				# print(time_covs[time_covs$collect_time==date_i, 2:ncol(time_covs)])
				model_mat[i,
					(1 + n_lags + p - length(time_cov_names) - as.numeric(new_recovered) + 1):(1 + n_lags + p - as.numeric(new_recovered))] <-
					as.numeric(time_covs[time_covs$collect_time==date_i, 2:ncol(time_covs)])
			}
		}
		# Add log(new_recovered) (with no offset)
		if(new_recovered){
			df_i_inds_lag_recov <- (n_lags - j + 1 + offset):(T - j + 1 + offset)
			# print(df_i_inds_lag_recov)
			model_mat[indices, ncol(model_mat)] <- log(df_i[df_i_inds_lag_recov,
			"New_Recovered"])
		}
	}
	# generate predictions
	preds <- exp(as.numeric(model_mat %*% coef(model)))
	# If response is "new_infections", subtract 1 from predicted count (as
	# well as observed count) since we added 1 previously to make sure we could
	# take logs
	if(response_var == "New_Infected"){
		preds <- preds - 1
	}
	# dates for which predictions were generated
	dates_preds <- dates + 1
	dates_preds <- dates_preds[n_lags:T]
	dates_df <- rep(dates_preds, n)
	# print("str(dates_df):")
	# print(str(dates_df))
	# print("dates_df:")
	# print(dates_df)
	ret <- data.frame(names, dates_df, preds, numeric(length(preds)))
	colnames(ret) <- c(id_var, "Date", "Prediction", "Observed")
	# For dates that were observed, collect actual observed values for 
	# comparison
	if(length(dates_preds) > 1 + offset){
		for(i in 1:n){
			for(j in 1:(length(dates_preds) - 1 - offset)){
				ret_ind <- ret[, "Date"] == dates_preds[j] &
					ret[, id_var] == units[i]
				data_ind <- dat[, date_var] == dates_preds[j] &
					dat[, id_var] == units[i]
				# print("i:")
				# print(i)
				# print("j:")
				# print(j)
				# print("dates_preds[j]:")
				# print(dates_preds[j])
				# print("units[i]:")
				# print(units[i])
				# print(data_ind)
				# print(dat[data_ind, response_var])
				ret[ret_ind, "Observed"] <- dat[data_ind, response_var]
			}
			# Fill in observed for first day outside of data with NA
			ret_ind <- ret[, "Date"] %in%
				dates_preds[(length(dates_preds) - offset):length(dates_preds)] &
				ret[, id_var] == units[i]
			ret[ret_ind, "Observed"] <- NA
		}
	}
	if(!is.na(date)){
		ret <- ret[ret[, "Date"] == date, ]
	}
	if(!is.na(loc)){
		ret <- ret[ret[, id_var] == loc, ]
	}
	# If response is "new_infections", subtract 1 from predicted count (as
	# well as observed count) since we added 1 previously to make sure we could
	# take logs
	if(response_var == "New_Infected"){
		ret$Observed <- ret$Observed - 1
	}
	# Calculate mean squared error for observed dates
	obs <- ret[!is.na(ret$Observed), "Observed"]
	preds <- ret[!is.na(ret$Observed), "Prediction"]
	if(length(obs) > 0){
		rmse <- Metrics::rmse(actual = obs, predicted = preds)
	} else{
		rmse <- NA
	}
	
	return(list(ret, rmse))
}

makeGgplotFriendly <- function(df, x, cols){
	# Takes in a data.frame with separate columns I'd like to be plotted on
	# the same graph (with different colors). outputs a data.frame which can
	# be easily used for this purpose in ggplot2.
	x.new <- rep(df[, x], length(cols))
	if(is.numeric(df[, cols[1]][1])){
		y.new <- numeric()
	} else if(is.integer(df[, cols[1]][1])){
		y.new <- integer()
	} else {
		stop("Error: cols not integer or numeric")
	}
	for(i in 1:length(cols)){
		y.new <- c(y.new, df[, cols[i]])
	}
	names <- character()
	for(i in 1:length(cols)){
		names <- c(names, rep(cols[i], nrow(df)))
	}
	df.new <- data.frame(x.new, y.new, names)
	colnames(df.new) <- c(x, "y", "Labels")
	return(df.new)
}

######### Holdout sample, cases

# Divide data into training and test sets

dates <- unique(cases$collect_time)
train_samples <- round(train_pct*length(dates))
train_date_cutoff <- dates[train_samples]
cases_train <- cases[cases$collect_time <= train_date_cutoff, ]
cases_test <- cases[cases$collect_time > train_date_cutoff, ]

# Incorporate time covariates
colnames(time_covs)[colnames(time_covs)=="Date"] <- "collect_time"
cases_train <- dplyr::left_join(cases_train, time_covs, by="collect_time")
cases_test <- dplyr::left_join(cases_test, time_covs, by="collect_time")
cases <- dplyr::left_join(cases, time_covs, by="collect_time")

# Create panel data.frame

pdf_train <- pdata.frame(cases_train, index=c("Locations", "collect_time"),
	drop.index=FALSE)

set.seed(5347834)
model_train <- pglm(formula, data=pdf_train, family=poisson,
	model = model_type, method="nr")

predictions_test <- pred.pglm(model_train, cases_test, n_lags = n_lags,
	response_var = "Total.Cases", time_cov_true=TRUE)

print("Root mean squared error for out-of-sample predictions of new cases:")

print(predictions_test[[2]])

# predictions_cur <- pred.pglm(model, cases_test, n_lags = n_lags,
# 	response_var = "Total.Cases", date = max(cases_test$collect_time))

# print(predictions_cur[[1]])

# print("Root mean squared error for predictions:")

# print(predictions_cur[[2]])

predictions_example_test <- pred.pglm(model_train, cases, n_lags = n_lags,
	response_var = "Total.Cases", loc = example_neighborhood,
	time_cov_true=TRUE)

# print(predictions_example_test)

preds_example_df_test <- makeGgplotFriendly(predictions_example_test[[1]], "Date", 
	c("Prediction", "Observed"))

# Label in-sample and out-of-sample predictions
levels(preds_example_df_test$Labels) <- c(levels(preds_example_df_test$Labels),
	"In-sample Prediction", "Out-of-sample Prediction")

preds_example_df_test[preds_example_df_test$Labels=="Prediction" &
	preds_example_df_test$Date <= train_date_cutoff, "Labels"] <-
	"In-sample Prediction"

preds_example_df_test[preds_example_df_test$Labels=="Prediction" &
	preds_example_df_test$Date > train_date_cutoff, "Labels"] <-
	"Out-of-sample Prediction"

preds_plot_test <- ggplot(preds_example_df_test,
	aes(x=Date, y=y, color=Labels)) + geom_line() + geom_point() +
	ylab("Cumulative Positive Tests") +
	ggtitle(paste("Predictions (Cumulative Cases):",
		example_neighborhood))

print(preds_plot_test)

######### Full data, cases

# Create panel data.frame

pdf <- pdata.frame(cases, index=c("Locations", "collect_time"),
	drop.index=FALSE)

set.seed(235334)
model <- pglm(formula, data=pdf, family=poisson,
	model = model_type, method="nr")

print("Model trained on full data set (same model as trained on part of data set above):")

print(summary(model))

# predictions <- pred.pglm(model, cases, n_lags = n_lags,
# 	response_var = "Total.Cases")

# print("Root mean squared error for predictions:")

# print(predictions[[2]])

predictions_cur <- pred.pglm(model, cases, n_lags = n_lags,
	response_var = "Total.Cases", date = max(cases$collect_time) + 1,
	time_cov_true=TRUE)

print("Predicted number of new cases tomorrow in each neighborhood (based on same model trained on full data set):")
predictions_cur[[1]]$Prediction <- round(predictions_cur[[1]]$Prediction,
	digits=0)
print(predictions_cur[[1]])

# print("Root mean squared error for predictions:")

# print(predictions_cur[[2]])

# predictions_shoaks <- pred.pglm(model, cases, n_lags = n_lags,
# 	response_var = "Total.Cases", loc = example_neighborhood)

# print(predictions_shoaks)

# preds_example_df <- makeGgplotFriendly(predictions_shoaks[[1]], "Date", 
# 	c("Prediction", "Observed"))

# preds_plot <- ggplot(preds_example_df, aes(x=Date, y=y, color=Labels)) +
# 	geom_line() + ylab("Cumulative Positive Tests")

# print(preds_plot)

#
#
#
#
#
#
#

########## Modeling New Infections

#
#
#
#
#
#

# Now try to predict actual number of new infections when the most current lag 
# I may use is infection_test_delay + reporting_delay days ago (in order to be
# able to predict infections tomorrow)

########### Prepare Data

# # Drop PI because it has missing values

# cases_backcast <- cases_backcast[, colnames(cases_backcast) != "PI"]



# Variables to keep
vars_to_keep <- c("Infected", "Recovered", "New_Infected", "New_Recovered",
	"Locations", "collect_time", "tot_pop", "Infect_Cumul")
if(!any(is.na(model_covariates))){
	vars_backcast <- c(vars_to_keep, model_covariates)
}else{
	vars_backcast <- vars_to_keep
}


cases_backcast <- cases_backcast[, vars_backcast]

# Remove dates before all locations have at least one infection
cases_backcast <- cases_backcast[order(cases_backcast$collect_time), ]
cases_backcast <- cases_backcast[order(cases_backcast$Locations), ]
dates <- order(unique(cases_backcast$collect_time))
dates_to_exclude <- integer(0)
class(dates_to_exclude) <- "Date"
for(t in 1:length(dates)){
	infected_counts <- cases_backcast[cases_backcast$collect_time == dates[t], 
		"Infected"]
	# recovered_counts <- cases_backcast[cases_backcast$collect_time == dates[t], 
	# 	"Recovered"]
	if(any(infected_counts == 0)){
		# # locations with 0 cases
		# indices <- cases_backcast[cases_backcast$collect_time == dates[t] &
		# 	cases_backcast$, ]
		dates_to_exclude <- c(dates_to_exclude, dates[t])
	}
}

print("Dates to exclude:")
print(dates_to_exclude)

cases_backcast <-
	cases_backcast[!(cases_backcast$collect_time %in% dates_to_exclude), ]


# Add 1 to New_Infected and New_Recovered so I can safely take logs

cases_backcast$New_Infected <- cases_backcast$New_Infected + 1
cases_backcast$New_Recovered <- cases_backcast$New_Recovered + 1

# Model
if(!any(is.na(model_covariates))){
	formula_2 <- as.formula(paste("New_Infected ~ lag(log(New_Infected), ",
		infection_test_delay + reporting_delay, ":",
		infection_test_delay + reporting_delay + n_lags - 1,
		") + ", paste(model_covariates, collapse=" + "), " + ",
		paste(time_cov_names, collapse=" + "),
		" + lag(log(New_Recovered))", sep = ""))
} else{
	formula_2 <- as.formula(paste("New_Infected ~ lag(log(New_Infected), ",
		infection_test_delay + reporting_delay, ":",
		infection_test_delay + reporting_delay + n_lags - 1,
		")  + lag(log(New_Recovered))", sep = ""))
}

# In order to help predictions, we will take advantage of the fact that
# we can estimate what the Recovered population will be up to recover_days in
# advance (since the Recovered population on any given day is equal to the
# cumulative number of infections recover_days days ago). However, there is
# no point in projecting more than infection_test_delay + reporting_delay - 1 days past the most
# recent date of Infections estimate, because that is today.


finish_date <- min(max(cases_backcast$collect_time) + infection_test_delay +
	reporting_delay - 1, max(cases_backcast$collect_time) + recover_days)
dates_to_fill <- seq(max(cases_backcast$collect_time) + 1, finish_date, by=1)

# Duplicate cases_backcast to create a separate copy for predictions

cases_backcast_preds <- cases_backcast

# Add blank rows for these dates to cases_backcast

for(i in 1:length(areas)){
	# This location
	loc <- areas[i]
	# Create a new data.frame chunk for the dates we need to fill in. For now,
	# set all numeric entries equal to NA.
	df_i <- cases_backcast[cases_backcast$Locations==loc, ]
	# Only need to add length(dates_to_fill) rows to data.frame
	df_i <- df_i[(nrow(df_i) - length(dates_to_fill) + 1):nrow(df_i), ]
	# Make the dates the dates that we want to fill
	df_i$collect_time <- dates_to_fill
	df_i$Locations <- loc
	# Set all of the numeric entries equal to NA for now
	vars_to_fill <- vars_to_keep[!(vars_to_keep %in% c("Locations",
		"collect_time", "tot_pop"))]
	for(j in 1:length(vars_to_fill)){
		df_i[, vars_to_fill[j]] <- NA
	}
	# Add to cases data.frame
	cases_backcast_preds <- rbind(cases_backcast_preds, df_i)
}

cases_backcast_preds <- cases_backcast_preds[order(cases_backcast_preds$collect_time), ]
cases_backcast_preds <- cases_backcast_preds[order(cases_backcast_preds$Locations), ]
rownames(cases_backcast_preds) <- c()

# Now fill in Recovered and New_Recovered values with cumulative number of
# infections recover_days
# days ago (and fill in Infected and Susceptible as NA, since we don't have
# a way to estimate those as of now.)

for(t in 1:length(dates_to_fill)){
	# this date
	today <- dates_to_fill[t]
	yesterday <- today - 1
	for(i in 1:length(areas)){
		# This location
		loc <- areas[i]
		ind <- cases_backcast_preds$collect_time == today &
			cases_backcast_preds$Locations==loc
		# Recovered is equal to cumulative infected recover_days days ago
		ind_rec <- cases_backcast_preds$collect_time == (today - recover_days) &
			cases_backcast_preds$Locations==loc
		cases_backcast_preds[ind, "Recovered"] <-
			cases_backcast_preds[ind_rec, "Infect_Cumul"]
		# New_Recovered is equal to currently recovered minus previously
		# recovered
		ind_yest <- cases_backcast_preds$collect_time == yesterday &
			cases_backcast_preds$Locations==loc
		cases_backcast_preds[ind, "New_Recovered"] <-
			max(0, cases_backcast_preds[ind, "Recovered"] -
			cases_backcast_preds[ind_yest, "Recovered"])
		# As before, add 1 so I xan safely take logs
		cases_backcast_preds[ind, "New_Recovered"] <-
			cases_backcast_preds[ind, "New_Recovered"] + 1
		# New_Infected is NA since haven't observed these days yet
		cases_backcast_preds[ind, "New_Infected"] <- NA
		cases_backcast_preds[ind, "Infected"] <- NA
	}
}

#
#
#
#
#
#
#
##### Training Models For New Infections
#
#
#
#
#
#
#

############ Holdout sample, cases

# Divide data into training and test sets

dates_backcast <- unique(cases_backcast$collect_time)
train_samples_backcast <- round(train_pct*length(dates_backcast))
train_date_cutoff_backcast <- dates_backcast[train_samples_backcast]
cases_train_backcast <- cases_backcast[cases_backcast$collect_time <=
	train_date_cutoff_backcast, ]
# cases_test_backcast <- cases_backcast[cases_backcast$collect_time >
# 	train_date_cutoff_backcast, ]
cases_test_backcast_preds <- cases_backcast_preds[cases_backcast_preds$collect_time >
	train_date_cutoff_backcast, ]

# Incorporate time covariates
cases_train_backcast <- dplyr::left_join(cases_train_backcast, time_covs, 
	by="collect_time")
cases_test_backcast_preds <- dplyr::left_join(cases_test_backcast_preds,
	time_covs, by="collect_time")
cases_backcast <- dplyr::left_join(cases_backcast, time_covs, by="collect_time")
cases_backcast_preds <- dplyr::left_join(cases_backcast_preds, time_covs,
	by="collect_time")

# Create panel data.frame

pdf_back_train <- pdata.frame(cases_train_backcast,
	index=c("Locations", "collect_time"), drop.index=FALSE)

set.seed(7834534)
model_infect_train <- pglm(formula_2, data=pdf_back_train, family=poisson,
	model = model_type, method="nr")

predictions_inf_test <- pred.pglm(model_infect_train,
	cases_test_backcast_preds, n_lags = n_lags, response_var = "New_Infected",
	offset = infection_test_delay + reporting_delay - 1, new_recovered=TRUE,
	time_cov_true=TRUE)

# print(format(predictions_example_inf[[1]], scientific=FALSE, digits=0))

print("RMSE for out-of-sample predicted new infections:")

print(predictions_inf_test[[2]])

predictions_example_inf_test <- pred.pglm(model_infect_train,
	cases_backcast_preds, n_lags = n_lags, response_var = "New_Infected",
	offset = infection_test_delay + reporting_delay - 1, new_recovered=TRUE,
	loc = example_neighborhood, time_cov_true=TRUE)

preds_example_df_inf_test <- makeGgplotFriendly(predictions_example_inf_test[[1]],
	"Date", c("Prediction", "Observed"))

# Label in-sample and out-of-sample predictions
levels(preds_example_df_inf_test$Labels) <-
	c(levels(preds_example_df_inf_test$Labels), "In-sample Prediction",
		"Out-of-sample Prediction")

preds_example_df_inf_test[preds_example_df_inf_test$Labels=="Prediction" &
	preds_example_df_inf_test$Date <= train_date_cutoff_backcast, "Labels"] <-
	"In-sample Prediction"

preds_example_df_inf_test[preds_example_df_inf_test$Labels=="Prediction" &
	preds_example_df_inf_test$Date > train_date_cutoff_backcast, "Labels"] <-
	"Out-of-sample Prediction"

preds_plot_inf_test <- ggplot(preds_example_df_inf_test, aes(x=Date, y=y,
	color=Labels)) + geom_line() + geom_point() + ylab("New Infections") +
	ggtitle(paste("Predictions (New Infections):",
		example_neighborhood))

print(preds_plot_inf_test)




# Create panel data.frame for full data set

pdf_back <- pdata.frame(cases_backcast, index=c("Locations", "collect_time"),
	drop.index=FALSE)

set.seed(5346823)
model_infect <- pglm(formula_2, data=pdf_back, family=poisson,
	model = model_type, method="nr")

print("Model trained on full new infections data set (same model as trained on part of new infections data set above):")

print(summary(model_infect))

# Now we're ready to make predicitons.

# predictions_inf <- pred.pglm(model_infect, cases_backcast_preds, n_lags = n_lags,
# 	response_var = "New_Infected", offset = infection_test_delay +  1,
# 	new_recovered=TRUE)

# print("Root mean squared error for predictions:")

# print(predictions_inf[[2]])

predictions_cur_inf <- pred.pglm(model_infect, cases_backcast_preds,
	n_lags = n_lags, response_var = "New_Infected",
	offset = infection_test_delay + reporting_delay - 1,
	new_recovered=TRUE, date = max(cases_backcast_preds$collect_time) + 1,
	time_cov_true=TRUE)

print("Predicted number of new infections tomorrow in each neighborhood (based on same model trained on full data set):")
predictions_cur_inf[[1]]$Prediction <- round(predictions_cur_inf[[1]]$Prediction,
	digits=0)
print(predictions_cur_inf[[1]])


######## Generate ad hoc predictions for cities where predictions were not
# available

# Backcasting case counts for all neighborhoods (including neighborhoods
# with no covariates)

####### Backcast case counts

# Now for each neighborhood, we backcast case counts by assuming number of cases
# added per day (from la_cases data.frame) is split among neighborhoods by
# population.

print("Backcasting case counts...")

t0 <- Sys.time()

cases_backcast_old <- cases_old

dates_to_fill <- seq(min(la_cases$Date), min(cases_backcast_old$collect_time) - 1, by=1)
# Keep last day we backcast for
last_backcast_date <- max(dates_to_fill)
areas <- unique(cases_backcast_old$Locations)

for(t in length(dates_to_fill):1){
	# this date
	today <- dates_to_fill[t]
	# We want to get the number of cases in each neighborhood on this day. To do
	# this, we'll get the number of cases that were added the following day
	# and subtract those from each number. 
	tomorrow <- today + 1
	new_cases_t <- la_cases$new_case[la_cases$Date == tomorrow]
	for(i in 1:length(areas)){
		# This location
		loc <- areas[i]
		# Get the population of this area
		pop_i <- covs[covs$Locations==loc, "tot_pop"]
		# Get the proportion of new cases attributed to this area
		new_cases_t_i <- new_cases_t*pop_i/la_pop
		# Number of total cases tomorrow
		n_cases_tomorrow <- cases_backcast_old[cases_backcast_old$Locations==loc &
			cases_backcast_old$collect_time ==tomorrow, "Total.Cases"]
		# Create a new data.frame row with number of cases equal to tomorrow's
		# number minus this number
		df_t_i <- cases_backcast_old[cases_backcast_old$Locations==loc &
			cases_backcast_old$collect_time ==tomorrow, ]
		df_t_i$collect_time <- today
		df_t_i$Total.Cases <- max(n_cases_tomorrow - new_cases_t_i, 0)
		# Add to cases data.frame
		cases_backcast_old <- rbind(cases_backcast_old, df_t_i)
	}
}


cases_backcast_old <- cases_backcast_old[order(cases_backcast_old$collect_time), ]
cases_backcast_old <- cases_backcast_old[order(cases_backcast_old$Locations), ]
rownames(cases_backcast_old) <- c()
cases_backcast_old$Total.Cases <- round(cases_backcast_old$Total.Cases)

print("Done! Total time:")
print(Sys.time() - t0)


# Now we assume that test result shows up 7 days after infection, so subtract
# 7 from each date to get infection levels.

cases_backcast_old$collect_time <- cases_backcast_old$collect_time -
	infection_test_delay

# We also assume that each positive test case represents 8 actual infections,
# so multiply number of test cases by 8 to get cumulative infections
cases_backcast_old$Infect_Cumul <- cases_backcast_old$Total.Cases*infect_mult



################ Compute S, I, and R levels for each neighborhood on each day

# Add Susceptible, Infected, and Recovered populations for each neighborhood
# on each day
cases_backcast_old <- data.frame(cases_backcast_old, integer(nrow(cases_backcast_old)), 
	integer(nrow(cases_backcast_old)), integer(nrow(cases_backcast_old)),
	integer(nrow(cases_backcast_old)), integer(nrow(cases_backcast_old)))

colnames(cases_backcast_old)[(ncol(cases_backcast_old) - 4):ncol(cases_backcast_old)] <- c(
	"Susceptible", "Infected", "Recovered", "New_Infected", "New_Recovered")


# # Since all areas start with 0 infections in our model, start with everyone
# # in susceptible population

# for(i in areas){
# 	# This location
# 	loc <- areas[i]
# 	cases_backcast_old[cases_backcast_old$Locations==loc &
# 		cases_backcast_old$collect_time==min(cases_backcast_old$collect_time), 
# 		"Susceptible"] <- cases_backcast_old[cases_backcast_old$Locations==loc &
# 		cases_backcast_old$collect_time==min(cases_backcast_old$collect_time), 
# 		"tot_pop"]
# }

# Now proceed through dates

print("Estimating susceptible, infected, and recovered populations in each neighborhood on each date...")

t0 <- Sys.time()

dates <- unique(cases_backcast_old$collect_time)

for(t in 2:length(dates)){
	# this date
	today <- dates[t]
	yesterday <- today - 1
	for(i in 1:length(areas)){
		# This location
		loc <- areas[i]
		ind <- cases_backcast_old$collect_time == today &
			cases_backcast_old$Locations==loc
		# Infected is equal to cumulative infected minus number of cases that
		# are recover_days days old
		if(t > recover_days){
			ind_rec <- cases_backcast_old$collect_time == (today - recover_days) &
				cases_backcast_old$Locations==loc
			cases_backcast_old[ind, "Infected"] <-
				cases_backcast_old[ind, "Infect_Cumul"] -
				cases_backcast_old[ind_rec, "Infect_Cumul"]
		} else{
			cases_backcast_old[ind, "Infected"] <-
				cases_backcast_old[ind, "Infect_Cumul"]
		}
		# Recovered is equal to cumulative infected minus currently infected
		# (also equal to cumulative number of cases that are recover_days days
		# old)
		if(t > recover_days){
			cases_backcast_old[ind, "Recovered"] <-
				cases_backcast_old[ind, "Infect_Cumul"] -
				cases_backcast_old[ind, "Infected"]
		}
		# New_Recovered is equal to currently recovered minus previously
		# recovered
		ind_yest <- cases_backcast_old$collect_time == yesterday &
			cases_backcast_old$Locations==loc
		cases_backcast_old[ind, "New_Recovered"] <-
			max(0, cases_backcast_old[ind, "Recovered"] -
				cases_backcast_old[ind_yest, "Recovered"])
		# New_Infected is equal to currently infected minus previously
		# infected plus New_Recoveries
		cases_backcast_old[ind, "New_Infected"] <-
			max(0, cases_backcast_old[ind, "Infected"] -
			cases_backcast_old[ind_yest, "Infected"] + 
			cases_backcast_old[ind, "New_Recovered"])
		# Susceptible is equal to total population minus infected minus
		# recovered
		# cases_backcast_old[ind, "Susceptible"] <- round(
		# 	cases_backcast_old[ind, "tot_pop"] - cases_backcast_old[ind, "Infected"] -
		# 	cases_backcast_old[ind, "Recovered"])

		# cases_backcast_old[cases_backcast_old$Locations==loc &
		# 	cases_backcast_old$collect_time==today, "Infected"] <- 
		# 	cases_backcast_old[cases_backcast_old$Locations==loc &
		# 	cases_backcast_old$collect_time==yesterday, "Infected"] + new_cases
		# new_cases <- cases_backcast_old[cases_backcast_old$Locations==loc &
		# 	cases_backcast_old$collect_time==today, "Total.Cases"] -
		# 	cases_backcast_old[cases_backcast_old$Locations==loc &
		# 	cases_backcast_old$collect_time==yesterday, "Total.Cases"]
		# Susceptible is equal to previous susceptible minus number of new cases
		
	}
}

print("Done! Total time:")
print(Sys.time() - t0)

########### Find weights for missing neighborhoods

# Observed locations

all_locs <- unique(cases_backcast_old$Locations)

observed_locations <- all_locs[!(all_locs %in% missing_locations)]

pred_infecs <- cases_backcast_old[cases_backcast_old$Locations %in%
	observed_locations, c("Locations", "collect_time", "New_Infected")]

resp_infecs <- cases_backcast_old[cases_backcast_old$Locations %in%
	missing_locations, c("Locations", "collect_time", "New_Infected")]

# Create design matrix of predictors

preds <- matrix(0,  nrow(resp_infecs)/length(unique(resp_infecs$Locations)),
	length(observed_locations))

for(i in 1:length(observed_locations)){
	# this predictor
	pred_loc <- observed_locations[i]
	# predictor vector
	pred_i <- pred_infecs[pred_infecs$Locations==pred_loc, c("collect_time",
		"New_Infected")]
	pred_i <- pred_i[order(pred_i["collect_time"]), ]
	preds[, i] <- as.numeric(pred_i[, "New_Infected"])
}

colnames(preds) <- observed_locations

# Matrix of loadings
loadings <- matrix(0, length(missing_locations), length(observed_locations))

# Get loadings for each variable
for(i in 1:length(missing_locations)){
	# this location
	loc <- missing_locations[i]
	# response vector
	resp_i <- resp_infecs[resp_infecs$Locations==loc, c("collect_time",
		"New_Infected")]
	resp_i <- resp_i[order(resp_i["collect_time"]), ]
	resp_i <- as.numeric(resp_i[, "New_Infected"])
	# If resp_i is all 0s, just use 0s for loadings
	if(sum(resp_i != 0) < 3){
		loadings[i, ] <- 0
	} else{
		# Choose a lambda
		lambda_i <- cv.glmnet(x=preds, y=resp_i, alpha=0)$lambda.min
		# Get loadings
		coefs_i <- as.numeric(coef(glmnet(x=preds, y=resp_i, lambda=lambda_i,
			alpha=0, intercept=FALSE)))
		loadings[i, ] <- coefs_i[2:length(coefs_i)]	
	}
}

########## Risk Scores

# Risk scores for now: look at estimated daily new infections based on past
# observed data. Divide into three quantiles. Then for new predicted infections
# /population, "high risk" if these predicted infections/population would be
# in the highest third of new infections/population, "medium risk" if in middle
# third, "low risk" if in bottom third.

cases_scores <- cases_backcast

# Eliminate backward-projected data

cases_scores <- cases_scores[cases_scores$collect_time > last_backcast_date, ]

# Per capita new infections
cases_scores$New_Infec_Per_Capita <- cases_scores$New_Infected/
	cases_scores$tot_pop

# Risk thresholds
low_risk <- as.numeric(quantile(cases_scores$New_Infec_Per_Capita, 1/3))
high_risk <- as.numeric(quantile(cases_scores$New_Infec_Per_Capita, 2/3))

# Generate risk scores

risks <- predictions_cur_inf[[1]][,
	colnames(predictions_cur_inf[[1]]) != "Observed"]

cases_scores_join <- cases_scores[, c("Locations", "tot_pop")]
cases_scores_join <- unique(cases_scores_join)

risks <- suppressWarnings(dplyr::left_join(risks, cases_scores_join,
	by="Locations"))

risks$New_Infec_Per_Capita <- risks$Prediction/risks$tot_pop

risks$Risk_Category <- character(nrow(risks))

risks$Risk_Category[risks$New_Infec_Per_Capita >= high_risk] <- "High Risk"
risks$Risk_Category[risks$New_Infec_Per_Capita < high_risk &
	risks$New_Infec_Per_Capita >= low_risk] <- "Medium Risk"
risks$Risk_Category[risks$New_Infec_Per_Capita < low_risk] <- "Low Risk"

risks$Risk_Category <- as.factor(risks$Risk_Category)

tomorrow <- unique(risks$Date)

# Finally, infer risk scores for neighborhoods where we couldn't calculate
# them directly

preds_infer <- risks$Prediction[order(match(risks$Prediction,
	observed_locations))]

risks_missing <- as.numeric(loadings %*% preds_infer)

risks <- risks[, !(colnames(risks) %in% c("Date", "tot_pop"))]


for(i in 1:length(risks_missing)){
	df <- data.frame(missing_locations[i], risks_missing[i],
		as.numeric(NA), as.character(NA))
	colnames(df) <- colnames(risks)
	risks <- rbind(risks, df)
}

print(paste("Risk Predictions For ", tomorrow, ":", sep=""))
print(risks)

# risks$New_Infec_Per_Capita








