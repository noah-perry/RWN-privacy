##### Import and Clean Body Fat Data #####

### Import Data
library(mfp)
data(bodyfat, package = "mfp")

### Clean Data
bf_clean <- bodyfat[,3:17]
  # Dropping case and brozek variables

names(bf_clean)[1] <- "bodyfat_pct"
  # Renaming siri variable as bodyfat_pct

bf_keep_vars <- names(bf_clean)
  # Used to drop temp variables added for cleaning at the end

bf_clean$bf_pct_calc <- round((495/bf_clean$density)-450, digits = 1)
bf_clean$abs_diff <- abs(bf_clean$bf_pct_calc - bf_clean$bodyfat_pct)
bf_clean$flag_excl <- ifelse(bf_clean$abs_diff > 1, 1, 0)
  # Checking relationship between density and bodyfat_pct is Siri's equation
  # Siri's equation: % Body Fat = (495 / Body Density) - 450
  # Flag cases where relationship doesn't hold for exclusion

bf_clean$flag_excl <- ifelse(bf_clean$bodyfat_pct < 4, 1, bf_clean$flag_excl)
  # Flag cases where bodyfat_pct is less than the lower limit of body fat percentage for healthy men 

bf_clean$flag_excl <- ifelse(bf_clean$height < 30, 1, bf_clean$flag_excl)
  # Flag cases with extremely small heights for exclusion

bf_clean <- bf_clean[bf_clean$flag_excl == 0,]
  # Remove cases flagged for exclusion

drops <- c('density', setdiff(names(bf_clean), bf_keep_vars))
bf_clean <- bf_clean[, !(names(bf_clean) %in% drops)]
rm(drops)
  # Remove density variable as it is redundant with bodyfat_pct
  # Remove variables created for data checks, exclusions

#bf_clean$bmi <- (703 * bf_clean$weight) / (bf_clean$height^2)
  # Add BMI variable
  # BMI Formula: 703 x weight (lb) / [height (in)]^2

row.names(bf_clean) <- NULL

