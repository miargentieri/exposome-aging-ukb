

pretty_labels <- function(x) {
    
    x$pretty_labels <- x$Variable
    
    # make pretty labels for plot
    # this section will need to be tweaked for each new set of results
    x$pretty_labels <-
        gsub("_", " ",
             x$pretty_labels,
             fixed = TRUE)
    x$pretty_labels <-
        gsub("hshld", "household", 
             x$pretty_labels, 
             fixed = TRUE)
    x$pretty_labels <-
        gsub("freq", "frequency", 
             x$pretty_labels, 
             fixed = TRUE)
    x$pretty_labels <- 
        gsub("pack", "smoking pack",
             x$pretty_labels,
             fixed = TRUE)
    x$pretty_labels <- 
        gsub("10yrs", "10 yrs",
             x$pretty_labels,
             fixed = TRUE)
    
    
    x$pretty_labels[x$Variable == "smoking_statusCurrent"] <- 
        "Current smoker"
    
    x$pretty_labels[x$Variable == "smoking_statusPrevious"] <- 
        "Previous smoker"
    
    x$pretty_labels[x$Variable == "accommodation_typeA_flat_maisonette_or_apartment"] <- 
        "Living in a flat vs. house"
    
    x$pretty_labels[x$Variable == "alcohol_freqDaily_or_almost_daily"] <- 
        "Daily alcohol intake"
    
    x$pretty_labels[x$Variable == "body_size_10yrs_oldPlumper"] <- 
        "Relatively plumper at 10 years old"
    
    x$pretty_labels[x$Variable == "cereal_fiber"] <- 
        "Cereal fiber intake"
    
    x$pretty_labels[x$Variable == "cheese"] <- 
        "Cheese intake"
    
    x$pretty_labels[x$Variable == "chronotypeDefinitely_an_evening_person"] <- 
        "Definitely evening chronotype"
    
    x$pretty_labels[x$Variable == "chronotypeMore_an_evening_than_a_morning_person"] <- 
        "Somewhat evening chronotype"
    
    x$pretty_labels[x$Variable == "ethnicityAsian"] <- 
        "Asian ethnicity"
    
    x$pretty_labels[x$Variable == "ethnicityBlack"] <- 
        "Black ethnicity"
    
    x$pretty_labels[x$Variable == "facial_agingAbout_your_age"] <- 
        "Facial aging: look about your age"
    
    x$pretty_labels[x$Variable == "facial_agingOlder_than_you_are"] <- 
        "Facial aging: look older than you are"
    
    x$pretty_labels[x$Variable == "height_10yrs_oldTaller"] <- 
        "Relatively taller at 10 years old"
    
    x$pretty_labels[x$Variable == "maternal_smoking"] <- 
        "Maternal smoking around birth"
    
    x$pretty_labels[x$Variable == "computer_games"] <- 
        "Frequency playing computer games"
    
    x$pretty_labels[x$Variable == "total_fruit"] <- 
        "Total fruit intake"
    
    x$pretty_labels[x$Variable == "bread"] <- 
        "Bread intake"
    
    x$pretty_labels[x$Variable == "salt"] <- 
        "Salt intake"
    
    x$pretty_labels[x$Variable == "red_meat"] <- 
        "Red meat intake"
    
    x$pretty_labels[x$Variable == "processed_meat"] <- 
        "Processed meat intake"
    
    x$pretty_labels[x$Variable == "poultry"] <- 
        "Poultry intake"
    
    x$pretty_labels[x$Variable == "sedentary"] <- 
        "Sedentary time"
    
    x$pretty_labels[x$Variable == "hshld_partner"] <- 
        "Living with partner"
    
    x$pretty_labels[x$Variable == "nap"] <- 
        "nap frequency"
    
    x$pretty_labels[x$Variable == "religious_group"] <- 
        "Part of religious group"
    
    x$pretty_labels[x$Variable == "air_pollution_PC1"] <- 
        "air pollution"
    
    x$pretty_labels[x$Variable == "sleep_hours_categorical_less_than_7_hours"] <- 
        "Sleep <7 hours per day"
    
    x$pretty_labels[x$Variable == "sleep_hours_categorical_more_than_9_hours"] <- 
        "Sleep >9 hours per day"
    
    x$pretty_labels[x$Variable == "gym"] <- 
        "gym use"
    
    x$pretty_labels[x$Variable == "own_or_rentRent_from_local_authority_local_council_housing_association"] <- 
        "Renting home from local council"
    
    x$pretty_labels[x$Variable == "own_or_rentRent_from_private_landlord_or_letting_agency"] <- 
        "Renting home from private landlord"
    
    x$pretty_labels[x$Variable == "own_or_rentPay_part_rent_and_part_mortgage_(shared_ownership)"] <- 
        "Pay part rent and part mortagage"
    
    x$pretty_labels[x$Variable == "own_or_rentLive_in_accommodation_rent_free"] <- 
        "Live in accommodation rent free"
    
    x$pretty_labels[x$Variable == "breastfed"] <- 
        "Breastfed as a child"
    
    x$pretty_labels[x$Variable == "death_partner"] <- 
        "Death of partner in past 2 years"
    
    x$pretty_labels[x$Variable == "ethnicityMixed"] <- 
        "Mixed ethnicity"
    
    x$pretty_labels[x$Variable == "gas_hob_heat"] <- 
        "Uses gas hob for heating"
    
    x$pretty_labels[x$Variable == "open_fire_heat"] <- 
        "Uses open fire for heating"
    
    x$pretty_labels[x$Variable == "oil_central_heat"] <- 
        "Uses oil (kerosene) central heating"
    
    x$pretty_labels[x$Variable == "alcohol_freqOnce_or_twice_a_week"] <- 
        "Drink alcohol 1-2x per week"
    
    x$pretty_labels[x$Variable == "alcohol_freqThree_or_four_times_a_week"] <- 
        "Drink alcohol 3-4x per week"
    
    x$pretty_labels[x$Variable == "total_veg"] <- 
        "Total vegetable intake"
    
    x$pretty_labels[x$Variable == "financial_difficulty"] <- 
        "Financial difficulty in past 2 years"
    
    x$pretty_labels[x$Variable == "chronotypeDefinitely_a_morning_person"] <- 
        "Definitely morning chronotype"
    
    x$pretty_labels[x$Variable == "pack_years_prop"] <- 
        "Smoking pack years as proportion"
    
    x$pretty_labels[x$Variable == "household_number"] <- 
        "Number of people living in household"
    
    x$pretty_labels[x$Variable == "worry_embarassment"] <- 
        "Worry after embarassment"
    
    x$pretty_labels[x$Variable == "age_scaled"] <- 
        "Age"
    
    x$pretty_labels[x$Variable == "sex"] <- 
        "Sex"
    
    # make first letter uppercase
    library(stringr)
    skip <- grep("PM|NO|NO2|IPAQ|LTPA|OPA|TOTAL", x$Variable)
    x$pretty_labels[-skip] <- str_to_sentence(x$pretty_labels[-skip])
    
    return(x)
}
