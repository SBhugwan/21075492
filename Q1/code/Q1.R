deaths<-read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/Covid/Deaths_by_cause.csv")
colnames(deaths)
covid<-read_csv("/Users/sahilbhugwan/Downloads/DatsciPractical23/data/Covid/owid-covid-data.csv")
colnames(covid)

combined <- bind_rows(deaths, covid)
colnames(combined)

#1 #How African countries’ experience differed from other regions.

##Comparing ave deaths in africa to europe
africa_data <- covid[covid$continent == "Africa", ]
europe_data <- covid[covid$continent == "Europe", ]


africa_avg_deaths <- africa_data %>% group_by(date) %>% summarize(avg_deaths = mean(total_deaths, na.rm = TRUE))
europe_avg_deaths <- europe_data %>% group_by(date) %>% summarize(avg_deaths = mean(total_deaths, na.rm = TRUE))


AVD<- ggplot() +
    geom_line(data = africa_avg_deaths, aes(x = date, y = avg_deaths, color = "Africa"), size = 1.2) +
    geom_line(data = europe_avg_deaths, aes(x = date, y = avg_deaths, color = "Europe"), size = 1.2) +
    labs(x = "Date", y = "Average Deaths", title = "Average Deaths in Africa and Europe") +
    scale_color_manual(values = c("Africa" = "blue", "Europe" = "red")) +
    theme_minimal()
#AVD

##testing and positivity rates in Africa compared to other regions

africa_data <- covid[covid$continent == "Africa", ]
other_regions_data <- covid[covid$continent != "Africa", ]

africa_testing_rates <- africa_data %>%
    mutate(positive_rate = total_cases / total_tests * 100) %>%
    group_by(date) %>%
    summarize(
        total_tests = sum(total_tests, na.rm = TRUE),
        new_tests_per_thousand = mean(new_tests_per_thousand, na.rm = TRUE),
        positive_rate = mean(positive_rate, na.rm = TRUE)
    )

other_regions_testing_rates <- other_regions_data %>%
    mutate(positive_rate = total_cases / total_tests * 100) %>%
    group_by(date) %>%
    summarize(
        total_tests = sum(total_tests, na.rm = TRUE),
        new_tests_per_thousand = mean(new_tests_per_thousand, na.rm = TRUE),
        positive_rate = mean(positive_rate, na.rm = TRUE)
    )

TP<- ggplot() +
    geom_line(data = africa_testing_rates, aes(x = date, y = new_tests_per_thousand, color = "Africa"), size = 1.2) +
    geom_line(data = other_regions_testing_rates, aes(x = date, y = new_tests_per_thousand, color = "Other Regions"), size = 1.2) +
    labs(x = "Date", y = "New Tests per Thousand", title = "Testing Rates: Africa vs. Other Regions") +
    scale_color_manual(values = c("Africa" = "blue", "Other Regions" = "red")) +
    theme_minimal()
#TP

##Exploring health care capacity
africa_healthcare_capacity <- africa_data %>%
    group_by(date) %>%
    summarize(
        hospital_beds_per_thousand = mean(hospital_beds_per_thousand, na.rm = TRUE),
        icu_patients = sum(icu_patients, na.rm = TRUE),
        healthcare_facilities = mean(handwashing_facilities, na.rm = TRUE)
    )

other_regions_healthcare_capacity <- other_regions_data %>%
    group_by(date) %>%
    summarize(
        hospital_beds_per_thousand = mean(hospital_beds_per_thousand, na.rm = TRUE),
        icu_patients = sum(icu_patients, na.rm = TRUE),
        healthcare_facilities = mean(handwashing_facilities, na.rm = TRUE)
    )
#hospital beds
HB<-ggplot() +
    geom_line(data = africa_healthcare_capacity, aes(x = date, y = hospital_beds_per_thousand, color = "Africa"), size = 1.2) +
    geom_line(data = other_regions_healthcare_capacity, aes(x = date, y = hospital_beds_per_thousand, color = "Other Regions"), size = 1.2) +
    labs(x = "Date", y = "Hospital Beds per Thousand", title = "Healthcare Capacity: Hospital Beds per Thousand") +
    scale_color_manual(values = c("Africa" = "blue", "Other Regions" = "red")) +
    theme_minimal()

#healthcare capacity : ICU patients
HCC<- ggplot() +
    geom_line(data = africa_healthcare_capacity, aes(x = date, y = icu_patients, color = "Africa"), size = 1.2) +
    geom_line(data = other_regions_healthcare_capacity, aes(x = date, y = icu_patients, color = "Other Regions"), size = 1.2) +
    labs(x = "Date", y = "ICU Patients", title = "Healthcare Capacity: ICU Patients") +
    scale_color_manual(values = c("Africa" = "blue", "Other Regions" = "red")) +
    theme_minimal()

HCC

#2#



#3#

hospital_icu_data <- covid %>%
    select(date, continent, hosp_patients_per_million, icu_patients_per_million) %>%
    na.omit()

hospital_icu_data <- hospital_icu_data %>%
    group_by(continent, date) %>%
    summarize(mean_hosp_patients_per_million = mean(hosp_patients_per_million),
              mean_icu_patients_per_million = mean(icu_patients_per_million)) %>%
    mutate(cumulative_hosp = cumsum(mean_hosp_patients_per_million),
           cumulative_icu = cumsum(mean_icu_patients_per_million))

HFvICU<-ggplot(hospital_icu_data, aes(x = date)) +
    geom_line(aes(y = cumulative_hosp, color = "Hospitalization Facilities")) +
    geom_line(aes(y = cumulative_icu, color = "ICU Admissions")) +
    facet_wrap(~ continent, scales = "free_y") +
    labs(title = "Increase in Hospitalization Facilities vs. ICU Admissions by Continent",
         x = "Date",
         y = "Cumulative per Million",
         color = "Metric") +
    theme_minimal()

#HFvICU

#OR can view it as a stacked plot

hospital_icu_data_long <- hospital_icu_data %>%
    select(date, continent, cumulative_hosp, cumulative_icu) %>%
    pivot_longer(cols = c(cumulative_hosp, cumulative_icu),
                 names_to = "Metric",
                 values_to = "Cumulative per Million")

SHFICU<- ggplot(hospital_icu_data_long, aes(x = date, y = `Cumulative per Million`, fill = Metric)) +
    geom_area() +
    facet_wrap(~ continent, scales = "free_y") +
    labs(title = "Increase in Hospitalization Facilities vs. ICU Admissions by Continent",
         x = "Date",
         y = "Cumulative per Million",
         fill = "Metric") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Extra

C19 <- covid %>%
    mutate(date = as.Date(date),
           Month = format(date, "%b") ) %>%
    select(date, Month, continent, new_cases_per_million, people_fully_vaccinated_per_hundred) %>% arrange(Month)

C19 <- C19 %>%
    group_by(Month, continent) %>%
    summarise(across(.cols = 2:3, .fns = mean, na.rm = TRUE, .names = "Mean_{.col}")) %>% gather(key = Observation, value = Rate, -Month, -continent  ) %>% arrange(continent, Month)

MCFV<- C19%>%
    ggplot(aes(x = Month, y = Rate, fill = Observation)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ continent, scales = "free_y") +
    labs(title = "Mean Rates by Month and Continent",
         x = "Month",
         y = "Mean Rate",
         fill = "Observation") +
    theme_minimal() +
    theme(plot.title = element_text(size = 4, face = "bold"),
          axis.text = element_text(size = 4),
          axis.title = element_text(size = 4, face = "bold"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 4))

#MCFV
