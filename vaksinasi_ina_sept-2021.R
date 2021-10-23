# Load packages -----------------------------------------------------------

library(curl)
library(vroom)
library(tidyquery)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(hrbrthemes)
library(lubridate)
library(scales)

# Download data -----------------------------------------------------------

vaccov_url <-
  "https://covid.ourworldindata.org/data/owid-covid-data.csv"

vaccov_path <-
  file.path("data", basename(vaccov_url))

curl_download(
  url = vaccov_url,
  destfile = vaccov_path,
  quiet = FALSE
)

# Import data -------------------------------------------------------------

vaccov_data <- read.csv(vaccov_path, sep=",")
vaccov_ind <- read.csv("data/vaxrecap.csv", sep=",")

# Use SQL to explore data -------------------------------------------------

#' How many data points are in the data?

query(
  "
  SELECT COUNT(*) AS nrows
  FROM vaccov_data;
  "
)

#' When is the first available date? How about the last date?

query(
  "
  SELECT min(date) AS start_date, max(date) AS end_date, 'World' AS place
  FROM vaccov_data;
  "
)

query(
  "
  SELECT min(date) AS start_date, max(date) AS end_date, 'Indonesia' AS place
  FROM vaccov_ind
  ; "
)

#' because owid data more updated then

vaccov_ind <-
query(
  "
  SELECT date, total_vaccinations AS total , people_vaccinated AS vaccine_1 ,
   people_fully_vaccinated AS vaccine_2 
  FROM vaccov_data
  WHERE iso_code = 'IDN' AND (total_vaccinations > 0)
  ;"
)  

#' How many countries are in the data?

query(
  "
  SELECT COUNT (DISTINCT location) AS n_country
  FROM vaccov_data;
  "
)

#' How many population in each countries are in the data?

query(
  "
  SELECT DISTINCT iso_code, location AS country, population
  FROM vaccov_data
  ; "
)

# data cleansing ----------------------------------------------------------

vaccov_data <-
query(
  "
  SELECT * FROM vaccov_data
  WHERE iso_code not like 'OWID%';
  "
)

vaccov_data[is.na(vaccov_data)] <- 0

# Problem Formulation  -------------------- 

#' How many population in the world?

country_population <-
query(
  "
  SELECT DISTINCT iso_code AS code, location AS country, continent, 
                  population , gdp_per_capita AS gdp
  FROM vaccov_data
  ; "
)

world_population <-
query(
  "
  SELECT 'population' AS Title , sum(population) AS tot
  FROM country_population
  ; "
)

#' For each country, search vaccine data by date

country_vaccine <-
query(
  "
  SELECT iso_code AS code , date , total_vaccinations AS total , 
         people_vaccinated AS vaccine_1 , people_fully_vaccinated AS vaccine_2 , 
         total_boosters AS boosters
  FROM vaccov_data
  ; "
)

max_country_vaccine <-
  query(
    "
    SELECT iso_code AS code , max(date) AS lastdate, max(total_vaccinations) AS total , 
    max(people_vaccinated) AS vaccine_1 , max(people_fully_vaccinated) AS vaccine_2 
    FROM vaccov_data
    GROUP BY iso_code  
    ; "
  )

#' Check Suspicious data, running double check

query(
  "
  SELECT *
  FROM max_country_vaccine
  WHERE total != vaccine_1 + vaccine_2
  ; "
)

#' repairing data, because double check found unmatch data

max_country_vaccine <- max_country_vaccine %>%
  mutate(code, lastdate, total, vaccine_1, vaccine_2 = total - vaccine_1)

#' union data

F_vaccine <-
  query(
    "
    SELECT 'vaccine 1' AS Title, sum(vaccine_1) AS tot
    FROM max_country_vaccine ;
    " )

S_vaccine <-
  query(
    "
    SELECT 'vaccine 2' AS Title, sum(vaccine_2) AS tot
    FROM max_country_vaccine ;
    "
  )

world_together <- rbind(world_population,F_vaccine,S_vaccine)

world_together <- world_together %>%
    mutate(sbX = 1, sbY = tot/(4*world_population$tot), uk = (tot)/world_population$tot, Title, tot, persen = tot/world_population$tot)


# grafik plot ----------------------

plot.world <- ggplot(world_together) + labs(title = "People Vaccinated VS World Population", subtitle = max_country_vaccine$lastdate)
plot.world <- plot.world + aes(x=sbX, y=sbY, size = uk, colour = Title) 
plot.world <- plot.world + labs(x = "", y = "") + xlim(0,2) + ylim(-0.9,1)
plot.world <- plot.world + geom_point(show.legend = FALSE, alpha = 0.8) +
              scale_size(range = c(60, 100))+  
  theme(axis.text = element_text(size = 12, face="bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0, size = 16),
        panel.background = element_rect(fill = "white"))
plot.world + annotate("text", x = 0.9, y = 0.7, label = "World Population") +
  annotate("text", x = 1.32, y = 0.7, label = 100*world_population$tot/world_population$tot) +
  annotate("text", x = 1.44, y = 0.7, label = "%") +
  annotate("text", x = 0.9, y = 0.49, label = "1st Vaccine") +
  annotate("text", x = 1.2, y = 0.49, label = round(100*F_vaccine$tot/world_population$tot)) +
  annotate("text", x = 1.29, y = 0.49, label = "%") +
  annotate("text", x = 0.9, y = 0.1, label = "2st Vaccine")+
  annotate("text", x = 1.2, y = 0.1, label = round(100*S_vaccine$tot/world_population$tot))+
  annotate("text", x = 1.29, y = 0.1, label = "%")

#Indonesia Condition ------------
IDN_population <-
  query(
    "
    SELECT DISTINCT 'IDN Population' AS Title, population AS tot
    FROM vaccov_data
    WHERE iso_code = 'IDN'
    ; "
  )


max_Indonesia_vaccine <-
  query(
    "
    SELECT iso_code AS code , min(date) AS startdate, max(date) AS lastdate, max(total_vaccinations) AS total , 
    max(people_vaccinated) AS vaccine_1 , max(people_fully_vaccinated) AS vaccine_2 
    FROM vaccov_data
    WHERE iso_code = 'IDN'
    GROUP BY iso_code  
    ; "
  )

#' Check Suspicious data, running double check

query(
  "
  SELECT *
  FROM max_Indonesia_vaccine
  WHERE total != vaccine_1 + vaccine_2
  ; "
)

#' norepairing data, because double check found match data

#' union data

F_IDN_vaccine <-
  query(
    "
    SELECT 'vaccine 1' AS Title, sum(vaccine_1) AS tot
    FROM max_Indonesia_vaccine ;
    " )

S_IDN_vaccine <-
  query(
    "
    SELECT 'vaccine 2' AS Title, sum(vaccine_2) AS tot
    FROM max_Indonesia_vaccine ;
    "
  )

IDN_together <- rbind(IDN_population,F_IDN_vaccine,S_IDN_vaccine)

IDN_together <- IDN_together %>%
  mutate(sbX = 1, sbY = tot/(3*IDN_population$tot), uk = (tot)/IDN_population$tot, Title, tot, persen = tot/IDN_population$tot)


# grafik IDN plot ----------------------

plot.IDN <- ggplot(IDN_together) + labs(title = "People Vaccinated VS Indonesia Population", subtitle = max_Indonesia_vaccine$lastdate)
plot.IDN <- plot.IDN + aes(x=sbX, y=sbY, size = uk, colour = Title) 
plot.IDN <- plot.IDN + labs(x = "", y = "") + xlim(0,2) + ylim(-0.9,1)
plot.IDN <- plot.IDN + geom_point(show.legend = FALSE, alpha = 0.8) +
            scale_size(range = c(40, 100)) +  
  theme(axis.text = element_text(size = 12, face="bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0, size = 16),
        panel.background = element_rect(fill = "white"))
plot.IDN + annotate("text", x = 0.9, y = 0.6, label = "IDN Population") +
  annotate("text", x = 1.32, y = 0.6, label = 100*IDN_population$tot/IDN_population$tot) +
  annotate("text", x = 1.44, y = 0.6, label = "%") +
  annotate("text", x = 0.9, y = 0.38, label = "1st Vaccine") +
  annotate("text", x = 1.2, y = 0.38, label = round(100*F_IDN_vaccine$tot/IDN_population$tot)) +
  annotate("text", x = 1.29, y = 0.38, label = "%") +
  annotate("text", x = 0.9, y = 0.07, label = "2st Vaccine")+
  annotate("text", x = 1.2, y = 0.07, label = round(100*S_IDN_vaccine$tot/IDN_population$tot))+
  annotate("text", x = 1.29, y = 0.07, label = "%")

# 10 populated country -----------------

max_10_vaccine <-
  query(
    "
    SELECT iso_code AS code , min(date) AS startdate, max(date) AS lastdate, max(total_vaccinations) AS total , 
    max(people_vaccinated) AS vaccine_1 , max(people_fully_vaccinated) AS vaccine_2 , population , location AS country
    FROM vaccov_data
    GROUP BY iso_code, population, country
    ORDER BY population DESC
    LIMIT 10
    ; "
  )

#' Check Suspicious data, running double check

query(
  "
  SELECT *
  FROM max_10_vaccine
  WHERE total != vaccine_1 + vaccine_2
  ; "
)

#' repairing data, because double check found unmatch data

max_10_vaccine <- max_10_vaccine %>%
  mutate(code, startdate, lastdate, total, vaccine_1, vaccine_2 = total - vaccine_1, population)

#' union data

Top_10_together <-
  query(
    "SELECT code , startdate, lastdate, total , vaccine_1 , vaccine_2 , population , country ,
            round(((vaccine_1-vaccine_2)/population)*100) AS F_10_vaccine, round((vaccine_2/population)*100) AS S_10_vaccine
    FROM max_10_vaccine
    ORDER BY S_10_vaccine DESC
    ;"
  )

# grafik Top 10 plot ----------------------

plot.Top10 <- ggplot(data=Top_10_together,aes(x = reorder(country,S_10_vaccine)))
plot.Top10 <- plot.Top10 + 
  geom_bar(stat = "identity", width = 0.8, aes(y = F_10_vaccine + S_10_vaccine, fill="1st vaccine")) +
  geom_bar(stat = "identity", width = 0.8, aes(y = S_10_vaccine, fill="2nd vaccine")) +

  geom_text(aes(x = country, y = S_10_vaccine-2, label = S_10_vaccine), colour = "black", size= 3 )+
                  coord_flip() + theme(legend.position = "bottom")
plot.Top10 <- plot.Top10 + labs(title="Vaccine in Top 10 Populated Country", x="Country", y="% Population")
plot.Top10

# Indonesian speed  -----------------

plot.Speed <- ggplot(data=vaccov_ind,aes(date,vaccine_2)) +
  geom_col(fill = "#F95738") +
  labs(
    x = NULL,
    y = NULL,
    title = "Daily 2nd Vaccine in Indonesia"
  )
plot.Speed

# save to csv ----------------
write_csv(world_together, "data/world_together.csv")
write_rds(world_together, "data/world_together.rds")

write_csv(IDN_together, "data/IDN_together.csv")
write_rds(IDN_together, "data/IDN_together.rds")

write_csv(Top_10_together, "data/Top_10_together.csv")
write_rds(Top_10_together, "data/Top_10_together.rds")

write_csv(vaccov_ind, "data/vaccov_ind.csv")
write_rds(vaccov_ind, "data/vaccov_ind.rds")


# end program -----------------