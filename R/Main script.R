# This R script gives access to the main Google sheet

library(geojsonio)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(googlesheets4)
library(httr)
library(jsonlite)

##GCFs Stuff ----

#GET GCF's Project API

GCF.URL <- "https://api.gcfund.org/v1/projects"
GCF.result <- httr::GET(GCF.URL)
#Get the GCF.result$content part as text
GCF.content <- httr::content(GCF.result, as = "text")
#transform that text to JSON to see it as data frames
GCF.content.from.json <- jsonlite::fromJSON(GCF.content)

#unnest the internal data frames into wide columns
GCF.content.from.json <- GCF.content.from.json %>%
  as_tibble() %>%
  unnest(cols = c(Countries), names_sep = ".")
#spread(key = Countries, value = )

#identify if they are multi-country projects
table2 <- GCF.content.from.json %>% group_by(ProjectsID) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(Multicountry = TRUE) %>%
  select(Multicountry, ProjectsID)

#join with the original table
GCF.content.from.json <- left_join(GCF.content.from.json, table2, by = "ProjectsID")

#Fill NAs of not multicountry projects
GCF.content.from.json <- GCF.content.from.json %>% mutate(Multicountry = case_when(Multicountry == "TRUE" ~ TRUE,
                                                                                   is.na(Multicountry) ~ FALSE))
# ###Continue unnesting the other variables
# GCF.content.from.json <- GCF.content.from.json %>%
#   unnest(cols = c(Entities, Disbursements), names_sep = ".")
# 
# GCF.content.from.json <- GCF.content.from.json %>%
#   unnest(cols = c(Funding), names_sep = ".")

#Changing to Date format
GCF.content.from.json$StartDate <- GCF.content.from.json$StartDate %>% as.Date(format = "%Y-%m-%d")
GCF.content.from.json$EndDate <- GCF.content.from.json$EndDate %>% as.Date(format = "%Y-%m-%d")
GCF.content.from.json$ApprovalDate <- GCF.content.from.json$ApprovalDate %>% as.Date(format = "%Y-%m-%d")
# GCF.content.from.json$Entities.AccreditationDate <- GCF.content.from.json$Entities.AccreditationDate %>% as.Date(format = "%Y-%m-%d")
# GCF.content.from.json$Disbursements.DateEffective <- GCF.content.from.json$Disbursements.DateEffective %>% as.Date(format = "%Y-%m-%d")

###GCF data is ready to use on any project

#Select only the variable that matter for this project
GCF.ProjectsbyCountry <- GCF.content.from.json %>%
  select(ProjectsID, ProjectName, Multicountry, Countries.CountryName, Countries.Region, Countries.ISO3, Countries.LDCs, Countries.SIDS, Theme, Sector, StartDate, EndDate, ApprovalDate, TotalGCFFunding, TotalCoFinancing, TotalValue, ProjectURL)


GCF.Multicountry <- GCF.ProjectsbyCountry %>% filter(Multicountry == TRUE)

GCF.Singlecountry <- GCF.ProjectsbyCountry %>% filter(Multicountry == FALSE)


#Get country total of GCF projects (not using) ----
# GCF.Singlecountrytotal <- GCF.Singlecountry %>% filter(Theme == "Adaptation") %>%
#   group_by(Countries.CountryName) %>%
#   summarise( TotalGCFFunding = sum(TotalGCFFunding), 
#              TotalCoFinancing = sum(TotalCoFinancing),
#              TotalValue = sum(TotalValue),
#              Countries.ISO3 = Countries.ISO3,
#              Countries.LDCs = Countries.LDCs, 
#              Countries.SIDS = Countries.SIDS )




#-----
##Examples from previous code on choropleth from the US (can delete) ----
# states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
# class(states)
# 
# 
# names(states)
# names(countries)
# 
# m <- leaflet(states) %>%
#   setView(-96, 37.8, 4) %>%
#   addTiles()
# 
# m %>% addPolygons()
# 
# 
# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
# 
# m %>% addPolygons(
#   fillColor = ~pal(density),
#   weight = 2,
#   opacity = 1,
#   color = "white",
#   dashArray = "3",
#   fillOpacity = 0.7)
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#   states$name, states$density
# ) %>% lapply(htmltools::HTML)
# 
# m <- m %>% addPolygons(
#   fillColor = ~pal(density),
#   weight = 2,
#   opacity = 1,
#   color = "white",
#   dashArray = "3",
#   fillOpacity = 0.7,
#   highlightOptions = highlightOptions(
#     weight = 5,
#     color = "#666",
#     dashArray = "",
#     fillOpacity = 0.7,
#     bringToFront = TRUE),
#   label = labels,
#   labelOptions = labelOptions(
#     style = list("font-weight" = "normal", padding = "3px 8px"),
#     textsize = "15px",
#     direction = "auto"))
# m

#-----


## Stuff with NAP Tracking sheet and World Map polygons

##Read the geojson world map file----
countries <- geojsonio::geojson_read("https://r2.datahub.io/clvyjaryy0000la0cxieg4o8o/master/raw/data/countries.geojson", what = "sp")

##Read excel file from online, with column headers and declaring NAs
NAP_sheet <- read_sheet('1D5qRsCMfNKkORZuz3qov0HGs4t1LiulrO116pPyhywk', sheet = 1, col_names = TRUE, na = c("","NA"))

##Transform dates into date format
NAP_sheet$Date.Posted <- NAP_sheet$Date.Posted %>% as.Date(format = "%Y-%m-%d")
NAP_sheet$SectoralNAP1.Date <- NAP_sheet$SectoralNAP1.Date %>% as.Date(format = "%Y-%m-%d")



##Fix country names so they can match the NAP_Sheet (UNFCCC Editorial Guidelines)----
countries@data$ADMIN <- recode(countries@data$ADMIN, 
                               "Vietnam" = "Viet Nam",
                               "United States Virgin Islands" = "United States Virgin Islands",
                               "Venezuela" = "Venezuela (Bolivarian Republic of)",
                               "British Virgin Islands" = "British Virgin Islands",
                               "Vatican" = "Holy See (Vatican City State)",
                               "East Timor" = "Timor-Leste",
                               "Syria" = "Syrian Arab Republic",
                               "Sint Maarten" = "Sint Maarten (Dutch part)",
                               "Swaziland" = "Eswatini",
                               "Republic of Serbia" = "Serbia",
                               "Somaliland" = "Somalia/Somaliland",
                               "Russia" = "Russian Federation",
                               "Palestine" = "State of Palestine",
                               "North Korea" = "Democratic People's Republic of Korea",
                               "Macedonia" = "North Macedonia",
                               "Moldova" = "Republic of Moldova", 
                               "Macao S.A.R" = "Macau",
                               "Laos" = "Lao People's Democratic Republic",
                               "South Korea" = "Republic of Korea",
                               "Iran" = "Iran (Islamic Republic of)",
                               "Hong Kong S.A.R." = "Hong Kong",
                               "Guinea Bissau" = "Guinea-Bissau",
                               "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
                               "Federated States of Micronesia" = "Micronesia (Federated States of)",
                               "Falkland Islands" = "Falkland Islands (Malvinas)",
                               "Czech Republic" = "Czechia",
                               "Cape Verde" = "Cabo Verde",
                               "Republic of Congo" = "Congo",
                               "Ivory Coast" = "Côte d'Ivoire",
                               "Brunei" = "Brunei Darussalam",
                               "Bolivia" = "Bolivia (Plurinational State of)",
                               "The Bahamas" = "Bahamas",
                               "Aland" = "Aland Islands")


##Put the two country tibbles together ----
countries@data <- left_join(countries@data, NAP_sheet, by = c("ADMIN" = "Country"))

##Recode NAs in type to fill all Non UN members
countries@data$Type <- replace_na(countries@data$Type, "Non UN Member")

##Modify the data into numerical format. 
countries@data <- countries@data %>% mutate(Type.Points = case_when(Type == "LDC" ~ 2, 
                                                                    Type == "Other developing country" ~ 0,
                                                                    Type == "Non UN Member" ~ -10,
                                                                    Type == "Annex 1" ~ -17,
                                                                    is.na(Type) ~ -10),
                                            LLDC.SIDS.Points = case_when(LLDC.SIDS == "LLDC" ~ 4,
                                                                         LLDC.SIDS == "SIDS" ~ 5,
                                                                         is.na(LLDC.SIDS) ~ 0),
                                            NAP.Points = case_when(NAP == "Yes" ~ 10,
                                                                   NAP == "No" ~ 0,
                                                                   is.na(NAP) ~ 0))


##Create the col list of the columns to be added for total points
col_list = c("Type.Points", "LLDC.SIDS.Points", "NAP.Points")

##Calculate the total
countries@data$Total.Points = rowSums(countries@data[,col_list]) 


##Change Total Points to descriptive factors I need 12 colors----
##Describing the levels in my factor
country.levels <- c("NA / Annex 1 Country",
                    "NA / Non UN Member",
                    "NA / Non UN Member & SIDS",
                    "Other developing country No NAP",
                    "Other developing country & LLDC No NAP",
                    "Other developing country & SIDS No NAP",
                    "LDC No NAP",
                    "LDC & LLDC No NAP",
                    "LDC & SIDS No NAP",
                    "Other developing country with NAP",
                    "Other developing country & LLDC with a NAP",
                    "Other developing country & SIDS with a NAP",
                    "LDC with NAP",
                    "LDC & LLDC with a NAP",
                    "LDC & SIDS with a NAP")


##Changing the Numerical data in Total.Points columns to Characters----
countries@data <- countries@data %>% mutate(NAPs.by.country.type = case_when(Total.Points == -17 ~ "NA / Annex 1 Country",
                                                                             Total.Points == -7 ~ "Annex 1 Country with NAP",
                                                                             Total.Points == -10 ~ "NA / Non UN Member",  
                                                                             Total.Points == -5 ~ "NA / Non UN Member & SIDS",
                                                                             Total.Points == 0 ~ "Other developing country No NAP",
                                                                             Total.Points == 2 ~"LDC No NAP",
                                                                             Total.Points == 4 ~ "Other developing country & LLDC No NAP",
                                                                             Total.Points == 5 ~ "Other developing country & SIDS No NAP",
                                                                             Total.Points == 6 ~ "LDC & LLDC No NAP",
                                                                             Total.Points == 7 ~ "LDC & SIDS No NAP",
                                                                             Total.Points == 10 ~ "Other developing country with NAP",
                                                                             Total.Points == 12 ~ "LDC with NAP",
                                                                             Total.Points == 14 ~ "Other developing country & LLDC with a NAP",
                                                                             Total.Points == 15 ~ "Other developing country & SIDS with a NAP",
                                                                             Total.Points == 16 ~ "LDC & LLDC with a NAP",
                                                                             Total.Points == 17 ~ "LDC & SIDS with a NAP" ))

##Changing the class from character to factors ---
countries@data$NAPs.by.country.type <- factor(countries@data$NAPs.by.country.type, levels = country.levels)


##Decide our own colors for the palette ----
countries@data <- countries@data %>% mutate(color.code = case_when(Total.Points == -17 ~ "rgb(255, 255, 255,.3)",
                                                                  Total.Points == -7 ~ "#4c92d8",
                                                                   Total.Points == -10 ~ "rgb(255, 255, 255,.3)",
                                                                   Total.Points == -5 ~ "rgb(255, 255, 255,.3)",
                                                                   Total.Points == -0 ~ "rgb(194, 203, 49,.1)",
                                                                   Total.Points == 2 ~"rgb(244, 158, 74,.1)",
                                                                   Total.Points == 4 ~ "rgb(194, 203, 49,.1)",
                                                                   Total.Points == 5 ~ "rgb(194, 203, 49,.1)",
                                                                   Total.Points == 6 ~ "rgb(244, 158, 74,.1)",
                                                                   Total.Points == 7 ~ "rgb(244, 158, 74,.1)",
                                                                   Total.Points == 10 ~ "rgb(194, 203, 49)",
                                                                   Total.Points == 12 ~ "rgb(244, 158, 74)",
                                                                   Total.Points == 14 ~ "rgb(194, 203, 49)",
                                                                   Total.Points == 15 ~ "rgb(194, 203, 49)",
                                                                   Total.Points == 16 ~ "rgb(244, 158, 74)",
                                                                   Total.Points == 17 ~ "rgb(244, 158, 74)" ))


###Make the palette Color bins (not using)----
#bins.countries <- c(0, 2, 4, 5, 6, 7, 10, 11, 14, 15, 16, 17, Inf)
#pal.countries <- colorBin("YlGnBu", domain = countries@data$NAPs.by.country.type, bins = bins.countries)




##Make the leaflet object, set the zoom level and view and Base Map ----
NAPmap <- leaflet(countries) %>%
  setView(0, 1, 2.5) %>%
  addProviderTiles(providers$CartoDB.Positron)



##Change NA to blanks for the written pop up ----
countries@data$LLDC.SIDS[is.na(countries@data$LLDC.SIDS)] <- ""
countries@data$NAP.language.1[is.na(countries@data$NAP.language.1)] <- "None"
countries@data$NAP.language.2[is.na(countries@data$NAP.language.2)] <- ""
countries@data$SectoralNAP1.Title.1[is.na(countries@data$SectoralNAP1.Title.1)] <- "None"
countries@data$SectoralNAP1.language.1[is.na(countries@data$SectoralNAP1.language.1)] <- "None"
countries@data$SectoralNAP1.language.2[is.na(countries@data$SectoralNAP1.language.2)] <- "None"
countries@data$SectoralNAP2.Title.1[is.na(countries@data$SectoralNAP2.Title.1)] <- "None"
countries@data$SectoralNAP2.language.1[is.na(countries@data$SectoralNAP2.language.1)] <- "None"
countries@data$SectoralNAP2.language.2[is.na(countries@data$SectoralNAP2.language.2)] <- "None"
countries@data$SectoralNAP3.Title.1[is.na(countries@data$SectoralNAP1.Title.1)] <- "None"
countries@data$SectoralNAP3.language.1[is.na(countries@data$SectoralNAP3.language.1)] <- "None"
countries@data$SectoralNAP3.language.2[is.na(countries@data$SectoralNAP3.language.2)] <- "None"




##Make the strings in HTML for Countries pop up label ----
labels.countries <- sprintf(
  '<h3>%s</h3><h4>%s</br>%s | %s</h4>
  <hr><strong>NAP Submitted:</strong> %s on %s<br/><strong>Link to NAP:</strong> <a href="%s">%s</a>, <a href="%s">%s</a> 
  <hr><strong> Sectoral NAPs:</strong> 
  <ul> <li>%s available in <a href="%s">%s</a> and <a href="%s">%s</a> </li> <li> %s available in <a href="%s">%s</a></li> </ul> ',
  countries@data$ADMIN,countries@data$Region,
  countries@data$Type, countries@data$LLDC.SIDS, 
  countries@data$NAP, countries@data$Date.Posted, 
  countries@data$NAP.Link.1, countries@data$NAP.language.1, countries@data$NAP.Link.2, countries@data$NAP.language.2, 
  countries@data$SectoralNAP1.Title.1 , countries@data$SectoralNAP1.Link.1, countries@data$SectoralNAP1.language.1, countries@data$SectoralNAP1.Link.2, countries@data$SectoralNAP1.language.2, 
  countries@data$SectoralNAP2.Title.1 , countries@data$SectoralNAP2.Link.1, countries@data$SectoralNAP2.language.1
) %>% lapply(htmltools::HTML)


#Declare text for map legend ----
legend.country.type <- c("NA / Annex 1 Country, Non UN Member (SIDS included)",
                         "Annex 1 country or developed country with NAP",
                         # "Other developing country & LLDC No NAP",
                         # "Other developing country & SIDS No NAP",
                         # "LDC No NAP",
                         # "LDC & LLDC No NAP",
                         # "LDC & SIDS No NAP",
                         "Other developing country with NAP",
                         # "Other developing country & LLDC with a NAP",
                         # "Other developing country & SIDS with a NAP",
                         "LDC with NAP"
                         # "LDC & LLDC with a NAP",
                         # "LDC & SIDS with a NAP"
                         )



##Make the choropleth map ----
NAPmaplayer <- NAPmap %>% addPolygons(fillColor = countries@data$color.code,
                       weight = 1.3,
                       opacity = 1,
                       color = "white",
                       dashArray = "3",
                       fillOpacity = 0.8,
                       highlightOptions = highlightOptions(
                         weight = 3,
                         color = "white",
                         dashArray = "",
                         fillOpacity = 1,
                         bringToFront = TRUE),
                       popup = labels.countries,
                       popupOptions = popupOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "text-size" = "15px"),
                         textsize = "15px",
                         direction = "auto"))  %>%
  addLegend(position = "bottomright",
            colors = c("rgb(255, 255, 255,.15)", 
                       "#4c92d8", 
                       # "rgb(128, 193, 185,.15)", 
                       # "rgb(76, 144, 215,.15)",
                       # "rgb(244, 158, 74,.15)",
                       # "rgb(243, 167, 167,.15)", 
                       # "rgb(233, 81, 82,.15)", 
                       "rgb(194, 203, 49)",
                       # "rgb(128, 193, 185)",
                       # "rgb(76, 144, 215)",
                       "rgb(244, 158, 74)"
                       # "rgb(243, 167, 167)",
                       # "rgb(233, 81, 82)"
                       ),
            labels = legend.country.type,
            values = countries@data$NAPs.by.country.type,
            title = "NAPs by Country Type")

NAPmaplayer

##Make R tibbles ----
NAPssubmitted <- countries@data %>% select (ADMIN, Region, Type, LLDC.SIDS, NAP, Date.Posted, NAP.language.1, NAP.Link.1, NAP.language.2, NAP.Link.2) %>% 
  as_tibble() %>%
  rename("Country" = ADMIN,
         "LLDC or SIDS" = LLDC.SIDS,
         "Date Posted" = Date.Posted,
         "NAP Language 1" = NAP.language.1 ,
         "NAP Link 1" = NAP.Link.1 ,
         "NAP Language 2" = NAP.language.2,
         "NAP Link 2" = NAP.Link.2) %>%
  filter(NAP == "Yes")


SectoralNAPs <- countries@data %>% select (ADMIN, Region, Type, LLDC.SIDS, SectoralNAP1.Date, 
                                           SectoralNAP1.Title.1 , SectoralNAP1.language.1, SectoralNAP1.Link.1, 
                                           SectoralNAP1.language.2, SectoralNAP1.Link.2, 
                                           SectoralNAP2.Title.1, SectoralNAP2.language.1, SectoralNAP2.Link.1, 
                                           SectoralNAP2.language.2, SectoralNAP2.Link.2, 
                                           SectoralNAP3.Title.1, SectoralNAP3.language.1, SectoralNAP3.Link.1,
                                           SectoralNAP3.language.2, SectoralNAP3.Link.2,
                                           SectoralNAP4.Title.1, SectoralNAP4.language.1, SectoralNAP4.Link.1,
                                           SectoralNAP5.Title.1, SectoralNAP5.language.1, SectoralNAP5.Link.1) %>% 
  as_tibble() %>%
  filter(!is.na(SectoralNAP1.Date)) %>%
  rename("Country" = ADMIN,
         "LLDC or SIDS" = LLDC.SIDS,
         "Date Posted" = SectoralNAP1.Date,
         "1 Sectoral NAP Title " = SectoralNAP1.Title.1,
         "1 Sectoral NAP Language 1" = SectoralNAP1.language.1,
         "1 Sectoral NAP Link 1" = SectoralNAP1.Link.1,
         "1 Sectoral NAP Language 2" = SectoralNAP1.language.2,
         "1 Sectoral NAP Link 2" = SectoralNAP1.Link.2,
         "2 Sectoral NAP Title " = SectoralNAP2.Title.1,
         "2 Sectoral NAP Language 1" = SectoralNAP2.language.1,
         "2 Sectoral NAP Link 1" = SectoralNAP2.Link.1,
         "2 Sectoral NAP Language 2" = SectoralNAP2.language.2,
         "2 Sectoral NAP Link 2" = SectoralNAP2.Link.2,
         "3 Sectoral NAP Title " = SectoralNAP3.Title.1,
         "3 Sectoral NAP Language 1" = SectoralNAP3.language.1,
         "3 Sectoral NAP Link 1" = SectoralNAP3.Link.1,
         "3 Sectoral NAP Language 2" = SectoralNAP3.language.2,
         "3 Sectoral NAP Link 2" = SectoralNAP3.Link.2,
         "4 Sectoral NAP Title " = SectoralNAP4.Title.1,
         "4 Sectoral NAP Language 1" = SectoralNAP4.language.1,
         "4 Sectoral NAP Link 1" = SectoralNAP4.Link.1,
         "5 Sectoral NAP Title " = SectoralNAP5.Title.1,
         "5 Sectoral NAP Language 1" = SectoralNAP5.language.1,
         "5 Sectoral NAP Link 1" = SectoralNAP5.Link.1)

##Make html links for NAPs ---
NAP.language1 <- sprintf('<a href="%s" target= "_blank">%s</a>',
                         NAPssubmitted$`NAP Link 1`, NAPssubmitted$`NAP Language 1`)
NAP.language2 <- sprintf('<a href="%s" target= "_blank">%s</a>',
                         NAPssubmitted$`NAP Link 2`, NAPssubmitted$`NAP Language 2`)

SN.language1 <-  sprintf('<a href="%s" target= "_blank">%s</a> and <a href="%s">%s</a>',
                         SectoralNAPs$`1 Sectoral NAP Language 1`, SectoralNAPs$`1 Sectoral NAP Link 1`, SectoralNAPs$`1 Sectoral NAP Language 2`, SectoralNAPs$`1 Sectoral NAP Link 2`)


##Add Link to table ----
NAPssubmitted <- NAPssubmitted %>% mutate(`NAP Language` = NAP.language1,
                                          `NAP Language 2` = NAP.language2) %>%
  select(1,2,3,4,5,6,11,9)

#Save / End of R script





