# ------------------------------------------------------------------------------
#
# let´s load some packages
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)
#
# data : luxembourg housing
#
# url points to an excel file hosted on github
url <- "https://is.gd/1vvBAc"
raw_data <- tempfile(fileext = ".xlsx")
download.file(url, raw_data, method = "auto", mode = "wb")
#download.file(url, "./data/ch3.xlsx", method = "auto", mode = "wb")

# saving sheet names in a variable
sheets <- excel_sheets(raw_data)

# function for reading in data, set sheetname in column year
read_clean <- function(..., sheet) {
  read_excel(..., sheet = sheet) |> 
    mutate(year = sheet)
}

# read all sheets in a dataframe and clean names, skip 10 lines
# map functions transform their input by 
# applying a function to each element of a list
raw_data <- map(
  sheets,
  ~read_clean(raw_data, skip = 10, sheet = .)) |> 
  bind_rows() |> 
  clean_names()

# clean
raw_data <- raw_data |>
  rename(
    locality = commune,
    n_offers = nombre_doffres,
    average_price_nominal_euros = prix_moyen_annonce_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
  ) |>
  mutate(locality = str_trim(locality)) |>
  select(year, locality, n_offers, starts_with("average"))

# naming is not consistent
# grepl returns a logical vector
raw_data |>
  filter(grepl("Luxembourg", locality)) |>  
  count(locality)
raw_data |>
  filter(grepl("P.tange", locality)) |>
  count(locality)

# correct both issues
raw_data <- raw_data |>
  mutate(
    locality = ifelse(grepl("Luxembourg-Ville", locality),
                      "Luxembourg",
                      locality),
    locality = ifelse(grepl("P.tange", locality),
                      "Pétange",
                      locality)
  ) |>
  mutate(across(starts_with("average"),
                as.numeric))

# more cleaning
# delete rows with source in location
raw_data <- raw_data |>
  filter(!grepl("Source", locality))

# new data: only communes:
commune_level_data <- raw_data |>
  filter(!grepl("nationale|offres", locality),
         !is.na(locality))

# country data - average prices:
country_level <- raw_data |>
  filter(grepl("nationale", locality)) |>
  select(-n_offers)

# country data - filter totals, offers per year
offers_country <- raw_data |>
  filter(grepl("Total d.offres", locality)) |>
  select(year, n_offers)

# combine country data
country_level_data <- full_join(country_level, offers_country) |>
  select(year, locality, n_offers, everything()) |> # only for sorting
  mutate(locality = "Grand-Duchy of Luxembourg")

# scrape a list of all luxembourg communes
current_communes <- "https://is.gd/lux_communes" |>
  rvest::read_html() |>   
  rvest::html_table() |> # returns list of html-tables
  purrr::pluck(2) |> # keep the second table
  janitor::clean_names() |>
  dplyr::filter(name_2 != "Name") |>
  dplyr::rename(commune = name_2) |>
  dplyr::mutate(commune = stringr::str_remove(commune, " .$"))
current_communes

# let´s see if we have all communes in our data
# dplyr::setdiff(x, y) finds all rows in x that aren't in y
setdiff(unique(commune_level_data$locality), current_communes$commune)

# we need former communes
former_communes <- "https://is.gd/lux_former_communes" |>
  rvest::read_html() |>
  rvest::html_table() |>
  purrr::pluck(3) |>
  janitor::clean_names() |>
  dplyr::filter(year_dissolved > 2009)
former_communes

# combine list of former and current communes
communes <- unique(c(former_communes$name,
                     current_communes$commune))

# we need to rename some communes
# Different spelling of these communes between wikipedia and the data
communes[which(communes == "Clemency")] <- "Clémency"
communes[which(communes == "Redange")] <- "Redange-sur-Attert"
communes[which(communes == "Erpeldange-sur-Sûre")] <- "Erpeldange"
communes[which(communes == "Luxembourg City")] <- "Luxembourg"
communes[which(communes == "Käerjeng")] <- "Kaerjeng"
communes[which(communes == "Petange")] <- "Pétange"

# test again
setdiff(unique(commune_level_data$locality),
        communes)

# save the data (uncomment if you need to save)
# you may need to create the `datasets` folder first
write.csv(commune_level_data, "datasets/commune_level_data.csv", row.names = TRUE)
write.csv(country_level_data, "datasets/country_level_data.csv", row.names = TRUE)


# final script is also here:
# https://github.com/b-rodrigues/rap4all/blob/master/scripts/save_data.R

# let´s do some analysis
# compute index to year 2010
# script is in github
# https://github.com/b-rodrigues/rap4all/blob/master/scripts/analysis.R

url <- "https://is.gd/7PhUjd"
download.file(url, "save_data.R", method = "auto", mode = "wb")
url <- "https://is.gd/qCJEbi"
download.file(url, "analysis.R", method = "auto", mode = "wb")
