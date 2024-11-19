#Load relevant libraries
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(assertable)
library(purrr)
library(readr)
library(stringr)

`%nin%` = Negate(`%in%`)

# #removals
marketing_list_nc <- read_excel("~/Contacts/Exclusion and Salesforce List/6-27-2023-emails_with_INDG_master_domain_exclusions.xlsx") %>% select(Email = Email)%>%
 mutate(
 Email = str_trim(Email)
 )

req_removal <- read_excel("~/Contacts/Exclusion and Salesforce List/Request to be removed list.xlsx")%>% select(Email = Email)%>%
   mutate(
    Email = str_trim(Email)
  )
# 
# 
# # Salesforce In-house and Law firms
# 
in_house_salesforce<- read.csv("~/Contacts/Exclusion and Salesforce List/salesforce_In-House_contacts_NEEDS CLEANING AND DE-DUPING AGAINST EXISTING QUALTRICS LISTS_10.15.2024.csv") %>%
   select(Email, 'FirstName' = "First.Name", 'LastName' = "Last.Name") %>%
  mutate(
    Email = tolower(str_trim(Email))
  )
# 
law_firm_salesforce<- read.csv("~/Contacts/Exclusion and Salesforce List/salesforce_Law Firm_contacts_NEEDS CLEANING AND DE-DUPING AGAINST EXISTING LEOPARD LISTS_10.15.2024.csv") %>%
   select(Email, 'FirstName' = "First.Name", 'LastName' = "Last.Name") %>%
   mutate(
    Email = str_trim(Email)
  )

# Qualtrics In-house
qc_file_names_in <- list.files("~/Contacts/In-house/In-House-July 24", pattern="*.csv")
qc_full_paths_in <- file.path("~/Contacts/In-house/In-House-July 24", qc_file_names_in)
qc_all_in <- do.call("rbind", lapply(qc_full_paths_in, FUN = function(files){read.csv(files)})) %>%
  select(FirstName, LastName, Email, Unsubscribed) %>%
  mutate(
    Email = tolower(str_trim(Email))
  )

# Qualtrics Leopard
qc_file_names_lep <- list.files("~/Contacts/Leopard/Leopard - July 24", pattern="*.csv")
qc_full_paths_lep <- file.path("~/Contacts/Leopard/Leopard - July 24", qc_file_names_lep)
qc_all_lep <- do.call("rbind", lapply(qc_full_paths_lep, FUN = function(files){read.csv(files)})) %>%
  select(FirstName, LastName, Email, Unsubscribed) %>%
  mutate(
    Email = tolower(str_trim(Email))
  )


# Clean In-House
clean_inhouse <- qc_all_in %>%
  filter(Unsubscribed == 0) %>%
  select(-Unsubscribed) %>%
  filter(!duplicated(Email)) %>%
  filter(Email != '') 


# Clean Leopard
clean_lep <- qc_all_lep %>%
  filter(Unsubscribed == 0) %>%
  select(-Unsubscribed) %>%
  filter(Email %nin% clean_inhouse$Email) %>%
  filter(!duplicated(Email)) %>%
  filter(Email != '')

#  Leopard from Dbeaver

leopard_file_names <- list.files(path = "~/Contacts/Leopard/Leopard - July 24", pattern="*.csv")
full_paths <- file.path("~/Contacts/Leopard/Leopard - July 24", leopard_file_names)
# 
# 
all_files <- do.call("bind_rows", lapply(full_paths, FUN = function(files){read.csv(files) %>%
     select(FirstName,LastName,Email) %>%
    dplyr::rename(FirstName = FirstName, LastName = LastName, Email = Email)
}))
# 
# # clean leopard
new_lep <- all_files %>%
  rbind(law_firm_salesforce)%>%
  filter(!duplicated(Email)) %>%
  filter(Email %nin% in_house_salesforce$Email) %>%
  filter(Email %nin% qc_all_in$Email) %>%
  filter(Email %nin% qc_all_lep$Email) %>%
  filter(Email %nin% marketing_list_nc$Email) %>%
  filter(Email %nin% req_removal$Email) %>%
  filter(Email != "")
# 
# # Clean In-house 
# 
new_inhouse <- in_house_salesforce %>%
  filter(!duplicated(Email)) %>%
  filter(Email %nin% qc_all_lep$Email ) %>%
  filter(Email %nin% qc_all_in$Email ) %>%
  filter(Email %nin% law_firm_salesforce$Email) %>%
  filter(Email %nin% marketing_list_nc$Email) %>%
  filter(Email %nin% req_removal$Email) %>%
  filter(Email != "")
# 
# 
# 
clean_lep <- qc_all_lep %>%
  filter(Unsubscribed == 0) %>%
  filter(Email %nin% in_house_salesforce$Email) %>%
  filter(Email %nin% req_removal$Email) %>%
  filter(Email %nin% marketing_list_nc$Email) %>%
  select(-Unsubscribed) %>%
  rbind(new_lep) %>%
  filter(!duplicated(Email))
# 
# 
# 
# 
clean_inhouse <- qc_all_in %>%
  filter(Unsubscribed == 0) %>%
  filter(Email %nin% marketing_list_nc$Email) %>%
  filter(Email %nin% req_removal$Email) %>%
  select(-Unsubscribed) %>%
  rbind(new_inhouse) %>%
  filter(!duplicated(Email)) %>%
  filter(Email %nin% clean_lep$Email)


# Split Leopard
SPLITS <- 12
tmp <- sort(rep(seq_len(SPLITS), length.out = nrow(clean_lep)))
purrr::iwalk(split(clean_lep, tmp), ~write_csv(.x, str_c(str_c(getwd(), "/Contacts/Leopard", .y, ".CSV"))))


# Split In House
SPLITS_in <- 4
tmp_in <- sort(rep(seq_len(SPLITS_in), length.out = nrow(clean_inhouse)))
purrr::iwalk(split(clean_inhouse, tmp_in), ~write_csv(.x, str_c(str_c(getwd(), "/Contacts/In-house", .y, ".CSV"))))



# Law School Lists - From T. Stedman (Used Current Students)
law_students<- read_xlsx("~/Contacts/Law School Lists/Current Students 11-12-24.xlsx") %>%
  select(Email, 'FirstName' = "First Name", 'LastName' = "Last Name") %>%
  mutate(
    Email = str_trim(Email)
  )

#Clean and filter Law students from other lists
new_law_students <- law_students %>%
  filter(!duplicated(Email)) %>%
  filter(Email %nin% qc_all_lep$Email ) %>%
  filter(Email %nin% qc_all_in$Email ) %>%
  filter(Email %nin% law_firm_salesforce$Email) %>%
  filter(Email %nin% marketing_list_nc$Email) %>%
  filter(Email %nin% req_removal$Email) %>%
  filter(Email != "")

# Split New Law Students
SPLITS_in <- 2
tmp_in <- sort(rep(seq_len(SPLITS_in), length.out = nrow(new_law_students)))
purrr::iwalk(split(clean_inhouse, tmp_in), ~write_csv(.x, str_c(str_c(getwd(), "/Contacts/Law School Lists", .y, ".CSV"))))

# Create Law School Faculty List - From T. Stedman (Used Current Students)
law_school<- read_xlsx("~/Contacts/Law School Lists/current nonstudents 11-12-24.xlsx") #Read in Data
law_school_faculty <- law_school %>% filter(Role != 'Dean/Administrator') #Remove "Dean/Administrator"

law_school_faculty <- law_school_faculty%>%
  select(Email, 'FirstName' = "First Name", 'LastName' = "Last Name") %>%
  mutate(
    Email = str_trim(Email)
  ) #Select email, first and last name

#Clean and filter Law students from other lists
new_law_school_faculty <- law_school_faculty %>%
  filter(!duplicated(Email)) %>%
  filter(Email %nin% qc_all_lep$Email ) %>%
  filter(Email %nin% qc_all_in$Email ) %>%
  filter(Email %nin% law_firm_salesforce$Email) %>%
  filter(Email %nin% marketing_list_nc$Email) %>%
  filter(Email %nin% new_law_students$Email)
  filter(Email %nin% req_removal$Email) %>%
  filter(Email != "")
  
SPLITS_in <- 1
tmp_in <- sort(rep(seq_len(SPLITS_in), length.out = nrow(new_law_school_faculty)))
purrr::iwalk(split(clean_inhouse, tmp_in), ~write_csv(.x, str_c(str_c(getwd(), "/Contacts/Law School Lists", .y, ".CSV"))))
