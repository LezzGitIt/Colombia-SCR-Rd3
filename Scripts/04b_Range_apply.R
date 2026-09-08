## PhD birds in silvopastoral landscapes ##
## Data wrangling 04b -- Apply the curated out-of-range decisions to the observations

## Reads the manually reviewed remove/change list (Spp_remove_change.csv, built from the maps and Excels that 04a_Range_screening.R generates) and applies it to Bird_pcs_all.csv, writing Bird_pcs_dist.csv for 05/06.
## Deterministic and light (tidyverse only) -- run it every pipeline pass; 04a is the heavy geospatial review step and is run only when the range screening needs refreshing.

# Load libraries & data ---------------------------------------------------
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# guess_max = Inf: Recording is sparse (only ~180 non-NA, near the end) so the default type guess misreads it
Bird_pcs_all <- read_csv("Derived/Excels/Bird_pcs/Bird_pcs_all.csv", guess_max = Inf)
Site_covs <- read_csv("Derived/Excels/Site_covs.csv")

# Manually generated list of species to remove / change, from the 04a review
Remove_change <- read_csv("Derived/Excels/Spp_remove_change.csv") %>%
  select(-c(Editor, Observaciones)) %>%
  # One row per affected department
  separate_rows(Departamentos_afectados, sep = ",\\s*")

# Split the decision list ------------------------------------------------
## Department information for each observation, so department-specific rules can match
Bird_pcs_all2 <- Bird_pcs_all %>%
  left_join(Site_covs[, c("Id_survey_no_dc", "Department", "Elev")])

## Separate by 1) remove vs change and 2) department-specific vs global
Remove_dept <- Remove_change %>%
  filter(Recomendacion == "Remove" & !is.na(Departamentos_afectados))
Remove_all <- Remove_change %>% anti_join(Remove_dept) %>%
  filter(Recomendacion == "Remove")
Change_dept <- Remove_change %>%
  filter(Recomendacion == "Change" & !is.na(Departamentos_afectados))
Change_all <- Remove_change %>%
  filter(Recomendacion == "Change" & is.na(Departamentos_afectados))

# Apply removes --------------------------------------------------------
## Around the Santa Marta area Henicorhina leucophrys above 600 m is within range -- keep those rows
row_add <- Bird_pcs_all2 %>%
  filter(Species_ayerbe == "Henicorhina leucophrys" & Department == "Guajira" & Elev > 600)

Bird_pcs_all3 <- Bird_pcs_all2 %>%
  anti_join(
    Remove_dept,
    by = c("Species_ayerbe", "Department" = "Departamentos_afectados")
  ) %>%
  anti_join(Remove_all) %>%
  bind_rows(row_add) %>%
  select(-Elev)

# Apply changes -------------------------------------------------------
## Two joins (department-specific, then global); each adds Species_cambiado, so re-implement and drop the helper columns between them
implement_changes <- function(df){
  df %>% mutate(Species_ayerbe = ifelse(
    !is.na(Species_cambiado), Species_cambiado, Species_ayerbe
  ))
}

Bird_pcs_all4 <- Bird_pcs_all3 %>%
  left_join(
    Change_dept,
    by = c("Species_ayerbe", "Department" = "Departamentos_afectados")
  ) %>%
  implement_changes() %>%
  select(-c(Species_cambiado, Recomendacion)) %>%
  left_join(Change_all) %>%
  implement_changes()

# Examine / confirm --------------------------------------------------
## Removed ~41 observations, ~12 species
nrow(Bird_pcs_all) - nrow(Bird_pcs_all4)
length(unique(Bird_pcs_all$Species_ayerbe)) - length(unique(Bird_pcs_all4$Species_ayerbe))

## Every global-remove species should now be absent (expect 0 rows)
Spp_rm <- Remove_change %>%
  filter(Recomendacion == "Remove" & is.na(Departamentos_afectados)) %>%
  pull(Species_ayerbe)
Bird_pcs_all4 %>% filter(Species_ayerbe %in% Spp_rm)

## Example: Myiarchus apicalis -> ferox in Meta, apicalis removed in Guajira
Bird_pcs_all4 %>% filter(Species_ayerbe == "Myiarchus apicalis") %>% count(Department)

# Export ------------------------------------------------------------------
Bird_pcs_all4 %>%
  select(-c(Species_cambiado, Recomendacion, contains(c("Department", "Departamentos_afectados")))) %>%
  write_csv("Derived/Excels/Bird_pcs/Bird_pcs_dist.csv")
