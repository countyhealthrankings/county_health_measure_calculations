#################################################################################################
# v004- Primary Care Physicians -- NEW VERSION
# Author: KG
# This file is a copy of v004_PCPs.R (lines 1-223 unchanged) with an add-on at the end
# (see "# ADD-ON") that applies the new zero-handling and small-count rules in _new columns.
# The original v004_PCPs.R is left as is. This script is for comparison only and writes no files.
#Description: Ratio of population to primary care physicians.
#Data Source: Area Health Resource File/American Medical Association
#Data Download Link: https://data.hrsa.gov/data/download
#Numerator: The left side of the ratio represents the county population.
#Denominator: The right side of the ratio represents the primary care physicians corresponding to county population. Primary care physicians include practicing non-federal physicians (M.D.s and D.O.s) under age 75 specializing in general practice medicine, family medicine, internal medicine, and pediatrics.

  #v004 is the Primary Care Physicians measure. This measure was not updated for the CHR&R 2025 Annual Release, 
#but was updated in the post 2025 release rolling CHR&R updates in the Fall of 2025. This code uses the 2024-2025
#release of Area Health Resources File data, which includes provider data through 2023. The output dataset is 
#saved as v004_s2026 in measure_datasets to reflect that it was not calculated for the 2025 annual release. 

#NOTE that this measure was not updated for Connecticut with the 2024-2025 release of the AHRF. The AHRF contains 
#data for the eight former CT counties but there is no recent population data available to use as a denominator for 
#those counties for this measure. Additionally, CHR&R decided not to update CT data for any measures in the rolling 
#2025 data updates that took place after the 2025 Annual Release due to difficulties created by the geography changes.
  
##################################################################################################


library(tidyverse)

##########################NUMERATOR#####################
#bring in AHRF dataset
ahrf_raw <- read_csv("raw_data/AHRF/ahrf2024_Feb2025.csv") %>% 
  select(
    fipscode = fips_st_cnty, 
    st_name, cnty_name, 
    statecode = fips_st, 
    countycode=fips_cnty, 
    v004_numerator=phys_nf_prim_care_pc_exc_rsdt_22) %>% 
#remove Puerto Rico and USVI and guam. Leave in CT for now for national value.
  filter(!statecode %in% c("66","72","78")) #this leaves 3,158

#bring in official list of chrr counties with old CT fips to make sure there aren't other discrepancies
library(haven)
chrr_fips <-  read_sas("inputs/county_fips_with_ct_old.sas7bdat")

county_check <- full_join(ahrf_raw, chrr_fips, by ="fipscode")

#Counties below in AK and VA have missing values in the pcp dataset AND aren't listed in our master list of counties. 
##These are counties that no longer exist due to name changes or being combined with other counties. See the NOTE at the bottom of the 
#AHRF 2023-2024 Technical DocumentationCSV_Feb2025 and this webpage from the Census 
#for more details: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html#list-tab-957819518

#Delete the old/incorrect counties (but retain CT counties with missing data).

ahrf_correct_counties <- county_check %>% 
  filter(!fipscode %in% c("02201", "02232", "02261", "02280", "51515", "51560"))

#calcuate state values by summing county values
state_values <- ahrf_correct_counties %>% 
  group_by(statecode.x) %>%
  summarise(
    v004_numerator = sum(v004_numerator, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
#set CT to missing because the state value is not updated for 2026  
  mutate(
    v004_numerator = if_else(statecode.x=="09", NA_real_, v004_numerator)
  )

#calculate national value by summing county values
ahrf_correct_counties %>% 
summarise(total = sum(v004_numerator, na.rm = TRUE))
#result was 253,985

state_and_nat <- state_values %>% 
  rename(statecode = statecode.x) %>% 
  mutate(countycode = "000") %>% 
  mutate(fipscode = paste0(statecode, countycode)) %>% 
add_row(
    statecode = "00",
    fipscode = "00000",
    countycode = "000",
    v004_numerator = 253985
  )
 

#clean up, merge, and create final numerator dataset
county_clean <-ahrf_correct_counties %>% 
  select(
    fipscode,
    statecode = statecode.x,
    countycode = countycode.x,
    v004_numerator
  ) %>% 
  mutate(
    v004_numerator = if_else(statecode=="09", NA_real_, v004_numerator)
  )
v004_numerator<- bind_rows(state_and_nat, county_clean)



####################DENOMINATOR##############

county_pop22 <- read_sas("inputs/vintage2022_with_ct_new.sas7bdat") %>% 
  mutate(fipscode = paste0(statecode, countycode)) %>% 
  filter(countycode != "000") %>% 
  select(fipscode, 
         statecode, 
         countycode, 
         v004_denominator = POPESTIMATE2022
         )

#calculate state population from sum of counties

state_pop <- county_pop22 %>% 
group_by(statecode) %>%
  summarise(
    v004_denominator = sum(v004_denominator, na.rm = TRUE),
  .groups = "drop"
  ) %>% 
  mutate(countycode = "000") %>% 
  mutate(fipscode = paste0(statecode, countycode)) %>% 
  #set CT to missing because the state value is not updated for 2026  
  mutate(
    v004_denominator = if_else(statecode=="09", NA_real_, v004_denominator)
  )

#calculate national population by summing county values
county_pop22 %>% 
  summarise(total = sum(v004_denominator, na.rm = TRUE))
#result was 333287557

state_and_nat_pop <- state_pop %>% 
  add_row(
    statecode = "00",
    fipscode = "00000",
    countycode = "000",
    v004_denominator = 333287557
  )

v004_denominator<- bind_rows(state_and_nat_pop, county_pop22) %>% 
#set CT county values to missing now that you calculated national population
  mutate(
  v004_denominator = if_else(statecode=="09", NA_real_, v004_denominator)
)

##########MEASURE CALC########
v004 <- full_join(v004_denominator, v004_numerator, by ="fipscode") %>% 
  select(
    fipscode,
    statecode = statecode.y,
    countycode = countycode.y,
    v004_numerator, v004_denominator
) %>% 
  mutate(v004_cilow = NA) %>% 
  mutate(v004_cihigh = NA) %>% 
  mutate(problem = NA) %>% 
  mutate(v004_rawvalue = v004_numerator/v004_denominator) %>% 
  mutate(v004_rawalternatevalue = v004_denominator/v004_numerator) %>% 
  

#v004_rawalternatevalue is the ratio (this measure is displayed as a ratio on our website).
#If a county has a population greater than 2,000 and 0 primary care providers, the countys v004
#value is set to missing. Because of errors with division by 0 for the rawalternatevalue, if a county pop 
#is less than 2000 and has 0 primary care providers, the rawalternatevalue is missing. On the website, the
#measure value will display as a ratio that looks like denominator:0. */ 
  mutate(
    v004_rawvalue = if_else(
      v004_numerator == 0 & v004_denominator > 2000,
      NA_real_,
      v004_rawvalue
    )
  ) %>% 
  mutate(
    v004_rawalternatevalue = if_else(
      v004_numerator ==0,
      NA_real_,
      v004_rawalternatevalue)) %>% 
#create a flag for CT counties
  mutate(
    v004_flag_ct = if_else(
    statecode =="09",
    "U",
    NA_character_
  ))


#sanity check
v004 %>%
  summarise(
    n = sum(!is.na(v004_rawvalue)),
    mean = mean(v004_rawvalue, na.rm = TRUE),
    sd = sd(v004_rawvalue, na.rm = TRUE),
    min = min(v004_rawvalue, na.rm = TRUE),
    q1 = quantile(v004_rawvalue, 0.25, na.rm = TRUE),
    median = median(v004_rawvalue, na.rm = TRUE),
    q3 = quantile(v004_rawvalue, 0.75, na.rm = TRUE),
    max = max(v004_rawvalue, na.rm = TRUE)
  )

v004 %>%
  summarise(
    n = sum(!is.na(v004_denominator)),
    mean = mean(v004_denominator, na.rm = TRUE),
    sd = sd(v004_denominator, na.rm = TRUE),
    min = min(v004_denominator, na.rm = TRUE),
    q1 = quantile(v004_denominator, 0.25, na.rm = TRUE),
    median = median(v004_denominator, na.rm = TRUE),
    q3 = quantile(v004_denominator, 0.75, na.rm = TRUE),
    max = max(v004_denominator, na.rm = TRUE)
  )

v004 %>%
  summarise(
    n = sum(!is.na(v004_numerator)),
    mean = mean(v004_numerator, na.rm = TRUE),
    sd = sd(v004_numerator, na.rm = TRUE),
    min = min(v004_numerator, na.rm = TRUE),
    q1 = quantile(v004_numerator, 0.25, na.rm = TRUE),
    median = median(v004_numerator, na.rm = TRUE),
    q3 = quantile(v004_numerator, 0.75, na.rm = TRUE),
    max = max(v004_numerator, na.rm = TRUE)
  )

#saving csv -- turned off in this comparison script so it never touches the official output.
#The official file is written only by v004_PCPs.R.
#write.csv(v004, "measure_datasets/v004_r2026.csv",row.names = FALSE)



##################################################################################################
# ADD-ON (Nov 2026): new zero-handling and small-count rules for v004
# See R-Provider-Measures-Handoff.md, sections 2-4. The original calculation above is untouched;
# everything below only ADDS columns with a _new suffix (plus v004_blank_change) to a copy of v004.
#
# Rules applied (handoff decisions A-E):
#   A  two population cutoffs, 5,000 and 10,000 (replace 2,000 for PCPs)
#   B  0 PCPs and population <= 5,000          -> real zero, status "none_reported"
#      0 PCPs and population 5,001 to 10,000   -> real zero, status "none_reported_caution"
#   C  0 PCPs and population >  10,000         -> blank, status "none_reported_verify"
#      (new status rule decided Nov 24, 2026; CT stays "not_calculated", anything else "no_data")
#   D  ratio (rawalternatevalue) for 0 PCPs: NA, with status explaining it (open question Q1)
#   E  1-4 PCPs -> status "fewer_than_5" (value kept, flagged as less reliable)
# Population = v004_denominator (same population the measure itself uses).
# Rates keep the same units as v004_rawvalue (PCPs per person, not per 100,000).
##################################################################################################

#----- switches: set any to its "old" setting to turn that change off --------------------------
v004_cutoff_low_new    <- 5000    # A: none_reported / none_reported_caution boundary
v004_cutoff_new        <- 10000   # A: old rule = 2000
v004_flag_small_counts <- TRUE    # E: old rule = FALSE (no small-count status)
v004_zero_ratio_rule   <- "NA"    # D/Q1: "NA" or "zero" (mental health pattern)
#Q1 DECIDED (Nov 19, 2026): the ratio is NA whenever there are 0 providers, and the status column
#("none_reported", "none_reported_caution" or "none_reported_verify") explains why. The same rule applies to v004, v088, v062 and v131.

#----- checks on the original calculation -------------------------------------------------------
#the national values above are typed in; confirm they equal the sums computed from the data
stopifnot(
  sum(ahrf_correct_counties$v004_numerator, na.rm = TRUE) == 253985,
  sum(county_pop22$v004_denominator, na.rm = TRUE) == 333287557,
  #no county failed to match a state when state values were summed
  !any(is.na(state_values$statecode.x))
)

#----- new columns -------------------------------------------------------------------------------
v004_new <- v004 %>%
  mutate(
    v004_status_new = case_when(
      statecode == "09"                                              ~ "not_calculated", # CT
      v004_numerator == 0 & v004_denominator <= v004_cutoff_low_new  ~ "none_reported",
      v004_numerator == 0 & v004_denominator <= v004_cutoff_new      ~ "none_reported_caution",
      v004_numerator == 0 & v004_denominator >  v004_cutoff_new      ~ "none_reported_verify",
      v004_flag_small_counts & v004_numerator >= 1 &
        v004_numerator < 5                                           ~ "fewer_than_5",
      v004_numerator >= 1                                            ~ "reported",
      TRUE                                                           ~ "no_data"
    ),
    v004_rawvalue_new = if_else(
      v004_status_new == "none_reported_verify",
      NA_real_,
      v004_numerator / v004_denominator
    ),
    v004_rawalternatevalue_new = case_when(
      str_starts(v004_status_new, "none_reported") & v004_zero_ratio_rule == "zero" ~ 0,
      v004_numerator == 0 ~ NA_real_,
      TRUE ~ v004_denominator / v004_numerator
    ),
    v004_blank_change = case_when(
      is.na(v004_rawvalue) & v004_status_new == "none_reported_verify" ~ "blank_stays",
      is.na(v004_rawvalue) & !is.na(v004_rawvalue_new)        ~ "blank_to_zero",
      (is.na(v004_rawvalue) & is.na(v004_rawvalue_new)) |
        (v004_rawvalue == v004_rawvalue_new)                  ~ "same",
      TRUE                                                    ~ "other"
    )
  )

#----- comparison output (handoff section 4), counties only -------------------------------------
#county urbanicity from the 2023 NCHS Urban-Rural Classification Scheme, grouped by CHR&R into CHRR_2023_4
#(NCHS code 1 -> 1, 2 -> 2, 3-4 -> 3, 5-6 -> 4)
urbanicity <- read_sas("inputs/nchs_urbal_rural_code.sas7bdat") %>%
  select(fipscode, CHRR_2023_4) %>%
  mutate(urbanicity = factor(CHRR_2023_4, levels = 1:4,
                             labels = c("1 large urban metro", "2 fringe metro",
                                        "3 small/medium metro", "4 rural")))

v004_new_cty <- v004_new %>%
  filter(countycode != "000") %>%
  left_join(urbanicity, by = "fipscode")   # old CT county fips are not in this file -> NA

#0. switch summary: how many counties each change affects
tibble(
  change = c(paste0("A-C: cutoff ", v004_cutoff_new, " (blank -> none reported)"),
             "A-C: still blank above cutoff (none_reported_verify)",
             "B: zero, population 5,001-10,000 (none_reported_caution)",
             "E: flagged fewer than 5 PCPs",
             paste0("D: zero ratio rule = ", v004_zero_ratio_rule, " (ratio changed)")),
  counties = c(sum(v004_new_cty$v004_blank_change == "blank_to_zero"),
               sum(v004_new_cty$v004_status_new == "none_reported_verify"),
               sum(v004_new_cty$v004_status_new == "none_reported_caution"),
               sum(v004_new_cty$v004_status_new == "fewer_than_5"),
               with(v004_new_cty, sum(!(is.na(v004_rawalternatevalue) & is.na(v004_rawalternatevalue_new)) &
                                        !coalesce(v004_rawalternatevalue == v004_rawalternatevalue_new, FALSE))))
) %>% print()

#1. counties by status
v004_new_cty %>% count(v004_status_new) %>% print()

#2. counties by blank_change
v004_new_cty %>% count(v004_blank_change) %>% print()

#3. the same counts split by urbanicity and by state
v004_new_cty %>% count(urbanicity, v004_status_new) %>%
  pivot_wider(names_from = v004_status_new, values_from = n, values_fill = 0) %>% print()
v004_new_cty %>% count(urbanicity, v004_blank_change) %>%
  pivot_wider(names_from = v004_blank_change, values_from = n, values_fill = 0) %>% print()
v004_new_cty %>% count(statecode, v004_status_new) %>%
  pivot_wider(names_from = v004_status_new, values_from = n, values_fill = 0) %>% print(n = Inf)
v004_new_cty %>% count(statecode, v004_blank_change) %>%
  pivot_wider(names_from = v004_blank_change, values_from = n, values_fill = 0) %>% print(n = Inf)

#4. counties that changed from blank to zero, for spot checks
v004_new_cty %>%
  filter(v004_blank_change == "blank_to_zero") %>%
  select(fipscode, statecode, urbanicity, population = v004_denominator, v004_status_new) %>%
  arrange(statecode, fipscode) %>%
  print(n = Inf)

#5. rows marked "same" must agree on both rate and ratio; count any mismatches (should be 0)
v004_new_cty %>%
  filter(v004_blank_change == "same") %>%
  summarise(
    n_same = n(),
    rawvalue_mismatch = sum(!(is.na(v004_rawvalue) & is.na(v004_rawvalue_new)) &
                              !coalesce(v004_rawvalue == v004_rawvalue_new, FALSE)),
    ratio_mismatch    = sum(!(is.na(v004_rawalternatevalue) & is.na(v004_rawalternatevalue_new)) &
                              !coalesce(v004_rawalternatevalue == v004_rawalternatevalue_new, FALSE))
  ) %>% print()

#also list any "other" rows (should be none)
v004_new_cty %>% filter(v004_blank_change == "other") %>% print()
