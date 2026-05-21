#' Clean LFS longitudinal data
#'
#' A function to apply all cleaning processes to the LFS five-quarter longitudinal data to.
#'
#' @param data Data table - the Labour Force Survey (longitudinal) dataset.
#' @param ages Integer vector - the ages - in wave 1 - in single years to retain (defaults to 16 to 89 years).
#' @param years Integer vector - the years in single years to retain.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to year, age and gender).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @param deflator character. Inflation index to use in producing real values. One of "cpih" or "rpi".
#' Default is cpih.
#' @param long logical. If TRUE, present data in long form (one observation per person per quarter), if FALSE (default) present data in wide form (one observation per person)
#' @return Returns a new set of variables
#' @export
lfs_clean_global_5q <- function(data,
                                ages = 16:89,
                                years = 2022,
                                keep_vars = NULL,
                                complete_vars = NULL,
                                deflator = "cpih",
                                long = TRUE
) {

  ############################################################
  ### Do age/years filtering, complete-case selection here ###

  ## fix bug that occurs if age is not in keep_vars

  if (!("age1" %in% names(data)) ) {
    ages = NULL
  }

  ##################################################
  ## Create clean variables

  ## create an employment pattern variable

  data[, empl_sequence := paste0(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5)]
  data[, empl_sequence := stringr::str_replace_all(empl_sequence, c("1" = "E",
                                                                    "2" = "U",
                                                                    "3" = "I",
                                                                    "4" = "I"))]

  ## sex

  data[, sex := factor(sex, levels = 1:2, labels = c("male","female"))]

  ## qualifications

  data[, hiqual1 := factor(hiqul22d1, levels = 1:6, labels = c("degree", "other_he", "alevel", "gcse", "other_qual","no_qual"))]
  data[, hiqual2 := factor(hiqul22d2, levels = 1:6, labels = c("degree", "other_he", "alevel", "gcse", "other_qual","no_qual"))]
  data[, hiqual3 := factor(hiqul22d3, levels = 1:6, labels = c("degree", "other_he", "alevel", "gcse", "other_qual","no_qual"))]
  data[, hiqual4 := factor(hiqul22d4, levels = 1:6, labels = c("degree", "other_he", "alevel", "gcse", "other_qual","no_qual"))]
  data[, hiqual5 := factor(hiqul22d5, levels = 1:6, labels = c("degree", "other_he", "alevel", "gcse", "other_qual","no_qual"))]

  ## ethnicity

  data[!(is.na(etukeul1)), eth2cat1 := ifelse(etukeul1 == 1, "white", "non_white")]
  data[!(is.na(etukeul2)), eth2cat2 := ifelse(etukeul2 == 1, "white", "non_white")]
  data[!(is.na(etukeul3)), eth2cat3 := ifelse(etukeul3 == 1, "white", "non_white")]
  data[!(is.na(etukeul4)), eth2cat4 := ifelse(etukeul4 == 1, "white", "non_white")]
  data[!(is.na(etukeul5)), eth2cat5 := ifelse(etukeul5 == 1, "white", "non_white")]

  ## employment status

  data[, empstat2cat1 := dplyr::case_match(incac051, c(1:4) ~ "employed", c(5:33) ~ "not_employed")]
  data[, empstat2cat2 := dplyr::case_match(incac052, c(1:4) ~ "employed", c(5:33) ~ "not_employed")]
  data[, empstat2cat3 := dplyr::case_match(incac053, c(1:4) ~ "employed", c(5:33) ~ "not_employed")]
  data[, empstat2cat4 := dplyr::case_match(incac054, c(1:4) ~ "employed", c(5:33) ~ "not_employed")]
  data[, empstat2cat5 := dplyr::case_match(incac055, c(1:4) ~ "employed", c(5:33) ~ "not_employed")]

  data[, empstat3cat1 := dplyr::case_match(incac051, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")]
  data[, empstat3cat2 := dplyr::case_match(incac052, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")]
  data[, empstat3cat3 := dplyr::case_match(incac053, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")]
  data[, empstat3cat4 := dplyr::case_match(incac054, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")]
  data[, empstat3cat5 := dplyr::case_match(incac055, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")]

  data[, empstat8cat1 := dplyr::case_match(incac051, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")]
  data[, empstat8cat2 := dplyr::case_match(incac052, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")]
  data[, empstat8cat3 := dplyr::case_match(incac053, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")]
  data[, empstat8cat4 := dplyr::case_match(incac054, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")]
  data[, empstat8cat5 := dplyr::case_match(incac055, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")]

  data[, empstat2cat1 := as.factor(empstat2cat1)]
  data[, empstat2cat2 := as.factor(empstat2cat2)]
  data[, empstat2cat3 := as.factor(empstat2cat3)]
  data[, empstat2cat4 := as.factor(empstat2cat4)]
  data[, empstat2cat5 := as.factor(empstat2cat5)]

  data[, empstat3cat1 := as.factor(empstat3cat1)]
  data[, empstat3cat2 := as.factor(empstat3cat2)]
  data[, empstat3cat3 := as.factor(empstat3cat3)]
  data[, empstat3cat4 := as.factor(empstat3cat4)]
  data[, empstat3cat5 := as.factor(empstat3cat5)]

  data[, empstat8cat1 := as.factor(empstat8cat1)]
  data[, empstat8cat2 := as.factor(empstat8cat2)]
  data[, empstat8cat3 := as.factor(empstat8cat3)]
  data[, empstat8cat4 := as.factor(empstat8cat4)]
  data[, empstat8cat5 := as.factor(empstat8cat5)]

  ## region

  data[, region1 := factor(govtof21, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                     "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))]
  data[, region2 := factor(govtof22, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                     "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))]
  data[, region3 := factor(govtof23, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                     "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))]
  data[, region4 := factor(govtof24, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                     "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))]
  data[, region5 := factor(govtof25, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                     "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))]

  ## disability

  data[, disab1 := factor(disea1, levels = 1:2, labels = c("disabled","not_disabled"))]
  data[, disab2 := factor(disea2, levels = 1:2, labels = c("disabled","not_disabled"))]
  data[, disab3 := factor(disea3, levels = 1:2, labels = c("disabled","not_disabled"))]
  data[, disab4 := factor(disea4, levels = 1:2, labels = c("disabled","not_disabled"))]
  data[, disab5 := factor(disea5, levels = 1:2, labels = c("disabled","not_disabled"))]

  ## benefit receipt reason

  data[, benclaim1 := factor(ooben1, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))]
  data[, benclaim2 := factor(ooben2, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))]
  data[, benclaim3 := factor(ooben2, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))]
  data[, benclaim4 := factor(ooben2, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))]
  data[, benclaim5 := factor(ooben2, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))]

  ## usual hours worked per week

  data.table::setnames(data, c("ttushr1","ttushr2","ttushr3","ttushr4","ttushr5"), c("uhours1","uhours2","uhours3","uhours4","uhours5"))

  ## number of sick days

  data.table::setnames(data, c("illoff1","illoff2","illoff3","illoff4","illoff5"), c("numsickdays1","numsickdays2","numsickdays3","numsickdays4","numsickdays5"))

  ## weight

  data.table::setnames(data, "lgwt22", "lgwt")



  #################################
  ### remove variables not needed and retain

  data <- data[, c("id", "persid", "lgwt", "sex", "quarter", "month", "year", "empl_sequence",
                   "age1", "age2", "age3", "age4", "age5",
                   "hiqual1", "hiqual2", "hiqual3", "hiqual4", "hiqual5",
                   "disab1", "disab2", "disab3", "disab4", "disab5",
                   "region1", "region2", "region3", "region4", "region5",
                   "eth2cat1", "eth2cat2", "eth2cat3", "eth2cat4", "eth2cat5",
                   "etukeul1", "etukeul2", "etukeul3", "etukeul4", "etukeul5",
                   "empstat2cat1", "empstat2cat2", "empstat2cat3", "empstat2cat4", "empstat2cat5",
                   "empstat3cat1", "empstat3cat2", "empstat3cat3", "empstat3cat4", "empstat3cat5",
                   "empstat8cat1", "empstat8cat2", "empstat8cat3", "empstat8cat4", "empstat8cat5",
                   "numsickdays1", "numsickdays2", "numsickdays3", "numsickdays4", "numsickdays5",
                   "benclaim1", "benclaim2", "benclaim3", "benclaim4", "benclaim5",
                   "uhours1", "uhours2", "uhours3", "uhours4", "uhours5",
                   "grsswk1",                            "grsswk5")]

  ##################################################################
  ### Match in inflation data and create real-earnings variables


  ## get inflation data for the 1st and 5th wave - the 5th wave is 12 months
  ## after the first so make 2 inflation datasets and for the 2nd one subtract
  ## 1 from the year so that when matched, there is an inflation index for the
  ## 5th quarter of the wave

  infl1 <- lfsclean::inflation[measure == deflator, c("year","month","index")]
  infl5 <- lfsclean::inflation[measure == deflator, c("year","month","index")]

  infl5[, year := year - 1]

  setnames(infl1, "index", "index1")
  setnames(infl5, "index", "index5")

  # merge CPI into the main dataset (NOTE CPIH IS MONTHLY BUT LONGITUDINAL DATA
  # DOES NOT HAVE INTERVIEW DATE - EACH INDIVIDUAL IS ALLOCATED THE MID-MONTH OF THE QUARTER)
  data <- merge(data, infl1, by = c("month","year"), all.x = TRUE)
  data <- merge(data, infl5, by = c("month","year"), all.x = TRUE)

  # convert weekly earnings from nominal to real
  data[, weekly_earnings1 := grsswk1*(100/index1) ]
  data[, weekly_earnings5 := grsswk5*(100/index1) ]

  data[, weekly_earnings_nom1 := grsswk1 ]
  data[, weekly_earnings_nom5 := grsswk5 ]

  # calculate hourly wages for both usual and actual hours worked.
  data[, uwage1 := weekly_earnings1/uhours1 ]
  data[, uwage5 := weekly_earnings5/uhours5 ]

  data[, uwage_nom1 := weekly_earnings_nom1/uhours1 ]
  data[, uwage_nom5 := weekly_earnings_nom5/uhours5 ]

  # clean infinite values
  data[is.infinite(uwage1), uwage1 := NA ]
  data[is.infinite(uwage5), uwage5 := NA ]

  data[is.infinite(uwage_nom1), uwage_nom1 := NA ]
  data[is.infinite(uwage_nom5), uwage_nom5 := NA ]

  data[, c("grsswk1","grsswk5") := NULL]

  ######################
  ### Data filtering ###

  final_data <- lfsclean::select_data(
    data = data,
    ages = ages,
    years = years,
    keep_vars = keep_vars,
    complete_vars = complete_vars,
    longitudinal = TRUE
  )

  return(final_data)
}
