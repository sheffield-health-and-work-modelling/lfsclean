#' Read LFS 2018 longitudinal
#'
#' Reads and performs basic cleaning on the Labour Force Survey five-quarter longitudinal
#' data with a 1st wave that began in calendar year 2018.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_5q_2018 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/LFS/Data/longitudinal/tab/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2018:\n"))

  ###### Read in each quarter
  cat(crayon::green("\tJan-Mar 2018 to Jan-Mar 2019"))
  data.q1 <- data.table::fread(
    paste0(path,"/five_q_jm18_jm19_eul_lgwt17.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tApr-Jun 2018 to Apr-Jun 2019"))
  data.q2 <- data.table::fread(
    paste0(path,"/lgwt18_5q_aj18_aj19_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tJul-Sep 2018 to Jul-Sep 2019"))
  data.q3 <- data.table::fread(
    paste0(path,"/lgwt18_5q_js18_js19_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tOct-Dec 2018 to Oct-Dec 2019"))
  data.q4 <- data.table::fread(
    paste0(path,"/lgwt18_5q_od18_od19_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.q1[, month := 2]
  data.q2[, month := 5]
  data.q3[, month := 8]
  data.q4[, month := 11]

  data.q1[, id := paste0(1:nrow(data.q1),"-1-2018") ]
  data.q2[, id := paste0(1:nrow(data.q2),"-2-2018") ]
  data.q3[, id := paste0(1:nrow(data.q3),"-3-2018") ]
  data.q4[, id := paste0(1:nrow(data.q4),"-4-2018") ]

  data.list <- list(data.q1, data.q2, data.q3, data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters
  for (l in c(1)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    setnames(data,
             c("lgwt17","hiul15d1","hiul15d2","hiul15d3","hiul15d4","hiul15d5"),
             c("lgwt22","hiqul22d1","hiqul22d2","hiqul22d3","hiqul22d4","hiqul22d5"))

    id_weights_vars  <- Hmisc::Cs(id, persid, lgwt22, month)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    #health_vars      <- Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
    #                              illoff1, illoff2, illoff3, illoff4, illoff5)

    #benefit_vars     <- Hmisc::Cs(clims141, clims142, clims143, clims144, clims145,
    #                              benfts1, benfts2, benfts3, benfts4, benfts5,
    #                              ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2018

    clean.data.list[[l]] <- data
  }

  for (l in c(2:4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    setnames(data,
             c("lgwt18","hiul15d1","hiul15d2","hiul15d3","hiul15d4","hiul15d5"),
             c("lgwt22","hiqul22d1","hiqul22d2","hiqul22d3","hiqul22d4","hiqul22d5"))

    id_weights_vars  <- Hmisc::Cs(persid, lgwt22)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    #health_vars      <- Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
    #                              illoff1, illoff2, illoff3, illoff4, illoff5)

    #benefit_vars     <- Hmisc::Cs(clims141, clims142, clims143, clims144, clims145,
    #                              benfts1, benfts2, benfts3, benfts4, benfts5,
    #                              ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2018

    clean.data.list[[l]] <- data
  }

  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],
                clean.data.list[[2]],
                clean.data.list[[3]],
                clean.data.list[[4]], fill=TRUE)

  ### generate missing values for variables not in this year

  data[, etukeul1 := NA]
  data[, etukeul2 := NA]
  data[, etukeul3 := NA]
  data[, etukeul4 := NA]
  data[, etukeul5 := NA]

  data[, govtof21 := NA]
  data[, govtof22 := NA]
  data[, govtof23 := NA]
  data[, govtof24 := NA]
  data[, govtof25 := NA]

  data[, disea1 := NA]
  data[, disea2 := NA]
  data[, disea3 := NA]
  data[, disea4 := NA]
  data[, disea5 := NA]

  data[, illoff1 := NA]
  data[, illoff2 := NA]
  data[, illoff3 := NA]
  data[, illoff4 := NA]
  data[, illoff5 := NA]

  data[, clims141 := NA]
  data[, clims142 := NA]
  data[, clims143 := NA]
  data[, clims144 := NA]
  data[, clims145 := NA]

  data[, benfts1 := NA]
  data[, benfts2 := NA]
  data[, benfts3 := NA]
  data[, benfts4 := NA]
  data[, benfts5 := NA]

  data[, ooben1 := NA]
  data[, ooben2 := NA]
  data[, ooben3 := NA]
  data[, ooben4 := NA]
  data[, ooben5 := NA]

  return(data)
}
