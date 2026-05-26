#' Read LFS 2021 longitudinal
#'
#' Reads and performs basic cleaning on the Labour Force Survey five-quarter longitudinal
#' data with a 1st wave that began in calendar year 2021.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_5q_2021 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/LFS/Data/longitudinal/tab/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2021:\n"))

  ###### Read in each quarter
  cat(crayon::green("\tJan-Mar 2021 to Jan-Mar 2022"))
  data.q1 <- data.table::fread(
    paste0(path,"/lgwt22_5q_jm21_jm22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tApr-Jun 2021 to Apr-Jun 2022"))
  data.q2 <- data.table::fread(
    paste0(path,"/lgwt22_5q_aj21_aj22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tJul-Sep 2021 to Jul-Sep 2022"))
  data.q3 <- data.table::fread(
    paste0(path,"/lgwt22_5q_js21_js22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tOct-Dec 2021 to Oct-Dec 2022"))
  data.q4 <- data.table::fread(
    paste0(path,"/lgwt22_5q_od21_od22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.q1[, month := 2]
  data.q2[, month := 5]
  data.q3[, month := 8]
  data.q4[, month := 11]

  data.q1[, id := paste0(1:nrow(data.q1),"-1-2021") ]
  data.q2[, id := paste0(1:nrow(data.q2),"-2-2021") ]
  data.q3[, id := paste0(1:nrow(data.q3),"-3-2021") ]
  data.q4[, id := paste0(1:nrow(data.q4),"-4-2021") ]

  data.list <- list(data.q1, data.q2, data.q3, data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters
  for (l in c(1)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    setnames(data,
             c("hiul15d1","hiul15d2","hiul15d3","hiul15d4"),
             c("hiqul22d1","hiqul22d2","hiqul22d3","hiqul22d4"))

    id_weights_vars  <- Hmisc::Cs(id, persid, lgwt22, month)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5,
                                  etukeul1, etukeul2, etukeul3, etukeul4, etukeul5,
                                  govtof21, govtof22, govtof23, govtof24, govtof25)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    health_vars      <- c(
      Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
                illoff1, illoff2, illoff3, illoff4, illoff5),
      intersect(c("heal011", "heal021", "heal031",
                  "heal012", "heal022", "heal032",
                  "heal013", "heal023", "heal033",
                  "heal014", "heal024", "heal034",
                  "heal015", "heal025", "heal035"), names(data))
    )

    benefit_vars     <- Hmisc::Cs(ucredit1, ucredit2, ucredit3, ucredit4, ucredit5,
                                  clims141, clims142, clims143, clims144, clims145,
                                  benfts1, benfts2, benfts3, benfts4, benfts5,
                                  ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars, health_vars, benefit_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2021

    clean.data.list[[l]] <- data
  }

  for (l in c(2)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    setnames(data,
             c("hiul15d1","hiul15d2","hiul15d3"),
             c("hiqul22d1","hiqul22d2","hiqul22d3"))

    id_weights_vars  <- Hmisc::Cs(persid, lgwt22)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5,
                                  etukeul1, etukeul2, etukeul3, etukeul4, etukeul5,
                                  govtof21, govtof22, govtof23, govtof24, govtof25)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    health_vars      <- c(
      Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
                illoff1, illoff2, illoff3, illoff4, illoff5),
      intersect(c("heal011", "heal021", "heal031",
                  "heal012", "heal022", "heal032",
                  "heal013", "heal023", "heal033",
                  "heal014", "heal024", "heal034",
                  "heal015", "heal025", "heal035"), names(data))
    )

    benefit_vars     <- Hmisc::Cs(ucredit1, ucredit2, ucredit3, ucredit4, ucredit5,
                                  clims141, clims142, clims143, clims144, clims145,
                                  benfts1, benfts2, benfts3, benfts4, benfts5,
                                  ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars, health_vars, benefit_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2021

    clean.data.list[[l]] <- data
  }

  for (l in c(3)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    setnames(data,
             c("hiul15d1","hiul15d2"),
             c("hiqul22d1","hiqul22d2"))

    id_weights_vars  <- Hmisc::Cs(persid, lgwt22)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5,
                                  etukeul1, etukeul2, etukeul3, etukeul4, etukeul5,
                                  govtof21, govtof22, govtof23, govtof24, govtof25)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    health_vars      <- c(
      Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
                illoff1, illoff2, illoff3, illoff4, illoff5),
      intersect(c("heal011", "heal021", "heal031",
                  "heal012", "heal022", "heal032",
                  "heal013", "heal023", "heal033",
                  "heal014", "heal024", "heal034",
                  "heal015", "heal025", "heal035"), names(data))
    )

    benefit_vars     <- Hmisc::Cs(ucredit1, ucredit2, ucredit3, ucredit4, ucredit5,
                                  clims141, clims142, clims143, clims144, clims145,
                                  benfts1, benfts2, benfts3, benfts4, benfts5,
                                  ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars, health_vars, benefit_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2021

    clean.data.list[[l]] <- data
  }

  for (l in c(4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    data[, hiqul22d1 := NA]

    id_weights_vars  <- Hmisc::Cs(persid, lgwt22)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5,
                                  etukeul1, etukeul2, etukeul3, etukeul4, etukeul5,
                                  govtof21, govtof22, govtof23, govtof24, govtof25)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    health_vars      <- c(
      Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
                illoff1, illoff2, illoff3, illoff4, illoff5),
      intersect(c("heal011", "heal021", "heal031",
                  "heal012", "heal022", "heal032",
                  "heal013", "heal023", "heal033",
                  "heal014", "heal024", "heal034",
                  "heal015", "heal025", "heal035"), names(data))
    )

    benefit_vars     <- Hmisc::Cs(ucredit1, ucredit2, ucredit3, ucredit4, ucredit5,
                                  clims141, clims142, clims143, clims144, clims145,
                                  benfts1, benfts2, benfts3, benfts4, benfts5,
                                  ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars, health_vars, benefit_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2021

    clean.data.list[[l]] <- data
  }

  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],
                clean.data.list[[2]],
                clean.data.list[[3]],
                clean.data.list[[4]], fill=TRUE)

  setDT(data)

  return(data)
}
