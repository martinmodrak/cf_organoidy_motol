

read_single_data <- function(file, patient, date, verbose = FALSE) {
  if(verbose) {
    cat(paste0("Reading ", file, "\n"))
  }

  #TODO back to using rows 2 - 4, handle overwrite in "04-CF 20200820.xlsx" explicitly

  # single_data <- read_excel(file, range = "B2:YX4", sheet = "Paste results", col_names = c("rowname", paste0("ID",1:(96 * 7)  )))
  # single_data$rowname[3] <- "area"
  # long <- single_data %>% column_to_rownames("rowname") %>% t() %>% as.data.frame()

  single_data <- read_excel(file, range = "A7:H102", sheet = "Paste results",
                            col_names = c("well", paste0("t_", 10 * 0:6)),
                            col_types = rep("numeric", 8), na = c("", "nan"))
  long <- single_data %>%
    pivot_longer(-well, names_to = "time", names_prefix = "t_", values_to = "area") %>%
    mutate(time = as.integer(time), well = as.integer(well))

  long$patient <-  patient
  long$date <- date
  long$filename <- basename(file)

  long
}

read_patient_data <- function(ID, verbose = FALSE) {
  files <- list.files(paste0("data_orig/",ID), pattern = paste0("^",ID, " [0-9]*\\.xlsx$"))
  data_list <- list()
  for(f in files) {
    date_str <- substr(f, start = str_length(ID) + 1, stop = str_length(f) - 4)
    data_list[[f]] <- read_single_data(paste0("data_orig/", ID, "/", f), ID, ymd(date_str), verbose)
  }

  do.call(rbind, data_list)
}

fsk_levels <- 5 * (1/2.5)^(7:0)

mix_mapping <- data.frame(mix = c(as.character(1:8), letters[1:8]), fsk_concentration = rep(fsk_levels, 2)
                          #has_770 = c(rep(FALSE, 8), rep(TRUE, 8))
                          )

well_mapping_2_2 <- data.frame(well = 1:96,
                               mix = c(rep(rep(as.character(1:8), each = 2), 2),
                                       rep(rep(letters[1:8], each = 2), 2),
                                       rep(c("2","4","6","8", "h", "f","d","b"), each = 2),
                                       rep(c("7","8", "g", "h","h","g","8","7"), each = 2)),
                               #has_661 = c(rep(c(rep(FALSE, 16), rep(TRUE, 16)), 2), rep(FALSE, 24), rep(TRUE, 8)),
                               #has_445 = c(rep(16, FALSE)
                               p = c(rep("P1", 32), rep("P2", 32), rep("Other", 32)),
                               replicate = rep(letters[1:2], 48),
                               type = c(rep("TEZ/IVA", 16), rep("ELX/TEZ/IVA", 16), rep("TEZ/IVA", 16), rep("ELX/TEZ/IVA", 16), rep("FskOnly", 16), rep("Other", 16)),
                               origin = c(rep("patient", 64), rep("ref114", 16), rep("ref117", 16))
) %>%
  inner_join(mix_mapping, by = "mix")

well_mapping_1_3 <- well_mapping_2_2 %>%
  mutate(type = if_else(well >= 33 & well <= 48, "ELX/TEZ/IVA", type),
         p = if_else(well >= 33 & well <= 48, "raw_data", p)
         )

well_mapping <- rbind(
  well_mapping_1_3 %>% mutate(mapping = "1_3"),
  well_mapping_2_2 %>% mutate(mapping = "2_2")
)

read_area_data <- function(verbose = FALSE) {
  collected_data_file <- here::here("area_data.csv")
  if(file.exists(collected_data_file)) {
    if(verbose) {
      cat("Preprocessed data exists, reading\n")
    }
    area_data <- readr::read_csv(collected_data_file, col_types = cols(
      well = col_integer(),
      time = col_integer(),
      area = col_double(),
      patient = col_character(),
      date = col_date(format = ""),
      filename = col_character(),
      mapping = col_character(),
      mix = col_character(),
      replicate = col_character(),
      type = col_character(),
      fsk_concentration = col_double()
    ))
  } else {
    all_patients <- list()
    for(ID in list.dirs("data_orig/", full.names = FALSE, recursive = FALSE)) {
      all_patients[[ID]] <- read_patient_data(ID, verbose)
    }

    map_1_3_files <- c("04-CF 20200820.xlsx", "21-CF 20200813.xlsx", "41-CF 20200820.xlsx",
                       "41-CF 20200813.xlsx", "58-CF 20200813.xlsx", "59-CF 20200813.xlsx"
    )

    raw_data <- do.call(rbind, all_patients) %>%
      mutate(mapping = if_else(filename %in% map_1_3_files, "1_3", "2_2")) %>%
      inner_join(well_mapping, by = c("well", "mapping")) %>%
      filter(!is.na(area), area != 0) %>%
      ungroup()

    # Remove some stray wells
    area_data <- raw_data %>% filter(!(well == 48 & patient == "04-CF" & date == ymd("20200820")),
                                    !(well == 64 & patient == "21-CF" & date == ymd("20200918")),
                                    !(well %in% c(40, 45) & patient == "68-CF" & date == ymd("20201021")),
                                    !(well %in% c(17, 48) & filename == "41-CF 20200820.xlsx"),
                                    !(well >= 33 & well <= 72 & filename == "64-CF 20201002.xlsx"),
                                    !(well >= 33 & well <= 64 & filename == "60-CF 20210217.xlsx")
    )

    check_data_integrity(area_data)

    area_data <- area_data %>% select(-origin, -p)

    readr::write_csv(area_data, collected_data_file)
  }

  area_data %>% rebuild_factors()
}

rebuild_factors <- function(data) {
  data %>%
    mutate(
      type = factor(type, levels = c("FskOnly", "TEZ/IVA", "ELX/TEZ/IVA")),
      fsk_label = factor(fsk_concentration, levels = fsk_levels, labels = as.character(round(fsk_levels, 3))),
      fsk_label_ord = factor(fsk_concentration, levels = fsk_levels, labels = as.character(round(fsk_levels, 3)), ordered = TRUE)
    )
}

check_data_integrity <- function(raw_data) {

  missing_timepoints <-  raw_data %>% group_by(patient, date, well) %>% summarise(count = n(), .groups = "drop") %>% filter(count != 7)
  if(nrow(missing_timepoints) > 0) {
    print(missing_timepoints)
    stop("Missing timepoints")
  }

  missing_duplicates <- raw_data %>% group_by(patient, origin, type, date, fsk_concentration, filename) %>% summarise(count = n(), .groups = "drop") %>%
    filter(count != 14,
           # For some, this is indeed the case
           filename != "21-CF 20200918.xlsx" & type != "TEZ/IVA" & fsk_concentration != 0.02048
    )

  if(nrow(missing_duplicates) > 0) {
    print(missing_duplicates)
    stop("Missing duplicates")
  }


  missing_concentrations <- raw_data %>% select(patient, origin, type, date, p,  fsk_concentration) %>% distinct() %>%
    group_by(patient, origin, type, date, p) %>%
    summarise(count = n(), .groups = "drop") %>% filter((origin != "patient" & count != 4) | (origin == "patient"  & count != 8))

  if(nrow(missing_concentrations) > 0) {
    print(missing_concentrations)
    stop("Missing concentrations")
  }
}

compute_derived_quantitites <- function(raw_data) {
  raw_data <- raw_data %>%
    filter(!(well == 16 & filename == "58-CF 20200813.xlsx")) %>% # There is one weird outlier that just moves everything away, because it started with very low area
    arrange(patient, date, well, time) %>%
    group_by(patient, date, well) %>%
    mutate(base_area = area[time == 0],
           area_relative = area/base_area,
           swell = 100 * area / base_area - 100) %>%
    ungroup()

  raw_data
}

remove_few_replicates <- function(raw_data) {
  raw_data <- raw_data %>%
    group_by(patient) %>%
    filter(length(unique(filename)) > 1) %>%
    ungroup()

}

raw_data_to_auc <- function(raw_data) {
  raw_data %>%
    group_by(patient, date, well, filename, mapping) %>%
    summarise(auc = sum((time[2:7] - time[1:6]) * (swell[1:6] + swell[2:7]) / 2), .groups = "drop") %>%
    inner_join(well_mapping, by = c("well", "mapping")) %>%
    ungroup() %>%
    rebuild_factors()
}
