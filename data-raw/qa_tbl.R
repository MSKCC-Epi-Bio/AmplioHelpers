
set.seed(1000)
n <- 300

# sim_data lookalike ------------------------------------------------------

qa_tbl <-
  tibble::tibble(
    mrn = abs(rnorm(n, mean = 5000000, sd = 2500000) %>% as.integer()),
    opdate = sample(seq(as.Date('1980/01/01'), as.Date('2021/10/01'), by = "day"), n),
    procname = sample(c("PANCREATICODUODENECTOMY",
                        "Pancreaticoduodenectomy",
                        "Pylorus Preserving Pancreaticoduodenectomy",
                        "Pylorus Preserving Pancreaticoduodenectomy, Distal",
                        "Pancreaticoduodenectomy ",
                        "PYLORUS PRESERVING PANCREATICODUODENECTOMY",
                        "Pylorus preserving pancreaticoduodenectomy",
                        "Choledocho-jejunostomy",
                        "Duodenal Resection",
                        "Pylorus Preserving Pancreaticoduodenectomy, Splene",
                        "Pancreaticoduodenectomy with Distal gastrectomy",
                        "Distal Pancreatectomy",
                        "Pylorus-Preserving Pancreaticoduodenectomy",
                        "Pancreaticoduodeneomy "),
                      size = n,
                      replace = TRUE),
    los = abs(rnorm(n, mean = 50, sd = 25) %>% as.integer()),
    ebl = seq(from = 0, to = 15000, by = 50)[1:n],
    asa = sample(seq(1,5), n, replace = TRUE),
    trt = sample(c("Drug A", "Drug B"), n, replace = TRUE), ####
    marker = rgamma(n, 1, 1) %>% round(digits = 3),
    stage = sample(c("T1", "T2", "T3", "T4"), size = n, replace = TRUE), # %>% factor(),
    grade = sample(c("I", "II", "III"), size = n, replace = TRUE), # %>% factor(),
    # response_prob =
    #   ((trt == "Drug") - 0.2 * as.numeric(stage) - 0.1 * as.numeric(grade) + 0.1 * marker) %>% {
    #     1 / (1 + exp(-.))
    #   },
    # response = runif(n) < response_prob,
    # ttdeath_true =
    #   exp(1 + 0.2 * response +
    #         -0.1 * as.numeric(stage) +
    #         -0.1 * as.numeric(grade) +
    #         rnorm(n, sd = 0.5)) * 12,
    # death = ifelse(ttdeath_true <= 24, 1L, 0L),
    # ttdeath = pmin(ttdeath_true, 24) %>% round(digits = 2)
  ) %>%
  dplyr::mutate(
    # age = ifelse(runif(n) < 0.95, age, NA_real_),
    marker = ifelse(runif(n) < 0.95, marker, NA_real_)
    # response = ifelse(runif(n) < 0.95, response, NA_integer_)
  ) %>%
  dplyr::mutate(
    MRN      = as.character(mrn),
    OPDATE   = as.character(opdate),
    PROCNAME =  sample(c("PANCREATICODUODENECTOMY",
                         "Pancreaticoduodenectomy",
                         "Pylorus Preserving Pancreaticoduodenectomy",
                         "Pylorus Preserving Pancreaticoduodenectomy, Distal",
                         "Pancreaticoduodenectomy ",
                         "PYLORUS PRESERVING PANCREATICODUODENECTOMY",
                         "Pylorus preserving pancreaticoduodenectomy",
                         "Choledocho-jejunostomy",
                         "Duodenal Resection",
                         "Pylorus Preserving Pancreaticoduodenectomy, Splene",
                         "Pancreaticoduodenectomy with Distal gastrectomy",
                         "Distal Pancreatectomy",
                         "Pylorus-Preserving Pancreaticoduodenectomy",
                         "Pancreaticoduodeneomy "),
                       size = n,
                       replace = TRUE),
    LOS      = as.character(los),
    EBL      = as.character(ebl),
    ebl500   = as.numeric(ebl > 500),
    ebl1000  = as.numeric(ebl > 1000),
    ASA      = as.character(asa),
    TRT      = as.character(trt),
    MARKER   = as.character(marker),
    STAGE    = as.character(stage),
    GRADE    = as.character(grade)
  ) %>%
  dplyr::mutate(panc_ca = stringr::str_detect(procname, 'panc'),
                yos = lubridate::year(opdate),
                mos = format(opdate, "%m"))

usethis::use_data(qa_tbl, overwrite = TRUE)



