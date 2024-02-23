# This is for getting the trend of corresponding variables from scenarios
# Henry Chan, 1.1.2024


# library & source-----------------------
library(data.table)
library(yaml)
library(ggplot2)

source("prog/vetting_function.R") # use the vetting function


# CONSTANT DEFINITION-----------------------
AR6_SCENARIO_PATH <- "data/AR6_Scenarios_Database_World_v1.1.csv"
AR6_R5_SCENARIO_PATH <- "data/AR6_Scenarios_Database_R5_regions_v1.1.csv"
ANNEX3_VETTING_META <- "data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"


# read in important columns from meta information of annex III---------------------------
annex3Meta <- readxl::read_xlsx(ANNEX3_VETTING_META, sheet = "meta") |>
    as.data.table() |>
    _[
        ,
        .(
            Model, Scenario, Category, Category_name, Category_subset,
            Vetting_future, Vetting_historical, Scenario_scope, Sectoral_scope,
            Category_color_hex, `Time horizon`
        )
    ] |>
    setnames(tolower)


# read R5 scenarios--------------------------------------
ar6ScenarioR5 <- read_iamc_table(AR6_R5_SCENARIO_PATH)
ar6ScenarioR5 <- ar6ScenarioR5[
    annex3Meta[
        vetting_historical %in% c("Pass", "Fail"),
        .(model, scenario)
    ],
    on = .(model, scenario)
][
    region != "R5ROWO"
]


# read world level scenario----------------
ar6Scenario <- read_iamc_table(AR6_SCENARIO_PATH)
ar6Scenario <- ar6Scenario[
    annex3Meta[
        vetting_historical %in% c("Pass", "Fail"),
        .(model, scenario)
    ],
    on = .(model, scenario)
]


# sort out variables and wranggle-----------------
## take from 2010 to 2100 with 10-year interval-----------------------
### R5 regions
referenceYAML <- yaml.load_file("define/trend_r5_2010_2100.yaml") # read vetting reference yaml & set logging
variableOrder <- yaml.load_file("define/variable_order_vetting.yaml")
ar6ScenarioR5 <- compose_from_order(ar6ScenarioR5, variableOrder)
lapply(
    referenceYAML |>
        unlist(recursive = FALSE) |> # unlist to historical & future
        unlist(recursive = FALSE), # unlist to indicator category (e.g. Emissions)
    function(item) {
        cat("------------------------------------------------", sep = "\n")
        cat(paste("Vetting", item$description, "..."), sep = "\n")
        parent_df <- NULL   # default not to use parent variales unless specified
        if ("use_parent" %in% names(item)) {
            parent_df <- ar6ScenarioR5[
                .(item$use_parent),
                on = .(variable)
            ][
                !is.na(value)
            ]
        }
        return(
            ar6ScenarioR5[.(item$region), on = c("region")
            ][.(item$variable), on = c("variable")
            ][.(.get_period(item$period, step = 10)), on = c("period")
            ] |>
                combine_variable(item, parent_df = parent_df) |>
                calc_percent_change(item) |>
                export_csv(item, "output/")
        )
    }
)

### World
referenceYAML <- yaml.load_file("define/trend_world_2010_2100.yaml") # read vetting reference yaml & set logging
ar6Scenario <- compose_from_order(ar6Scenario, variableOrder)
lapply(
    referenceYAML |>
        unlist(recursive = FALSE) |> # unlist to historical & future
        unlist(recursive = FALSE), # unlist to indicator category (e.g. Emissions)
    function(item) {
        cat("------------------------------------------------", sep = "\n")
        cat(paste("Vetting", item$description, "..."), sep = "\n")
        parent_df <- NULL   # default not to use parent variales unless specified
        if ("use_parent" %in% names(item)) {
            parent_df <- ar6Scenario[
                .(item$use_parent),
                on = .(variable)
            ][
                !is.na(value)
            ]
        }
        return(
            ar6Scenario[.(item$region), on = c("region")
            ][.(item$variable), on = c("variable")
            ][.(.get_period(item$period, step = 10)), on = c("period")
            ] |>
                combine_variable(item, parent_df = parent_df) |>
                calc_percent_change(item) |>
                export_csv(item, "output/")
        )
    }
)

## find median and 90% range for each vetting-----------------------
csv_r5 <- list.files(
    path = "output/",
    pattern = "between 2010 to 2100 for R5 regions"
)
csv_r5[!stringr::str_starts(csv_r5, "quantile")]

csv_world <- list.files(
    path = "output/",
    pattern = "between 2010 to 2100 for World"
)
csv_world[!stringr::str_starts(csv_world, "quantile")]

lapply(
    list(csv_r5, csv_world),
    function(csv_files) {
        for (csv_file in csv_files) {
            fread(paste0("output/", csv_file)) |>
                _[
                    !is.na(value)
                ][
                    ,
                    .(
                        count = .N,
                        mean = mean(value),
                        median = median(value),
                        lower_90 = quantile(value, 0.05),
                        upper_90 = quantile(value, 0.95)
                    ),
                    by = .(region, period)
                ] |>
                fwrite(paste0("output/quantile ", csv_file))
        }
    }
)
