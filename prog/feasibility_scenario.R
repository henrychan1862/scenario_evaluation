# This is for getting the  feasibility indicators from scenario data
# Henry Chan, 13.1.2024



# library & source-----------------------
library(data.table)
library(yaml)

source("prog/vetting_function.R")


# CONSTANT and DEFINITION----------------
AR6_R10_SCENARIO_PATH <- "data/AR6_Scenarios_Database_R10_regions_v1.1.csv"
ANNEX3_VETTING_META <- "data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
var_order <- "define/variable_order_feasibility.yaml"


# read in important columns from meta information of annex III---------------------------
# NOTE: read scenarios with cliamte category
annex3Meta <- readxl::read_xlsx(ANNEX3_VETTING_META, sheet = "meta_Ch3vetted_withclimate") |>
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


# read scenario data R10------------------------
# filter out scenarios without climate category
ar6ScenarioR10 <- read_iamc_table(AR6_R10_SCENARIO_PATH)
ar6ScenarioR10 <- ar6ScenarioR10[
    annex3Meta[
        ,
        .(model, scenario)
    ],
    on = .(model, scenario)
][
    region != "R10ROWO"
]


# indicator aggregation--------------------------
## variable loooking for
variable_list <- list(
    "Primary Energy",
    "Final Energy",
    "Final Energy|Electricity",
    "Emissions|CO2|Energy",
    "GDP|MER",
    "GDP|PPP"
)

# NOTE: interpolate only scenarios with more than 1 observations
# if scenario have a different base year than 2010,
# do not interpolate 2010
## check missing period and base year---------------------
lapply(
    variable_list,
    function(var) {
        return(ar6ScenarioR10[
            .(var),
            on = .(variable)
        ][
            .(seq(2000, 2100, 5)),
            on = .(period)
        ] |>
            dcast.data.table(
                model + scenario + region ~ period, value.var = "value"
            ) |>
            _[
                ,
                total_na := rowSums(is.na(.SD)),
                by = .(model, scenario, region)
            ][
                ar6ScenarioR10[
                    .(var),
                    on = .(variable)
                ][
                    .(seq(2000, 2100, 5)),
                    on = .(period)
                ][
                    !is.na(value)
                ][
                    ,
                    .(base_period = min(period)),
                    by = .(model, scenario, region)
                ],
                on = .(model, scenario, region)
            ][
                ,
                .(
                    max_na = max(total_na),
                    max_base_period = max(base_period)
                )
            ])
    }
)

## interpolate variables-----------------------------
# NOTE: compsoe energy emissions and final electricity from sub-category first
variableOrder <- yaml.load_file(var_order)
ar6ScenarioR10 <- compose_from_order(ar6ScenarioR10, variableOrder)
interpolated_scenarios <- lapply(
    variable_list,
    function(var) {
        return (
            ar6ScenarioR10[
                .(var),
                on = .(variable)
            ][
                .(seq(2000, 2100, 5)),
                on = .(period)
            ][
                order(period)
            ][
                ,
                .(
                    # return only desired period
                    unit = first(unit),
                    period = approx(period, value, xout = seq(2010, 2100, 5),
                                    method = "linear", rule = 1)$x, # do not interpolate value outside base year
                    # same for value as well
                    value = approx(period, value, xout = seq(2010, 2100, 5),
                                   method = "linear", rule = 1)$y
                ),
                by = .(model, scenario, region, variable)
            ]
        )
    }
) |>
    rbindlist(use.names = TRUE)

## wranggle to get indicators--------------------
# find the indicator
interpolated_scenarios |>
    dcast.data.table(
        model + scenario + region + period ~ variable, value.var = "value"
    ) |>
    _[
        ,
        `Energy Intensity in MER (MJ/USD2010)` := (`Primary Energy` * 10^12) /
            (`GDP|MER` * 10^9)
    ][
        ,
        `Energy Intensity in PPP (MJ/USD2010)` := (`Primary Energy` * 10^12) /
            (`GDP|PPP` * 10^9)
    ][
        ,
        `Carbon Intensity (g CO2/MJ)` :=  (`Emissions|CO2|Energy` * 10^12) / (`Primary Energy` * 10^12)
    ][
        ,
        `Electrification (%)` :=  `Final Energy|Electricity` / `Final Energy` * 100
    ] |>
    fwrite("output/scenario_indicator.csv")

# find the rate of change
fread("output/scenario_indicator.csv") |>
    _[
        ,
        .(
            model, scenario, region, period,
            `Energy Intensity in MER (MJ/USD2010)`,
            `Carbon Intensity (g CO2/MJ)`,
            `Electrification (%)`
        )
    ][
        order(period)
    ][
        ,
        .(
            period,
            `Energy Intensity Rate of Change (%/yr)` =
                (`Energy Intensity in MER (MJ/USD2010)` - shift(`Energy Intensity in MER (MJ/USD2010)`)) /
                shift(`Energy Intensity in MER (MJ/USD2010)`) * 100 / 5,
            `Carbon Intensity Change (g CO2/MJ/yr)` =
                (`Carbon Intensity (g CO2/MJ)` - shift(`Carbon Intensity (g CO2/MJ)`)) / 5,
            `Electrification Change (%/yr)` =
                (`Electrification (%)` - shift(`Electrification (%)`)) / 5
        ),
        by = model:region
    ][
        ,
        all_na := rowSums(is.na(.SD)),
        .SDcols = is.numeric
    ][
        all_na != 3
    ] |>
    fwrite("output/scenario_indicator_change.csv")

