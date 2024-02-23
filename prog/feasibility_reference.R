# This is for getting the reference value of feasibility indicators from historical data
# Henry Chan, 11.1.2024


# library & source-----------------------
library(data.table)
library(WDI)
library(ggplot2)

source("prog/plot.R")


# CONSTANT and DEFINITION----------------
r10_mapping_iea <- fread("define/r10_region_iea.csv")
variation <- fread("define/name_variation.csv")
tes_path <- "data/iea_1971_2020_tes_all.csv"
indc_path <- "data/iea_1975_2020_indc.csv"
co2_energy_path <- "data/iea_1975_2020_co2_energy_kt.csv"


# indicator aggregation-------------------
## Total Energy---------------------
tes <- fread(
    tes_path,
    skip = 2,
    col.names = c("country", "product", seq(1971, 2020, 1)),
    encoding = "Latin-1"    # deal w/ shitty latin word
) |>
    setnames(tolower) |>
    merge.data.table(
        y = variation, all.x = TRUE, by.x = "country", by.y = "alt_name"    # left join
    ) |>
    _[
        ,
        country := fifelse(
            is.na(use_name), country, use_name
        )
    ] |>
    merge.data.table(
        y = r10_mapping_iea, by = "country"  # inner join
    )

# apply formula to convert into direct-equivalent
tes |>
    melt.data.table(
        id.vars = c("r10_region", "country", "product"),
        measure.vars = seq(1975, 2020, 1) |> as.character(),
        variable.name = "year", value.name = "TES (TJ)"
    ) |>
    _[
        ,
        `TES (TJ)` := as.numeric(`TES (TJ)`)
    ][
        ,
        .(`TES (EJ)` = sum(`TES (TJ)`, na.rm = TRUE) / 10^6),   # convert from TJ to EJ
        by = .(r10_region, product, year)
    ] |>
    dcast.data.table(
        r10_region + year ~ product, value.var = "TES (EJ)"
    ) |>
    _[
        ,
        .(
            # use direct-equivalent method for TES
            r10_region,
            year,
            `TES (EJ)` = Total - Nuclear * 0.667 - Geothermal * 0.9
        )
    ] |>
    fwrite("output/tes_historical_r10.csv")

## Electrification and GDP-----------------------------
elef_gdp <- fread(
    indc_path,
    skip = 2,
    col.names = c("country", "product", seq(1975, 2020, 1)),
    encoding = "Latin-1"    # deal w/ shitty latin word
) |>
    setnames(tolower) |>
    merge.data.table(
        y = variation, all.x = TRUE, by.x = "country", by.y = "alt_name"    # left join
    ) |>
    _[
        ,
        country := fifelse(
            is.na(use_name), country, use_name
        )
    ] |>
    merge.data.table(
        y = r10_mapping_iea, by = "country"  # inner join
    )

# NOTE: 1 TWh = 3.6 PJ
# aggregate into long format
elef_gdp |>
    melt.data.table(
        id.vars = c("r10_region", "country", "product"),
        measure.vars = seq(1975, 2020, 1) |> as.character(),
        variable.name = "year", value.name = "value"
    ) |>
    _[
        ,
        .(value = sum(as.numeric(value), na.rm = TRUE)),
        by = .(r10_region, product, year)
    ] |>
    dcast.data.table(
        r10_region + year ~ product, value.var = "value"
    ) |>
    fwrite("output/elef_gdp_r10.csv")

## CO2 emissions by energy (fuel combustion)-----------------------------
co2_energy <- fread(
    co2_energy_path,
    skip = 2,
    col.names = c("country", seq(1975, 2020, 1)),
    encoding = "Latin-1"    # deal w/ shitty latin word
) |>
    setnames(tolower) |>
    merge.data.table(
        y = variation, all.x = TRUE, by.x = "country", by.y = "alt_name"    # left join
    ) |>
    _[
        ,
        country := fifelse(
            is.na(use_name), country, use_name
        )
    ] |>
    merge.data.table(
        y = r10_mapping_iea, by = "country"  # inner join
    )

# aggregate and export
co2_energy |>
    melt.data.table(
        id.vars = c("r10_region", "country"),
        measure.vars = seq(1975, 2020, 1) |> as.character(),
        variable.name = "year", value.name = "CO2 (kt)"
    ) |>
    _[
        ,
        `CO2 (kt)` := as.numeric(`CO2 (kt)`)
    ][
        ,
        .(`CO2 (Mt)` = sum(`CO2 (kt)`, na.rm = TRUE) / 10^3),   # convert from TJ to EJ
        by = .(r10_region, year)
    ] |>
    fwrite("output/co2_energy_historical_r10.csv")

## GDP deflator-------------------------------
# download USD deflator from world bank, base year is 2015
deflator <- WDI(indicator = "NY.GDP.DEFL.ZS", country = "US") |>
    as.data.table()

# create gdp(USD2010)
deflator_us <- deflator |>
    setnames("NY.GDP.DEFL.ZS", "us_deflator_2015") |>
    _[
        ,
        .(
            year, us_deflator_2015,
            us_deflator_2010 = (us_deflator_2015 / .SD[year == 2010, us_deflator_2015]) * 100
        )
    ]
deflator_us |>
    fwrite("output/us_gdp_deflator.csv")


# indicator preparation--------------------------------------
## calculate electrification rate, energy intensity and carbon intensity---------------
merge.data.table(
    fread("output/elef_gdp_r10.csv"),
    fread("output/us_gdp_deflator.csv"),
    on = "year"
) |>
    _[
        ,
        `GDP (billion USD 2010 prices and PPPs)` :=
            `GDP (billion USD 2015 prices and PPPs)` * us_deflator_2015 / us_deflator_2010,
    ][
        ,
        `GDP (billion USD 2010 prices and exchange rates)` :=
            `GDP (billion USD 2015 prices and exchange rates)` * us_deflator_2015 / us_deflator_2010,
    ][
        ,
        `Electrification (%)` :=  `Electricity consumption (TWh)` * 3.6 / `Total final consumption (TFC) (PJ)` * 100
    ] |>
    merge.data.table(
        fread("output/co2_energy_historical_r10.csv"),
        by = c("year", "r10_region")
    ) |>
    merge.data.table(
        fread("output/tes_historical_r10.csv"),
        by = c("year", "r10_region")
    ) |>
    _[
        ,
        `Energy Intensity in MER (MJ/USD2010)` := (`TES (EJ)` * 10^12) /
            (`GDP (billion USD 2010 prices and exchange rates)` * 10^9)
    ][
        ,
        `Energy Intensity in PPP (MJ/USD2010)` := (`TES (EJ)` * 10^12) /
            (`GDP (billion USD 2010 prices and PPPs)` * 10^9)
    ][
        ,
        `Carbon Intensity (g CO2/MJ)` :=  (`CO2 (Mt)` * 10^12) / (`TES (EJ)` * 10^12)
    ] |>
    fwrite("output/feasibility_indicator.csv")

## calculate the rate of change of indicator (5-year interval, take 5-year avg)------------------
fread("output/feasibility_indicator.csv") |>
    _[
        year %in% seq(1975, 2015, 5)
    ][
        order(r10_region, year),
        .(
            r10_region, year,
            `Energy Intensity in MER (MJ/USD2010)`,
            `Carbon Intensity (g CO2/MJ)`,
            `Electrification (%)`
        )
    ][
        ,
        .(
            year,
            `Energy Intensity Rate of Change (%/yr)` =
                (`Energy Intensity in MER (MJ/USD2010)` - shift(`Energy Intensity in MER (MJ/USD2010)`)) /
                shift(`Energy Intensity in MER (MJ/USD2010)`) * 100 / 5,
            `Carbon Intensity Change (g CO2/MJ/yr)` =
                (`Carbon Intensity (g CO2/MJ)` - shift(`Carbon Intensity (g CO2/MJ)`)) / 5,
            `Electrification Change (%/yr)` =
                (`Electrification (%)` - shift(`Electrification (%)`)) / 5
        ),
        by = .(r10_region)
    ] |>
    na.omit() |>
    fwrite("output/feasibility_indicator_change.csv")