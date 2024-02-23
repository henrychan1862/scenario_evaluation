# This is for getting the reference value from historical data
# Henry Chan, 14.12.2023


# library & source-----------------------
library(data.table)
library(yaml)


# region mapping-------------------------
countries <- fread("define/wiki_iso.csv")
regions <- fread("define/r5_region.csv")
regions_iea <- fread("define/r5_region_iea.csv")
variation <- fread("define/name_variation.csv")

# create mapping
r5_mapping <- merge.data.table(
    x = countries, y = regions, all.y = TRUE
)
r5_mapping_iea <- merge.data.table(
    x = countries, y = regions_iea, all.y = TRUE
)

# check how many countries in a region
r5_mapping[, .N, by = .(r5_region)]
r5_mapping_iea[, .N, by = .(r5_region)]


# IEA---------------------------------
## primary energy-------------------------------
tes <- fread(
    "data/iea_1971_2020_tes_all.csv",
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
        y = r5_mapping_iea, by = "country"  # inner join
    )

## aggregate TES-----------------------
tes |>
    melt.data.table(
        id.vars = c("r5_region", "country", "product"), measure.vars = seq(1971, 2020, 1) |> as.character(),
        variable.name = "year", value.name = "TES (TJ)"
    ) |>
    _[
        ,
        `TES (TJ)` := as.numeric(`TES (TJ)`)
    ][
        ,
        .(`TES (EJ)` = sum(`TES (TJ)`, na.rm = TRUE) / 10^6),
        by = .(r5_region, product, year)
    ] |>
    dcast.data.table(
        r5_region + year ~ product, value.var = "TES (EJ)"
    ) |>
    _[
        ,
        .(
            # use direct-equivalent method for TES
            r5_region,
            year,
            `TES (EJ)` = Total - Nuclear * 0.667 - Geothermal * 0.9
        )
    ] |>
    fwrite("output/tes_historical_r5_world.csv")

## electricity----------------------------
elec <- fread(
    "data/iea_1971_2020_elec_all.csv",
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
        y = r5_mapping_iea, by = "country"  # inner join
    )

## aggregate nuclear-----------------------------
elec |>
    _[
        product == "Nuclear"
    ] |>
    melt.data.table(
        id.vars = c("r5_region", "country", "product"), measure.vars = seq(1971, 2020, 1) |> as.character(),
        variable.name = "year", value.name = "elec (GWh)"
    ) |>
    _[
        ,
        `elec (GWh)` := as.numeric(`elec (GWh)`)
    ][
        ,
        .(`elec (EJ)` = sum(`elec (GWh)`, na.rm = TRUE) * 3.6 / 10^6),
        by = .(r5_region, product, year)
    ] |>
    dcast.data.table(
        r5_region + year ~ product, value.var = "elec (EJ)"
    ) |>
    fwrite("output/elec_nuclear_historical_r5_world.csv")

## solar and wind electricity-------------------------
elec |>
    _[
        product != "Nuclear"
    ] |>
    melt.data.table(
        id.vars = c("r5_region", "country", "product"), measure.vars = seq(1971, 2020, 1) |> as.character(),
        variable.name = "year", value.name = "elec (GWh)"
    ) |>
    _[
        ,
        `elec (GWh)` := as.numeric(`elec (GWh)`)
    ][
        order(r5_region, year),
        .(`elec (EJ)` = sum(`elec (GWh)`, na.rm = TRUE) * 3.6 / 10^6),
        by = .(r5_region, year)
    ] |>
    fwrite("output/elec_solar_wind_historical_r5_world.csv")



# EDGAR-------------------------------
# for fossil----------------
## fossil historical trend line (R5 regions)-------------------------
fossil <- readxl::read_xlsx(path = "data/IEA_EDGAR_CO2_1970_2022.xlsx", sheet = "IPCC 2006", skip = 9) |>
    as.data.table() |>
    setnames(tolower) |>
    _[
        ,
        .SD,
        .SDcols = patterns(
            "^y_\\d{4}|country_code_a3|name|ipcc_code_2006_for_standard_report|ipcc_code_2006_for_standard_report_name"
        )
    ] |>
    setnames(paste0("y_", seq(1970, 2022, 1)), as.character(seq(1970, 2022, 1))) |>
    _[  # get EIP sectors only
        ipcc_code_2006_for_standard_report %like% c("^1") |
            ipcc_code_2006_for_standard_report %like% c("^2")
    ][
        !ipcc_code_2006_for_standard_report %like% c("^2.D")
    ] |>
    merge.data.table(
        y = r5_mapping, all = TRUE, by.x = "country_code_a3", by.y = "alpha3"   # full join on country code
    )

## aggregate r5 regions and export------------------------
fossil |>
    _[
        !is.na(r5_region) | name == "Serbia and Montenegro"
    ][
        ,
        r5_region := fifelse(name == "Serbia and Montenegro", "OECD & EU (R5)", r5_region)
    ][
        !is.na(name)
    ][
        ,
        lapply(.SD, function(x) sum(x, na.rm = TRUE) * 0.001),
        by = .(r5_region),
        .SDcols = as.character(seq(1970, 2022, 1))
    ][
        r5_region != "World"
    ] |>
    melt.data.table(
        id.vars = "r5_region", variable.name = "year", value.name = "co2_eip_fossil_mt"
    ) |>
    fwrite(file = "output/co2_eip_fossil_historical_r5.csv")

# ## find difference between region's countries and EDGAR countries-----------------------
# fossil[
#     !is.na(r5_region) | name == "Serbia and Montenegro"
# ][
#     ,
#     r5_region := fifelse(name == "Serbia and Montenegro", "OECD & EU (R5)", r5_region)
# ][
#     !is.na(name)
# ][
#     ,
#     .(name, country, country_code_a3, r5_region)
# ] |>
#     unique() |>
#     View()

# ## check which categories to use-------------------------
# fossil[
#     ,
#     .(
#         ipcc_code_2006_for_standard_report,
#         ipcc_code_2006_for_standard_report_name
#     )
# ][
#     order(ipcc_code_2006_for_standard_report)
# ][
#     ipcc_code_2006_for_standard_report %like% c("^1") |
#     ipcc_code_2006_for_standard_report %like% c("^2")
# ][
#     !ipcc_code_2006_for_standard_report %like% c("^2.D")
# ] |>
#     unique()

# ## check if country name is matchable-------------------------------
# fossil[
#     ,
#     .(
#         country_code_a3, name
#     )
# ][
#     order(name)
# ] |>
#     unique() |>
#     merge.data.table(
#         y = r5_mapping, all = TRUE, by.x = "country_code_a3", by.y = "alpha3"
#     ) |>
#     _[
#         !is.na(r5_region) | name == "Serbia and Montenegro"
#     ][
#         !is.na(country)
#     ] |>
#     View()

## aggregate for world------------------------------------
fossil |>
    _[
        ,
        r5_region := "World",
    ][
        ,
        lapply(.SD, function(x) sum(x, na.rm = TRUE) * 0.001),
        by = .(r5_region),
        .SDcols = as.character(seq(1970, 2022, 1))
    ] |>
    melt.data.table(
        id.vars = "r5_region", variable.name = "year", value.name = "co2_eip_fossil_mt"
    ) |>
    fwrite(file = "output/co2_eip_fossil_historical_world.csv")

## read the exported files again and compute 10-19 EIP change-----------
rbindlist(
    list(
        fread("output/co2_eip_fossil_historical_r5.csv"),
        fread("output/co2_eip_fossil_historical_world.csv")
    )
) |>
    _[
        year %in% c(2010, 2019)
    ] |>
    dcast.data.table(
        r5_region ~ year, value.var = "co2_eip_fossil_mt"
    )|>
    _[
        ,
        ppt_change := (`2019` - `2010`) / `2010`
    ][
        ,
        .(r5_region, ppt_change)
    ] |>
    View()

# for ch4-----------------
## read files and parse---------------------
ch4 <- readxl::read_xlsx(path = "data/EDGAR_CH4_1970_2022.xlsx", sheet = "IPCC 2006", skip = 9) |>
    as.data.table() |>
    setnames(tolower) |>
    _[
        ,
        .SD,
        .SDcols = patterns(
            "^y_\\d{4}|country_code_a3|name|ipcc_code_2006_for_standard_report|ipcc_code_2006_for_standard_report_name"
        )
    ] |>
    setnames(paste0("y_", seq(1970, 2022, 1)), as.character(seq(1970, 2022, 1))) |>
    merge.data.table(
        y = r5_mapping, all = TRUE, by.x = "country_code_a3", by.y = "alpha3"   # full join on country code
    )

## check which categories to use---------------------------------
# ch4[
#     ,
#     .(
#         ipcc_code_2006_for_standard_report,
#         ipcc_code_2006_for_standard_report_name
#     )
# ][
#     order(ipcc_code_2006_for_standard_report)
# ] |>
#     unique()

# ## check if country name is matchable---------------------------
# ch4[
#     ,
#     .(
#         country_code_a3, name
#     )
# ][
#     order(name)
# ] |>
#     unique() |>
#     merge.data.table(
#         y = r5_mapping, all = TRUE, by.x = "country_code_a3", by.y = "alpha3"
#     ) |>
#     _[
#         !is.na(r5_region) | name == "Serbia and Montenegro"
#     ][
#         !is.na(country)
#     ] |>
#     View()

## aggregate to r5 region and export---------------------------
ch4 |>
    _[
        !is.na(r5_region) | name == "Serbia and Montenegro"
    ][
        ,
        r5_region := fifelse(name == "Serbia and Montenegro", "OECD & EU (R5)", r5_region)
    ][
        !is.na(name)
    ][
        ,
        lapply(.SD, function(x) sum(x, na.rm = TRUE) * 0.001),
        by = .(r5_region),
        .SDcols = as.character(seq(1970, 2022, 1))
    ][
        r5_region != "World"
    ] |>
    melt.data.table(
        id.vars = "r5_region", variable.name = "year", value.name = "ch4_mt"
    ) |>
    fwrite(file = "output/ch4_historical_r5.csv")

## aggregate to world and export----------------------------
ch4 |>
    _[
        ,
        r5_region := "World",
    ][
        ,
        lapply(.SD, function(x) sum(x, na.rm = TRUE) * 0.001),
        by = .(r5_region),
        .SDcols = as.character(seq(1970, 2022, 1))
    ] |>
    melt.data.table(
        id.vars = "r5_region", variable.name = "year", value.name = "ch4_mt"
    ) |>
    fwrite(file = "output/ch4_historical_world.csv")
