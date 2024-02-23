# As file name suggests
# Henry Chan, 17.1.2024

# library & source-----------------------
library(data.table)
library(ggplot2)
library(ggbeeswarm)

source("prog/plot.R")


# CONSTANT-----------------------------
ref_change_path <- "output/feasibility_indicator_change.csv"
sce_change_path <- "output/scenario_indicator_change.csv"
ANNEX3_VETTING_META <- "data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
r10_region <- "define/r10_region.csv"

## annexIII for climate category and color-----------------------
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

### color list-----------------------------
climate_color <- annex3Meta[
    order(category),
    .(
        category,
        category_color_hex = paste0("#", category_color_hex)
    )
] |> unique()
climate_color_list <- climate_color$category_color_hex
names(climate_color_list) <- climate_color$category
climate_color_list <- c(climate_color_list, Historical = "#000000")
climate_color_list

## R10 region mapping--------------------------
r10_region_code <- fread(r10_region) |>
    _[
        ,
        .(
            r10_region, region = code
        )
    ][
        order(r10_region)
    ] |>
    unique()


# read data, preparate and check passing rate----------------------
## historical data---------------------------
cutting_points <- fread(ref_change_path) |>
    _[
        ,
        lapply(.SD, function(x) c(quantile(x, 0.05), quantile(x, 0.95))),
        .SDcols = !c("r10_region", "year")
    ] |>
    as.list()

cutting_points |> as.data.table()

fread(sce_change_path) |>
    _[
        annex3Meta[
            ,
            .(
                model, scenario, category
            )
        ],
        on = .(model, scenario),
        nomatch = NULL
    ] |>
    _[
        ,
        paste0("q5_", c("ene", "carbon", "elec")) := lapply(.SD, function(x) quantile(x, 0.05, na.rm = TRUE)),
        .SDcols = `Energy Intensity Rate of Change (%/yr)`:`Electrification Change (%/yr)`,
        by = .(category)
    ][
        ,
        paste0("q95_", c("ene", "carbon", "elec")) := lapply(.SD, function(x) quantile(x, 0.95, na.rm = TRUE)),
        .SDcols = `Energy Intensity Rate of Change (%/yr)`:`Electrification Change (%/yr)`,
        by = .(category)
    ][
        order(category),
        .SD,
        .SDcols = patterns("(category)|(^q)")
    ] |>
    unique() |>
    fwrite("output/feasibility_scenario_90_coverage.csv")

## scenario data--------------------------
passing_rate <- fread(sce_change_path) |>
    _[
        annex3Meta[
            ,
            .(
                model, scenario, category
            )
        ],
        on = .(model, scenario),
        nomatch = NULL
    ][
        ,
        .(
            ene_pass_rate =
                100 * sum(
                    between(
                        `Energy Intensity Rate of Change (%/yr)`,
                        cutting_points$`Energy Intensity Rate of Change (%/yr)`[1],
                        cutting_points$`Energy Intensity Rate of Change (%/yr)`[2]
                    ), na.rm = TRUE) / sum(!is.na(`Energy Intensity Rate of Change (%/yr)`)),
            carbon_pass_rate =
                100 * sum(
                    between(
                        `Carbon Intensity Change (g CO2/MJ/yr)`,
                        cutting_points$`Carbon Intensity Change (g CO2/MJ/yr)`[1],
                        cutting_points$`Carbon Intensity Change (g CO2/MJ/yr)`[2]
                    ), na.rm = TRUE) / sum(!is.na(`Carbon Intensity Change (g CO2/MJ/yr)`)),
            elec_pass_rate =
                100 * sum(
                    between(
                        `Electrification Change (%/yr)`,
                        cutting_points$`Electrification Change (%/yr)`[1],
                        cutting_points$`Electrification Change (%/yr)`[2]
                    ), na.rm = TRUE) / sum(!is.na(`Electrification Change (%/yr)`))
        ),
        by = .(category)
    ][
        order(category)
    ]

passing_rate |>
    fwrite("output/feasibility_passing_rate.csv")

## show aggregated failing rate------------------
passing_rate |>
    melt.data.table(
        id.vars = "category"
    ) |>
    _[
        ,
        .(
            category, variable,
            value = 100 - value
        )
    ][
        list(
            variable = c(
                "ene_pass_rate",
                "carbon_pass_rate",
                "elec_pass_rate"
            ),
            Indicator = c(
                "Energy Intensity Rate of Change",
                "Carbon Intensity Change",
                "Electrification Change"
            )
        ) |> as.data.table(),
        on = .(variable)
    ][
        ,
        Indicator := factor(
            Indicator, levels =
            c(
                "Energy Intensity Rate of Change",
                "Carbon Intensity Change",
                "Electrification Change"
            )
        )
    ][
        ,
        sum := sum(value),
        by = category
    ][]


# visualise---------------------
## feasibility concern----------------------
ggplot(
    data = passing_rate |>
        melt.data.table(
            id.vars = "category"
        ) |>
        _[
            ,
            .(
                category, variable,
                value = 100 - value
            )
        ][
            list(
                variable = c(
                    "ene_pass_rate",
                    "carbon_pass_rate",
                    "elec_pass_rate"
                ),
                Indicator = c(
                    "Energy Intensity",
                    "Carbon Intensity",
                    "Electrification Rate"
                )
            ) |> as.data.table(),
            on = .(variable)
        ][
            ,
            Indicator := factor(
                Indicator, levels =
                    c(
                        "Energy Intensity",
                        "Carbon Intensity",
                        "Electrification Rate"
                    )
            )
        ][]
) +
    geom_col(
        mapping = aes(
            y = category,
            x = value,
            fill = Indicator
        ),
        position = position_dodge2()
    ) +
    MyThemeLine_grid +
    ylab("Category") +
    xlab("Failing Rate (%)") +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 36),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 40),
        axis.title.x = element_text(size = 36),
        axis.text.y = element_text(size = 40),
        axis.title.y = element_text(size = 36)
    )

ggsave(filename = "output/feasibility_concern_dodge.png", width = 15, height = 10)


# passing per regions---------------------
## find the region ratio in failed scenarios------------------------
failing_rate_region <- fread(sce_change_path) |>
    _[
        annex3Meta[
            ,
            .(
                model, scenario, category
            )
        ],
        on = .(model, scenario),
        nomatch = NULL
    ][
        ,
        .(
            ene_fail_rate =
                100 * sum(
                    !between(
                        `Energy Intensity Rate of Change (%/yr)`,
                        cutting_points$`Energy Intensity Rate of Change (%/yr)`[1],
                        cutting_points$`Energy Intensity Rate of Change (%/yr)`[2]
                    ), na.rm = TRUE) / sum(!is.na(`Energy Intensity Rate of Change (%/yr)`)),
            carbon_fail_rate =
                100 * sum(
                    !between(
                        `Carbon Intensity Change (g CO2/MJ/yr)`,
                        cutting_points$`Carbon Intensity Change (g CO2/MJ/yr)`[1],
                        cutting_points$`Carbon Intensity Change (g CO2/MJ/yr)`[2]
                    ), na.rm = TRUE) / sum(!is.na(`Carbon Intensity Change (g CO2/MJ/yr)`)),
            elec_fail_rate =
                100 * sum(
                    !between(
                        `Electrification Change (%/yr)`,
                        cutting_points$`Electrification Change (%/yr)`[1],
                        cutting_points$`Electrification Change (%/yr)`[2]
                    ), na.rm = TRUE) / sum(!is.na(`Electrification Change (%/yr)`))
        ),
        by = .(category, region)
    ] |>
    melt.data.table(
        measure.vars = c("ene_fail_rate", "carbon_fail_rate", "elec_fail_rate"),
        variable.name = "indicator",
        value.name = "prop"
    )


# regional deviation from avg.-----------------
## carbon intensity-----------------------
carbon_fail_region <- failing_rate_region[
    indicator %in% c("carbon_fail_rate")
][
    passing_rate |>
        melt.data.table(
            id.vars = "category"
        ) |>
        _[
            variable == "carbon_pass_rate",
            .(
                category, variable,
                prop_avg = 100 - value
            )
        ],
    on = .(category)
][
    r10_region_code,
    on = .(region)
][
    ,
    r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
][
    category %in% paste0("C", c(1:4))
][
    ,
    deviation := round((prop - prop_avg), 1)
][
    ,
    Category := paste0(category, " (", round(prop_avg, 1), "%)")
][
    order(r10_region)
]

### plot: use point and jitter-----------------
ggplot(
    data = carbon_fail_region
) +
    geom_beeswarm(
        mapping = aes(
            x = category,
            y = prop,
            color = r10_region,
        ),
        size = 5,
        alpha = 0.6,
        cex = 3
    ) +
    geom_point(
        data = carbon_fail_region[, .(category, prop_avg, name = "Overall")] |> unique(),
        mapping = aes(
            x = category,
            y = prop_avg,
            shape = name
        ),
        color = "black",
        size = 6,
        stroke = 1.5
    ) +
    MyThemeLine_grid +
    # ggtitle("Failing Rate (Carbon Intensity)") +
    ylab("Failing Rate (%)") +
    xlab("Category") +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 20)
    ) +
    theme(
        axis.text.x = element_text(size = 18, angle = 0),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_blank()
    ) +
    scale_color_manual(values = unname(pals::glasbey())) +
    scale_shape_manual(breaks = c("Overall"), values = c(0))

ggsave(filename = "output/carbon_fail_region.png", width = 15, height = 10)

## electrification difference-----------------------
elec_fail_region <- failing_rate_region[
    indicator %in% c("elec_fail_rate")
][
    passing_rate |>
        melt.data.table(
            id.vars = "category"
        ) |>
        _[
            variable == "elec_pass_rate",
            .(
                category, variable,
                prop_avg = 100 - value
            )
        ],
    on = .(category)
][
    category %in% paste0("C", c(1:4))
][
    r10_region_code,
    on = .(region)
][
    ,
    r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
][
    ,
    deviation := round(prop - prop_avg, 1)
][
    ,
    Category := paste0(category, " (", round(prop_avg, 1), "%)")
][
    order(region)
]

### plot: use point and jitter-----------------
ggplot(
    data = elec_fail_region
) +
    geom_beeswarm(
        mapping = aes(
            x = category,
            y = prop,
            color = r10_region,
        ),
        size = 5,
        alpha = 0.6,
        cex = 4
    ) +
    geom_point(
        data = elec_fail_region[, .(category, prop_avg, name = "Overall")] |> unique(),
        mapping = aes(
            x = category,
            y = prop_avg,
            shape = name
        ),
        color = "black",
        size = 6,
        stroke = 1.5
    ) +
    MyThemeLine_grid +
    # ggtitle("Failing Rate (Electrification Rate)") +
    ylab("Failing Rate (%)") +
    xlab("Category") +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 20)
    ) +
    theme(
        axis.text.x = element_text(size = 18, angle = 0),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_blank()
    ) +
    scale_color_manual(values = unname(pals::glasbey())) +
    scale_shape_manual(breaks = c("Overall"), values = c(0))

ggsave(filename = "output/electrification_fail_region.png", width = 15, height = 10)
