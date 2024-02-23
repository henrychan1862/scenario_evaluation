# This is for visualising grpahs depicting feasibility indicators
# Henry Chan, 14.1.2024

# library & source-----------------------
library(data.table)
library(ggplot2)
library(patchwork)

source("prog/plot.R")


# CONSTANT-----------------------------
fea_ref_path <- "output/feasibility_indicator.csv"
fea_sce_path <- "output/scenario_indicator.csv"
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

## define variables -------------------------
vars <- list(
    id = c(1:3),
    ind = c(
        "Energy Intensity in MER (MJ/USD2010)",
        "Carbon Intensity (g CO2/MJ)",
        "Electrification (%)"
    ),
    change = c(
        "Energy Intensity Rate of Change (%/yr)",
        "Carbon Intensity Change (g CO2/MJ/yr)",
        "Electrification Change (%/yr)"
    ),
    ind_unit = c(
        "MJ/USD2010",
        "g CO2/MJ",
        "%"
    ),
    chg_unit = c(
        "%/yr",
        "g CO2/MJ/yr",
        "%/yr"
    ),
    name = c(
        "energy intensity",
        "carbon intensity",
        "electrification"
    )
) |> as.data.table()


# visualisation-----------------------------
## all-in-one function (deprecated)-------------------
feasibility_vista <- function(var) {
    ## trend data--------------
    scenario_trend <- fread(fea_sce_path) |>
        _[
            ,
            .(
                model, scenario, region, year = period,
                value = get(var$ind)
            )
        ] |>
        na.omit() |>
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
            r10_region_code,
            on = .(region)
        ][
            ,
            .(
                n = .N,
                median = median(value),
                lower_90 = quantile(value, 0.05),
                upper_90 = quantile(value, 0.95)
            ),
            by = .(year, r10_region, category)
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ]

    ### join with historical indicator trend------------------
    historical_trend <- fread(fea_ref_path) |>
        _[
            ,
            .(
                year, r10_region, value = get(var$ind)
            )
        ][
            year <= 2015
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ]

    ### combine historical and projection and visualisation--------------------
    ggplot() +
        geom_ribbon(
            data = scenario_trend,
            mapping = aes(
                x = year,
                ymin = lower_90,
                ymax = upper_90,
                fill = r10_region
            ),
            alpha = 0.2
        ) +
        geom_line(
            data = scenario_trend,
            mapping = aes(
                x = year,
                y = median,
                color = r10_region
            ),
            linetype = "dashed",
        ) +
        geom_line(
            data = historical_trend,
            mapping = aes(
                x = year,
                y = value,
                color = r10_region
            ),
            linewidth = 1.5,
            alpha = 0.7
        ) +
        facet_wrap(~category, nrow = 4) +
        MyThemeLine_grid +
        xlab("Year") +
        ylab(var$ind_unit) +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank()
        ) +
        scale_colour_manual(values = unname(pals::glasbey())) +
        scale_fill_manual(values = unname(pals::glasbey()))
    ggsave(
        filename = paste0(
            "output/",
            stringr::str_split(var$ind, pattern = "\\(", simplify = TRUE)[1],
            "trend complex.png"
        ), width = 15, height = 10)

    ## change data------------------------
    ### scenario data----------------------
    chg_sce <- fread(sce_change_path) |>
        _[
            ,
            .(
                model, scenario, region, year = period,
                value = get(var$change)
            )
        ] |>
        na.omit() |>
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
            r10_region_code,
            on = .(region)
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### historical data---------------------------
    chg_ref <- fread(ref_change_path) |>
        _[
            ,
            .(
                year, r10_region, value = get(var$change)
            )
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### combine historical and projection change (trend) and visualisation--------------------------
    # alternative: try line range to indicate the 90% range of future projection
    ggplot() +
        geom_point(
            data = chg_ref,
            mapping = aes(
                x = year,
                y = value,
                color = r10_region
            ),
            position = position_dodge(width = 1)
        ) +
        geom_linerange(
            data = chg_sce[
                ,
                .(
                    # median = median(value),
                    low05 = quantile(value, 0.05),
                    high95 = quantile(value, 0.95)
                ),
                by = .(category, year, r10_region)
            ],
            mapping = aes(
                x = year,
                # y = median,
                ymin = low05,
                ymax = high95,
                color = r10_region
            ),
            alpha = 0.7,
            position = position_dodge(width = 4)
        ) +
        facet_wrap(~category, nrow = 4) +
        MyThemeLine_grid +
        xlab("Year") +
        ylab(var$chg_unit) +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank()
        ) +
        scale_colour_manual(values = unname(pals::glasbey()))
    ggsave(
        filename = paste0(
            "output/",
            stringr::str_split(var$change, pattern = "\\(", simplify = TRUE)[1],
            "trend complex.png"
        ), width = 15, height = 10)

    ### create outlier masking and statistics---------------
    # create outlier masking
    tmp <- rbindlist(
        list(
            chg_sce,
            chg_ref[
                ,
                c(.SD, category = "Historical")
            ]
        ), use.names = TRUE, fill = TRUE
    ) |>
        _[
            ,
            .(
                value,
                outlier = value < (quantile(value, .25) - 1.5 * IQR(value)) |
                    value > (quantile(value, .75) + 1.5 * IQR(value))
            ),
            by = category
        ]

    # mark the 5% and 95% qauantile
    boundary <- tmp[
        .("Historical"),
        on = .(category)
    ][
        ,
        .(
            low = quantile(value, 0.05),
            high = quantile(value, 0.95)
        )
    ] |>
        as.list()

    # get statistical information
    lapply(
        split(tmp, tmp[["category"]]),
        function(x) data.table(rbind(summary(x$value))) |> _[, N := x[, .N]]
    ) |>
        rbindlist(idcol = "category") |>
        fwrite(
            file = paste0(
                "output/",
                stringr::str_split(var$change, pattern = "\\(", simplify = TRUE)[1],
                "stat.csv"
            ))
    ### change density plot--------------------------
    ggplot(
        data = rbindlist(
            list(
                chg_sce,
                chg_ref[
                    ,
                    c(.SD, category = "Historical")
                ]
            ), use.names = TRUE, fill = TRUE
        )
    ) +
        geom_density(
            mapping = aes(
                x = value,
                color = category
            ),
            linewidth = 1.5
        ) +
        MyThemeLine_grid +
        xlab(var$chg_unit) +
        ylab("Density") +
        geom_vline(xintercept = boundary$low, linetype  = "dashed", alpha = 0.7, color = "Black") +
        geom_vline(xintercept = boundary$high, linetype  = "dashed", alpha = 0.7, color = "Black") +
        xlim(round(boundary$low - 1), round(boundary$high + 1)) +
        theme(
            # legend.position = "bottom",
            # legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank()
        ) +
        scale_color_manual(
            values = climate_color_list
        )
    ggsave(
        filename = paste0(
            "output/",
            stringr::str_split(var$change, pattern = "\\(", simplify = TRUE)[1],
            "density.png"
        ), width = 15, height = 10)

    ### change box plot--------------------------
    ggplot(
        data = tmp
    ) +
        geom_boxplot(
            mapping = aes(
                x = value,
                y = category,
                color = category
            ),
            outlier.shape = NA
        ) +
        geom_jitter(
            data = tmp[outlier == TRUE],
            mapping = aes(
                x = value,
                y = category,
                color = category
            ),
            width = 0,
            height = 0.3
        ) +
        MyThemeLine_grid +
        xlab(var$chg_unit) +
        ylab("") +
        geom_vline(xintercept = boundary$low, linetype  = "dashed", alpha = 0.7, color = "Black") +
        geom_vline(xintercept = boundary$high, linetype  = "dashed", alpha = 0.7, color = "Black") +
        xlim(round(boundary$low - 1), round(boundary$high + 1)) +
        theme(
            # legend.position = "bottom",
            # legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank()
        ) +
        scale_color_manual(
            values = climate_color_list
        )
    ggsave(
        filename = paste0(
            "output/",
            stringr::str_split(var$change, pattern = "\\(", simplify = TRUE)[1],
            "box.png"
        ), width = 15, height = 10)
}

## run thorugh 3 indicators------------------------------
lapply(
    split(vars, vars$ind),
    function(x) feasibility_vista(x)
)


# figures for presentation---------------------------------
## trend graphs---------------------------
ppt_trends <- vector("list", 3)
for (i in 1:3) {
    var <- vars[i]
    ### scenario  projection--------------------------------
    scenario_trend <- fread(fea_sce_path) |>
        _[
            ,
            .(
                model, scenario, region, year = period,
                value = get(var$ind)
            )
        ] |>
        na.omit() |>
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
            r10_region_code,
            on = .(region)
        ][
            ,
            .(
                n = .N,
                median = median(value)
            ),
            by = .(year, category)
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ]

    ### join with historical indicator trend------------------
    historical_trend <- fread(fea_ref_path) |>
        _[
            ,
            .(
                year, r10_region, value = get(var$ind)
            )
        ][
            year <= 2015
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][
            ,
            .(value = mean(value)),
            by = .(year)
        ]

    ### ppt: trend--------------------
    ppt_trends[[i]] <- ggplot() +
        geom_line(
            data = scenario_trend,
            mapping = aes(
                x = year,
                y = median,
                color = category
            ),
            linetype = "dashed",
            linewidth = 1.5
        ) +
        geom_line(
            data = historical_trend,
            mapping = aes(
                x = year,
                y = value,
            ),
            linewidth = 1.5,
            alpha = 0.7,
            color = "Black"
        ) +
        MyThemeLine_grid +
        xlab("Year") +
        ylab(var$ind_unit) +
        theme(
            legend.position = "none"
        ) +
        theme(
            axis.text.x = element_text(size = 18),
            axis.title.x = element_text(size = 28, face = "bold"),
            axis.text.y = element_text(size = 18),
            axis.title.y = element_text(size = 28, face = "bold"),
            strip.text.x = element_blank()
        ) +
        scale_color_manual(
            values = climate_color_list
        )
}

### export ggsave-----------------------
ggsave(
    filename = "output/feasibility_trends_dropped.png",
    plot = ppt_trends[[1]] + ppt_trends[[2]] + ppt_trends[[3]] + plot_layout(axes = "collect_x"),
    width = 16, height = 9
)

## change graphs------------------------
ppt_densities <- vector("list", 3)
for (i in 1:3) {
    var <- vars[i]
    ### scenario data----------------------
    chg_sce <- fread(sce_change_path) |>
        _[
            ,
            .(
                model, scenario, region, year = period,
                value = get(var$change)
            )
        ] |>
        na.omit() |>
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
            r10_region_code,
            on = .(region)
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### historical data---------------------------
    chg_ref <- fread(ref_change_path) |>
        _[
            ,
            .(
                year, r10_region, value = get(var$change)
            )
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### create outlier masking and statistics---------------
    # create outlier masking
    tmp <- rbindlist(
        list(
            chg_sce,
            chg_ref[
                ,
                c(.SD, category = "Historical")
            ]
        ), use.names = TRUE, fill = TRUE
    ) |>
        _[
            ,
            .(
                value,
                outlier = value < (quantile(value, .25) - 1.5 * IQR(value)) |
                    value > (quantile(value, .75) + 1.5 * IQR(value))
            ),
            by = category
        ]

    # mark the 5% and 95% qauantile
    boundary <- tmp[
        .("Historical"),
        on = .(category)
    ][
        ,
        .(
            low = quantile(value, 0.05),
            high = quantile(value, 0.95)
        )
    ] |>
        as.list()

    ### change density plot-------------------------
    ppt_densities[[i]] <- ggplot(
        data = rbindlist(
            list(
                chg_sce,
                chg_ref[
                    ,
                    c(.SD, category = "Historical")
                ]
            ), use.names = TRUE, fill = TRUE
        )
    ) +
        geom_density(
            mapping = aes(
                x = value,
                color = category
            ),
            linewidth = 1.5
        ) +
        MyThemeLine_grid +
        xlab(var$chg_unit) +
        ylab("Density") +
        geom_vline(xintercept = boundary$low, linetype  = "dashed", alpha = 0.7, color = "Black", linewidth = 2) +
        geom_vline(xintercept = boundary$high, linetype  = "dashed", alpha = 0.7, color = "Black", linewidth = 2) +
        xlim(round(boundary$low - 1), round(boundary$high + 1)) +
        theme(
            legend.position = "none"
        ) +
        theme(
            axis.text.x = element_text(size = 18),
            axis.title.x = element_text(size = 28, face = "bold"),
            axis.text.y = element_text(size = 18),
            axis.title.y = element_text(size = 28, face = "bold"),
            strip.text.x = element_blank()
        ) +
        scale_color_manual(
            values = climate_color_list
        )
}

### export ggsave------------------------
ggsave(
    filename = "output/feasibility_densities.png",
    plot = ppt_densities[[1]] + ppt_densities[[2]] + ppt_densities[[3]] + plot_layout(axis_titles = "collect_y"),
    width = 16, height = 6
)


# amend of final thesis: add legend to graphs------------------------
## all-in-one function----------------------
feasibility_vista_amend <- function(var) {

    ## trend and projection--------------
    ### sceenario data-----------------------------------
    scenario_trend <- fread(fea_sce_path) |>
        _[
            ,
            .(
                model, scenario, region, year = period,
                value = get(var$ind)
            )
        ] |>
        na.omit() |>
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
            r10_region_code,
            on = .(region)
        ][
            ,
            .(
                median = median(value)
            ),
            by = .(year, r10_region, category)
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ]

    ### historical data-----------------------------------
    historical_trend <- fread(fea_ref_path) |>
        _[
            ,
            .(
                year, r10_region, value = get(var$ind)
            )
        ][
            year <= 2015
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### plot-----------------------------------
    ggplot() +
        geom_line(
            data = scenario_trend,
            mapping = aes(
                x = year,
                y = median,
                color = r10_region,
                linetype = "Projection"
            ),
        ) +
        geom_line(
            data = historical_trend,
            mapping = aes(
                x = year,
                y = value,
                color = r10_region,
                linetype = "Historical"
            ),
            linewidth = 0.7,
            alpha = 0.7,
        ) +
        facet_wrap(~category, nrow = 2) +
        MyThemeLine_grid +
        xlab("Year") +
        ylab(var$ind_unit) +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank(),
            legend.spacing = unit(0.05, units = "npc"),
            axis.text.x = element_text(angle = 0, hjust = 0.5)
        ) +
        scale_colour_manual(values = unname(pals::glasbey())) +
        scale_linetype_manual(
            breaks = c("Historical", "Projection"),
            values = c(
                "Historical" = 1,
                "Projection" = 2
            )
        ) +
        guides(
            linetype = guide_legend(ncol = 1, override.aes = list(color = "#484a4c")),
            color = guide_legend(override.aes = list(linewidth = 1.5))
        )

    ### manual patchwork------------------------
    ggsave(
        filename = paste0("output/", var$name, " trend dropped.png"),
        width = 15, height = 10
    )

    ## change trend plot-------------------------
    ### scenario data----------------------
    chg_sce <- fread(sce_change_path) |>
        _[
            ,
            .(
                model, scenario, region, year = period,
                value = get(var$change)
            )
        ] |>
        na.omit() |>
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
            r10_region_code,
            on = .(region)
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### historical data---------------------------
    chg_ref <- fread(ref_change_path) |>
        _[
            ,
            .(
                year, r10_region, value = get(var$change)
            )
        ][
            ,
            r10_region := gsub(pattern = " \\(R10\\)", replacement = "", x = r10_region)
        ][]

    ### create outlier masking and statistics---------------
    # create outlier masking
    tmp <- rbindlist(
        list(
            chg_sce,
            chg_ref[
                ,
                c(.SD, category = "Historical")
            ]
        ), use.names = TRUE, fill = TRUE
    ) |>
        _[
            ,
            .(
                value,
                outlier = value < (quantile(value, .25) - 1.5 * IQR(value)) |
                    value > (quantile(value, .75) + 1.5 * IQR(value))
            ),
            by = category
        ]

    # mark the 5% and 95% qauantile
    boundary <- tmp[
        .("Historical"),
        on = .(category)
    ][
        ,
        .(
            low = quantile(value, 0.05),
            high = quantile(value, 0.95)
        )
    ] |>
        as.list()

    ### plot-------------------
    ggplot() +
        geom_point(
            data = chg_ref,
            mapping = aes(
                x = year,
                y = value,
                color = r10_region
            ),
            alpha = 0.9,
            position = position_dodge(width = 1.5)

        ) +
        geom_point(
            data = chg_sce[
                ,
                .(
                    median = median(value)
                ),
                by = .(category, year)
            ],
            mapping = aes(
                x = year,
                y = median,
                shape = "Median",
            )
        ) +
        geom_linerange(
            data = chg_sce[
                ,
                .(
                    low05 = quantile(value, 0.05),
                    high95 = quantile(value, 0.95)
                ),
                by = .(category, year)
            ],
            mapping = aes(
                x = year,
                ymin = low05,
                ymax = high95,
                linetype = "5-95% range"
            ),
        ) +
        facet_wrap(~category, nrow = 2) +
        MyThemeLine_grid +
        xlab("Year") +
        ylab(var$chg_unit) +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 20),
            legend.title = element_blank(),
            legend.spacing = unit(0.02, "npc"),
            axis.text.x = element_text(angle = 0, hjust = 0.5)
        ) +
        scale_colour_manual(values = unname(pals::glasbey())) +
        scale_x_continuous(breaks = seq(2000, 2080, by = 40)) +
        scale_shape_manual(breaks = c("Median"), values = c("Median" = 0)) +
        scale_linetype_manual(breaks = c("5-95% range"), values = c("5-95% range" = "solid")) +
        guides(
            color = guide_legend(order = 1, override.aes = list(size = 5)),
            shape = guide_legend(order = 3, override.aes = list(size = 5)),
            linetype = guide_legend(order = 2)
        )

    ### manual patchwork------------------------
    ggsave(
        filename = paste0("output/", var$name, " change trend dropped.png"),
        width = 15, height = 10
    )

    ## boxplot legend explain--------------
    box_exp <- ggplot_box_legend()

    ## change density plot w/ legend-------------------------
    ggplot(
        data = rbindlist(
            list(
                chg_sce,
                chg_ref[
                    ,
                    c(.SD, category = "Historical")
                ]
            ), use.names = TRUE, fill = TRUE
        )
    ) +
        stat_density(
            mapping = aes(
                x = value,
                color = category,
            ),
            geom = "line",
            position = "identity",
            linewidth = 1.5
        ) +
        MyThemeLine_grid +
        xlab(var$chg_unit) +
        ylab("Density") +
        geom_vline(
            aes(
                xintercept = boundary$low, linetype = "Historical\n5–95th percentile"
            ), color = "#484a4c", linewidth = 1.5
        ) +
        geom_vline(
            aes(
                xintercept = boundary$high, linetype = "Historical\n5–95th percentile"
            ), color = "#484a4c", linewidth = 1.5
        ) +
        xlim(round(boundary$low - 1), round(boundary$high + 1)) +
        theme(
            axis.text.x = element_text(size = 18, angle = 0),
            axis.text.y = element_text(size = 18),
            strip.text.x = element_blank(),
            legend.key.size = unit(0.05, units = "npc"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18, face = "bold"),
            legend.position = "bottom",
            legend.direction = "horizontal",
        ) +
        scale_color_manual(
            "Climate category",
            values = climate_color_list
        ) +
        scale_linetype_manual(
            "",
            breaks = c("Historical\n5–95th percentile"),
            values = c("Historical\n5–95th percentile" = 2)
        )

    ### export-------------
    ggsave(
        filename = paste0("output/", var$name, " change density.png"),
        width = 15, height = 10
    )

    ## change boxplot plot w/ legend-------------------------
    box <- ggplot(
        data = tmp
    ) +
        geom_boxplot(
            mapping = aes(
                y = value,
                x = category,
                color = category
            ),
            outlier.shape = NA
        ) +
        geom_jitter(
            data = tmp[outlier == TRUE],
            mapping = aes(
                y = value,
                x = category,
                color = category
            ),
            width = 0.3,
            height = 0
        ) +
        MyThemeLine_grid +
        ylab(var$chg_unit) +
        xlab("") +
        geom_hline(
            aes(
                yintercept = boundary$low, linetype = "Historical\n5–95th percentile"
            ), color = "#484a4c"
        ) +
        geom_hline(
            aes(
                yintercept = boundary$high, linetype = "Historical\n5–95th percentile"
            ), color = "#484a4c"
        ) +
        ylim(round(boundary$low - 1), round(boundary$high + 1)) +
        theme(
            axis.text.y = element_text(size = 18, angle = 0),
            axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5),
            strip.text.x = element_blank(),
            legend.key.size = unit(0.03, units = "npc"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18, face = "bold"),
            legend.position = "bottom",
            legend.direction = "horizontal",
        ) +
        scale_color_manual(
            "Climate category",
            values = climate_color_list
        ) +
        scale_linetype_manual(
            "",
            breaks = c("Historical\n5–95th percentile"),
            values = c("Historical\n5–95th percentile" = 2)
        ) +
        guides(colour = guide_legend(nrow = 2))

    ### export w/ patchwork-----------
    ggsave(
        filename = paste0("output/", var$name, " change boxplot.png"),
        plot = box + box_exp + plot_layout(widths = c(4, 1)),
        width = 15, height = 10
    )

}

## run through 3 indicators-------------
lapply(
    split(vars, vars$ind),
    function(x) feasibility_vista_amend(x)
)




# end