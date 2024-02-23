# Validation and revising the regional vetting result done b4
# Henry Chan, 8.1.2024 set it on fire


# library & source-----------------------
library(data.table)
library(yaml)
library(ggplot2)
library(patchwork)

source("prog/vetting_function.R") # use the vetting function
source("prog/plot.R") # use the vetting function


# CONSTANT DEFINITION-----------------------
AR6_SCENARIO_PATH <- "data/AR6_Scenarios_Database_World_v1.1.csv"
AR6_R5_SCENARIO_PATH <- "data/AR6_Scenarios_Database_R5_regions_v1.1.csv"
ANNEX3_VETTING_META <- "data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
region_factor <- c("World", "R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")
region_factor_nor5 <- c("World", "OECD90+EU", "ASIA", "LAM", "MAF", "REF")


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


# vetting----------------------
## 2020, R5------------------------
resultAr6R5_2020 <- vetting(
    ar6ScenarioR5,
    paste0("define/amend_r5_2020.yaml"),
    var_order = "define/variable_order_vetting.yaml",
    log = paste0("vetting_amend_r5_2020.log"),
    csv_path = "output/"
)
resultAr6R5_2020 |>
    fwrite(file = paste0("output/ar6_2020_vetting_result_r5.csv"), na = "NA")
test_summary(resultAr6R5_2020) |>
    fwrite(paste0("output/ar6_2020_summary_r5.csv"))

## 2020, World------------------------
resultAr6World_2020 <- vetting(
    ar6Scenario,
    paste0("define/amend_world_2020.yaml"),
    var_order = "define/variable_order_vetting.yaml",
    log = paste0("vetting_amend_world_2020.log"),
    csv_path = "output/"
)
resultAr6World_2020 |>
    fwrite(file = paste0("output/ar6_2020_vetting_result_world.csv"), na = "NA")
test_summary(resultAr6World_2020) |>
    fwrite(paste0("output/ar6_2020_summary_world.csv"))

## 2019, R5-----------------------
resultAr6R5_2019 <- vetting(
    ar6ScenarioR5,
    paste0("define/amend_r5_2019.yaml"),
    var_order = "define/variable_order_vetting.yaml",
    log = paste0("vetting_amend_r5_2019.log"),
    csv_path = "output/"
)
resultAr6R5_2019 |>
    fwrite(file = paste0("output/ar6_2019_vetting_result_r5.csv"), na = "NA")
test_summary(resultAr6R5_2019) |>
    fwrite(paste0("output/ar6_2019_summary_r5.csv"))

## 2019, World---------------------
resultAr6World_2019 <- vetting(
    ar6Scenario,
    paste0("define/amend_world_2019.yaml"),
    var_order = "define/variable_order_vetting.yaml",
    log = paste0("vetting_amend_world_2019.log"),
    csv_path = "output/"
)
resultAr6World_2019 |>
    fwrite(file = paste0("output/ar6_2019_vetting_result_world.csv"), na = "NA")
test_summary(resultAr6World_2019) |>
    fwrite(paste0("output/ar6_2019_summary_world.csv"))


# visualisation, 2020------------------
## data preparation, 2020------------------
### read in data ref data and range for plotting
ref_values <- lapply(
    yaml.load_file("define/amend_r5_2020.yaml") |>
        unlist(recursive = FALSE) |>
        unlist(recursive = FALSE),
    function(item) {
        tmp <- item$value
        names(tmp) <- item$region
        return(
            list(tmp, range = item$range) |> unlist()
        )
    }
) |>
    as.data.frame() |>
    rbind.data.frame(
        lapply(
            yaml.load_file("define/amend_world_2020.yaml") |>
                unlist(recursive = FALSE) |>
                unlist(recursive = FALSE),
            function(item) {
                tmp <- item$value
                names(tmp) <- item$region
                return(
                    list(tmp, range = item$range) |> unlist()
                )
            }
        ) |>
            as.data.frame()
    )
ref_values["range", sort(names(ref_values))[[1]]]
names(ref_values) |> sort()

### read in distribution data
dis_world <- list.files("output/", pattern = "*in 2020 for World*", full.names = TRUE) |> sort()
dis_r5 <- list.files("output/", pattern = "*in 2020 for R5*", full.names = TRUE) |> sort()

### read in summary data
summary_files <- list.files("output/", pattern = "^ar6_2020_summary_*", full.names = TRUE) |> sort()
summary <- lapply(
    summary_files,
    function(file) {
        return(
            fread(file)
        )
    }
) |>
    rbindlist() |>
    _[
        data.table(region = region_factor),
        on = .(region)
    ][
        ,
        `:=`(
            passing_rate = pass / tested
        )
    ][
        ,
        `:=`(
            fail_rate = fail / tested
        )
    ][
        ,
        text := paste(region, paste0("(n = ", tested, ")"), sep = "\n")
    ]
summary |>
    fwrite("output/proc_ar6_summary_2020.csv")

## output all the plot----------------
plot_dis_pass_2020 <- function(i) {

    # boxplot
    dis_data <- rbindlist(
        list(
            fread(dis_world[i]),
            fread(dis_r5[i])
        )
    ) |>
        _[
            ,
            value_dis := (value - ref_values[region, sort(names(ref_values))[[i]]])
            / ref_values[region, sort(names(ref_values))[[i]]]
        ][
            ,
            region := factor(region, levels = region_factor)
        ][]

    ggplot(data = dis_data) +
        geom_boxplot(
            mapping = aes(
                x = region,
                y = value_dis
            )
        ) +
        ylim(-1, 1) +
        geom_hline(
            yintercept = ref_values["range", sort(names(ref_values))[[i]]],
            linetype  = "dashed", alpha = 0.7, color = "Red"
        ) +
        geom_hline(
            yintercept = -ref_values["range", sort(names(ref_values))[[i]]],
            linetype  = "dashed", alpha = 0.7, color = "Red"
        ) +
        MyThemeLine_grid +
        xlab("World & R5-region") +
        ylab(paste("Deviation from Reference"))
    ggsave(filename = paste0("output/", sort(names(ref_values))[[i]], ".2020.dis.png"), width = 15, height = 10)

    # passing plot
    pass_plot <- summary[
        name %like% sort(names(ref_values))[[i]]
    ][
        ,
        region_factor := factor(region, levels = region_factor)
    ][
        order(region_factor)
    ]

    ggplot(
        data = pass_plot
    ) +
        geom_col(
            mapping = aes(
                x = region_factor,
                y = passing_rate
            )
        ) +
        ylim(0, 1) +
        scale_x_discrete(labels = pass_plot$text) +
        MyThemeLine_grid +
        xlab("World & R5-region") +
        ylab("Passing Rate")
    ggsave(filename = paste0("output/", sort(names(ref_values))[[i]], ".2020.pass.png"), width = 15, height = 10, )
}

## export all plots for 2020 vetting---------------
for (i in c(1:5)) {
    plot_dis_pass_2020(i)
}


# visualisation, 2019----------------------
## data preparation, 2019------------------
### read in data ref data and range for plotting
ref_values <- lapply(
    yaml.load_file("define/amend_r5_2019.yaml") |>
        unlist(recursive = FALSE) |>
        unlist(recursive = FALSE),
    function(item) {
        tmp <- item$value
        names(tmp) <- item$region
        return(
            list(tmp, range = item$range) |> unlist()
        )
    }
) |>
    as.data.frame() |>
    rbind.data.frame(
        lapply(
            yaml.load_file("define/amend_world_2019.yaml") |>
                unlist(recursive = FALSE) |>
                unlist(recursive = FALSE),
            function(item) {
                tmp <- item$value
                names(tmp) <- item$region
                return(
                    list(tmp, range = item$range) |> unlist()
                )
            }
        ) |>
            as.data.frame()
    )
names(ref_values)[[2]] <- "Historical.Emissions.CO2 EIP emissions % change" # parse name correctly

### read in distribution data
dis_world <- list.files("output/", pattern = "*2019 for World*", full.names = TRUE) |> sort()
dis_r5 <- list.files("output/", pattern = "*2019 for R5*", full.names = TRUE) |> sort()

### read in summary data
summary_files <- list.files("output/", pattern = "^ar6_2019_summary_*", full.names = TRUE) |> sort()
summary <- lapply(
    summary_files,
    function(file) {
        return(
            fread(file)
        )
    }
) |>
    rbindlist() |>
    _[
        data.table(region = region_factor),
        on = .(region)
    ][
        ,
        `:=`(
            passing_rate = pass / tested
        )
    ][
        ,
        `:=`(
            fail_rate = fail / tested
        )
    ][
        ,
        text := paste(region, paste0("(n = ", tested, ")"), sep = "\n")
    ]

summary |>
    fwrite("output/proc_ar6_summary_2019.csv")

## output all the plot----------------
plot_dis_pass_2019 <- function(i) {

    # ### boxplot-----------------
    if (grepl("% change", sort(names(ref_values))[[i]])) {
        dis_data <- rbindlist(
            list(
                fread(dis_world[i]),
                fread(dis_r5[i])
            )
        ) |>
            _[
                ,   # for pct change, deviation is defined as percentage point difference
                value_dis := (value - ref_values[region, sort(names(ref_values))[[i]]] * 100) / 100
            ][
                ,
                region := factor(stringr::str_replace_all(region, "R5", ""), levels = region_factor_nor5)
            ][]
    } else {
        dis_data <- rbindlist(
            list(
                fread(dis_world[i]),
                fread(dis_r5[i])
            )
        ) |>
            _[
                ,
                value_dis := (value - ref_values[region, sort(names(ref_values))[[i]]])
                / ref_values[region, sort(names(ref_values))[[i]]]
            ][
                ,
                region := factor(stringr::str_replace_all(region, "R5", ""), levels = region_factor_nor5)
            ][]
    }
    dis_plot <- ggplot(data = dis_data) +
        geom_boxplot(
            mapping = aes(
                x = region,
                y = value_dis
            )
        ) +
        ylim(-1, 1) +
        geom_hline(
            mapping = aes(
                linetype  = "Acceptable range",
                yintercept = ref_values["range", sort(names(ref_values))[[i]]],
            ), alpha = 0.7, color = "Red"
        ) +
        geom_hline(
            mapping = aes(
                linetype  = "Acceptable range",
                yintercept = -ref_values["range", sort(names(ref_values))[[i]]],
            ), alpha = 0.7, color = "Red"
        ) +
        MyThemeLine_grid +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = c(0.1, 0.93),
            legend.box.background = element_rect(color = "Black"),
            legend.title = element_blank()
        ) +
        xlab("World & R5-region") +
        ylab(paste("Deviation from Reference")) +
        scale_linetype_manual(
            "",
            breaks = c("Acceptable range"),
            values = c("Acceptable range" = "dashed"),
        ) +
        guides(linetype = guide_legend(override.aes = list(color = "Red")))

    box_exp <- ggplot_box_legend()

    ggsave(
        filename = paste0(
            "output/",
            sort(names(ref_values))[[i]] |>
                stringr::str_replace("% change", "pct change") |>
                stringr::str_replace_all("\\.", " "),
            " 2019 dis.png"
        ),
        plot = dis_plot + box_exp + plot_layout(widths = c(4, 1)),
        width = 15, height = 7
    )


    ### passing plot------------------------
    pass_plot <- summary[
        ,
        name := fifelse(
            name == "Historical.Emissions.CO2 EIP emissions % change",
            name,
            stringr::str_replace_all(name, " ", "\\.")
        )
    ][
        name == sort(names(ref_values))[[i]]
    ][
        ,
        region_factor := factor(region, levels = region_factor)
    ][
        order(region_factor),
        text := stringr::str_remove_all(text, "R5")
    ]

    ggplot(
        data = pass_plot
    ) +
        geom_col(
            mapping = aes(
                x = region_factor,
                y = passing_rate
            )
        ) +
        ylim(0, 1) +
        scale_x_discrete(labels = pass_plot$text) +
        MyThemeLine_grid +
        xlab("World & R5-region") +
        ylab("Passing Rate") +
        theme(
            axis.text.x = element_text(angle = 0, hjust = .5)
        )

    ggsave(
        filename = paste0(
            "output/",
            sort(names(ref_values))[[i]] |>
                stringr::str_replace("% change", "pct change") |>
                stringr::str_replace_all("\\.", " "),
            " 2019 pass.png"
        ),
        width = 15, height = 6
    )
}

## export all plots for 2019 vetting---------------
for (i in c(1:6)) {
    plot_dis_pass_2019(i)
}


# ppt: use patchwork to join all plots-------------------------
## deviations-----------------------------
deviations <- vector("list", 6)
for (i in c(1:6)) {
    if (grepl("% change", sort(names(ref_values))[[i]])) {
        dis_data <- rbindlist(
            list(
                fread(dis_world[i]),
                fread(dis_r5[i])
            )
        ) |>
            _[
                ,
                value_dis := value - ref_values[region, sort(names(ref_values))[[i]]] * 100
            ][
                ,
                region := factor(stringr::str_replace_all(region, "R5", ""), levels = region_factor_nor5)
            ][]

        fig <- ggplot(data = dis_data) +
            geom_boxplot(
                mapping = aes(
                    x = region,
                    y = value_dis,
                    color = region
                )
            ) +
            ylim(-100, 100) +
            geom_hline(
                yintercept = ref_values["range", sort(names(ref_values))[[i]]] * 100,
                linetype  = "dashed", alpha = 0.7, color = "Black"
            ) +
            geom_hline(
                yintercept = -(ref_values["range", sort(names(ref_values))[[i]]] * 100),
                linetype  = "dashed", alpha = 0.7, color = "Black"
            ) +
            MyThemeLine_grid +
            theme(
                axis.text.x = element_text(size = 18, face = "bold", angle = 45),
                axis.title.x = element_blank(),
                axis.text.y = element_text(size = 28, face = "bold"),
                axis.title.y = element_blank(),
                legend.position = "none"
            ) +
            ylab(paste("Deviation from Reference (%)"))
    } else {
        dis_data <- rbindlist(
            list(
                fread(dis_world[i]),
                fread(dis_r5[i])
            )
        ) |>
            _[
                ,
                value_dis := 100 * (value - ref_values[region, sort(names(ref_values))[[i]]])
                / ref_values[region, sort(names(ref_values))[[i]]]
            ][
                ,
                region := factor(stringr::str_replace_all(region, "R5", ""), levels = region_factor_nor5)
            ][]

        fig <- ggplot(data = dis_data) +
            geom_boxplot(
                mapping = aes(
                    x = region,
                    y = value_dis,
                    color = region
                )
            ) +
            ylim(-100, 100) +
            geom_hline(
                yintercept = ref_values["range", sort(names(ref_values))[[i]]] * 100,
                linetype  = "dashed", alpha = 0.7, color = "Black"
            ) +
            geom_hline(
                yintercept = -(ref_values["range", sort(names(ref_values))[[i]]] * 100),
                linetype  = "dashed", alpha = 0.7, color = "Black"
            ) +
            MyThemeLine_grid +
            theme(
                axis.text.x = element_text(size = 18, face = "bold", angle = 45),
                axis.title.x = element_blank(),
                axis.text.y = element_text(size = 28, face = "bold"),
                axis.title.y = element_blank(),
                legend.position = "none"
            ) +
            ylab(paste("Deviation from Reference (%)"))
    }
    if (i == 1 | i == 2 | i == 3) {
        fig <- fig + theme(axis.text.x = element_blank())
    }
    deviations[[i]] <- fig
}
names(deviations) <- sort(names(ref_values))
ggsave(
    filename = "output/deviations_pct_color.png",
    plot = (deviations[[3]] + deviations[[2]] + deviations[[1]] + plot_layout(axes = "collect_y")) /
        (deviations[[6]] + deviations[[4]] + deviations[[5]] + plot_layout(axes = "collect_y")),
    width = 15, height = 10
)

## fails---------------------------
failes <- vector("list", 6)
names(ref_values) <- c(
    "Historical.Emissions.CO2 EIP emissions",
    "Historical.Emissions.CO2 EIP emissions % change",
    "Historical.Emissions.CH4 emissions",
    "Historical.Energy.Primary energy",
    "Historical.Energy.Electricity from nuclear",
    "Historical.Energy.Electricity from solar and wind"
)
world_r5_fail_avg <- lapply(
    summary_files,
    function(file) {
        return(
            fread(file)
        )
    }
) |>
    rbindlist() |>
    _[
        ,
        `:=`(
            passing_rate = pass / tested
        )
    ][
        ,
        `:=`(
            fail_rate = fail / tested
        )
    ][
        ,
        is_world := region == "World"
    ][
        !is.na(passing_rate)
    ] |>
    dcast.data.table(
        name ~ is_world, value.var = "fail_rate", fun.aggregate = mean
    )
for (i in c(1:6)) {
    pass_plot <- summary[
        name == sort(names(ref_values))[[i]]
    ][
        ,
        region := factor(stringr::str_replace_all(region, "R5", ""), levels = region_factor_nor5)
    ][
        order(region)
    ]

    fig <- ggplot(
        data = pass_plot
    ) +
        geom_col(
            mapping = aes(
                x = region,
                y = fail_rate * 100,
                fill = region

            )
        ) +
        ylim(0, 100) +
        MyThemeLine_grid +
        theme(
            axis.text.x = element_text(size = 18, face = "bold", angle = 45),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 28, face = "bold"),
            axis.title.y = element_blank(),
            legend.position = "none"
        ) +
        geom_hline(
            yintercept = world_r5_fail_avg[name == sort(names(ref_values))[[i]], `FALSE`] * 100,
            linetype  = "dashed", alpha = 0.9, color = "Red"
        ) +
        ylab("Failing Rate (%)")
    if (i == 1 | i == 2 | i == 3) {
        fig <- fig + theme(axis.text.x = element_blank())
    }
    failes[[i]] <- fig
}
names(failes) <- sort(names(ref_values))
ggsave(
    filename = "output/failes_pct_color.png",
    plot = (failes[[2]] + failes[[3]] + failes[[1]] + plot_layout(axes = "collect_y")) /
        (failes[[6]] + failes[[4]] + failes[[5]] + plot_layout(axes = "collect_y")),
    width = 15, height = 10
)
