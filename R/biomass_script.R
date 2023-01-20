# forest inventory change

require(fortedata)
require(tidyverse)
require(ggplot2)

# plotting info
##### CUSTOM PLOT THEME
try_theme <- function() {
    theme(
        # add border 1)
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        # color background 2)
        #panel.background = element_rect(fill = "white"),
        # modify grid 3)
        panel.grid.major.x = element_line(colour = "#333333", linetype = 3, size = 0.5),
        panel.grid.minor.x = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
        panel.grid.major.y = element_line(colour = "#333333", linetype = 3, size = 0.5),
        panel.grid.minor.y = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
        # modify text, axis and colour 4) and 5)
        axis.text = element_text(colour = "black", family = "Times New Roman"),
        axis.title = element_text(colour = "black", family = "Times New Roman"),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length=unit(-0.1, "cm"),
        # legend at the bottom 6)
        legend.position = "bottom",
        strip.text.x = element_text(size=10, color="black",  family = "Times New Roman"),
        strip.text.y = element_text(size=10, color="black",  family = "Times New Roman"),
        strip.background = element_blank()
    )
}

##### PLOT METADATA
df <- fortedata:::fd_plot_metadata()

df <- data.frame(df)
df$subplot_id <- paste(df$replicate, 0, df$plot, df$subplot, sep = "")
df$subplot_id <- as.factor(df$subplot_id)

df %>%
    select(subplot_id, disturbance_severity, treatment) %>%
    distinct() %>%
    na.omit() %>%
    data.frame() -> x

## bring in the inventory and mortalitiy data sets
mor <- fd_mortality()
inv <- fd_inventory()
#inv <- fd_inventory()


# counts
sum(inv$dbh_cm > 8, na.rm = TRUE)

# filter out
inv %>%
    select(species, subplot_id, tag, dbh_cm, health_status) %>%
    filter(dbh_cm > 8 & health_status == "L") %>% 
    data.frame() -> inv2

mor %>%
    select( subplot_id, tag, dbh_cm, health_status, fate) %>%
    filter(dbh_cm > 8 & health_status == "L") %>% 
    data.frame() -> mor2
# make factors
mor$subplot_id <- as.factor(mor$subplot_id)

# calc biomass
bio <- calc_biomass()
# filter out
bio %>%
    select( subplot_id, tag, dbh_cm, health_status, biomass) %>%
    filter(dbh_cm > 8 & health_status == "L") %>% 
    data.frame() -> bio2

df <- merge(mor2, bio2)
df <- merge(df, x)

# group by disturbance, treatment
df %>%
    group_by(disturbance_severity, treatment, fate) %>%
    summarize(total_biomass = sum(biomass, na.rm = TRUE)) %>%
    data.frame() -> plot.sums

plot.sums$biomass_mg <- round((plot.sums$total_biomass * 0.001), 2)
# make wide/
plot.sums %>%
    spread(key = fate, value = total_biomass) -> plot.wide

plot.wide$kill_mg <- round((plot.wide$kill * 0.001), 2)

plot.wide[is.na(plot.wide)] <- 0

# forta coloring
forte_pal <- forte_colors()
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")


x11(width = 7, height = 4)
p.bio <- ggplot(plot.sums, aes(x = as.factor(disturbance_severity), y = biomass_mg, fill = as.factor(disturbance_severity), alpha = fate))+
    geom_bar(stat = "identity", width = 0.75, color = "black")+
    #geom_text(aes(label = biomass_mg))+
    xlab("")+
    ylab("Biomass [Mg]")+
    try_theme()+
    scale_fill_manual(values = forte_pal,
                       name = "Disturbance Severity",
                           labels = c("0%", "45%", "65%", "85%"))+
    scale_alpha_ordinal(NULL, range = c(0.4, 1),
                        labels = c("Pre-Treatment", "Post-Treatment"))+
    facet_grid(.~treatment, labeller = labeller(treatment = facet.labs))
    







#### VAI
# bring in canopy structure
x <- fd_canopy_structure()


forte_pal <- forte_colors()


# bring in metadata via the plot_metadata() function

# bring in metadata via the plot_metadata() function
df <- fortedata::fd_plot_metadata()

# now we convert the tibble to a data frame
df <- data.frame(df)

# First we want to concatenate our replicate, plot and subplot data to make a subplot_id column 
df$subplot_id <- paste(df$replicate, 0, df$plot, df$subplot, sep = "")
df$subplot_id <- as.factor(df$subplot_id)

# Now that we have our data in the form for this analysis, let's filter our metadata to the subplot level.
df %>%
    dplyr::select(subplot_id, disturbance_severity, treatment) %>%
    dplyr::distinct() %>%
    data.frame() -> dis.meta.data

# this filters the metadata down to the subplot_id level
dis.meta.data <- dis.meta.data[c(1:32), ]

# Then we merge with the metadata from above
x <- merge(x, dis.meta.data)

# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")
#x11(width = 8, height = 4)
p.vai <- ggplot2::ggplot(x, aes(y = vai_mean, x = as.factor(disturbance_severity), fill = as.factor(disturbance_severity)))+
    geom_boxplot(color = "black")+
    geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.3)+
    xlab("Disturbance Severity")+
    ylab("VAI")+
    try_theme()+
    scale_color_manual(values = forte_pal, guide = FALSE)+
    scale_fill_manual(values = forte_pal,
                      name = "Disturbance Severity",
                      labels = c("0%", "45%", "65%", "85%"))+
    theme(legend.position = "none")+
    #ggplot2::ggtitle(paste("Vegetation Area Index (VAI)"))+
    # facet_grid(year ~ treatment, labeller = labeller(treatment = facet.labs)) 
    facet_grid(treatment ~ year, labeller = labeller(treatment = facet.labs)) 


require(cowplot)
x11()
plot_grid(
    p.vai, p.bio,
    nrow = 2,
    labels = "AUTO",
    label_size = 12,
    align = "v"
)
# 
# # sort
# x %>%
#     group_by(year, disturbance_severity, treatment) %>%
#     summarize(vai = mean(vai_mean, na.rm = TRUE),
#               vai.sd = sd(vai_mean, na.rm = TRUE),
#               count = n()) %>%
#     data.frame() -> z
# 
# # make SE
# z$vai.se = z$vai.sd / sqrt(z$count)
# 
# x11()
# ggplot2::ggplot(z, aes(y = vai, x = year, color = as.factor(disturbance_severity)))+
#     geom_line(size = 1)+
#     geom_point(size = 2)+
#     geom_errorbar(aes(ymin=vai-vai.se, ymax=vai+vai.se), width = .1)+ 
#     xlab("")+
#     ylab("VAI [m-3/m3]")+
#     theme_minimal()+
#     scale_color_manual(values = forte_pal,
#                        name = "Disturbance Severity",
#                        labels = c("0%", "45%", "65%", "85%"))+
#     theme(legend.position = "bottom")+
#     facet_grid(.~treatment, labeller = labeller(treatment = facet.labs))
# 
# 
