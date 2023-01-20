# Script for Kayla's Ecosystems paper

# need two plots:
# 1) 
require(fortedata)
require(ggplot2)
require(tidyverse)

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


labs(x = expression(Sepal~Length[cm]), y = expression(Petal~Length^cm))+
    labs(title = expression(Sepal~by~Petal~at~"20"*degree*C))
# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")
x11(width = 8, height = 4)
 ggplot2::ggplot(x, aes(y = vai_mean, x = as.factor(disturbance_severity), fill = as.factor(disturbance_severity), alpha = treatment))+
    geom_boxplot(color = "black")+
    geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.3)+
    xlab("Disturbance Severity")+
    ylab("VAI")+
    theme_minimal()+
    scale_color_manual(values = forte_pal, guide = FALSE)+
    scale_fill_manual(values = forte_pal,
                      name = "Disturbance Severity",
                      labels = c("0%", "45%", "65%", "85%"))+
    theme(legend.position = "none")+
    #ggplot2::ggtitle(paste("Vegetation Area Index (VAI)"))+
    # facet_grid(year ~ treatment, labeller = labeller(treatment = facet.labs)) 
    facet_grid(treatment ~ year) 



# sort
x %>%
    group_by(year, disturbance_severity, treatment) %>%
    summarize(vai = mean(vai_mean, na.rm = TRUE),
              vai.sd = sd(vai_mean, na.rm = TRUE),
              count = n()) %>%
    data.frame() -> z

# make SE
z$vai.se = z$vai.sd / sqrt(z$count)

ggplot( aes(x=date, y=value)) +
    geom_line() +
    geom_point()

x11()
ggplot2::ggplot(z, aes(y = vai, x = year, color = as.factor(disturbance_severity)))+
    geom_line(size = 1)+
    geom_point(size = 2)+
    geom_errorbar(aes(ymin=vai-vai.se, ymax=vai+vai.se), width = .1)+ 
    xlab("")+
    ylab("VAI [m-3/m3]")+
    theme_minimal()+
    scale_color_manual(values = forte_pal,
                       name = "Disturbance Severity",
                       labels = c("0%", "45%", "65%", "85%"))+
    theme(legend.position = "bottom")+
    facet_grid(.~treatment, labeller = labeller(treatment = facet.labs))


