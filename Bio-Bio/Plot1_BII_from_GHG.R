library(ggplot2)
library(tidyverse)
library(cowplot)
library(patchwork)
library(ggpattern)
library(stringr)
require(reshape)
library(RColorBrewer)
library(ncdf4)

data_folder <- "/home/j/Documents/IIASA/MM-Emulator"
data_file <-"magpie_input_MP00BI00nov23.csv"
figure_folder <- paste(sep = "/", data_folder,"figures/")
data <- read.csv(paste(sep = "/", data_folder,data_file))


### CO2 emission profiles
co2 <- dplyr::filter(data, Region=="World", BIOscen=="BIO00", Variable=="Emissions|CO2|AFOLU")
co2[c("Region" , "SSPscen", "BIOscen", "SDGscen", "X1995", "X2000", "X2005", "X2010", "X2015")] <- c(NULL)
co2[,4:17] <- apply(co2[,4:17], 2, function(x) as.numeric(x)) 

## cumulative emissions
co2_cumu <- co2
as.numeric(gsub('X', '', names(co2_cumu[1,4:17])))
for (i in 1:12) {
  c <- co2_cumu[i,4:17] * c(5,5,5,5,5,5,5,5,5,10,10,10,10,10) / 1000
  for (j in 2:14) 
  {
    c[,j] <- c[,j] + c[,j-1]
  }
  co2_cumu[i,4:17] <- c
}
co2_cumu$Unit <- "Gt CO2"
co2_cumu$Variable <- "Emissions|CO2|AFOLU|Cumulative"

# co2_cumu
# Pick four representative scenarios based on cumulative emissions by 2100:
# GHG000: 366 Gt, GHG050: 167 Gt, GHG400: 76 Gt, GHG4000: -86Gt, 

### use these scenarios for the plots to run other tests
years_x <- co2[4:16]
ghgs <- c("GHG000", "GHG050", "GHG400", "GHG4000")

### create emissions dataframe
df_co2 <- c()
l <- 1
for (g in ghgs) {
  c1 <- dplyr::filter(co2, GHGscen == g)
  c2 <- dplyr::filter(co2_cumu, GHGscen == g)
  for (y in colnames(years_x)) {
    o <- c1[,y]
    p <- c2[,y]
    
    df_co2$year[l] <- as.numeric(gsub('X', '', y))
    df_co2$ghg[l] <- g
    df_co2$co2[l] <- as.numeric(o)
    df_co2$co2_cumu[l] <- as.numeric(p)
    
    l <- l+1
  }
}
df_co2 <- as.data.frame(df_co2)

### create BII dataframe
bii <- dplyr::filter(data, Region=="World", BIOscen=="BIO00", Variable=="Biodiversity|BII", GHGscen %in% ghgs)
years_x <- bii[8:25]

bii_years <-  c(
  as.numeric(bii$X1995[1]),
  as.numeric(bii$X2000[1]),
  as.numeric(bii$X2005[1]),
  as.numeric(bii$X2010[1]),
  as.numeric(bii$X2015[1]),
  as.numeric(bii$X2020[1])
)

biiDelta <- bii
for (y in colnames(years_x)) { 
  for (m in 1:4) {
    biiDelta[y][m,1] <- (as.numeric(biiDelta[y][m,1])-as.numeric(bii$X1995[m]))/as.numeric(bii$X1995[m]) * 100
  }
}

df_bii <- c()
l <- 1
for (g in ghgs) {
  bii_t <- dplyr::filter(bii, GHGscen == g)
  bii_d <- dplyr::filter(biiDelta, GHGscen == g)
  for (y in colnames(years_x)) {
    o <- bii_t[,y]
    p <- bii_d[,y]
    
    df_bii$year[l] <- as.numeric(gsub('X', '', y))
    df_bii$ghg[l] <- g
    df_bii$bii[l] <- round(as.numeric(o),4)
    df_bii$biiDelta[l] <- round(as.numeric(p),2)
    
    l <- l+1
    
  }
}
df_bii <- as.data.frame(df_bii)

### define chart designs

ghg_labels <-c("Baseline", "Low", "Medium" ,"High")

#lcols <- c("chocolate2", "dodgerblue4", "firebrick3", "darkorchid4")
#lcols <- c("firebrick1", "firebrick2", "firebrick3", "firebrick4")
#lcols <- c("#ed322e", "#bd3123", "#8c2c18", "#5d240e")
#linetype=c("solid", "dashed", "dotted", "dotdash")
brewer_palette <- "PuOr"
lcols <- brewer.pal(4, brewer_palette)

lw = 1
ts = 30
ts_legend = ts * 0.6
text_color = "gray25"

### Subplot 1: Cumulative emissions
m0 <- ggplot(data=df_co2, aes(x=year, y=co2_cumu, group=ghg)) + 
  geom_line(
    aes(color=ghg), 
    linewidth=lw) +
  ylab(expression(paste('Cumulative AFOLU CO' , ''[2],' Emissions (Gt)'))) +
  xlab("Year") + 
  scale_color_manual(
    name="Climate Policy Stringency",
    labels=ghg_labels, 
    values=lcols) +
  scale_x_continuous(limits = c(2020,2110), breaks = scales::pretty_breaks(n = 3)) +
  theme(text = element_text(
    size = ts, 
    color=text_color)) +
  theme(
    legend.text = element_text(size=ts_legend),
    legend.title = element_text(size=ts_legend), 
    legend.margin=margin_auto(1),
    legend.position = "right",
    legend.key.height = unit(0.005, "npc"), 
    legend.key.width = unit(0.01, "npc")) +  
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank()) +
  #theme(axis.line=element_blank())+
  theme(
    axis.line = element_line(linewidth = lw/2, color=text_color),
    axis.ticks = element_line(linewidth = lw/2, color=text_color),
    axis.title = element_text(size = ts_legend),
    axis.title.y.left = element_text(margin = margin(r = 5)))
m0
#ggsave(paste0(figure_folder, "figure1_0_legend-right.png"), m0, width = 3000, height = 2000, units = c("px"))
ggsave(paste0(figure_folder, "figure1_0_legend-right.png"), m0)
m0_bottom <- m0  + 
  theme(legend.position = "bottom",
        legend.key.height = unit(0.02, "npc"), 
        legend.key.width = unit(0.03, "npc"),
        legend.key.spacing.x = unit(0.02, "npc"),
        legend.background = element_blank(),
        legend.title.position = "right",
        legend.title = element_text(margin = margin(l = 15)))
ggsave(paste0(figure_folder, "figure1_0_legend-bottom.png"), m0_bottom)
m0_none <- m0  + theme(legend.position = "none")
ggsave(paste0(figure_folder, "figure1_0_legend-none.png"), m0_none)
m0_legend <- cowplot::get_legend(ggplotGrob(m0_bottom))
m0_bottom

### Subplot 2: BII Co-Benefits of Climate Policies
ts_sub = ts * 0.25
marker_line_color = "azure3"
marker_text_color = "azure4"

m1 <- ggplot(data=df_bii, aes(x=year, y=bii, group=ghg)) + 
  geom_segment(aes(x = 1995, xend = 2100, y = bii_years[1], yend = bii_years[1]), color = marker_line_color, linetype = "solid", linewidth = lw/2) +
  geom_segment(aes(x = 2020, xend = 2100, y = bii_years[6], yend = bii_years[6]), color = marker_line_color, linetype = "solid", linewidth = lw/2) + 
  geom_line(aes(color=ghg), linewidth=lw) +
  #  ylim(0.77,0.82) + 
  scale_x_continuous(limits = c(1995,2110), breaks = scales::pretty_breaks(n = 4)) + 
  ylab("BII (1)") +
  xlab("Year") + 
  scale_color_manual(name="Climate Policy Stringency",labels=ghg_labels, values=lcols) +
  annotate("text", x=2094, y=bii_years[1]+0.004, label="BII equiv.", color = marker_text_color, size=ts_sub) +
  annotate("text", x=2095, y=bii_years[1]+0.0015, label="1995", color = marker_text_color, size=ts_sub) +
  annotate("text", x=2095, y=bii_years[6]-0.0015, label="2020", color = marker_text_color, size=ts_sub) + 
  theme(text = element_text(
    size = ts, 
    color=text_color)) +
  theme(
    legend.text = element_text(size=ts_legend),
    legend.title = element_text(size=ts_legend), 
    legend.margin=margin_auto(1),
    legend.position = "bottom",
    legend.key.height = unit(0.02, "npc"), 
    legend.key.width = unit(0.02, "npc")) +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank()) +
  theme(
    axis.line = element_line(linewidth = lw/2, color=text_color),
    #axis.line=element_blank()
    axis.ticks = element_line(linewidth = lw/2, color=text_color),
    axis.title = element_text(size = ts_legend),
    axis.title.y.left = element_text(margin = margin(r = 5)))
m1
ggsave(paste0(figure_folder, "figure1_1_legend-bottom.png"), m1)
m1_none <- m1  + theme(legend.position = "none")
ggsave(paste0(figure_folder, "figure1_1_legend-none.png"), m1_none)
#combinePlots()

### helper plot: Relative BII change
m1_relative <- ggplot(data=df_bii, aes(x=year, y=biiDelta, group=ghg)) + 
  geom_line(aes(color=ghg), linewidth=0.8) +
  xlim(1995,2110) + 
  ylab("BII Change (%, rel. to 1995)") +
  xlab("Year") +
  labs(title = "BII vs CO2 Emissions (0 US$2005/GJ)", tag = "A") +
  scale_color_manual(name="Climate Policy Stringency",labels=ghg_labels, values=lcols) +
  theme(text = element_text(
    size = ts, 
    color=text_color)) +
  theme(
    legend.text = element_text(size=ts_legend),
    legend.title = element_text(size=ts_legend), 
    legend.margin=margin_auto(1),
    legend.position = "bottom",
    legend.key.height = unit(0.02, "npc"), 
    legend.key.width = unit(0.02, "npc")) +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank()) +
  theme(
    #axis.line = element_line(linewidth = lw/2, color=text_color),
    axis.line=element_blank(),
    axis.ticks = element_line(linewidth = lw/2, color=text_color),
    axis.title = element_text(size = ts_legend),
    axis.title.y.left = element_text(margin = margin(r = 5)))
m1_relative

### Subplot(s) 3: land plots for different GHG scenarios

# filter land data
v_land <- c("Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Other Land", "Land Cover|Pasture", "Land Cover|Urban")

land <- dplyr::filter(data, Region=="World", BIOscen=="BIO00", GHGscen %in% ghgs, Variable %in% v_land)
land$varShort <- land$Variable
land$varShort  <- sub(".*Forest", "Forest", land$varShort)
land$varShort  <- sub(".*Other", "Other", land$varShort)
land$varShort  <- sub(".*Crop", "Crop", land$varShort)
land$varShort  <- sub(".*Pasture", "Pasture", land$varShort)
land$Variable <- land$varShort

v_land_short <- c("Cropland", "Forest", "Pasture", "Other Land")
# create land dataframe
df_land <- c()
l <- 1
for (g in ghgs) {
  for (v in v_land_short) {
    land_t <- dplyr::filter(land, GHGscen == g, Variable == v)
    hist_t <- as.numeric(land_t$X1995)
    for (y in colnames(years_x)) {
      o <- land_t[,y]
      
      df_land$year[l] <- as.numeric(gsub('X', '', y))
      df_land$ghg[l] <- g
      df_land$variable[l] <- v
      df_land$area[l] <- as.numeric(o)
      df_land$area_rel[l] <- df_land$area[l] / hist_t
      l <- l+1
    }
  }
}
df_land <- as.data.frame(df_land)
df_land

# Create single land variable side plot
sideplot1_line <- function(var) {
plot_data <- df_land[df_land$variable == var,]
ggplot(data=plot_data, aes(x=year, y=area, group=ghg)) + 
  geom_line(aes(color=ghg), linewidth=lw) +
  ylab("Land cover (Mha)") +
  xlab("Year") +
  scale_x_continuous(limits = c(1995,2110), breaks = scales::pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  ggtitle(var) +
  scale_color_manual(name=expression("A) Cumulative CO"[2]),labels=ghg_labels, values=lcols) +
  theme(
    text = element_text(
      size = ts, 
      color=text_color),
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(size = ts * 0.8, vjust = 0, hjust = 0.1),
    plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
    axis.line = element_line(linewidth = lw/2, color=text_color),
    axis.ticks = element_line(linewidth = lw/2, color=text_color),
    axis.title = element_text(size = ts_legend),
    axis.title.y.left = element_text(margin = margin(r = 5)))
}

sl1 <- sideplot1_line(v_land_short[1])
sl1
ggsave(paste0(figure_folder, "figure1_2-1.png"), sl1)
sl2 <- sideplot1_line(v_land_short[2])
ggsave(paste0(figure_folder, "figure1_2-2.png"), sl2)
sl3 <- sideplot1_line(v_land_short[3])
ggsave(paste0(figure_folder, "figure1_2-3.png"), sl3)
sl4 <- sideplot1_line(v_land_short[4])
ggsave(paste0(figure_folder, "figure1_2-4.png"), sl4)
ylab_sl <- get_plot_component(sl1,"ylab-l")
xlab_sl <- get_plot_component(sl1,"xlab-b")

# uppper left
sl1_comb <- sl1 + theme(
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y.left = element_blank(),
  plot.margin = unit(c(0.1, 0.1, 0.00, 0.3), "cm"))
sl1_comb
# uppper right
sl2_comb <- sl2 + theme(
  axis.text.x = element_blank(), 
  axis.title = element_blank(), 
  axis.ticks.x = element_blank(),  
  axis.line = element_blank(),
  axis.title.y.left = element_blank(),
  plot.margin = unit(c(0.1, 0.1, 0.00, 0), "cm"))
# lower left
#ggsave("figure1_side2.png", sl2)
sl3_comb <- sl3 + theme(
  axis.title = element_blank(),
  axis.title.y.left = element_blank(),
  plot.margin = unit(c(-0.2, 0.1, 0.3, 0.3), "cm")) 
sl3_comb
#ggsave("figure1_side3.png", p3)
sl4_comb <- sl4 + theme(
  axis.title = element_blank(),
  axis.title.y.left = element_blank(),
  axis.line.y = element_blank(),
  plot.margin = unit(c(-0.2, 0.1, 0.3, 0), "cm")) 
sl4_comb
#ggsave("figure1_side4.png", p4)
#combinePlots()


### BII delta maps

ncname <- "cell.bii_0.5"
ncpath_base <- "/Disagg data/BII00/GHG000/"
ncfname_base <- paste(data_folder, ncpath_base, ncname, ".nc", sep="")
ncpath_high <- "/Disagg data/BII00/GHG4000/"
ncfname_high <- paste(data_folder, ncpath_high, ncname, ".nc", sep="")


# Plot a single delta map
plot_map <- function(file, t = "") {
  dname <- "Variable"
  landmask <- nc_open(file)
  bii_nc <- ncvar_get(landmask,dname)
  nc_close(landmask)
  bii_delta <- bii_nc[,,18]-bii_nc[,,1]

  land_df <- melt(bii_delta)
  land_df$Y <- (361-land_df$X2)
  
  myPalette <- colorRampPalette(brewer.pal(11, "RdBu"))
  sc <- scale_colour_gradientn(na.value = "transparent", colours = myPalette(100))
  
  ggplot(aes(x = X1, y = Y, fill = value), data = land_df) + 
    geom_raster() + 
    coord_equal() + 
    scale_fill_gradientn(name = expression("BII"~Delta~" 2020-2100 (1)"),
                         #paste0("BII ", expression(Delta), "2100 - 2020"), 
                         na.value = "gray60", colours = myPalette(9), limits = c(-0.4, 0.4)) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) + 
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          text = element_text(
            size = ts, 
            color="gray25"),
          title = element_text(size = ts * 0.6),
          legend.text = element_text(size = ts_legend),
          legend.position = "bottom",
          legend.title.position = "top",
          legend.title = element_text(
            size=ts_legend,
            hjust = 0.5),
          legend.margin=margin(5,30,5,30),
          legend.key.width = unit(.05, "npc"),
          legend.key.height = unit(.03, "npc"),
          legend.direction = 'horizontal',
          plot.title = element_text(hjust = 0.03)) +
    ggtitle(t) 
}


m3_1 <- plot_map(ncfname_base, "Baseline")
ggsave(paste0(figure_folder, "figure1_3_1_legend-bottom.png"), m3_1)
m3_legend <- cowplot::get_legend(ggplotGrob(m3_1))
combinePlots()
m3_1_none <- m3_1 + theme(legend.position="none")

m3_2 <- plot_map(ncfname_high, "High Climate Policy Stringency")
ggsave(paste0(figure_folder, "figure1_3_2_legend-bottom.png"), m3_2)
m3_2_none <- m3_2 + theme(legend.position="none")
combinePlots()

### combine subplots

createW1 <- function() {
  w1 <- wrap_elements(get_plot_component(m1_relative, "ylab-l"), ignore_tag = TRUE) +
    wrap_elements(get_y_axis(m1_relative), ignore_tag = TRUE) +
    m1_none + 
    theme(plot.margin = unit(c( 0,0.2,0,0.2), "cm")) +
    plot_layout(widths = c(1.5, 1, 27)) 
  ggsave(paste0(figure_folder, "figure1_1_double.png"), w1)
  w1
}

createW2 <- function() {
  w2 <- plot_grid(sl1_comb, sl2_comb, sl3_comb, sl4_comb, 
                  ncol = 2, rel_widths = c(1, 1), rel_heights = c(0.9,1))
  w2 
  ggsave(paste0(figure_folder, "figure1_2_cluster.png"), w2)
  
  w2_labels <- w2 + 
    annotation_custom(xlab_sl, xmax = 1.08, ymax = 0.03) + annotation_custom(ylab_sl, xmin = -0.02, ymax = 1) + 
    theme(
      plot.margin = unit(c( 0.1,0.1,0.3,0.55), "cm"),
      plot.background = element_rect(fill = "white", color = "white"))
  w2_labels
  ggsave(paste0(figure_folder, "figure1_2_cluster-labels.png"), w2_labels)
  w2_labels
}

createW3 <- function() {
  mar_s <- 0.2
  mar_t <- 1.8
  m3_1_none <- m3_1_none + theme(plot.margin = unit(c(mar_t,mar_s,0,0),"cm"))
  m3_2_none <- m3_2_none + theme(plot.margin = unit(c(mar_t,0,0,mar_s),"cm"))
  w3 <- plot_grid(m3_1_none, m3_2_none, ncol = 2) + 
    theme(
      plot.margin = unit(c(0.1,0.1,0.3,0.1), "cm"),
      plot.background = element_rect(fill = "white", color = "white")) 
  w3
}

combinePlots <- function() {
  label_size = ts * 0.6
  
  # create upper half of plot
  w1 <- createW1()
  w2 <- createW2()
  w_up <-plot_grid(m0_none, w1, w2, 
                   ncol = 3, rel_widths = c(0.5,0.8,1), 
                   labels = "AUTO", label_size = label_size)
  ggsave(paste0(figure_folder, "figure1_top.png"), w_up, width = 5500, height = 2000, units = c("px"))
  
  # create lower half of plot
  w_down <-createW3()
  ggsave(paste0(figure_folder, "figure1_bottom.png"), w_down, width = 5500, height = 1500, units = c("px"))
  
  # combine halfs
  w <- plot_grid(w_up, w_down, nrow = 2, rel_heights = c(1,1),
                 labels = c('', 'D'), label_size = label_size, label_y = 0.94) 
  ggsave(paste0(figure_folder, "figure1_none.png"), w, width = 5500, height = 3450, units = c("px"))
  
  #add legends
  w_legends <- w + 
    annotation_custom(m0_legend, xmax = 1, ymax = 0.98) + 
    annotation_custom(m3_legend, xmax = 1, ymax = 0.1)
  ggsave(paste0(figure_folder, "figure1_final.png"), w_legends, width = 5500, height = 3450, units = c("px"))
  w_legends
}
