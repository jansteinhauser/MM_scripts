library(ggplot2)
library(tidyverse)
library(cowplot)
library(patchwork)
library(ggpattern)
library(stringr)
require(reshape)
library(RColorBrewer)
library(ncdf4)
library(plotly)

data_folder <- "/home/j/Documents/IIASA/MM-Emulator"
figure_folder <- paste(sep = "/", data_folder,"figures/")

prep_z <- function(k){
  bm <- dplyr::filter(k, Region=="World", GHGscen=="GHG000", Variable=="Primary Energy|Biomass")
  bm[c("Region" , "SSPscen", "GHGscen", "SDGscen", "X1995", "X2000", "X2005", "X2010", "X2015")] <- c(NULL)
  bm[,4:17] <- apply(bm[,4:17], 2, function(x) as.numeric(x))

  bv <- c()
  for (i in 1:7) {
    bv <- append(bv,bm[i,16])
  }
  print(bm)
  return(bv)
}

be_interpolate <- function(data) {
  o <- c()
  for (i in 1:length(x)) {
    start <- x[i]
    start_val <- data[i]
    goal <- start
    goal_val <- start_val
    if (!is.na(data[i+1])) {
      goal <- x[i+1]
      goal_val <- data[i+1]
    }
    
    delta_val <- 0
    if (goal != start) {
      delta_val <- (goal_val - start_val)/(goal-start)
    }
    new_val <- 0
    for (j in 0:(max(0,goal-start-1))) {
      new_val <- start_val + delta_val * j
      o <- c(o, new_val)
    }
  }
  return(o)
}

bi_interpolate <- function (dataS, dataT, n = 9) {
  mv <- rbind(dataS)
  for (m in 1:n) {
    t <- c()
    for (i in 1:length(dataS)) {
      p <- dataS[i]
      q <- dataT[i]
      r <- p + (q - p) / (n + 1) * m
      t <- c(t, r)
    }
    mv <- rbind(mv, t)
  }
  return(mv)
}

### Data prep

data00 <- read.csv(paste(sep = "/", data_folder,"magpie_input_MP00BI00nov23.csv"))
data70 <- read.csv(paste(sep = "/", data_folder,"magpie_input_MP00BI70nov23.csv"))
data74 <- read.csv(paste(sep = "/", data_folder,"magpie_input_MP00BI74nov23.csv"))
data78 <- read.csv(paste(sep = "/", data_folder,"magpie_input_MP00BI78nov23.csv"))

data_years <- data00
data_years[c("Region" , "SSPscen", "BIOscen", "SDGscen", "X1995", "X2000", "X2005", "X2010", "X2015")] <- c(NULL)
years_x <-data_years[4:16]

bv00 <- prep_z(data00)
bv70 <- prep_z(data70)
bv74 <- prep_z(data74)
bv78 <- prep_z(data78)

x = c(0,5,7,10,15,25,45)
y = c("Baseline","Low","Medium", "High")
z = rbind(bv00,bv70,bv74,bv78)

x_long <- 0:45
y_long <- 0:30

bv00_long <- be_interpolate(bv00)
bv70_long <- be_interpolate(bv70)
bv74_long <- be_interpolate(bv74)
bv78_long <- be_interpolate(bv78)

z_long <- rbind(
  bi_interpolate(bv00_long, bv70_long),
  bi_interpolate(bv70_long, bv74_long),
  bi_interpolate(bv74_long, bv78_long),
  bv78_long
)

### define chart designs

labels <-c("Baseline", "Low", "Medium" ,"High")
brewer_palette <- "PuOr"
lcols <- brewer.pal(4, brewer_palette)

lw = 1
ts = 30
ts_legend = ts * 0.6
text_color = "gray25"


#Statistical Review of World Energy 2023:
# 2022 Primary Energy Consumption 604 EJ
# 2022 Electricity Generation 29165.1 TWh ~ 105 EJ
# AR6 Scenarios Explorer
# 2100 C1 mean Biomass production 177 EJ

pe_level <- 604
eg_level <- 105
c1_level <- 177

plane <- matrix(pe_level, nrow = length(y), ncol = length(x))

m1 <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, color = 'white'),
    y = list(show = TRUE, color = 'white')),
  x = ~x,
  y = ~y,
  z = ~z,
  colorbar=list(
    title='EJ/yr'
  ),
  colors='PuOr',
  showscale=TRUE,
  )
m1
m1 <- m1 %>% layout(
  scene = list(
    xaxis = list(title = 'USD/GJ', autorange="reversed"),
    yaxis = list(title = 'Biodiversity conservation ambition'),
    zaxis = list(title = 'EJ/yr'),
    camera = list(eye = list(x = 2, y = 0.1, z = 0.3)),
    aspectratio = list(x = 1, y = 1.2, z = 0.8)
    ))
m1
m1 <- m1 %>% colorbar(
  orientation = "h"
)
m1

save_image(p = m1, file = "test.png") 

### Subplot 3: Biomass production maps
ncpathNo <- paste(sep = "/", data_folder,"Disagg data/BII00/BE45/")
ncpathHigh <- paste(sep = "/", data_folder,"Disagg data/BII78/BE45/") 
ncname <- "cell.land_split_0.5"  

# Plot a single delta map
plot_map <- function(file, t = "") {
  landmask <- nc_open(file)
  a <- ncvar_get(landmask,"crop_kbe_ir") + 
    ncvar_get(landmask,"crop_kbe_rf")
  nc_close(landmask)
  c <- a[,,18]
  land_df <- melt(c)
  land_df$Y <- (361-land_df$X2)
  
  myPalette <- colorRampPalette(brewer.pal(11, "RdBu"))
  sc <- scale_colour_gradientn(na.value = "transparent", colours = myPalette(100))

  ggplot(aes(x = X1, y = Y, fill = value), data = land_df) + 
    geom_raster() + 
    coord_equal() + 
    scale_fill_gradient2(name = "Bioenergy cropland (Mha per grid-cell)", na.value = "gray60", low = "white", mid = "firebrick3", high = "firebrick", midpoint = 0.13, limits = c(0, 0.25)) +
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
          legend.key.width = unit(.08, "npc"),
          legend.key.height = unit(.03, "npc"),
          legend.direction = 'horizontal',
          plot.title = element_text(hjust = 0.03)) +
    ggtitle(t) 
}
ncfNo <- paste(ncpathNo, ncname, ".nc", sep="")
ncfHigh <- paste(ncpathHigh, ncname, ".nc", sep="")
mapNo <- plot_map(ncfNo, "Baseline conservation ambition")
ncfNo
ggsave(paste0(figure_folder, "figure2_3_1_legend-bottom.png"), mapNo, width = 2515, height = 1950, units = c("px"))
mapHigh <- plot_map(ncfHigh, "High conservation ambition")
ggsave(paste0(figure_folder, "figure2_3_2_legend-bottom.png"), mapHigh, width = 2515, height = 1950, units = c("px"))
map_legend <- cowplot::get_legend(ggplotGrob(mapNo))
mapNo_none <- mapNo + theme(legend.position = "none")
mapHigh_none <- mapHigh + theme(legend.position = "none")

mar_s <- 0.2
mar_t <- 1.8
m3_1_none <- mapNo_none + theme(plot.margin = unit(c(mar_t,mar_s,0,0),"cm"))
m3_2_none <- mapHigh_none + theme(plot.margin = unit(c(mar_t,0,0,mar_s),"cm"))
w_bottom <- plot_grid(m3_1_none, m3_2_none, ncol = 2) +
  theme(
    plot.margin = unit(c(0.1,0.1,0.3,0.1), "cm"),
    plot.background = element_rect(fill = "white", color = "white")
    )
w_bottom
ggsave(paste0(figure_folder, "figure2_bottom.png"), w_bottom, width = 5500, height = 1500, units = c("px"))





### 

data_bii <- list(data00, data70, data78, data78)
data00[c("Region" , "SSPscen", "BIOscen", "SDGscen", "X1995", "X2000", "X2005", "X2010", "X2015")] <- c(NULL)
years_x <-data00[4:16]

data_bii[1][8:26]
bii_be <- c("BIO45", "BIO45", "BIO45", "BIO00")
bii_be_scen <- c("A", "B", "C", "D")
#bii_be_scen <- c("Tech", "Stop", "Sust", "Min")
#scen_names <- c("Min", "Stop", "Sust", "Tech")
scen_names <-  c("Technical potential", "Stop Loss", "Sustainable", "Mininmal")

years_x <- bii[8:26]

load_land_data <- function(land_v) {
  land_v <- paste0("Land Cover|", land_v)
  b <- c()
  l <- 1
  for (i in 1:length(data_bii)) {
    data_l <- data_bii[[i]]
    bii_be_l <- bii_be[i]
    bii_be_scen_l <- bii_be_scen[i]
    
    land_t <- dplyr::filter(data_l, Region=="World", BIOscen==bii_be_l, GHGscen == "GHG000", Variable == land_v)
    
    hist_t <- as.numeric(land_t$X1995)
    for (y in colnames(years_x)) {
      o <- land_t[,y]
      
      b$year[l] <- as.numeric(gsub('X', '', y))
      b$scen[l] <- bii_be_scen_l
      b$variable[l] <- land_v
      b$area[l] <- as.numeric(o)
      b$area_rel[l] <- b$area[l] / hist_t
      l <- l+1
      }
  }
  return(as.data.frame(b))
}

land_crop <- load_land_data("Cropland")
land_pasture <- load_land_data("Pasture")
land_forest <- load_land_data("Forest")
land_other <- load_land_data("Other Land")

lcols <- c("#ed322e", "#bd3123", "#8c2c18", "#5d240e")
sideplot2_line <- function(data, title) {
  b <- data
  ggplot(data=b, aes(x=year, y=area, group=scen)) + 
    geom_line(aes(color=scen), linewidth=lw) +
    #  ylim(0.77,0.82) + 
    xlim(1995,2100) + 
    ylab("Land cover (Mha)") +
    xlab("Year") + 
    ggtitle(title) +
    scale_color_manual(name=expression("Biomass production"), labels=scen_names,values=lcols) +
    #theme(legend.position = c(.15, .2)) +
    theme(legend.key.height = unit(0.005, "npc"), legend.key.width = unit(0.01, "npc")) +
    theme(text = element_text(size = ts, color="gray25")) +
    theme(plot.title = element_text(size = ts * 0.8, vjust = -3, hjust = 0.04)) + 
    #theme(legend.title = element_text(size=ts*0.8), legend.margin=margin(c(0.3,6,0.3,2))) +
#    theme(legend.position = "none") +
    theme(panel.background = element_rect(fill = 'gray95')) +
    theme(plot.margin = unit(c(0,0,0,0.05), "cm")) +
    #theme(axis.line=element_blank())+
    theme(panel.grid.major = element_line(color = 'white', linewidth = lw/2)) +
    theme(panel.border = element_blank()) + 
    theme(axis.ticks = element_line(linewidth = lw/3, color="gray9"))
}

sp1 <- sideplot2_line(land_crop, "Cropland")
sp1
#ggsave("figure2_side1.png", sp1)
sp2 <- sideplot2_line(land_pasture, "Pasture")
sp3 <- sideplot2_line(land_forest, "Forest")
sp4 <- sideplot2_line(land_other, "Other land")

ylab <- get_plot_component(sp1,"ylab-l")
xlab <- get_plot_component(sp1,"xlab-b")

label_color = "gray30"
label_size = 5
sub_label_size = 6

sp1 <- sp1 + theme(axis.text.x = element_blank(), axis.title = element_blank(), axis.ticks.x = element_blank()) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.00, 0.3), "cm"))
#sp1
sp_legend <- cowplot::get_legend(ggplotGrob(sp1))
sp1 <- sp1  + theme(legend.position = "none")

#ggsave("figure1_side1.png", sp1)
sp2 <- sp2 + theme(axis.text.x = element_blank(), axis.title = element_blank(), axis.ticks.x = element_blank())  +
  theme(plot.margin = unit(c(0.1, 0.1, 0.00, 0), "cm"), legend.position = "none")
#ggsave("figure1_side2.png", sp2)
sp3 <- sp3 + theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(-0.2, 0.1, 0.3, 0.3), "cm"), legend.position = "none") 
#ggsave("figure1_side3.png", p3)
sp4 <- sp4 + theme(axis.title = element_blank())  +
  theme(plot.margin = unit(c(-0.2, 0.1, 0.3, 0), "cm"), legend.position = "none") 

w2 <- plot_grid(sp1, sp2, sp3, sp4, ncol = 2, rel_widths = c(1, 0.9), rel_heights = c(0.9,1), label_x = c(0.09,0.06 ,0.09,0.06), label_y = c(0.95,0.95,1.02,1.02), labels = c("i","ii","iii","iv"), label_colour= label_color, label_size = sub_label_size)
w2
ggsave(filename = "figure2_land_new.png", width = 7,  height = 3.6, w2)

