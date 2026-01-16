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
years_x <- co2[4:17]
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
df_co2

### create BII dataframe
bii <- dplyr::filter(data, Region=="World", BIOscen=="BIO00", Variable=="Biodiversity|BII", GHGscen %in% ghgs)
years_x <- bii[8:26]

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

#lcols <- c("chocolate2", "dodgerblue4", "firebrick3", "darkorchid4")
#lcols <- c("firebrick1", "firebrick2", "firebrick3", "firebrick4")
lcols <- c("#ed322e", "#bd3123", "#8c2c18", "#5d240e")
#linetype=c("solid", "dashed", "dotted", "dotdash")

lw = 0.4
ts = 10
ts_sub = ts / 3.0

ghg_labels <-c("High", "Medium", "Low", "Baseline")

m0 <- ggplot(data=df_co2, aes(x=year, y=co2_cumu, group=ghg)) + 
  #geom_line(aes(color=ghg, linetype=ghg), linewidth=lw) +
  geom_line(aes(color=ghg), linewidth=lw) +
  #  ylim(0.77,0.82) + 
  ylab(expression(paste('Cumulative AFOLU CO' , ''[2],' Emissions (Gt 2020-2100)'))) +
  xlab("Year") + 
  #scale_linetype_manual(name=expression("Cumulative CO"[2]),labels=ghg_labels, values=c("solid", "dashed", "dotted", "dotdash")) +
  scale_color_manual(name="Climate Policy Stringency",labels=ghg_labels, values=lcols) +
  scale_x_continuous(limits = c(2020,2100)) + 
  #labs(title = "Biodiversity ") +
  #theme(legend.position = c(.15, .2)) +
  theme(legend.key.height = unit(0.005, "npc"), legend.key.width = unit(0.01, "npc")) +
  theme(text = element_text(size = ts, color="gray25")) +
  theme(legend.title = element_text(size=ts*0.8), legend.margin=margin(c(0.3,6,0.3,2))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(plot.margin = unit(c(0.05,0.1,0.05,0.05), "cm")) +
  #theme(axis.line=element_blank())+
  theme(panel.grid.major = element_line(color = 'white', linewidth = lw/2)) +
  theme(panel.border = element_blank()) + 
  theme(axis.ticks = element_line(linewidth = lw/3, color="gray9"))
m0
ggsave(paste0(figure_folder, "figure1_0.png"), m0)
m0 <- m0  + theme(legend.position = "none")
m0
#combinePlots()
ggsave(paste0(figure_folder, "figure1_0_no-legend.png"), m0)

m1 <- ggplot(data=a, aes(x=year, y=bii, group=ghg)) + 
  #geom_hline(yintercept = bii_years, color = "azure4", linetype = "dashed") +
  geom_segment(aes(x = 1995, xend = 2100, y = bii_years[1], yend = bii_years[1]), color = "azure3", linetype = "solid", linewidth = lw/2) +
  #geom_segment(aes(x = 2000, xend = 2100, y = bii_years[2], yend = bii_years[2]), color = "azure4", linetype = "dashed", linewidth = 0.5) +
  #geom_segment(aes(x = 2005, xend = 2100, y = bii_years[3], yend = bii_years[3]), color = "azure4", linetype = "dashed", linewidth = 0.5) + 
  #geom_segment(aes(x = 2010, xend = 2100, y = bii_years[4], yend = bii_years[4]), color = "azure4", linetype = "dashed", linewidth = 0.5) + 
  #geom_segment(aes(x = 2015, xend = 2100, y = bii_years[5], yend = bii_years[5]), color = "azure4", linetype = "dashed", linewidth = 0.5) + 
  geom_segment(aes(x = 2020, xend = 2100, y = bii_years[6], yend = bii_years[6]), color = "azure3", linetype = "solid", linewidth = lw/2) + 
  #geom_line(aes(color=ghg, linetype=ghg), linewidth=lw) +
  geom_line(aes(color=ghg), linewidth=lw) +
  #  ylim(0.77,0.82) + 
  xlim(1995,2100) + 
  ylab("BII (1)") +
  xlab("Year") + 
  #scale_linetype_manual(name=expression("Cumulative CO"[2]),labels=ghg_labels, values=c("solid", "dashed", "dotted", "dotdash")) +
  scale_color_manual(name=expression("Cumulative CO"[2]),labels=ghg_labels, values=lcols) +
  #labs(title = "Biodiversity ") +
  annotate("text", x=2095, y=bii_years[1]+0.005, label="BII equiv.", color = "azure4", size=ts_sub) +
  annotate("text", x=2095, y=bii_years[1]+0.002, label="1995", color = "azure4", size=ts_sub) +
  #annotate("text", x=2100, y=bii_years[2]+0.0005, label="2000", color = "azure4", size=5) +
  #annotate("text", x=2100, y=bii_years[2]+0.0005, label="2005", color = "azure4", size=5) +
  #annotate("text", x=2100, y=bii_years[3]-0.0005, label="2010", color = "azure4", size=5) +
  #annotate("text", x=2100, y=bii_years[4]-0.0005, label="2015", color = "azure4", size=5) +
  annotate("text", x=2095, y=bii_years[6]-0.002, label="2020", color = "azure4", size=ts_sub) + 
  #theme(legend.position = c(.15, .2)) +
  theme(legend.key.height = unit(0.005, "npc"), legend.key.width = unit(0.01, "npc")) +
  theme(text = element_text(size = ts, color="gray25")) +
  theme(legend.title = element_text(size=ts*0.8), legend.margin=margin(c(0.3,6,0.3,2))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(plot.margin = unit(c(0,0,0,0.05), "cm")) +
  #theme(axis.line=element_blank())+
  theme(panel.grid.major = element_line(color = 'white', linewidth = lw/2)) +
  theme(panel.border = element_blank()) + 
  theme(axis.ticks = element_line(linewidth = lw/3, color="gray9"))
mp_legend <- cowplot::get_legend(ggplotGrob(m1))
m1 <- m1  + theme(legend.position = "none")
#m1
ggsave(paste0(figure_folder, "figure1_1.png"), m1)

combinePlots()

m2 <- ggplot(data=a, aes(x=year, y=biiDelta, group=ghg)) + 
  #geom_line(aes(color=ghg, linetype=ghg), linewidth=0.8) +
  geom_line(aes(color=ghg), linewidth=0.8) +
  #  ylim(0.77,0.82) + 
  xlim(1995,2100) + 
  ylab("BII Change (%, rel. to 1995)") +
  xlab("Year") +
  #scale_linetype_manual(name="Cumulative CO2",labels=ghg_labels, values=c("solid", "dashed", "dotted", "dotdash")) +
  scale_color_manual(name="Cumulative CO2",labels=ghg_labels, values=lcols) +
  #scale_color_discrete(name="Cumulative CO2",labels=ghg_labels) +
  labs(title = "BII vs CO2 Emissions (0 US$2005/GJ)", tag = "A") +
  theme(text = element_text(size = ts, color="gray25")) +
  theme(axis.ticks = element_line(linewidth = lw/3, color="gray9"))
#m2


### land plots for different GHG scenarios
a <- c()
l <- 1
for (g in ghgs) {
  bii_t <- dplyr::filter(bii, GHGscen == g)
  bii_d <- dplyr::filter(biiDelta, GHGscen == g)
  for (y in colnames(years_x)) {
    o <- bii_t[,y]
    p <- bii_d[,y]
    
    a$year[l] <- as.numeric(gsub('X', '', y))
    a$ghg[l] <- g
    a$bii[l] <- round(as.numeric(o),4)
    a$biiDelta[l] <- round(as.numeric(p),2)
    
    l <- l+1
    
  }
}
a <- as.data.frame(a)


# Pick GHG000 - chocolate2, GHG050 - dodgerblue4, GHG400 - firebrick3, GHG4000 - darkorchid4
v_land <- c("Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Other Land", "Land Cover|Pasture", "Land Cover|Urban")

land <- dplyr::filter(data, Region=="World", BIOscen=="BIO00", GHGscen %in% ghgs, Variable %in% v_land)
land$varShort <- land$Variable
land$varShort  <- sub(".*Forest", "Forest", land$varShort)
land$varShort  <- sub(".*Other", "Other", land$varShort)
land$varShort  <- sub(".*Crop", "Crop", land$varShort)
land$varShort  <- sub(".*Pasture", "Pasture", land$varShort)
land$Variable <- land$varShort
#land
v_land <- c("Cropland", "Forest", "Pasture", "Other Land")
b <- c()
l <- 1
for (g in ghgs) {
  for (v in v_land) {
    land_t <- dplyr::filter(land, GHGscen == g, Variable == v)
    hist_t <- as.numeric(land_t$X1995)
    for (y in colnames(years_x)) {
      o <- land_t[,y]
      
      b$year[l] <- as.numeric(gsub('X', '', y))
      b$ghg[l] <- g
      b$variable[l] <- v
      b$area[l] <- as.numeric(o)
      b$area_rel[l] <- b$area[l] / hist_t
      l <- l+1
    }
  }
}
b <- as.data.frame(b)

sideplot1_line <- function(var) {
b_test <- b[b$variable == var,]
ggplot(data=b_test, aes(x=year, y=area, group=ghg)) + 
  #geom_line(aes(color=ghg, linetype=ghg), linewidth=lw) +
  geom_line(aes(color=ghg), linewidth=lw) +
  #  ylim(0.77,0.82) + 
  xlim(1995,2100) + 
  ylab("Land cover (Mha)") +
  xlab("Year") + 
  ggtitle(var) +
  #scale_linetype_manual(name=expression("A) Cumulative CO"[2]),labels=ghg_labels, values=c("solid", "dashed", "dotted", "dotdash")) +
  scale_color_manual(name=expression("A) Cumulative CO"[2]),labels=ghg_labels, values=lcols) +
  #theme(legend.position = c(.15, .2)) +
  theme(legend.key.height = unit(0.005, "npc"), legend.key.width = unit(0.01, "npc")) +
  theme(text = element_text(size = ts, color="gray25")) +
  theme(plot.title = element_text(size = ts * 0.8, vjust = -5, hjust = 0.1)) + 
  #theme(legend.title = element_text(size=ts*0.8), legend.margin=margin(c(0.3,6,0.3,2))) +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(plot.margin = unit(c(0,0,0,0.05), "cm")) +
  #theme(axis.line=element_blank())+
  theme(panel.grid.major = element_line(color = 'white', linewidth = lw/2)) +
  theme(panel.border = element_blank()) + 
  theme(axis.ticks = element_line(linewidth = lw/3, color="gray9"))
}

sl1 <- sideplot1_line(v_land[1])
sl2 <- sideplot1_line(v_land[2])
sl3 <- sideplot1_line(v_land[3])
sl4 <- sideplot1_line(v_land[4])
#combinePlots()
ylab <- get_plot_component(sl1,"ylab-l")
xlab <- get_plot_component(sl1,"xlab-b")

sl1 <- sl1 + theme(axis.text.x = element_blank(), axis.title = element_blank(), axis.ticks.x = element_blank()) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.00, 0.3), "cm"))
#ggsave("figure1_side1.png", sl1)
sl2 <- sl2 + theme(axis.text.x = element_blank(), axis.title = element_blank(), axis.ticks.x = element_blank())  +
  theme(plot.margin = unit(c(0.1, 0.1, 0.00, 0), "cm"))
#ggsave("figure1_side2.png", sl2)
sl3 <- sl3 + theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(-0.2, 0.1, 0.3, 0.3), "cm")) 
#ggsave("figure1_side3.png", p3)
sl4 <- sl4 + theme(axis.title = element_blank())  +
  theme(plot.margin = unit(c(-0.2, 0.1, 0.3, 0), "cm")) 
#ggsave("figure1_side4.png", p4)
combinePlots()


 ### BII delta maps

ncpath <- "Disagg data/BII00/GHG000/"
ncname <- "cell.bii_0.5"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
ncpath2 <- "Disagg data/BII00/GHG4000/"
ncfname2 <- paste(ncpath2, ncname, ".nc", sep="")
#plot_map(ncfname)

plot_map <- function(file, t = "") {
  dname <- "Variable"
  
  landmask <- nc_open(file)
  a <- ncvar_get(landmask,dname)
  nc_close(landmask)
  b <- a
  
  b[,,18] <- a[,,18]-a[,,1]
  c <- b[,,18]
  
  land_df
  land_df <- melt(c)
  land_df$Y <- (361-land_df$X2)
  
  myPalette <- colorRampPalette(brewer.pal(11, "RdBu"))
  sc <- scale_colour_gradientn(na.value = "transparent", colours = myPalette(100))
  
  ggplot(aes(x = X1, y = Y, fill = value), data = land_df) + 
    geom_raster() + 
    coord_equal() + 
    scale_fill_gradientn(name = expression("BII"~Delta~" 2020-2100"),
                         #paste0("BII ", expression(Delta), "2100 - 2020"), 
                         na.value = "gray60", colours = myPalette(9), limits = c(-0.4, 0.4)) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) + 
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank()) +
    theme(text = element_text(size = ts, color="gray25"),
          title = element_text(size = ts * 0.8),
          legend.title.position = "top", 
          legend.title = element_text(size=ts*0.8),
          legend.margin=margin(c(2,3,2,3)),
          legend.key.width = unit(.01, "npc"),
          legend.key.height = unit(.01, "npc"),
          #          legend.position = c(.5, 0.07),
          #          legend.position = "none",
          legend.direction = 'horizontal',
          plot.title = element_text(hjust = 0.03)) +
    ggtitle(t) 
}


m0000 <- plot_map(ncfname, "High cumulative emissions")
mlegend <- cowplot::get_legend(ggplotGrob(m0000))
m0000 <- m0000 + theme(legend.position="none")
#m0000
m4000 <- plot_map(ncfname2, "Negative cumulative emissions")
m4000 <- m4000 + theme(legend.position="none")
combinePlots()

combinePlots <- function() {
  label_color = "gray30"
  label_size = 5
  sub_label_size = 4
  
  w1 <- wrap_elements(get_plot_component(m2, "ylab-l"), ignore_tag = TRUE) +
    wrap_elements(get_y_axis(m2), ignore_tag = TRUE) +
    m1 +
    plot_layout(widths = c(0.7, 0.1, 30))
  #w1 + theme(plot.margin = unit(c( 0,0,0,0.5), "cm"))
  #w1 
  ggsave("figure1_main-new.png", w1)
  w2 <- plot_grid(sl1, sl2, sl3, sl4, ncol = 2, rel_widths = c(1, 0.9), rel_heights = c(0.9,1), label_x = c(0.2,0.12,0.2,0.12), label_y = c(0.9,0.9,1.03,1.03), labels = c("i","ii","iii","iv"), label_colour= label_color, label_size = sub_label_size)
  w2
  #w2 <- w2 + annotation_custom(sp_legend, xmax = 1, ymax = 1.05)
  w2 <- w2 + annotation_custom(xlab, xmax = 1.05, ymax = 0.07) + annotation_custom(ylab, xmin = 0.0, ymax = 1.1)
  #w2
  w3 <- plot_grid(m0, w1, w2, ncol = 3, rel_widths = c(0.5, 1.3, 1.2), labels = c('A', 'B', 'C'), label_size = label_size, label_colour= label_color)  
  #w3
  ggsave(filename = "figure1_combo.png", width = 30, w3)
  mar_s <- 1
  mar_t <- 0.2
  m0000 <- m0000 + theme(plot.margin = unit(c(mar_t,mar_s,0,0.1),"cm"))
  m4000 <- m4000 + theme(plot.margin = unit(c(mar_t,0.1,0,mar_s),"cm"))
  w4 <- plot_grid(m0000, m4000, ncol = 2, labels = c('i', 'ii'), label_size = sub_label_size, label_x = c(0.021, 0.12), label_y = 0.96, label_colour= label_color) 
  w4 <- w4 + theme(plot.margin=unit(c(0,0,0,0), "cm")) 
  w5 <- plot_grid(w3, w4, nrow = 2, labels = c('', 'D'), label_size = label_size, label_colour= label_color) #
  xpos <- 0.99
  w5 <- w5 + 
    annotation_custom(mp_legend, xmax = xpos+0.01, ymax = 0.7) + 
#    annotation_custom(sp_legend, xmax = xpos, ymax = 0.5) + 
    annotation_custom(mlegend, xmax = xpos+0.01, ymax = 0.4)
  
  
  ggsave(filename = "figure1_new_new_new.png", width = 7,  height = 3.6, w5)
  print(w5)
}
combinePlots()
w5


t_out[[1]][[2]] <- t_out[[1]][[2]] + plot_layout(tag_level = 'new')
t_out[[2]] <-t_out[[2]] + plot_layout(tag_level = 'new')
t_out <- t_out + plot_annotation(tag_levels = c('A', '1'), tag_sep	= '.')

### with tags

t1 <- w1
t2 <- ( p1 | p2 ) / (p3 | p4) + plot_layout(widths = c(1, 0.9), heights = c(1,1.1)) 
t2 <- wrap_elements(ylab, ignore_tag = TRUE) + t2 + plot_layout(widths = c(0.3, 29))
t2
#  annotation_custom(xlab, xmin = 0, xmax = 2100, ymin = 0, ymax = 0) + 
#  annotation_custom(ylab, xmin = 0, xmax = 2100, ymin = 0, ymax = 0)
#wrap_elements(ylab, ignore_tag = TRUE) + 
#wrap_elements(xlab, ignore_tag = TRUE) +
# t2 + plot_layout(widths = c(0.7, 0.5, 30))

#wrap_elements(ylab) + t2 + plot_layout(widths = c(0.7, 30))  # + wrap_elements(ylab(ylab))

#?wrap_elements

#wrap_elements(get_plot_component(m2, "ylab-l"), ignore_tag = TRUE) +
# wrap_elements(get_y_axis(m2), ignore_tag = TRUE) +
#m1 +
#plot_layout(widths = c(0.7, 0.1, 30))


mar_s <- 1
m0000 <- m0000 + theme(plot.margin = unit(c(0,mar_s,0,0),"cm"))
m4000 <- m4000 + theme(plot.margin = unit(c(0,0,0,mar_s),"cm"))
t3 <-  (m0000 | m4000) 
xpos = -250

t3 <- t3 + 
  annotation_custom(mp_legend, xmax = xpos, ymin = 300) +
  annotation_custom(sp_legend, xmax = xpos, ymin = 0) +
  annotation_custom(mlegend, xmax=xpos, ymin = -300) +
  annotation_custom(xlab, xmax=xpos+100, ymin = 150)
t3
t_out <- (t1 | t2) / t3 
#t_out

t_out[[1]][[2]] <- t_out[[1]][[2]] + plot_layout(tag_level = 'new')
t_out[[2]] <-t_out[[2]] + plot_layout(tag_level = 'new')
t_out <- t_out + plot_annotation(tag_levels = c('A', '1'), tag_sep	= '.')
ggsave(filename = "figure1_new_tags.png", width = 7,  height = 3.6, t_out)

