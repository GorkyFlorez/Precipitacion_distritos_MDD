#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
# Cargammos los SHp del Peru ---------------------------------------------------------------
Peru        <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
MD          = st_read("SHP/MDD_dis.shp")
MDD         <- st_transform(MDD,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
MDD_xy      <- cbind(MDD  , st_coordinates(st_centroid(MDD$geometry)))

elev        = get_elev_raster(MDD, z=10)
Poligo_alt  <- crop(elev, MDD)                           #   
Poligo_alt  <- Poligo_alt <- mask(Poligo_alt, MDD_xy )
plot(Poligo_alt)

slope       = terrain(Poligo_alt , opt = "slope") 
aspect      = terrain(Poligo_alt, opt = "aspect")
hill        = hillShade(slope, aspect, angle = 40, direction = 270)

Prec        <- getData("worldclim", var = "prec", res=0.5, lon=-74.8773, lat=-11.54012)
Prec_MDD    <- crop(Prec, MDD)
Prec_MDD    <- Prec_MDD <- mask(Prec_MDD,MDD)
PPAnual_MDD <- do.call("sum", unstack(Prec_MDD))
plot(PPAnual_MDD)

colores<- c('#9331dc', '#165dff', '#10aebe', '#00ffff', '#ffee21', '#f19e21', '#ff4223')

Madre_Dios   <- subset(MDD_xy, distrito_  == "Madre de Dios")
Dis_alt      <- crop(elev, Madre_Dios)                           #   
Dis_alt      <- Dis_alt <- mask(Dis_alt, Madre_Dios)
Dis_pre      <- crop(PPAnual_MDD, Madre_Dios)                           #   
Dis_pre      <- Dis_pre<- mask(Dis_pre, Madre_Dios)
slop         = terrain(Dis_alt , opt = "slope") 
aspec        = terrain(Dis_alt , opt = "aspect")
hil          = hillShade(slop, aspec, angle = 40, direction = 270)

Inambari         <- subset(MDD_xy, distrito_  == "Inambari")
Inambari_alt     <- crop(elev, Inambari)                           #   
Inambari_alt     <- Inambari_alt <- mask(Inambari_alt, Inambari)
Inambari_alt_pre <- crop(PPAnual_MDD, Inambari)                           #   
Inambari_alt_pre <- Inambari_alt_pre <- mask(Inambari_alt_pre , Inambari)
slo              = terrain(Inambari_alt , opt = "slope") 
aspe             = terrain(Inambari_alt , opt = "aspect")
hi               = hillShade(slo, aspe, angle = 40, direction = 270)
# Graficos ---------------------------------------------------------------

Mapa_MDD = tm_shape(hill) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE, alpha=0.8)+
  tm_shape(PPAnual_MDD) +
  tm_raster(alpha = 0.8, palette = colores ,n=10, style="cont",
            legend.show = T, title="Precipitacion \n(mm)")+
  tm_shape(MDD_xy)+
  tm_borders("white",lwd=2)+
  tm_text("distrito_",size = .7, col="black",shadow=TRUE,fontfamily = "serif",
          fontface = "bold",
          bg.color="white", bg.alpha=.25)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "white", color.dark = "lightsteelblue4", 
               position = c(.2, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.16, 0.75), text.color = "white")+
  tm_layout( title = "A)",
             bg.color="#022140", 
             legend.bg.color = "#416076",
             title.bg.color = "white",
             title.color  = "black",
             title.size = 1,
             legend.title.size=.8,
             legend.position = c(0.005,0.0050) , scale=0.70, legend.frame = T,
             fontface="bold",
             legend.text.color = "white",
             legend.title.color = "white",
             legend.text.fontfamily =  "serif",
             legend.format = c(text.align = "right", 
                               text.separator = "-"))+
  tm_credits("Precipitación Promedio  \nMadre de Dios con Relieve", position = c(.6, .8), col = "white", fontface="bold", size=2, fontfamily = "serif")+
  tm_credits("Data: https://www.worldclim.org \n#Aprende R desde Cero Para SIG \nIng. Gorky Florez Castillo", position = c(0.15, .04), col = "white", fontface="bold")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 1, position = c(0.90, 0.05))

colores2<- c('#9331dc', '#9331dc','#165dff', '#10aebe','#10aebe', '#00ffff', '#ffee21', '#f19e21', '#ff4223')

Madere_grafi = tm_shape(hil) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE, alpha=0.8)+
  tm_shape(Dis_pre) +
  tm_raster(alpha = 0.8, palette = colores2 ,n=10, style="cont",
            legend.show = T, title="Precipitacion \n(mm)")+
  tm_shape(Madre_Dios)+
  tm_borders("white",lwd=2)+
  tm_text("distrito_",size = .7, col="black",shadow=TRUE,fontfamily = "serif",
          fontface = "bold",
          bg.color="white", bg.alpha=.25)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "white", color.dark = "lightsteelblue4", 
               position = c(.6, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.66, 0.05), text.color = "white")+
  tm_layout( title = "B)",
             bg.color="#022140", 
             title.bg.color = "white",
             legend.bg.color = "#416076",
             title.color  = "black",
             title.size = 1,
             legend.title.size=.8,
             legend.position = c(0.005,0.60) , scale=0.70, legend.frame = T,
             fontface="bold",
             legend.text.color = "white",
             legend.title.color = "white",
             legend.text.fontfamily =  "serif",
             legend.format = c(text.align = "right", 
                               text.separator = "-"))

colores3<- c(  '#165dff','#10aebe', '#00ffff', '#ffee21','#ffee21','#f19e21', '#f19e21', '#ff4223')

Inambari_Map= tm_shape(hi) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE, alpha=0.8)+
  tm_shape(Inambari_alt_pre) +
  tm_raster(alpha = 0.8, palette = colores3 ,n=10, style="cont",
            legend.show = T, title="Precipitacion \n(mm)")+
  tm_shape(Inambari)+
  tm_borders("white",lwd=2)+
  tm_text("distrito_",size = .7, col="black",shadow=TRUE,fontfamily = "serif",
          fontface = "bold",
          bg.color="white", bg.alpha=.25)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "white", color.dark = "lightsteelblue4", 
               position = c(.2, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.01, 0.05), text.color = "white")+
  tm_layout( title = "C)",
             bg.color="#022140", 
             title.bg.color = "white",
             legend.bg.color = "#416076",
             title.color  = "black",
             title.size = 1,
             legend.title.size=.8,
             legend.position = c(0.9,0.001) , scale=0.70, legend.frame = T,
             fontface="bold",
             legend.text.color = "white",
             legend.title.color = "white",
             legend.text.fontfamily =  "serif",
             legend.format = c(text.align = "right", 
                               text.separator = "-"))
g1= tmap_grob(Mapa_MDD)
g2= tmap_grob(Madere_grafi)
g3= tmap_grob(Inambari_Map)

# Mapa final
library(cowplot)
im= ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(g1, width = 20, height = 20,x = 0.05, y = 0.05)+
  draw_plot(g2, width =9, height = 9 ,x = 20, y = 10)+
  draw_plot(g3, width = 8, height = 8 ,x = 20.5, y = 1)+
  theme(panel.background = element_rect(fill = "#022140"))+
  annotate(geom = "text", x = 24, y = 19, label = "Mapa elaborado en R y RStudio", 
           family="serif", color = "white", size = 3.5, face = "bold")

  # Exportacion
ggsave(plot = im ,"Mapa/Urubamba.png",
         units = "cm", width = 29,height = 21, dpi = 1300)# guardar grafico

