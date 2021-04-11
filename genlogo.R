install.packages("hexSticker")
library(hexSticker)
install.packages("UCSCXenaTools")
library(UCSCXenaTools)
install.packages("dplyr")
library(dplyr)
install.packages("ggpubr")
library(ggpubr)
# df =  XenaData %>%
#   dplyr::group_by(XenaHostNames) %>%
#   dplyr::summarise(count = n())
# p <- ggdotchart(
#   df,
#   x = "XenaHostNames",
#   y = "count",
#   color = "XenaHostNames",
#   palette = "jco",
#   sorting = "descending",
#   # Sort value in descending order
#   add = "segments",
#   # Add segments from y = 0 to dots
#   rotate = TRUE,
#   # Rotate vertically
#   dot.size = 2,
#   # Large dot size
#   label = round(df$count),
#   # Add mpg values as dot labels
#   font.label = list(
#     color = "white",
#     size = 2,
#     vjust = 0.5
#   ),
#   # Adjust label parameters
#   ggtheme = theme_void()
# ) +                       # ggplot2 theme
#   theme_transparent() + theme(legend.position = "none")
# p

library(sageR)
library(ggradar)
library(scales)
data(Europe)

Europe_rad <- Europe[, -1]
rownames(Europe_rad) <- Europe[, 1]

Europe_radar <- Europe_rad %>%
  as_tibble(rownames = "group") %>%
  mutate_at(vars(-group), rescale)
Europe_radar

p <- ggradar(
  Europe_radar,
  base.size = 10,
  axis.labels = rep("", 5),
  values.radar = "",
  #             background.circle.colour = "#D7D6D1",
  background.circle.transparency = 1,
  axis.line.colour = "black",
  background.circle.colour = "black",
  gridline.min.colour = "black",
  gridline.mid.colour = "black",
  gridline.max.colour = "black",
  grid.line.width = 1,
  legend.text.size = 10,
  plot.legend = FALSE,
  group.line.width = .5,
  group.point.size = .5
) +                       # ggplot2 theme
  theme_transparent() + theme(legend.position = "none")

p

data(Europe)
q <- GGally::ggpairs(Europe[, -1], progress = FALSE)


if (!("spatstat" %in% installed.packages())) {
  install.packages("spatstat")
}
library(spatstat)
if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2")
}
library(ggplot2)
source(
  "https://raw.githubusercontent.com/NicolasWoloszko/stat_ecdf_weighted/master/stat_ecdf_weighted.R"
)

colmagentas <- c("#FFFFFF", "#00FFFF", "#00FFFF", "#00FFFF")
xp <-
  data.frame(cbind(
    x = c(1, 7 / 3, 4, 13 / 2),
    p = c(1 / 6, 1 / 6, 1 / 6, 1 / 2),
    sp = c(1 / 6, 1 / 3, 1 / 2, 1)
  ))
q <-
  ggplot(aes(x = x), data = xp) + geom_point(
    aes(y = sp, col = "#FFFFFF"),
    data = xp,
    col = "#00FFFF",
    size = 1
  ) +
  geom_bar(
    aes(y = p),
    width = .025,
    stat = "identity",
    size = .5,
    col = "#FFFFFF"
  ) + xlim(0, 8) + ylab("Probabilité") +
  stat_ecdf(
    aes(x = x, weight = p),
    geom = "step",
    col = "#00FFFF",
    lty = 2,
    size = .5
  ) +
  geom_point(aes(y = p),
             data = xp,
             size = 1,
             col = "#FFFFFF") +
  #  annotate("text", x = .5, y = .95, label = paste(expression(p[X])), parse=TRUE) +
  #  annotate("text", x = .5, y = .875, label = paste(expression(F[X])), parse=TRUE,col="#00FFFF") +
  #  geom_segment(aes(x = 0, y = .95, xend = .3, yend = .95), data = xp,size=2)  +
  #  geom_segment(aes(x = 0, y = .90, xend = .3, yend = .90), data = xp, col="#00FFFF", lty=2,size=2) +
  theme_transparent() + theme(legend.position = "none") + bgcolor("black")




if (!("ggiraphExtra" %in% installed.packages())) {
  install.packages("ggiraphExtra")
}
library(ggiraphExtra)
data(Marque.Valeur)

data(Marque.Valeur)
str(Marque.Valeur)

Marque.Valeur$Marque <- as.factor(rep(paste("M", 1:3), rep(rep(30, 3))))
oav.Marque.Valeur <- aov(Valeur ~ Marque, data = Marque.Valeur)
resHSD <- TukeyHSD(oav.Marque.Valeur)
resHSD

r <-
  ggHSD(resHSD) + ylab("Differences entre les niveaux moyens de Marque") +
  ggtitle("Intervalles de confiance globale 95%") +
  theme_transparent() + theme(legend.position = "none")

data(AgevsProter_Canada_full)
AgevsProter_Canada <-
  AgevsProter_Canada_full[-nrow(AgevsProter_Canada_full), ]
AgevsProter_Canada
donnees = data.frame(
  Âge = c(
    AgevsProter_Canada$Terre.Neuve.et.Labrador,
    AgevsProter_Canada$Île.du.Prince.Édouard,
    AgevsProter_Canada$Nouvelle.Écosse,
    AgevsProter_Canada$Nouveau.Brunswick,
    AgevsProter_Canada$Québec,
    AgevsProter_Canada$Ontario,
    AgevsProter_Canada$Manitoba,
    AgevsProter_Canada$Saskatchewan,
    AgevsProter_Canada$Alberta,
    AgevsProter_Canada$Colombie.Britannique,
    AgevsProter_Canada$Yukon,
    AgevsProter_Canada$TerritoiresduNord.Ouest,
    AgevsProter_Canada$Nunavut
  ),
  Type = factor(
    c(
      rep("Terre Neuve et Labrador", nrow(AgevsProter_Canada)),
      rep("Île du Prince Édouard", nrow(AgevsProter_Canada)),
      rep("Nouvelle Écosse", nrow(AgevsProter_Canada)),
      rep("Nouveau Brunswick", nrow(AgevsProter_Canada)),
      rep("Québec", nrow(AgevsProter_Canada)),
      rep("Ontario", nrow(AgevsProter_Canada)),
      rep("Manitoba", nrow(AgevsProter_Canada)),
      rep("Saskatchewan", nrow(AgevsProter_Canada)),
      rep("Alberta", nrow(AgevsProter_Canada)),
      rep("Colombie Britannique", nrow(AgevsProter_Canada)),
      rep("Yukon", nrow(AgevsProter_Canada)),
      rep("Territoires du Nord Ouest", nrow(AgevsProter_Canada)),
      rep("Nunavut", nrow(AgevsProter_Canada))
    )
  )
)
nameXlit <- c(
  "Terre Neuve et Labrador",
  "Île du Prince Édouard",
  "Nouvelle Écosse",
  "Nouveau Brunswick",
  "Québec",
  "Ontario",
  "Manitoba",
  "\nSaskatchewan",
  "Alberta",
  "Colombie Britannique",
  "\nYukon",
  "\n\n\nTerritoires du Nord Ouest",
  "Nunavut"
)
lll = data.frame(Population = colSums(AgevsProter_Canada),
                 Lieu = colnames(AgevsProter_Canada))


s <- ggdotchart(
  lll,
  x = "Lieu",
  y = "Population",
  color = "Lieu",
  palette = "simpsons",
  sorting = "descending",
  # Sort value in descending order
  add = "segments",
  # Add segments from y = 0 to dots
  rotate = TRUE,
  # Rotate vertically
  dot.size = .5,
  # Large dot size
  label = round(lll$Population),
  # Add mpg values as dot labels
  font.label = list(
    color = "white",
    size = .5,
    vjust = 0.5
  ),
  # Adjust label parameters
  ggtheme = theme_void()
) +                       # ggplot2 theme
  theme_transparent() + theme(legend.position = "none")
s

r <- blankPlot <- ggplot() + geom_blank(aes(1, 1)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )

library(gridExtra)
pp <-
  grid.arrange(
    p,
    r,
    q,
    s,
    ncol = 2,
    nrow = 2,
    widths = c(1.2, 1),
    heights = c(1.2, 1),
    padding = 0
  )

sticker(
  pp,
  package = "sageR",
  p_size = 8,
  s_x = 0.9,
  s_y = 1,
  s_width = 1.7,
  s_height = 1.3,
  p_x = 1.30,
  p_y = 1.25,
  url = "https://cran.r-project.org/package=sageR",
  u_color = "white",
  u_size = 1.1,
  h_fill = "black",
  h_color = "grey",
  filename = "man/figures/logo.png"
)
