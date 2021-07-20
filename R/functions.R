#' @title Dotchart de Cleveland améliorés (Enhanced Cleveland's dotchart)
#'
#' @description dotchart3 est une version améliorée des fonctions dotchart et dotchart2 qui permettent de construire des diagrammes à points de Cleveland.
#'
#' @name dotchart3
#'
#' @param x	soit un tableau ou une matrice de valeurs numériques (les `NA` sont autorisées). Si `x` est une matrice, le tracé global est constitué de points juxtaposés pour chaque ligne. Les entrées qui satisfont `is.numeric(x)` mais pas `is.vector(x) || is.matrix(x)` sont converties par `as.numeric`, avec un avertissement.
#' @param labels un vecteur d'étiquettes pour chaque point. Pour les vecteurs, la valeur par défaut est d'utiliser `names(x)` et pour les matrices, les étiquettes de ligne `dimnames(x)[[1]]`.
#' @param groups un facteur optionnel indiquant comment les éléments de `x` sont regroupés. Si `x` est une matrice, les groupes seront formés par défaut par les colonnes de `x`.
#' @param gdata les valeurs des données pour les groupes. Il s'agit généralement d'un résumé tel que la médiane ou la moyenne de chaque groupe.
#' @param cex la taille des caractères à utiliser. Fixer `cex` à une valeur inférieure à un peut être un moyen utile d'éviter le chevauchement des étiquettes. Contrairement à de nombreuses autres fonctions graphiques, cette fonction définit la taille réelle, et non un multiple de `par("cex")`.
#' @param pch le caractère ou le symbole de traçage à utiliser.
#' @param gpch le caractère ou le symbole de tracé à utiliser pour les valeurs de groupe.
#' @param bg 	la couleur de fond des caractères ou symboles à utiliser pour le tracé ; utilisez `par(bg= *)` pour définir la couleur de fond de l'ensemble du tracé.
#' @param color la (les) couleur(s) à utiliser pour les points et les étiquettes.
#' @param gcolor la couleur unique à utiliser pour les étiquettes et les valeurs de groupe.
#' @param lcolor la (les) couleur(s) à utiliser pour les lignes horizontales.
#' @param xlim 	largeur horizontale de la zone de tracé, voir `plot.window`, par exemple.
#' @param main titre général du graphique, voir `title`.
#' @param xlab les annotations de l'axe des abscisses définies comme dans `title`.
#' @param ylab les annotations de l'axe des ordonnées définies comme dans `title`.
#' @param cex.axis la taille des caractères à utiliser pour les annotations des axes.
#' @param ... les paramètres graphiques peuvent également être spécifiés comme arguments.
#'
#' @return Un dotplot de la série statistique.
#' @family plot functions
#' @author Frederic Bertrand, \email{frederic.bertrand@utt.fr}
#' @references F. Bertrand, Ch. Derquenne, G. Dufrénot, F. Jawadi and M. Maumy, C. Borsenberger editor, \emph{Statistiques pour l’économie et la gestion}, De Boeck Supérieur, Louvain-la-Neuve, 2021.
#'
#' @examples
#' data(Total_Secteur)
#' NameX <- Total_Secteur$NameX
#' Effectif <- Total_Secteur$Effectif
#' dotchart3(Effectif,labels=NameX,pch=19,col="#00FFFF",cex=1.6,cex.axis=1.2)
#' dotchart3(Effectif,labels=NameX,pch=19,col="#00FFFF")
#'
#' @export

dotchart3 <-
  function(x,
           labels = NULL,
           groups = NULL,
           gdata = NULL,
           cex = par("cex"),
           pch = 21,
           gpch = 21,
           bg = par("bg"),
           color = par("fg"),
           gcolor = par("fg"),
           lcolor = "gray",
           xlim = range(x[is.finite(x)]),
           main = NULL,
           xlab = NULL,
           ylab = NULL,
           cex.axis = cex,
           ...)
  {
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex.axis, yaxs = "i")
    if (!is.numeric(x))
      stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if (is.matrix(x)) {
      if (is.null(labels))
        labels <- rownames(x)
      if (is.null(labels))
        labels <- as.character(1L:nrow(x))
      labels <- rep(labels, length.out = n)
      if (is.null(groups))
        groups <- col(x, as.factor = TRUE)
      glabels <- levels(groups)
    }
    else {
      if (is.null(labels))
        labels <- names(x)
      glabels <- if (!is.null(groups))
        levels(groups)
    }
    plot.new()
    linch <- if (!is.null(labels))
      max(strwidth(labels, "inch"), na.rm = TRUE)
    else
      0
    if (is.null(glabels)) {
      ginch <- 0
      goffset <- 0
    }
    else {
      ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
      goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
      nmai <- par("mai")
      nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) +
        0.1
      par(mai = nmai)
    }
    if (is.null(groups)) {
      o <- 1L:n
      y <- o
      ylim <- c(0, n + 1)
    }
    else {
      o <- sort.list(as.numeric(groups), decreasing = TRUE)
      x <- x[o]
      groups <- groups[o]
      color <- rep(color, length.out = length(groups))[o]
      lcolor <- rep(lcolor, length.out = length(groups))[o]
      offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
      y <- 1L:n + 2 * offset
      ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
      linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
      loffset <- (linch + 0.1) / lheight
      labs <- labels[o]
      mtext(
        labs,
        side = 2,
        line = loffset,
        at = y,
        adj = 0,
        col = "black",
        las = 2,
        cex = cex.axis,
        ...
      )
    }
    abline(
      h = y,
      lty = "dotted",
      col = lcolor,
      lwd = 2
    )
    points(
      x,
      y,
      pch = pch,
      col = color,
      bg = bg,
      cex = cex
    )
    if (!is.null(groups)) {
      gpos <- rev(cumsum(rev(tapply(
        groups, groups, length
      )) +
        2) - 1)
      ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
      goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1) / lheight
      mtext(
        glabels,
        side = 2,
        line = goffset,
        at = gpos,
        adj = 0,
        col = gcolor,
        las = 2,
        cex = cex.axis,
        ...
      )
      if (!is.null(gdata)) {
        abline(h = gpos, lty = "dotted")
        points(gdata,
               gpos,
               pch = gpch,
               col = gcolor,
               bg = bg,
               ...)
      }
    }
    axis(1)
    box()
    title(main = main,
          xlab = xlab,
          ylab = ylab,
          ...)
    invisible(NULL)
  }








#' @title Stéréogramme avec plotcdf3 (Stereogram with plotcdf3)
#'
#' @description Cette fonction construit un stéréogramme permettant de juger de l'association entre deux variables discrètes ou groupées en classes.
#'
#' @name plotcdf3
#'
#' @param x	Valeurs observées ou modalités de la première variable discrète
#' @param y	Valeurs observées ou modalités de la seconde variable discrète
#' @param f Si f=0 (donc length(f)=0), x et y sont deux séries statistiques. Si length(f)>1, f est un tableau de fréquences et x et y les noms des lignes et des colonnes de f.
#' @param xaxe Nom de l'axe des abscisses
#' @param yaxe Nom de l'axe des ordonnées
#' @param col	Couleur du stéréogramme
#' @param border Le maillage du graphique doit-il être affiché ?
#' @param Nxy	Pas du maillage pour chaque axe
#' @param theme	Le thème détermine la palette de couleurs utilisées. Il y a quatre choix possibles en couleurs "0", "1", "2", "3" et un en nuances de gris "bw"
#'
#' @return Un stéréogramme des deux séries statistiques groupées ou des deux séries statistiques discrètes étudiées.
#' @family plot functions
#' @author Frederic Bertrand, \email{frederic.bertrand@utt.fr}
#' @references F. Bertrand, Ch. Derquenne, G. Dufrénot, F. Jawadi and M. Maumy, C. Borsenberger editor, \emph{Statistiques pour l’économie et la gestion}, De Boeck Supérieur, Louvain-la-Neuve, 2021.
#'
#' @examples
#' ff=table(cut(Europe$Partiel_H,c(0,10,20,30)),
#'          cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80)))/
#'          sum(table(cut(Europe$Partiel_H,c(0,10,20,30)),
#'                    cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80))))
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="0")
#'
#' \donttest{
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="1")
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="2")
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="cyan")
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="cyan",border=TRUE)
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="bw")
#' }
#'
#' xx=seq(1.5,12.5)
#' yy=seq(0.5,6.5)
#' p=c(1/36,0,0,0,0,0,
#'     2/36,0,0,0,0,0,
#'     2/36,1/36,0,0,0,0,
#'     2/36,2/36,0,0,0,0,
#'     2/36,2/36,1/36,0,0,0,
#'     2/36,2/36,2/36,0,0,0,
#'     0,2/36,2/36,1/36,0,0,
#'     0,0,2/36,2/36,0,0,
#'     0,0,0,2/36,1/36,0,
#'     0,0,0,0,2/36,0,
#'     0,0,0,0,0,1/36)
#' p=matrix(p,byrow=TRUE,ncol=6)
#' plotcdf3(xx,yy,p,"somme des des","valeur du plus petit")
#'
#' @export

plotcdf3 <-
  function(x,
            y,
            f,
            xaxe,
            yaxe,
            col = NULL,
            border = FALSE,
            Nxy = 200,
            theme = "0")
  {
    if (length(f) > 1) {
      xi = sort(x)
      yj = sort(y)
      k = length(x) - 1
      l = length(y) - 1
    }
    else {
      xi = as.numeric(levels(as.factor(x)))
      yj = as.numeric(levels(as.factor(y)))
      f = table(x, y)
      k = length(xi)
      l = length(yj)
    }
    if (sum(sum(f)) > 1) {
      f = f / sum(sum(f))
    }
    F = matrix(0, ncol = l, nrow = k)
    F[1,] = cumsum(f[1,])
    F[, 1] = cumsum(f[, 1])
    for (i in 2:k) {
      for (j in 2:l) {
        F[i, j] = f[i, j] + F[i - 1, j] + F[i, j - 1] - F[i -
                                                            1, j - 1]
      }
    }
    deltax = (max(xi) - min(xi)) / Nxy
    deltay = (max(yj) - min(yj)) / Nxy
    x = seq(min(xi) - deltax, max(xi) + deltax, deltax)
    y = seq(min(yj) - deltay, max(yj) + deltay, deltay)
    n1 = length(x)
    n2 = length(y)
    z = matrix(rep(0, n1 * n2), ncol = n2)
    for (i in 1:n1) {
      for (j in 1:n2) {
        i1 = (x[i] >= xi)
        i2 = (y[j] >= yj)
        if (sum(i1) == 0 | sum(i2) == 0) {
          z[i, j] = 0
        }
        if (sum(i1) >= k & sum(i2) >= l) {
          z[i, j] = 1
        }
        if (sum(i1) >= k & sum(i2) < l & sum(i2) > 0) {
          z[i, j] = F[k, sum(i2)]
        }
        if (sum(i1) < k & sum(i2) >= l & sum(i1) > 0) {
          z[i, j] = F[sum(i1), l]
        }
        if (sum(i1) < k & sum(i2) < l & sum(i1) > 0 & sum(i2) >
            0) {
          z[i, j] = F[sum(i1), sum(i2)]
        }
      }
    }
    if (is.null(col)) {
      nrz <- nrow(z)
      ncz <- ncol(z)
      jet.colors <- colorRampPalette(c("blue", "red"))
      if (theme == "1") {
        jet.colors <- colorRampPalette(c("#BDFF00", "#FF00BD",
                                         "#00BDFF"))
      }
      if (theme == "2") {
        jet.colors <- colorRampPalette(c("#FF8400", "#8400FF",
                                         "#00FF84"))
      }
      if (theme == "3") {
        jet.colors <- colorRampPalette(c("#84FF00", "#FF0084",
                                         "#0084FF"))
      }
      if (theme == "cyan") {
        jet.colors <- colorRampPalette(c("white", "cyan"))
      }
      if (theme == "bw") {
        jet.colors <- function(nbcols) {
          gray(seq(0.1, 0.9, length.out = nbcols))
        }
      }
      nbcol <- 100
      color <- jet.colors(nbcol)
      zfacet <- z[-1,-1] + z[-1,-ncz] + z[-nrz,-1] + z[-nrz,-ncz]
      facetcol <- cut(zfacet, nbcol)
      persp(
        x,
        y,
        z,
        theta = -30,
        phi = 15,
        col = color[facetcol],
        shade = 0.15,
        main = "St\u00e9r\u00e9ogramme des deux variables",
        xlab = xaxe,
        ylab = yaxe,
        zlab = "",
        cex.axis = 0.75,
        ticktype = "detailed",
        border = border
      )
    }
    else {
      persp(
        x,
        y,
        z,
        theta = -30,
        phi = 15,
        col = col,
        shade = 0.15,
        main = "St\u00e9r\u00e9ogramme des deux variables",
        xlab = xaxe,
        ylab = yaxe,
        zlab = "",
        cex.axis = 0.75,
        ticktype = "detailed",
        border = border
      )
    }
    invisible(list(
      F = F,
      z = z,
      x = x,
      y = y
    ))
  }

#' @title Diagrammes en radar avancés pour ggplot2 (Enhanced Radar Plots for ggplot2)
#'
#' @param plot.data dataframe comprising one row per group
#' @param base.size text size
#' @param font.radar text font family
#' @param values.radar values to print at minimum, 'average', and maximum gridlines
#' @param axis.labels  names of axis labels if other than column names supplied via plot.data
#' @param grid.min value at which mininum grid line is plotted
#' @param grid.mid value at which 'average' grid line is plotted
#' @param grid.max value at which maximum grid line is plotted
#' @param centre.y value of y at centre of plot
#' @param plot.extent.x.sf controls relative size of plot horizontally
#' @param plot.extent.y.sf controls relative size of plot vertically
#' @param x.centre.range controls axis label alignment
#' @param label.centre.y whether value of y at centre of plot should be labelled
#' @param grid.line.width width of gridline
#' @param gridline.min.linetype line type of minimum gridline
#' @param gridline.mid.linetype line type of 'average' gridline
#' @param gridline.max.linetype line type of maximum gridline
#' @param gridline.min.colour colour of minimum gridline
#' @param gridline.mid.colour colour of 'average' gridline
#' @param gridline.max.colour colour of maximum gridline
#' @param grid.label.size text size of gridline label
#' @param gridline.label.offset displacement to left/right of central vertical axis
#' @param label.gridline.min whether or not to label the mininum gridline
#' @param label.gridline.mid whether or not to label the 'mininum'average' gridline
#' @param label.gridline.max whether or not to label the maximum gridline
#' @param axis.label.offset vertical displacement of axis labels from maximum grid line, measured relative to circle diameter
#' @param axis.label.size text size of axis label
#' @param axis.line.colour colour of axis line
#' @param group.line.width line width of group
#' @param group.point.size point size of group
#' @param group.colours colour of group
#' @param background.circle.colour colour of background circle/radar
#' @param background.circle.transparency transparency of background circle/radar
#' @param plot.legend whether to include a plot legend
#' @param legend.title title of legend
#' @param plot.title title of radar plot
#' @param legend.text.size text size in legend
#' @param legend.position position of legend, valid values are "top", "right", "bottom", "left"
#'
#' @import ggplot2
#' @return a ggplot object
#'
#' @name ggradar
#'
#' @export
#'
#' @source
#' Most of the code is from \url{https://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html}.
#'
#' @examples
#' library(dplyr)
#' library(scales)
#' library(tibble)
#'
#' mtcars_radar <- mtcars %>%
#'   as_tibble(rownames = "group") %>%
#'   mutate_at(vars(-group), rescale) %>%
#'   tail(4) %>%
#'   select(1:10)
#' mtcars_radar
#' ggradar(mtcars_radar)
ggradar <- function(plot.data,
                    base.size = 15,
                    font.radar = "sans",
                    values.radar = c("0%", "50%", "100%"),
                    axis.labels = colnames(plot.data)[-1],
                    grid.min = 0, # 10,
                    grid.mid = 0.5, # 50,
                    grid.max = 1, # 100,
                    centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.max - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    gridline.min.linetype = "longdash",
                    gridline.mid.linetype = "longdash",
                    gridline.max.linetype = "longdash",
                    gridline.min.colour = "grey",
                    gridline.mid.colour = "#007A87",
                    gridline.max.colour = "grey",
                    grid.label.size = 6,
                    gridline.label.offset = -0.1 * (grid.max - centre.y),
                    label.gridline.min = TRUE,
                    label.gridline.mid = TRUE,
                    label.gridline.max = TRUE,
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = "grey",
                    group.line.width = 1.5,
                    group.point.size = 6,
                    group.colours = NULL,
                    background.circle.colour = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title = "",
                    plot.title = "",
                    legend.text.size = 14,
                    legend.position = "left") {

  plot.data <- as.data.frame(plot.data)

  if(!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
  }

  names(plot.data)[1] <- "group"

  var.names <- colnames(plot.data)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n

  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf

  # Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }
  if (max(plot.data[, -1]) > grid.max) {
    stop("'plot.data' contains value(s) > grid.max", call. = FALSE)
  }

  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
  # print(plot.data.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  # print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0, 0), grid.min + abs(centre.y), npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0, 0), grid.mid + abs(centre.y), npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0, 0), grid.max + abs(centre.y), npoints = 360)
  # print(head(gridline$max$path))
  # gridline labels
  gridline$min$label <- data.frame(
    x = gridline.label.offset, y = grid.min + abs(centre.y),
    text = as.character(grid.min)
  )
  gridline$max$label <- data.frame(
    x = gridline.label.offset, y = grid.max + abs(centre.y),
    text = as.character(grid.max)
  )
  gridline$mid$label <- data.frame(
    x = gridline.label.offset, y = grid.mid + abs(centre.y),
    text = as.character(grid.mid)
  )
  # print(gridline$min$label)
  # print(gridline$max$label)
  # print(gridline$mid$label)
  ### Start building up the radar plot

  # Declare 'theme_clear', with or without a plot legend as required by user
  # [default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size = base.size) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.key = element_rect(linetype = "blank")
    )

  if (plot.legend == FALSE) legend.position = "none"

  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]

  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar
    ) +
    scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))

  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base + geom_path(
    data = gridline$min$path, aes(x = x, y = y),
    lty = gridline.min.linetype, colour = gridline.min.colour, size = grid.line.width
  )
  base <- base + geom_path(
    data = gridline$mid$path, aes(x = x, y = y),
    lty = gridline.mid.linetype, colour = gridline.mid.colour, size = grid.line.width
  )
  base <- base + geom_path(
    data = gridline$max$path, aes(x = x, y = y),
    lty = gridline.max.linetype, colour = gridline.max.colour, size = grid.line.width
  )

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0.5, family = font.radar
  )
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, axis$label$x > x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0, family = font.radar
  )
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(
    data = gridline$max$path, aes(x, y),
    fill = background.circle.colour,
    alpha = background.circle.transparency
  )

  # + radial axes
  base <- base + geom_path(
    data = axis$path, aes(x = x, y = y, group = axis.no),
    colour = axis.line.colour
  )

  # ... + group (cluster) 'paths'
  base <- base + geom_path(
    data = group$path, aes(x = x, y = y, group = group, colour = group),
    size = group.line.width
  )

  # ... + group points (cluster data)
  base <- base + geom_point(data = group$path, aes(x = x, y = y, group = group, colour = group), size = group.point.size)

  # ... + amend Legend title
  if (plot.legend == TRUE) base <- base + labs(colour = legend.title, size = legend.text.size)

  # ... + grid-line labels (max; mid; min)
  if (label.gridline.min == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[1]), data = gridline$min$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.mid == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[2]), data = gridline$mid$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.max == TRUE) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[3]), data = gridline$max$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text), data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar)
  }

  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, 100)
  } else {
    colour_values <- rep(c(
      "#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051",
      "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"
    ), 100)
  }

  base <- base + theme(legend.key.width = unit(3, "line")) + theme(text = element_text(
    size = 20,
    family = font.radar
  )) +
    theme(legend.text = element_text(size = legend.text.size), legend.position = legend.position) +
    theme(legend.key.height = unit(2, "line")) +
    scale_colour_manual(values = colour_values) +
    theme(text = element_text(family = font.radar)) +
    theme(legend.title = element_blank())

  if(legend.title != "") {
    base <- base + theme(legend.title = element_text())
  }

  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }

  return(base)
}

#' @title Calcule les coordonnées des points d'un cercle (Generate circle coordinates)
#'
#' @description Generate coordinates to draw a circle.
#'
#' @param center coordinate for centroid
#' @param r radius
#' @param npoints number of coordinates to generate
#'
#' @return a dataframe
#' @source Adapted from Joran's response to \url{https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2}.
#' @export
#' @examples
#' funcCircleCoords(c(1,2),1)
#' plot(funcCircleCoords(c(1,2),1))
funcCircleCoords <- function(center = c(0, 0), r = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#' @title Calcule les trajectoires par groupe pour un diagramme en radar (Calculate Group Path)
#'
#' @description Converts variable values into a set of radial x-y coordinates
#'
#' @param df a dataframe with Col 1 is group ('unique' cluster / group ID of entity) and Col 2-n are  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
#'
#' @return a dataframe of the calculated axis paths
#'
#' @source Code adapted from a solution posted by Tony M to \url{https://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r}.
#' @export
#' @examples
#' library(dplyr)
#' library(scales)
#' library(tibble)
#'
#' mtcars_radar <- mtcars %>%
#'   as_tibble(rownames = "group") %>%
#'   mutate_at(vars(-group), rescale) %>%
#'   tail(4) %>%
#'   select(1:10)
#' plot.data <- as.data.frame(mtcars_radar)
#' if(!is.factor(plot.data[, 1])) {
#'   plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
#'   }
#' names(plot.data)[1] <- "group"
#' CalculateGroupPath(plot.data)
CalculateGroupPath <- function(df) {
  path <- df[, 1]

  ## find increment
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (ncol(df) - 1))
  ## create graph data frame
  graphData <- data.frame(seg = "", x = 0, y = 0)
  graphData <- graphData[-1, ]

  for (i in levels(path)) {
    pathData <- subset(df, df[, 1] == i)
    for (j in c(2:ncol(df))) {
      # pathData[,j]= pathData[,j]


      graphData <- rbind(graphData, data.frame(
        group = i,
        x = pathData[, j] * sin(angles[j - 1]),
        y = pathData[, j] * cos(angles[j - 1])
      ))
    }
    ## complete the path by repeating first pair of coords in the path
    graphData <- rbind(graphData, data.frame(
      group = i,
      x = pathData[, 2] * sin(angles[1]),
      y = pathData[, 2] * cos(angles[1])
    ))
  }
  # Make sure that name of first column matches that of input data (in case !="group")
  colnames(graphData)[1] <- colnames(df)[1]
  graphData$group <- factor(graphData$group, levels=levels(df[, 1]) ) # keep group order
  graphData # data frame returned by function
}

#' @title Calcule les trajectoires par axe pour un diagramme en radar (Calculate Axis Path)
#'
#' @description Calculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
#'
#' @param var.names list of variables to be plotted on radar plot
#' @param min MININUM value required for the plotted axes (same value will be applied to all axes)
#' @param max MAXIMUM value required for the plotted axes (same value will be applied to all axes)
#'
#' @return a dataframe of the calculated axis paths
#' @export
#' @examples
#' library(dplyr)
#' library(scales)
#' library(tibble)
#'
#' mtcars_radar <- mtcars %>%
#'   as_tibble(rownames = "group") %>%
#'   mutate_at(vars(-group), rescale) %>%
#'   tail(4) %>%
#'   select(1:10)
#' plot.data <- as.data.frame(mtcars_radar)
#' if(!is.factor(plot.data[, 1])) {
#'   plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
#'   }
#' names(plot.data)[1] <- "group"
#' var.names <- colnames(plot.data)[-1]
#' grid.min = 0
#' grid.max = 1
#' centre.y = grid.min - ((1 / 9) * (grid.max - grid.min))
#' CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
CalculateAxisPath <- function(var.names, min, max) {
  # var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  # Cacluate required number of angles (in radians)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  # calculate vectors of min and max x+y coords
  min.x <- min * sin(angles)
  min.y <- min * cos(angles)
  max.x <- max * sin(angles)
  max.y <- max * cos(angles)
  # Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i, min.x[i], min.y[i])
    b <- c(i, max.x[i], max.y[i])
    axisData <- rbind(axisData, a, b)
  }
  # Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no", "x", "y")
  rownames(axisData) <- seq(1:nrow(axisData))
  # Return calculated axis paths
  as.data.frame(axisData)
}
