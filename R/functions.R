# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @title Dotchart 3
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
#' @references F. Bertrand, Ch. Derquenne, G. Dufrénot, F. Jawadi and M. Maumy, C. Borsenberger editor, \emph{Statistiques pour l’économie et la gestion}, De Boeck Supérieur, 2021.
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








#' @title plotcdf3
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
#' @references F. Bertrand, Ch. Derquenne, G. Dufrénot, F. Jawadi and M. Maumy, C. Borsenberger editor, \emph{Statistiques pour l’économie et la gestion}, De Boeck Supérieur, 2021.
#'
#' @examples
#' ff=table(cut(Europe$Partiel_H,c(0,10,20,30)),
#'          cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80)))/
#'          sum(table(cut(Europe$Partiel_H,c(0,10,20,30)),
#'                    cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80))))
#' plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),
#'          f=ff,xaxe="Hommes",yaxe="Femmes",theme="0")
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
