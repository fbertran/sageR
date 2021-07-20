test_that("plotcdf3", {

  data(Europe)
  ff=table(cut(Europe$Partiel_H,c(0,10,20,30)),cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80)))/sum(table(cut(Europe$Partiel_H,c(0,10,20,30)),cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80))))
  plot <- plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),f=ff,xaxe="Hommes",yaxe="Femmes",theme="0")

  expect_that( length(plot), equals(4) )

})
