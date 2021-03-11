test_that("Repeated root", {

  data(Europe)
  ff=table(cut(Europe$Partiel_H,c(0,10,20,30)),cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80)))/sum(table(cut(Europe$Partiel_H,c(0,10,20,30)),cut(Europe$Partiel_F,c(0,10,20,30,40,50,60,70,80))))
  plot <- plotcdf3(c(0,10,20,30),c(0,10,20,30,40,50,60,70,80),f=ff,xaxe="Hommes",yaxe="Femmes",theme="0")

  expect_that( length(plot), equals(4) )

#  expect_that( roots, equals(-3000) )

  # Test whether ABSOLUTE error is within 0.1
#  expect_that( roots, equals(-3000.01, tolerance  = 0.1) )

  # Test whether RELATIVE error is within 0.1
  # To test relative error, set 'scale' equal to expected value.
  # See base R function all.equal for optional argument documentation.
#  expect_equal( roots, -3001, tolerance  = 0.1, scale=-3001)
})
