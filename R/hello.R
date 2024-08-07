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

hello <- function() {
  print("Hello, world!")
}

alinha_regressao_yx <- function(xy) {
  x<-xy[,1]
  y<-xy[,2]
  mod_yx<-lm(x~y)
  #plot(y,x,asp=1)
  ca<-1
  co<-coefficients(mod_yx)[[2]]
  hip<-sqrt(ca^2+co^2)
  #theta<- acos(ca/hip)
  theta<- if(co>0){-acos(ca/hip)}else{acos(ca/hip)}
  xy_polares<-car2pol(x,y)
  novo_angulo_reg<-xy_polares$theta-theta
  xy<-pol2car(xy_polares$r,novo_angulo_reg)
  xy_novo<-pol2car_matrix(xy_polares$r, novo_angulo_reg)
  return( xy_novo)
}

#use_git()
