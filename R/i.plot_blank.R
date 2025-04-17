### blank square plot area -----------------------------------------------------
# func. by: jhheine@googlemail.com 
i.plot_blank <- function(gr=10){
  # gr <- 10
  # x.cent <- 0
  # y.cent <- 0
  plot.new()
  plot.window(xlim=c(-gr/2,gr/2), ylim=c(-gr/2,gr/2),asp=1)
}