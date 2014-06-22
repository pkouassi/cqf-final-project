#Trapezium rule / integration
 integral_trapezium_rule = function(f,x0,xn,N) {
  dx = (xn-x0)/N
  
  #0.5 * dx * ((y0 + yn) + 2 (y1 + y2 + ... + yn-1))
  result = 0
  
  #yo / yn
  #cat("y0:",f(x0),"\n")
  #cat("yn:",f(xn),"\n")
  result = result + 0.5 * f(x0) + 0.5 * f(xn)
  
  #y1 ... yn-1
  for (i in seq(1,N-1)) {
    #cat("y(",x0 + i*dx,"):",f(x0 + i*dx),"\n")
    result = result + f(x0 + i*dx)  
  }
  
  #multiply by dx
  result = result * dx
  
  return(result)
}

 