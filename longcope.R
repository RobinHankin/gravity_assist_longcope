`longcope` <- function(n=1000){

    ## 'n' is an overall fineness for plots; 100 is a bit rough, use 1000
    ## for production quality
    
    ## define levels used for contours;
    ## First the perihelion:
    levels_peri <- unique(sort(c(0.01*(1:10),0.1*(1:9),0.9+(1:9)/100))) 

    ## and now the aphelion:
    levels_ap <- unique(sort(c(1 + (1:10)/100,1+(1:10)/10,3:10,30)))

    ## range for vx == v_phi and vy==v_r:
    vx <- seq(from=0,to=sqrt(2),len=n)
    vy <- seq(from=0,to=sqrt(2),len=n)
    v <- expand.grid(vx,vy)
    
    perihelion <- apply(v,1,f_peri) 
    aphelion <- apply(v,1,f_ap)
    ## plot commands start
    plot(0:1, type = "n", asp=1, axes=FALSE,
         xlim=c(0,sqrt(2)),ylim=c(0,sqrt(2)),
         xlab=expression(v[phi]),ylab=expression(v[r]),
         main="Longcope velocity-space map of Keplerian orbits"
         )

    axis(1,pos=0,at=seq(from=0,by=0.2,to=1.4))
    axis(2,pos=0)
    contour(vx,vy,matrix(aphelion,n,n), levels=levels_ap,add=TRUE,lwd=0.5)
    contour(vx,vy,matrix(perihelion,n,n),add=TRUE,col='red',levels=levels_peri,lwd=0.5)
    
    theta <- seq(from=0,to=pi,len=n)
    vrel <- 0.2
    points(1+vrel*cos(theta),vrel*sin(theta),type="l",col='blue')
    points(sqrt(2)*cos(theta/2),sqrt(2)*sin(theta/2),type="l",col='green')
    points(1,0,pch=16,cex=2)
    
    legend("topright",lty=1,
           col=c("red","black","blue","green"),
           legend=c("perihelion","aphelion","scattering","escape"))
    
}  # function longcope() ends
