`longcope_inverse` <- function(n=80){
    
    ## 'n' is an overall fineness for plots; 100 is a bit rough, use 1000
    ## for production quality
    
    ## define levels used for contours:
    
    perihelion <- seq(from=0.01,to=0.99,len=n)
    aphelion <- seq(from=1.01,to=6,len=n)
    
    periap <- expand.grid(perihelion,aphelion)
    vv <- t(apply(periap,1,f_inv))
    
    plot(NA,xlim=range(perihelion),ylim=range(aphelion),type="n",xlab='perihelion',ylab='aphelion',axes=FALSE)
    axis(1,pos=1)
    axis(2,pos=0)
    contour(perihelion,aphelion,matrix(vv[,1],n,n),col='black',add=TRUE)
    contour(perihelion,aphelion,matrix(vv[,2],n,n),col='red',add=TRUE,levels=(1:6)/5)

    ## scattering arc:
    theta <- seq(from=0,to=pi,len=n)
    vrel <- c(0.1,0.2,0.3)
    for(u in vrel){
        jj <- t(apply(cbind(1+u*cos(theta),u*sin(theta)),1,f_vec))
        points(jj,type="l",col="blue")
    }

    op <- par(bg="white")
    legend("topright",lty=1,
           col=c("red","black","blue"),
           legend=c(expression(v[phi]),expression(v[r]),"scattering"))
    par(op)
}


