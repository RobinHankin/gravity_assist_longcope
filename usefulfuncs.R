f_peri <- function(v){ # v=c(v_phi,v_r); returns perihelion
    phi <- v[1]
    r <- v[2]
    out <- phi^2/(1+sqrt(1-phi^2*(2-r^2-phi^2)))
    out[out<0] <- Inf
    out[phi^2+r^2 > 3] <- Inf
    return(out)
}

f_ap <- function(v){ # v=c(v_phi,v_r); returns aphelion
    phi <- v[1]
    r <- v[2]
    out <- phi^2/(1-sqrt(1-phi^2*(2-r^2-phi^2)))
    out[out<0] <- Inf
    return(out)
}

f_vec <- function(v){c(f_peri(v),f_ap(v))}

f_inv <- function(peri_ap){  # inverts f_vec()
    e <- function(x){x^2}
    objective <- function(v){e(peri_ap[1]-f_peri(v)) + e(peri_ap[2]-f_ap(v))}

    optim_repeated(
        par=c(peri_ap[1],0.5),
#        par=c(0.51,0.51),
        fn=objective,
        n=10
    )$par
}

optim_repeated <- function(par,fn,n){
    out <- list()
    for(i in seq_len(n)){
        out[[i]] <- optim(par+rnorm(length(par),0,1e-4),fn)
    }
    v <- lapply(out,function(x){x$value})
    out[[which.min(v)]]
}
