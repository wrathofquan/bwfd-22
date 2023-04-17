get.e0 <- function(hx)
{
Hx = cumsum(hx) ## the cumulated sum; cumsum(1:3) gives [1] 1 3 6
lx = c(1, exp(-Hx)) ## we use 1 for l(0)
## for accuracy we sum person-years lived from x to x+ 1, known as Lx, rather than lx.
lxp1 = c(lx[-1],0) ## l(x+1)
Lx = (lx + lxp1)/2 ## Lx = [l(x) + l(x+1)] / 2
e0 = sum(Lx, na.rm = T)
return(e0)
}

get.lt <- function(hx)
{
    ## Warning: very simple approach, which assumes no one lives beyond last age-group
    ## Please don't use for real-life calculations.
    x = seq(hx)-1
    Hx = cumsum(hx) ## the cumulated sum; cumsum(1:3) gives [1] 1 3 6
    lx = c(1, exp(-Hx))[-(length(hx)+1)] ## we use 1 for l(0)
    ## for accuracy we sum person-years lived from x to x+ 1, known as Lx, rather than lx.
    lxp1 = c(lx[-1],0) ## l(x+1)
    Lx = (lx + lxp1)/2 ## Lx = [l(x) + l(x+1)] / 2
    Tx = rev(cumsum(rev(Lx)))
    dx = Lx * hx
    ex = Tx/lx
    return(data.table(x, hx, lx, Lx, dx, Tx, ex))
}
