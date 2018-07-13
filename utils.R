## Mapping a numeric vector to a continuous color scales using equidistant breaks
toColors_continuous=function (vector, palette = rev(c("#A50026", "#D73027", "#F46D43", 
    "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", 
    "#4575B4", "#313695")), maxLevels = 200, drop_quantiles = 0) 
{
    M = min(maxLevels, length(unique(vector)))
    if (drop_quantiles > 0) {
        q = quantile(vector, c(drop_quantiles, 1 - drop_quantiles))
        vector[vector <= q[1]] = q[1]
        vector[vector >= q[2]] = q[2]
    }
    levels = as.numeric(cut(vector, breaks = M))
    colorRampPalette(palette)(M)[levels]
}

## Aggregating a data.frame according to a categorical vector by applying a given function to each subset of the data.frame (e.g. the mean of each column for each subset to create a data.frame of data centroids)
aggregate_df=function (df, groups, fun = mean, margin = 1, ...) 
{
    if (margin == 2) {
        df = as.data.frame(t(df))
    }
    else {
        df = as.data.frame(df)
    }
    if (length(groups) != nrow(df)) {
        stop("Size of 'groups' vector is different that the one of the specified data margin")
    }
    df = split(df, groups)
    res = do.call(rbind, lapply(df, function(x) {
        apply(x, 2, fun, ...)
    }))
    if (margin == 2) {
        return(t(res))
    }
    else {
        return(res)
    }
}

## Loading the umap module from Python into R
library(reticulate)
use_python("/usr/bin/python3",TRUE) ## Set this to your python3 installation
umap_module=import("umap",convert=TRUE)

## Makes a color transparent. From https://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color

makeTransparent= function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

## Converts line to user coordinates. Credit : https://stackoverflow.com/questions/29125019/get-margin-line-locations-mgp-in-user-coordinates
line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(0:1, 'inches', 'user'))
  y_off <- diff(grconvertY(0:1, 'inches', 'user'))
  switch(side,
         `1` = par('usr')[3] - line * y_off * lh,
         `2` = par('usr')[1] - line * x_off * lh,
         `3` = par('usr')[4] + line * y_off * lh,
         `4` = par('usr')[2] + line * x_off * lh,
         stop("side must be 1, 2, 3, or 4", call.=FALSE))
}



