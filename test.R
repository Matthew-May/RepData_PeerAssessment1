
w=function(x) {
    if(is.weekend(x)){
        "weekend"
    } else
    {
        "weekday"
    }
}
weekday = sapply(d_impute$date,FUN = w)