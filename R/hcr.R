hcr.min.esc <- function(bt,
                        num.end.yrs,
                        ref.hr,
                        min.esc,
                        catch.cap,
                        bo = 1){
  bt <- bt[length(bt)]
  catch.lim <- 0
  bt <- bt / bo
  min.esc <- min.esc * bo

  if(bt <= min.esc){
    return(0)
  }

  if(bt > min.esc & bt <= min.esc/(1 - ref.hr) )
    catch.lim = bt - min.esc
  if(bt > min.esc / (1 - ref.hr) )
    catch.lim = ref.hr * bt
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
}

hcr.min.esc.slow <- function(bt,
                             num.end.yrs = 3,
                             ref.hr,
                             min.esc,
                             catch.cap,
                             bo = 1){
  ## Slow-up function, using a vector of the last num.end.yrs years from
  ## the spawning biomass vector. If you want to use future_bt3andolder
  ## then you'll have to recalculate it from the numbers at age
  ## and weight at age for the years of interest in the slow-up rule.

  bt <- bt[(length(bt) - num.end.yrs):length(bt)]
  catch.lim <- 0
  bt <- bt / bo
  min.esc <- min.esc * bo
  n.yrs <- length(bt)
  last.bt <- bt[n.yrs]
  bt.diff <- bt - min.esc

  if(any(bt.diff <= 0)){
    return(0)
  }

  if(last.bt > min.esc & last.bt <= min.esc/(1 - ref.hr)){
    catch.lim <- last.bt - min.esc
  }
  if(last.bt > min.esc / (1 - ref.hr)){
    catch.lim <- ref.hr * last.bt
  }
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
}

hcr.hs <- function(bt,
                   ref.hr,
                   lrp,
                   usr,
                   catch.cap,
                   bo = 1){
  bt <- bt[length(bt)]
  targ.hr <- 0
  dep <- bt / bo

  if(dep <= lrp){
    return(0)
  }

  if(dep > lrp & dep <= usr){
    targ.hr <- (dep - lrp) * ref.hr / (usr - lrp)
  }
  if(dep > usr){
    targ.hr <- ref.hr
  }
  catch.lim <- targ.hr * bt
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
}

hcr.hs.slow <- function(bt,
                        num.end.yrs = 3,
                        ref.hr,
                        lrp,
                        usr,
                        catch.cap,
                        bo = 1){

  bt <- bt[(length(bt) - num.end.yrs):length(bt)]
  targ.hr <- 0
  catch.lim <- 0
  n.yrs <- length(bt)
  last.bt <- bt[n.yrs]
  dep <- last.bt / bo
  bt.diff <- bt - lrp

  if(any(bt.diff <= 0)){
    return(0)
  }

  if(dep > lrp & dep <= usr){
    targ.hr <- (dep - lrp) * ref.hr / (usr - lrp)
  }
  if(dep > usr){
    targ.hr <- ref.hr
  }
  catch.lim <- targ.hr * last.bt
  if(catch.cap > 0 & catch.lim > catch.cap){
    catch.lim <- catch.cap
  }
  catch.lim
}
