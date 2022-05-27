

split_into_3_tables <- function(baselineDT, include_STS = FALSE) {
  library(data.table)
  # make 3 different data tables
  baselineContinuous <-
    baselineDT[, c(
      "proc_date",
      "age",
      "height",
      "weight",
      "bmi",
      "bsa",
      "scoreii_log",
      "calc_sts",
      "creatinine",
      "gfr",
      "hb",
      "thrombo",
      "ck",
      "hrate",
      "tte_lvef",
      "tte_gradient_mean",
      "gradient_mean",
      "lvef"
    )]
  
  baselineContinuous$year <- year(baselineContinuous$proc_date)
  baselineContinuous <- baselineContinuous[,-1]
  
  # 0/1 booleans: chf_killip, dyspnea_nyha34, rf, rest 1/2 (2=no)
  baselineBoolean <-
    baselineDT[, c(
      "proc_date",
      "calc_sts",
      "sex",
      "diab",
      "dialysis",
      "hyper",
      "dyslip",
      "copd",
      "cerebro",
      "cerebro_tia",
      "cerebro_strokebl",
      "pacemaker",
      "defib",
      "valvulo",
      "cad",
      "pci",
      "mi",
      "ad",
      "csurgery",
      "dyspnea",
      "chf_killip",
      "ap",
      "syncope",
      "dyspnea_nyha34",
      "medi",
      "medi_asp",
      "medi_p2y12",
      "medi_clopido",
      "medi_prasu",
      "medi_tica",
      "medi_anticoagu",
      "medi_noac",
      "medi_oac_noac",
      "medi_dabigatran",
      "medi_apixaban",
      "medi_rivaroxaban",
      "medi_statin",
      "medi_ace",
      "medi_at",
      "medi_betablock",
      "medi_ca",
      "medi_diuretic",
      "medi_steroid",
      "medi_immuno",
      "medi_insulin",
      "medi_antidiab",
      "rf",
      "block",
      "tte",
      "tte_function",
      "tte_hyper",
      "tee",
      "angio",
      "anesthesia",
      "concom_balloon",
      "concom_closure",
      "concom",
      "concom_pci",
      "concom_carotid",
      "concom_ilio",
      "concom_other",
      "concom_access",
      "premeasure",
      "pvalvular",
      "cvalvular",
      "indeterminate"
    )]
  if (!include_STS) {
    baselineBoolean <- baselineBoolean[, -c("calc_sts")]
  }
  baselineBoolean$year <- year(baselineBoolean$proc_date)
  baselineBoolean <- baselineBoolean[,-1]
  baselineBoolean[baselineBoolean == 2] <- 0
  
  baselineFactor <-
    baselineDT[, c(
      "proc_date",
      "calc_sts",
      "dyspnea_nyha",
      "ap_ccs",
      "ap_ccs34",
      "medi_combi",
      "tropo_type",
      "rhythm",
      "delay",
      "regurg_aortic",
      "tte_regurg_aortic",
      "regurg_mitral",
      "tte_regurg_mitral",
      "regurg_tricuspid",
      "tte_regurg_tricuspid",
      "image",
      "proc_location",
      "proc_access",
      "proc_access_main",
      "device",
      "device_size",
      "valve_nb",
      "regurgitation"
    )]
  if (!include_STS) {
    baselineFactor <- baselineFactor[, -c("calc_sts")]
  }
  baselineFactor$year <- year(baselineFactor$proc_date)
  baselineFactor <- baselineFactor[,-1]
  baselineFactor <- baselineFactor[, lapply(.SD, as.factor)]
  return(list(baselineContinuous, baselineBoolean, baselineFactor))
}


calculate_means_per_year <-
  function(baselineContinuous, include_risklvl = FALSE) {
    library(dplyr)
    library(tidyr)
    if (include_risklvl) {
      meanPerYear <-
        baselineContinuous[, lapply(.SD, mean, na.rm = T), by = .(risk_level, year)]
      meanPerYear <-
        meanPerYear %>% gather(colnames(meanPerYear[,-c(1, 2)]), key = 'variable', value = 'mean')
      sdPerYear <-
        baselineContinuous[, lapply(.SD, sd, na.rm = T), by = .(risk_level, year)]
      sdPerYear <-
        sdPerYear %>% gather(colnames(sdPerYear[,-c(1, 2)]), key = 'variable', value = 'sd')
      meanPerYear <-
        merge(meanPerYear,
              sdPerYear,
              by = c('risk_level', 'variable', 'year'))
    } else{
      meanPerYear <-
        baselineContinuous[, lapply(.SD, mean, na.rm = T), by = year]
      meanPerYear <-
        meanPerYear %>% gather(colnames(meanPerYear[,-1]), key = 'variable', value = 'mean')
      sdPerYear <-
        baselineContinuous[, lapply(.SD, sd, na.rm = T), by = year]
      sdPerYear <-
        sdPerYear %>% gather(colnames(sdPerYear[,-1]), key = 'variable', value = 'sd')
      meanPerYear <-
        merge(meanPerYear, sdPerYear, by = c('variable', 'year'))
    }
    return(meanPerYear)
  }


calculate_percent_per_year <-
  function(baselineBoolean, include_risklvl = FALSE) {
    library(dplyr)
    library(tidyr)
    if (include_risklvl) {
      percentPerYear <-
        baselineBoolean[, lapply(.SD, function(x)
          sum(x, na.rm = T) / .N), by = .(risk_level, year)]
      namecol <- sapply(colnames(percentPerYear), function(x) {
        getName(baselineNames, x)
      })
      namecol <- unname(namecol)
      namecol[2] <- 'Year'
      namecol[1] <- 'RiskLevel'
      namecol[67] <- 'Low Risk'
      colnames(percentPerYear) <- namecol
      percentPerYear <-
        percentPerYear %>% gather(colnames(percentPerYear[, -c(1, 2)]),
                                  key = 'variable',
                                  value = 'percent')
    } else{
      percentPerYear <-
        baselineBoolean[, lapply(.SD, function(x)
          sum(x, na.rm = T) / .N), by = year]
      namecol <- sapply(colnames(percentPerYear), function(x) {
        getName(baselineNames, x)
      })
      namecol <- unname(namecol)
      namecol[1] <- 'Year'
      colnames(percentPerYear) <- namecol
      percentPerYear <-
        percentPerYear %>% gather(colnames(percentPerYear[,-1]),
                                  key = 'variable',
                                  value = 'percent')
    }
    return(percentPerYear)
  }

getName <- function(baseline_lookup, name) {
  return(baseline_lookup[name])
}

getFactorDistr <- function(baselinetable, column, columnname) {
  dt <- count(baselinetable, get(columnname))
  l <- levels(baselinetable$year)
  names(dt) <- c("x", "freq")
  dt <- dt[!is.na(dt$x),]
  if (nrow(dt) != nlevels(column)) {
    dt2 <- data.table(x = levels(column))
    dt2 <- merge(dt2, dt, by = 'x', all.x = T)
    dt2[is.na(dt2)] <- 0
    dt2 <- (dt2$freq / sum(dt2$freq)) * 100
  } else{
    dt2 <- (dt$freq / sum(dt$freq)) * 100
  }
  dt2 <-
    data.table(lvl = levels(column),
               percent = dt2,
               variable = columnname)
  return(dt2)
}

calculate_factor_percent_per_year <-
  function(baselineFactor, include_risklvl = FALSE) {
    if (include_risklvl) {
      percentFactorPerYear <-
        data.table(
          lvl = factor(),
          percent = double(),
          variable = character(),
          risk_level = factor(),
          year = factor()
        )
      for (col in colnames(baselineFactor)) {
        if (col != 'year' & col != 'risk_level') {
          dt <-
            baselineFactor[, getFactorDistr(.SD, get(col), col), by = .(risk_level, year)]
          percentFactorPerYear <- rbind(percentFactorPerYear, dt)
        }
        
      }
      ggList <-
        lapply(split(percentFactorPerYear, percentFactorPerYear$variable), function(i) {
          g <-
            ggplot(i, aes(
              x = year,
              y = percent,
              group = lvl,
              colour = lvl
            )) +
            geom_line() +
            scale_colour_manual(values = rep(
              c(
                "#1B9E77",
                "#D95F02",
                "#7570B3",
                "#E7298A",
                "#66A61E",
                "#E6AB02",
                "#A6761D",
                "#666666",
                "#E41A1C",
                "#377EB8",
                "#4DAF4A",
                "#984EA3",
                "#FF7F00"
              ),
              2
            )) +
            facet_wrap(~ risk_level, nrow = 1) +
            labs(x = 'Time in Years',
                 y = 'Percent of each category',
                 title = i$variable[1])
          theme_bw()
          
          return(g)
        })
      
      p <- plot_grid(
        plotlist = ggList,
        ncol = 3,
        nrow = 7,
        align = 'v',
        labels = levels(percentFactorPerYear$variable)
      )
    } else{
      percentFactorPerYear <-
        data.table(
          lvl = factor(),
          percent = double(),
          variable = character(),
          year = factor()
        )
      for (col in colnames(baselineFactor)) {
        if (col != 'year') {
          dt <-
            baselineFactor[, getFactorDistr(.SD, get(col), col), by = year]
          percentFactorPerYear <- rbind(percentFactorPerYear, dt)
        }
      }
      
      ggList <-
        lapply(split(percentFactorPerYear, percentFactorPerYear$variable), function(i) {
          ggplot(i,
                 aes(
                   x = year,
                   y = percent,
                   group = lvl,
                   colour = lvl,
                   fill = lvl
                 )) +
            geom_line() +
            scale_fill_manual(values = rep(
              c(
                'yellow',
                'orange',
                'red',
                'green',
                'blue',
                'pink',
                'lightgreen',
                'lightblue',
                'darkred',
                'white',
                'black',
                'purple',
                'grey'
              ),
              2
            )) +
            scale_colour_manual(values = rep(
              c(
                'yellow',
                'orange',
                'red',
                'green',
                'blue',
                'pink',
                'lightgreen',
                'lightblue',
                'darkred',
                'white',
                'black',
                'purple',
                'grey'
              ),
              2
            )) +
            labs(x = 'Time in Years',
                 y = 'Percent of each category',
                 title = i$variable)
        })
      p <- plot_grid(
        plotlist = ggList,
        ncol = 3,
        nrow = 7,
        align = 'v',
        labels = levels(percentFactorPerYear$variable)
      )
    }
    return(p)
  }

getName <- function(baseline_lookup, name) {
  return(baseline_lookup[name])
}

calculateRiskLevels <- function(sts_score) {
  if(is.na(sts_score)){
    sts_score <- median(baselineDT$calc_sts, na.rm = T)
  }
  if(sts_score <=4){
    return("low risk")
  }
  else if(sts_score<=8){
    return("intermediate risk")
  }
  else{
    return("high risk")
  }
}

calculateGradLVEFStrat <- function(gradient, lvef) {
  if (gradient <= 40 & lvef <= 50) {
    return("low_grad&low_lvef")
  } else if (gradient > 40 & lvef <= 50) {
    return("high_grad&low_lvef")
  } else if (gradient <= 40 & lvef > 50) {
    return("low_grad&high_lvef")
  } else{
    return("high_grad&high_lvef")
  }
}
