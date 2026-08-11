library(ggplot2)
library(dplyr)
library(glue)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df1 <- read.csv('/Users/wenggeiwong/Downloads/AllFISData_WithLeafIDs_WithFlux_2026.csv')
df1 <- df1 %>% mutate(IsoFlux = IsoFlux*5/1000)

df2 <- read.csv('/Users/wenggeiwong/Downloads/AllLicorData_WithLeafIDs_2026.csv')
df2 <- df2 %>% mutate(gsw = as.numeric(gsw))

rounds <- c('Jun9-Jun30')
sites <- c('HF','AA','BRF','NYBG')
leaf_names <- c('QURU1','QURU2','QURU3','QURU4','QURU5','QURU6')
temps <- c(25,30,35,40)

avgs_df <- tibble(
  SamplingRound = character(),
  Site = character(),
  LeafName = character(),
  TempSegment_FirstThreeMin = numeric(),
  A = numeric(),
  gsw = numeric(),
  Tair = numeric(),
  IsoFlux = numeric()
)

avgs_df_cols <- c('SamplingRound','Site','LeafName','TempSegment_FirstThreeMin','A','gsw','Tair', 'IsoFlux')



find_avgs <- function(sr, site, ln, temp){
  leaf_df1 <- df1 %>% filter(SamplingRound == sr, Site == site, LeafName == ln,TempSegment_FirstThreeMin == temp)
  leaf_df2 <- df2 %>% filter(SamplingRound ==sr, Site ==site, LeafName == ln,TempSegment_FirstThreeMin == temp)
  avgs <- list()

  for(val in avgs_df_cols[5:8]){
    if(val == 'IsoFlux'){
      cur_df <- leaf_df1
    }else{
      cur_df <- leaf_df2
    }
    cur_df <- cur_df %>% filter(!is.na(.data[[val]]))
    avg <- mean(cur_df[,val])
    # print(avg)
    avgs <- append(avgs,avg)
  }
  return(avgs)
}


fill_df <- function(){
  for(round in rounds){ # add here later as more sampling rounds introduced
    for(site in sites){
      for(leaf in leaf_names){
        for(temp in temps){
          avgs <- find_avgs(round,site,leaf,temp)
          avgs_df <- avgs_df %>% add_row(SamplingRound=round,Site=site,LeafName=leaf,TempSegment_FirstThreeMin=temp,A=avgs[[1]],gsw=avgs[[2]],Tair=avgs[[3]],IsoFlux=avgs[[4]])
        }
      }
    }
  }
  return(avgs_df)
}

find_max <- function(val){
  maxes <- tibble(
    SamplingRound = character(),
    Site = character(),
    LeafName = character(),
    Max = numeric()
  )
  
  for(round in rounds){
    for(site in sites){
      for(leaf in leaf_names){
        same_leaf <- avgs_df %>% filter(SamplingRound==round,Site==site,LeafName==leaf)
        max_val_for_temp <- max(same_leaf[,val])
        # getting temp for max value
        # print(max_val_for_temp)
        # same_leaf %>% filter(.data[[val]] == max_val_for_temp)
        # print(same_leaf)
        # temp <- same_leaf[[1,'TempSegment_FirstThreeMin']]
        maxes <- maxes %>% add_row(SamplingRound=round,Site=site,LeafName=leaf,Max=max_val_for_temp)
      }
    }
  }
  
  return(maxes)
}

find_ratios <- function(max_isoflux, max_a){
  ratio_df <- tibble(
    SamplingRound = character(),
    Site = character(),
    LeafName = character(),
    IsoFlux = numeric(),
    A = numeric(),
    Ratio = numeric()
  )

  for(round in rounds){
    for(site in sites){
      for(leaf in leaf_names){
        row1 <- max_isoflux %>% filter(SamplingRound == round, Site==site,LeafName==leaf)
        isoflux <- row1$Max
        
        row2 <- max_a %>% filter(SamplingRound == round, Site== site, LeafName==leaf)
        a <- row2$Max
        
        ratio <- isoflux/a
        ratio_df <- ratio_df %>% add_row(SamplingRound=round,Site=site,LeafName=leaf,IsoFlux=isoflux,A=a,Ratio=ratio)
      }
    }
  }
  return(ratio_df)
}

create_boxplot_w <- function(){
  # w/ outliers
  return(ggplot(data=ratio_df,mapping=aes(x=Site,y=Ratio)) + geom_boxplot(outlier.shape=NA) + geom_point(aes(color=LeafName), position=position_jitter(width=.2)))
  
}

create_boxplot_wo <- function(){
  # w/out outliers
  cleaned <- ratio_df %>% filter(
    Ratio > quantile(Ratio,0.25,na.rm=TRUE) - 1.5*IQR(Ratio,na.rm=TRUE),
    Ratio < quantile(Ratio,0.75,na.rm=TRUE) + 1.5*IQR(Ratio,na.rm=TRUE)
  )
  return(ggplot(data=cleaned,mapping=aes(x=Site,y=Ratio)) + geom_boxplot(outlier.shape=NA) + geom_point(aes(color=LeafName), position=position_jitter(width=.2)))
}

find_testing_time <- function(sr,site,ln){
  filtered_df <- df2 %>% filter(SamplingRound==sr,Site==site,LeafName==ln)
  testing_start_date <- filtered_df[1,'date']
  if(is.na(testing_start_date)){
    return("")
  }
  testing_start_time <- strsplit(testing_start_date,' ')[[1]][[2]]
  return(testing_start_time)
}

time_to_mins <- function(time_string){
  time_split <- strsplit(time_string,':')
  time_split <- time_split[[1]]
  hours <- as.numeric(time_split[[1]])
  minutes <- as.numeric(time_split[[2]])
  seconds <- as.numeric(time_split[[3]])
  total_mins <- (hours*60) + minutes + (seconds/60)
  return(total_mins)
}

find_lens <- function(){
  ratios_times_df <- read.csv('ratio_df_wtimes.csv')
  difference_times <- list()
  for(i in 1:nrow(ratios_times_df)){
    
    # don't find difference in samplingTime/testingTime is NA
    row_index <- which(is.na(ratios_times_df$SamplingTime))
    testing_time <- find_testing_time(ratios_times_df[i,'SamplingRound'],ratios_times_df[i,'Site'],ratios_times_df[i,'LeafName'])
    if(i!=row_index && testing_time!=''){
      sampling_time <- time_to_mins(ratios_times_df[i,'SamplingTime'])
      testing_time <- time_to_mins(testing_time)
      difference <- testing_time - sampling_time
      difference_times <- append(difference_times,difference)
    }else{
      difference_times <- append(difference_times,NA)
    }
  }
  
  ratios_times_df$differences <- difference_times
  return(ratios_times_df)
}

scatter_difference_a <- function(ratios_times_df){
  print(class(ratios_times_df[1,'differences']))
  pearson <- cor.test(unlist(ratios_times_df$differences),ratios_times_df$A,method='pearson',drop.na=TRUE)
  p_val <- pearson$p.value
  conf_int <- pearson$conf.int
  cor <- pearson$estimate
  print(glue("p-val: {p_val}, confidence int: {conf_int}, r: {cor}"))
  
  return(ggplot(data=ratios_times_df,mapping=aes(x=unlist(differences),y=IsoFlux,color=factor(LeafName))) + 
    geom_point() + geom_smooth(method='lm',se=FALSE,color='red'))
}

# avgs <- find_avgs('Jun9-Jun30','HF','QURU1',25)

avgs_df <- fill_df()
A_maxes <- find_max('A')
IsoFlux_maxes <- find_max('IsoFlux')
ratio_df <- find_ratios(IsoFlux_maxes,A_maxes)
create_boxplot_w()
create_boxplot_wo()
ratios_times_df <- find_lens()
scatter_difference_a(ratios_times_df)