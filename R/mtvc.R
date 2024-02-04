#' (mtvc) Multiple Time Varying Covariates
#'
#' Restructure dataset into counting process format to model time varying variables
#' @param data Dataframe to be restructured. Has to be in wide format, with a line for each individual.
#' @param dates Name of the columns that contains dates that point out when the variables of interest change value.
#' If an individual does not experience the event of interest, then the respective date should
#' be either a missing value or the origin date.
#' @param complications Name of the columns that contain values of time varying covariates.
#' @param origin Day from which the function starts counting days to convert into dates.
#' @param start Date of first contact with the individual (i.e. first medical visit).
#' @param stop Date of death or last visit of the follow-up.
#' @param event Binary variable that indicates if the individual has experienced the event.
#' @return Dataset in counting process format.
#' @details
#' Time varying variables are covariates that might change during the follow-up,
#' so it is fundamental to apply the counting process structure to the data frame of
#' interest, in order to allocate properly the right amount of time that each patient
#' has contributed to the study in each health status.
#' @references
#' 1. F. W. Dekker, et al., Survival analysis: time-dependent effects and time-varying risk factors, Kidney International, Volume 74, Issue 8, 2008, Pages 994-997.
#' @examples
#' data(simwide)
#' cp.dataframe=mtvc(data=simwide,
#' origin='1970-01-01',
#' dates=c(FIRST_CHRONIC,FIRST_ACUTE,FIRST_RELAPSE),
#' complications=c(CHRONIC,ACUTE,RELAPSE),
#' start=DATETRAN,
#' stop=DLASTSE,
#' event=EVENT)
#' @export
mtvc=function(data,
             dates,
             origin='1970-01-01',
             start,
             stop,
             event,
             complications){
  #
  dtfrm=data %>%
    ungroup() %>%
    mutate(id=row_number()) %>%
    mutate_at(vars({{dates}}),~as.Date(ifelse(is.na(.),
                                              as.Date(origin),
                                              as.Date(.)
    )))
  # go from wide to long in order to order dates
  melted=gather(dtfrm,event,day,c({{start}},{{dates}})) %>%
    arrange(id,day) %>%
    group_by(id) %>%
    mutate(t=row_number()) %>%
    filter(day!=origin) %>%
    select(id,t,day) %>%
    mutate(tstart=day,
           tstop=lead(day))
  # merge with initial dataset
  merged=merge(dtfrm,melted,by='id',all.x = T) %>%
    group_by(id) %>%
    mutate(tstop=as.Date(ifelse(is.na(tstop),
                                as.Date({{stop}}),
                                as.Date(tstop))),
           time=as.numeric(tstop-tstart),
    )%>%
    filter(time!=0) %>%
    mutate(tevent=case_when(row_number()==n()~{{event}},
                            T~0)) %>%
    ungroup(id)
  # dates list
  dat.list=merged %>%
    select({{dates}}) %>%
    list()
  # comp list
  comp.list=merged %>%
    select({{complications}}) %>%
    list()
  # create df that contains results
  complication=matrix(NA,
                      nrow=dim(merged)[1],
                      ncol=dim(comp.list[[1]])[2])
  # match
  for(j in 1:dim(comp.list[[1]])[2]){
    for(i in 1:dim(comp.list[[1]])[1]){
      datecomp=as.numeric(dat.list[[1]][i,j])
      datest=as.numeric(merged$tstart[i])
      value=as.numeric(comp.list[[1]][i,j])
      complication[i,j]=ifelse(datecomp==datest,
                               value,
                               0)
    }
  }
  #
  baba=complication
  # lag
  for (j in 1:dim(complication)[2]){
    for(i in 2:dim(complication)[1])
      baba[i,j]=ifelse(merged$id[i]==merged$id[i-1] & baba[i-1,j]!=0,
                       baba[i-1,j],
                       baba[i,j])
  }
  # give names to variables
  baba=as.data.frame(baba)
  comp.names=c(names(comp.list[[1]]))
  for(i in 1:length(comp.names)){
    comp.names[i]=paste('tdep',tolower(comp.names[i]),sep='_')
  }
  names(baba)=comp.names
  # merge the two dataset
  example=as.data.frame(cbind(merged,baba))
  # add index for time
  example=example %>%
    group_by(id) %>%
    mutate(ind=row_number())
  # create start and stop from time
  for(i in 1:dim(example)[1]){
    # stop
    example[i,'stop']=ifelse(example[i,'ind']==1,
                             as.numeric(example[i,'tstop']-example[i,'tstart']),
                             example[i-1,'stop']+as.numeric(example[i,'tstop']-example[i,'tstart']))
    # start
    example[i,'start']=ifelse(example[i,'ind']==1,
                              0,
                              example[i-1,'stop'])

  }
  return(example)
}


