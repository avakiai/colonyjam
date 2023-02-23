# pull out data from only the first ~7.5-8 minutes of each condition from only the first recording day
filter_firstexposure <- function(data_2proc) {
  # require(magritte)
  # require(dplyr)
  # requires variable space...
  if (exp=="exp_1") {
    dat1 <- data_2proc %>% 
      dplyr::filter(session == 1, as.numeric(file_no)==1) 
    # 8 Hz groups 1-2 were recorded in 1-minute intervals and numberings did not reset 
    # with recording block; this is quite ugly/hacky but works for the moment
    dat2 <- rbind(data_2proc %>%
                    dplyr::filter(modulation == 8, session == 1, group == 1, 
                                  condition == "full-band masker",
                                  as.numeric(file_no) %in% seq(66,73)),
                  data_2proc %>%
                    dplyr::filter(modulation == 8, session == 1, group == 1, 
                                  condition == "half-band masker",
                                  as.numeric(file_no) %in% seq(127,134)),
                  data_2proc %>%
                    dplyr::filter(modulation == 8, session == 1, group == 2, 
                                  condition == "full-band masker",
                                  as.numeric(file_no) %in% seq(63,70)),
                  data_2proc %>%
                    dplyr::filter(modulation == 8, session == 1, group == 2, 
                                  condition == "half-band masker",
                                  as.numeric(file_no) %in% seq(124,131)))
    data_init <- rbind(dat1,dat2)
  } else if (exp=="exp_2") {
    dat1 <- data_2proc %>% 
      dplyr::filter(session == 1, 
                    condition=="steady-state masker" |  condition=="silence") 
    dat2 <- data_2proc %>% 
      dplyr::filter(session == 1, condition=="random masker", as.numeric(file_no)==1, minute <=8)
    data_init <- rbind(dat1, dat2)
  }  
  return(data_init)
}