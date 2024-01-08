
#****************************************************************************************************
# basic  functionsd
#****************************************************************************************************
# creat function round for bin plot
mround <- function(x,base){ 
  base*round(x/base) 
}

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

# read csv and xlsx
read_file <- function(x) read.csv(file.path(DATA_DIR, x), comment.char = "#", stringsAsFactors = FALSE)
read_xlsx <- function(x, n) read_excel(file.path(DATA_DIR, x), sheet = n)
# write csv 
writ_file <- function(input, output) write.csv(input, file.path(OUT_DIR, output), row.names = FALSE)

save_agu_plot <- function(fn, plot = last_plot(), ...) {
  if(!dir.exists(plot_dir)) dir.create(plot_dir)
  suppressMessages(ggsave(file.path(plot_dir, fn), plot, ...))
}



#****************************************************************************************************
# functions for longterm soil respiration data: whether we can detect the Q10 change in site scale
#****************************************************************************************************
# sdata = longterm
# i = 10
longtern_test <- function (sdata_rs, sdata_tm) {
  dat <- data.frame()
  for (i in 1:nrow(sdata_rs)) {
    StudyID <- sdata_rs$SRDB_study[i]
    ID <- sdata_rs$SiteID[i]
    
    # mk for annual_rs
    y <- sdata_rs[i, c(which(colnames(sdata_rs) == "X1"): which(colnames(sdata_rs) == "X26"))]
    y %>% 
      as.data.frame() %>%
      tidyr::gather() %>% 
      na.omit() ->
      y
    n <- y %>% nrow()
    mk <- MannKendall(na.exclude(y$value))
    tau <- mk$tau
    p <- mk$sl
    
    # mk for temperature
    y_tm <- sdata_tm[i, c(which(colnames(sdata_tm) == "X1"): which(colnames(sdata_tm) == "X26"))]
    y_tm %>% 
      as.data.frame() %>%
      tidyr::gather() %>% 
      na.omit() ->
      y_tm
    n_tm <- y_tm %>% nrow()
    mk_tm <- MannKendall(na.exclude(y_tm$value))
    tau_tm <- mk_tm$tau
    p_tm <- mk_tm$sl 
    
    # put all results together
    print(paste0("*****", i))
    dat <- rbind( dat, data.frame(StudyID, ID, tau, p, n, tau_tm, p_tm, n_tm) )
  }
  return(dat)
}

# linear regression
# sdata_rs = longterm
# sdata_tm = longterm_tm_del
# i = 10
longtern_lm <- function (sdata_rs, sdata_tm) {
  dat <- data.frame()
  for (i in 1:nrow(sdata_rs)) {
    StudyID <- sdata_rs$SRDB_study[i]
    ID <- sdata_rs$SiteID[i]
    
    # mk for annual_rs
    y <- sdata_rs[i, c(which(colnames(sdata_rs) == "X1"): which(colnames(sdata_rs) == "X26"))]
    y %>% 
      as.data.frame() %>%
      tidyr::gather() %>% 
      na.omit() -> y
    
    y %>% 
      mutate(yr = c(1:nrow(y))) -> y
    
    n <- y %>% nrow()
    # linear regression
    first_lm <- lm(value ~ yr, data = y)
    first_a <- summary(first_lm)$coefficients[1,1] %>% round(3)
    first_b <- summary(first_lm)$coefficients[2,1] %>% round(3)
    first_b_se <- summary(first_lm)$coefficients[2,2] %>% round(3)
    p_b <- summary(first_lm)$coefficients[2,4]%>% round(3)
    first_R2 <- summary(first_lm)$r.squared %>% round(3)
    
    # linear model for temperature
    y_tm <- sdata_tm[i, c(which(colnames(sdata_tm) == "X1"): which(colnames(sdata_tm) == "X26"))]
    y_tm %>% 
      as.data.frame() %>%
      tidyr::gather() %>% 
      na.omit() ->
      y_tm
    y_tm %>% 
      mutate(yr = c(1:nrow(y_tm))) ->
      y_tm
    n_tm <- y_tm %>% nrow()
    # linear regression
    first_lm_tm <- lm(value ~ yr, data = y_tm)
    first_a_tm <- summary(first_lm_tm)$coefficients[1,1] %>% round(3)
    first_b_tm <- summary(first_lm_tm)$coefficients[2,1] %>% round(3)
    p_b_tm <- summary(first_lm_tm)$coefficients[2,4]%>% round(3)
    first_R2_tm <- summary(first_lm_tm)$r.squared %>% round(3)
    
    # put all results together
    print(paste0("*****", i))
    dat <- rbind( dat, data.frame(StudyID, ID, first_a, first_b, first_b_se, p_b, first_R2, n,
                                  first_a_tm, first_b_tm, p_b_tm, first_R2_tm, n_tm) )
  }
  return(dat)
}


#****************************************************************************************************
# functions for Theil-Sen trend analysis
#****************************************************************************************************
fuzz <- function(x, error) {
  x * rnorm(length(x), mean = 1, sd = error)
}
fuzz_interannCV <- function(x, error) {
  x * rnorm(length(x), mean = 1, sd = error)
}
