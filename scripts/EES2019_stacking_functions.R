# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Functions for Stacking Data ('StackMe' version) 
# Author: G.Carteny
# last update: 2021-05-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Functions for generating party-voter distances - - - - - - - - - - - - - - - - - - - - - - - - - - - 

gendist.int.fun <- function(data, index, basevar){
  exprss <- paste0('1-((abs(10-1) - abs(', basevar, '_', index, '-', basevar, '_', index, '_mean', '))/abs(10-1))')
  newvar <- paste0(basevar, '_', index, "dist")
  q <- quote(mutate(data, !! newvar := exprss))
  df2 <- eval(parse(text=sub("exprss", exprss, deparse(q)))) %>% dplyr::select(all_of(newvar))
  return(df2)
}

gendist <- function(data, indices, stub) {
  df <- lapply(data=data, X = indices, basevar = stub, FUN = gendist.int.fun) %>% do.call('cbind',.)  
  return(df)
}


# Functions for generating dichotomous dependent variables - - - - - - - - - - - - - - - - - - - - - - 

gendicovar.int.fun <- function(data, depvar, index) {
  newvar = paste0(depvar, '_', index)
  data[[newvar]] <- data[[depvar]]
  exprss <- paste0('case_when(', newvar, '==', index, ' ~ 1, T ~ 0)')
  q <- quote(mutate(data, !! newvar := exprss))
  df2 <- 
    eval(parse(text=sub("exprss", exprss, deparse(q)))) %>% 
    dplyr::select(all_of(newvar)) %>%
    as_tibble()
  return(df2)
}

gendicovar <- function(data, indices, stub) {
  df <- lapply(data=data, X = indices, depvar = stub, FUN = gendicovar.int.fun) %>% do.call('cbind',.)
  return(df)
}


# Y-hat fun - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

genyhats <- function(data, depvar, regtype, indvar, newname) {
  
  frmla <- as.formula(paste(depvar, paste(indvar, collapse = " + "), sep = " ~ "))
  
  ics <- data %>% dplyr::select(all_of(depvar), all_of(indvar))

  # for(i in 2:length(ics)) {
  #   vec <- ics[[i]]
  # 
  #   cl <- class(ics[[i]])
  # 
  #   if(cl=='numeric') {
  #     ics[[i]] <- ifelse(is.na(vec), mean(vec, na.rm=T) , vec)
  #   } else if (cl=='factor') {
  #     ics[[i]] <- ifelse(is.na(vec), median(vec, na.rm=T) , vec)
  #   }
  # }
  
  if (regtype=='logit' | regtype=='log') {
    x <- glm(frmla, data = ics, family = "binomial")
    
    outcome <- data.frame(respos = predict(x) %>% attr(., 'names') %>% as.numeric(),
                          yhat = predict(x, type='response'))
  } else if (rlang::is_missing(regtype) | regtype=='linear' | regtype=='OLS') {
    x <- lm(frmla, data = ics)
    
    outcome <- data.frame(respos = predict(x) %>% attr(., 'names') %>% as.numeric(),
                          yhat = predict(x))
  }

  respid <- data.frame(respos = 1:nrow(data),
                       respid = data$respid)

  outcomedf <- left_join(respid, outcome, by='respos')

  outcomedf %<>% dplyr::select(respid, yhat)

  names(outcomedf)[names(outcomedf)=='yhat'] <- paste0(newname, '_yhat_', depvar)

  df <- left_join(data, outcomedf, by='respid')
  
  return(df)
}


# Generate the stacked data matrix - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

genstacks <- function(idvar, data, stubs, keepvar) {
  
  df <- data %>% dplyr::select(starts_with(paste0(stubs, '_')))
  
  id1 <- data %>% dplyr::select(all_of(idvar)) %>% unlist()
  id2 <- names(df) %>% gsub('.*\\_', '', ., perl = T) %>% unique %>% as.numeric()
  
  stack_df <- expand.grid(id1, id2) %>% as_tibble() %>% .[order(.$Var1),]
  
  for (i in stubs) {
    df2 <- data %>% 
      dplyr::select(all_of(idvar), starts_with(paste0(i, '_'))) %>%
      pivot_longer(cols = starts_with(paste0(i, '_')),
                   names_to = paste0(i, '_n'), 
                   values_to = i)
    df2[[paste0(i, '_n')]] %<>% gsub('.*\\_', '', ., perl = T) %>% as.numeric()
    names(df2)[names(df2)==idvar] <- 'Var1'
    names(df2)[names(df2)==paste0(i, '_n')] <- 'Var2'
    stack_df <- left_join(stack_df, df2, by = c('Var1', 'Var2'))
  }
  
  if (rlang::is_missing(keepvar)) {
    return(stack_df)  
  } else {
    if (any(is.null(keepvar)) | any(is.na(keepvar))) {
      warning("Argument 'keepvar' is invalid")
    } else {
      
      keepvar %<>% as.character()
      
      if (any(keepvar %in% colnames(EES2019_it))) {
        if(all(keepvar %in% colnames(EES2019_it))) {
          df <- data %>% dplyr::select(all_of(idvar), all_of(keepvar))
          names(df)[[1]] <- 'Var1'
          stack_df <- left_join(stack_df, df, by = 'Var1')
          return(stack_df)
        } else {
          x <- which(keepvar %in% colnames(EES2019_it))
          df <- data %>% dplyr::select(all_of(idvar), keepvar[c(x)])
          names(df)[[1]] <- 'Var1'
          stack_df <- left_join(stack_df, df, by = 'Var1')
          warning("Some elements of 'keepvar' are missing")
          return(stack_df)
        }
      } else {
        warning("Argument 'keepvar' is missing in the data")
      } 
    }
  }
}
