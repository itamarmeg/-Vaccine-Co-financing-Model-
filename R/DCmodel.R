# The implementation of the Donor-Country model
# We do not develop this in a package as this is the only
#   new function.
DCmodel <- function(cost, benefit,
                    intervention,
                    gdp_per_capita = NULL, 
                    gdp_threshold_multiple = NULL,
                    threshold = NULL) {
  
  # The implementation of the Donor-Country model
  #
  # The function returns a tibble with the amount of domestic and donor funds.
  #
  # Inputs: cost      a numeric vector of intervention costs (should be same length as
  #                   benefits and intervention vectors)
  #         benefit   a numeric vector intervention benefits (should be same length as
  #                   costs and intervention vectors)
  #         intervention   a character vector of intervention names (should be same length
  #                   as cost and benefit)
  #         gdp_per_capita  a numeric vector of country gdp per capita (must be the
  #                   the same length as gdp_threshold_multiple; can provide
  #                   the vector called threshold instead to explicitly provide the
  #                   threshold)
  #         gdp_threshold_multiple  a numeric vector of the threshold gdp multiple 
  #                   (must be the the same length as gdp_per_capita; can provide
  #                   the vector called threshold instead to explicitly provide the
  #                   threshold)
  #         threshold a numeric vecot of thresholds (will be overwritten by
  #                   gdp_per_capita and gdp_threshold_multiple if they are provided)
  #
  #   Output: a tibble with the columns intervention, cost, benefit, cer (cost-effectiveness
  #           of the intervetnion), gdp_per_capita and gdp_threshold_multiple (if applicable),
  #           threshold, domestic (funds), and donor (funds).
  
    
  # Check costs, benefits, and intervention  are the same length
  if (length(cost) != length(benefit) | 
      length(cost) != length(intervention)){
    stop("The vectors cost, benefit, and intervention must be the same",
         " length.")
  }
  # Check threshold provided
  if ((is.null(gdp_per_capita) | is.null(gdp_threshold_multiple)) & 
      is.null(threshold)) {
    stop("Either gdp_per_capita and gdp_threshold_multiple need to be provided",
         " to calculate a GDP-based threshold, or a threshold needs to be",
         " explicityly provided.")
    # Warn gdp_per_capit & gdp_threshold_multiple overriding threshold input
  } else if ((!is.null(gdp_per_capita) & !is.null(gdp_threshold_multiple)) & 
             !is.null(threshold)){
    warning("gdp_per_capita and gdp_trehsold_multiple are overriding",
            " the threshold input.")
  }
  
  # Create a tibble that will output
  out <- tibble(
    intervention = intervention,
    cost = cost,
    benefit = benefit,
    cer = round(cost/benefit)
  )
  
  # If using gdp based threshold and no explicitly providing threshold
  if (!is.null(gdp_per_capita)){
    # Check gdp_per_capita and gdp_threshold_are same lengths
    if (length(gdp_per_capita) != length(gdp_threshold_multiple)){
      stop("gdp_per_capita and gdp_threshold_multiple must be the same legnth.")
    }
    
    # Calculate threshold if gdp_percapt and gdp_threshold_multiple given
    a <- unlist(sapply(gdp_per_capita, rep, length(intervention), 
                       simplify = FALSE))
    b <- unlist(sapply(gdp_threshold_multiple, rep, length(intervention), 
                       simplify = FALSE))
    out <- out %>%
      slice(rep(row_number(), length(gdp_per_capita))) %>%
      mutate(gdp_per_capita = a,
             gdp_threshold_multiple = b) %>%
      mutate(threshold = round(gdp_per_capita * gdp_threshold_multiple))
    
  } else {
    # Add threshold to output if explicity given
    a <- unlist(sapply(threshold, rep, length(intervention),
                       simplify = F))
    out <- out %>%
      slice(rep(row_number(), length(threshold))) %>%
      mutate(threshold = a)
  }
  
  out %>%
    mutate(domestic = ifelse(cost > benefit*threshold,
                             benefit*threshold,
                             cost),
           donor = ifelse(cost > benefit*threshold,
                          cost - benefit*threshold,
                          0))
}