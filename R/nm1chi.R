nm1chi<-function(x,y,data){
  # Replaces n with n-1 when calculating chisquare value for 2 x 2 contingency tables.
  #
  # Args:
  #  x: One of two binary variables to calculate n-1 corrected chi-square
  #  y: One of two binary variables
  #  data: data.frame or tibble
  #
  # For use in crosssectional non-experimental data when all expected cells have at least 1
  # If any cells are empty (zero), the Fishers exact test is used
  #
  # Reason for use: Standard chi-square does not handle small sample sizes well (i.e. <5 expected in a cell).
  # Fishers exact test and other corrections are too conservative and lack power.

  ## References
  # https://sites.google.com/a/lakeheadu.ca/bweaver/Home/statistics/notes/chisqr_assumptions
  # http://influentialpoints.com/Training/Fishers_exact_test_use_and_misuse.htm
  # http://www.iancampbell.co.uk/twobytwo/background.htm

  library("vcd")
  q<-assocstats(xtabs(~ data[,x] + data[,y]))
  newchisq<-q[[2]][2]*(sum(q[[1]][1:4])-1)/sum(q[[1]][1:4])

  # Calculate the n-1 chi square
  newp <- 1 - pchisq(newchisq,1)
  nm1chisq <- list(chi_square = q[[2]][2], p_value = q[[2]][6], "corrected_chi_square" = newchisq, "corrected_p_value" = newp)
  return(nm1chisq)
}


