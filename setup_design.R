setup_design <-
  function(in_data, in_weights= "FWGT", in_repweights= "BSW[0-9]+", in_type="bootstrap", in_combined=TRUE) {
    survey::svrepdesign(
      data = in_data,
      weights = as.formula(paste0("~",in_weights)),
      repweights = in_repweights,
      type = in_type,
      combined.weights = in_combined)
  }