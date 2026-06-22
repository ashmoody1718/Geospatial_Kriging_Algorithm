eq = function(x, w){
  w*(1-exp((-3*abs(x))/10))
}

semi_var = function(val_near, val_far){
  svar = (1/2)*((val_near-val_far)^2)
  return(svar)
}
edist = function(x_near, y_near, x_far, y_far){
  dis = sqrt(((y_far-y_near)^2)+((x_far-x_near)^2))
  return(dis)
}

variogram_mf = function(data, bins = "Equal", primary, w){
  n = nrow(data)
  indices <- 1:n
  pairs <- combn(indices, 2)
  pairs_df <- as.data.frame(t(pairs))
  semi_rw = c()
  for(i in 1:length(pairs[1,])){
    value_nr = as.numeric(data[pairs_df[i,1], which(colnames(data) == primary)])
    value_fr = as.numeric(data[pairs_df[i,2], which(colnames(data) == primary)])
    x_nr = as.numeric(data[pairs_df[i,1], which(colnames(data) == "x")])
    x_fr = as.numeric(data[pairs_df[i,2], which(colnames(data) == "x")])
    y_nr = as.numeric(data[pairs_df[i,1], which(colnames(data) == "y")])
    y_fr = as.numeric(data[pairs_df[i,2], which(colnames(data) == "y")])
    new_semi = semi_var(value_nr, value_fr)
    new_dist = edist(x_nr, y_nr, x_fr, y_fr)
    semi_rw = rbind(semi_rw, c(new_semi, new_dist))
  }
  semi_rw = data.frame(semi_rw)
  colnames(semi_rw) = c("semi", "dist")
  semi_rw = semi_rw[order(unlist(semi_rw$dist), decreasing = FALSE), ]
  
  # Binning
  if(length(bins) == 1){
    if(bins=="Equal"){
      eq_bins = sqrt(length(semi_rw$semi))
      
      semi_avg = semi_rw |> 
        mutate(bin_num = ntile(dist, n=round(eq_bins))) |>
        group_by(bin_num)|>
        summarise(semi_mean = mean(semi), tot_count = n(), bin_mean = mean(dist))|>
        mutate(id = row_number())
    }
  }
  else{
    semi_avg = semi_rw |> 
      mutate(bin_num = cut(dist, breaks=bins)) |>
      group_by(bin_num)|>
      summarise(semi_mean = mean(semi), tot_count = n(), bin_mean = mean(dist))|>
      mutate(id = row_number())
  }
  dis = seq(1, max(semi_avg$bin_mean)+1, 0.01)
  semi = eq(dis, w)
  df = data.frame(dist = dis, semi = semi)
}