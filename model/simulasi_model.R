a_generate_sampel <- function(data,banyak_sampel, ukuran_sampel){
  gen = NULL;
  for(i in 1:banyak_sampel) gen=c(gen,mean(sample(data,ukuran_sampel)));
  return(gen);
}

a_dens_generate_sampel <-function(data,banyak_sampel, ukuran_sampel){
  return(density(f_generate_sampel(data,banyak_sampel, ukuran_sampel)));
}

a_cummean <- function(x)
  cumsum(x) / seq_along(x)