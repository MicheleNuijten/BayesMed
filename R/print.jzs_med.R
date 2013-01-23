print.jzs_med <-
function(x,...){
  print(x[!(names(x)%in%c("alpha","beta","tau_accent","rho",
                          "prob_alpha","prob_beta","prob_tau_accent",
                          "alpha_times_beta",
                          "jagssamples","jagssamplesA","jagssamplesTB"))])
}
