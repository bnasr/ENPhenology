library(phenocamapi)
library(rjags)
library(daymetr)
library(data.table)

data_dir <- 'data/modeling_time_series/'
out_dir <- 'data/modeling_outputs/'
figs_dir <- 'data/modeling_figures/'

rmse <- function(obs, sim){
  sqrt(mean((obs-sim)^2, na.rm=T))
}

rMse <- function(obs, sim){
  sqrt(median((obs-sim)^2, na.rm=T))
}



load_data <- function(study_rois){
  ts_data <- Reduce(f = rbind, 
                    x = lapply(X = 1:nrow(study_rois), FUN = function(i){
                      data.table(site = study_rois$site[i],
                                 roi_name = study_rois$roi_name[i],
                                 as.data.table(read.csv(study_rois$data_file[i])))
                    }))
}

model_single <- function(data, n.mcmc = 2000, n.burnin = n.mcmc){
  
  model <- jags.model(file = "model/tdm.bugs",
                      data = list(G = data$gcc,
                                  doy = data$doy,
                                  Tmin = data$tmin,
                                  Tmax = data$tmax,
                                  # theta =c(0,0),
                                  gmin = quantile(data$gcc, 0.02, na.rm = TRUE),
                                  gmax = quantile(data$gcc, 0.98, na.rm = TRUE),
                                  n = nrow(data)),
                      
                      inits = list(rho = c(1e-4, 2e-3),
                                   sigma = 0.005,
                                   theta =c(0,0),
                                   delta = 200, #c(180,180),
                                   Gmin = quantile(data$gcc, 0.02, na.rm = TRUE),
                                   Gmax = quantile(data$gcc, 0.98, na.rm = TRUE)
                      ),
                      n.chains = 1)
  
  
  update(model, n.burnin)
  
  out <- jags.samples(model,
                      n.iter = n.mcmc,
                      variable.names = c('rho', 'sigma', 'delta', 
                                         'theta',
                                         'Gmin', 'Gmax', 'Gp'))
  
  out$R2 <- cor(data$gcc, rowMeans(out$Gp), use = 'complete.obs')^2
  
  out$RMSE <- rmse(data$gcc, rowMeans(out$Gp))
  
  out$Gpred <- rowMeans(out$Gp)
  
  out
}

model_all <- function(site_name, veg_type, roi_id, plot = TRUE, save= plot){
  
  data_file <- sprintf('%s%s-%s-%04d-dat.csv', data_dir, site_name, veg_type, roi_id)
  out_file <- sprintf('%s%s-%s-%04d-out.RData', out_dir, site_name, veg_type, roi_id)
  fig_file <- sprintf('%s%s-%s-%04d-fig.png', figs_dir, site_name, veg_type, roi_id)
  
  
  if(!file.exists(data_file)){
    
    write.csv(fetch_data(site = site_name, 
                         veg_type = veg_type, 
                         roi_id = roi_id), 
              file = data_file, 
              row.names = FALSE)
  }
  
  data <- as.data.table(read.csv(data_file))
  data[,date:=as.Date(date),]
  
  
  if(!file.exists(out_file))
  {
    out <- model_single(data)
    save(out, file = out_file)
  }else{
    load(out_file)
  }
  
  if(plot){
    if(save)png(filename = fig_file, width = 8, height = 6, units = 'in', res = 150)
    plot(data$date, data$gcc)
    points(data$date, rowMeans(out$Gp), col = 'red', pch = 19, type = 'b')
    
    mtext(signif(out$R2,2))
    if(save)dev.off()
  }
  out
}
