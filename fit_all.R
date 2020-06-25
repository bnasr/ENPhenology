source('funcs.R')

study_rois <- fread('data/study_rois.csv')

n <- nrow(study_rois)

for(i in 1:n){
  
  site_name <- study_rois[i, site]
  veg_type <- study_rois[i, roitype]
  roi_id <- study_rois[i, sequence_number]
  
  message(i, site_name, veg_type, roi_id)
  
  out <- try(model_all(site_name = site_name,
                             veg_type = veg_type,
                             roi_id = roi_id))
  
  if(class(out)=='try-error') next()
  
  study_rois[i,R2:= out$R2]
  study_rois[i,RMSE:= out$RMSE]
  
  study_rois[i,Gmax:= mean(out$Gmax)]
  study_rois[i,Gmin:= mean(out$Gmin)]
  
  study_rois[i,delta:= mean(out$delta)]
  
  study_rois[i,theta1:= mean(out$theta[1,,])]
  study_rois[i,theta2:= mean(out$theta[2,,])]
  
  study_rois[i,rho1:= mean(out$rho[1,,])]
  study_rois[i,rho2:= mean(out$rho[2,,])]
  
  study_rois[i,sigma:= mean(out$sigma)]
}


