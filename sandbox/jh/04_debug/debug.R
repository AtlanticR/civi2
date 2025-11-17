tar_load(data_CIVI_Sites)
x <- ind_proximity(data_CIVI_Sites=data_CIVI_Sites, ors_api_key=read.table(file.path(path_to_store(),"data","ors_api_key.txt"))$V1, full_results=TRUE)
