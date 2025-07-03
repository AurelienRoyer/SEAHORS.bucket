name_correction<- function(name_of_file,name_of_column){
  species<-name_of_column
  data.df<-name_of_file
  #data.df<-read.csv(paste0(name_of_file,".csv", sep=",",header=T))
  data.df$species[data.df$species=="Eliomys quercinus"]<-"ELIO"
  data.df$species[data.df$species=="Eliomys ?"]<-"ELIO"
  data.df$species[data.df$species=="Murid sp."]<-"MURID"
  data.df$species[data.df$species=="Apodemus sp."]<-"MURID"
  data.df$species[data.df$species=="A. sylvaticus"]<-"MURID"
  data.df$species[data.df$species=="M. AGRESTIS"]<-"M.AGRESTIS"
  data.df$species[data.df$species=="M. arvalis/agrestis"]<-"M.ARVALIS.AGRESTIS"
  data.df$species[data.df$species=="M. ARVALIS"]<-"M.ARVALIS"
  data.df$species[data.df$species=="A. amphibius"]<-"ARVICOLA"
  data.df$species[data.df$species=="Arvicola sp."]<-"ARVICOLA"
  data.df$species[data.df$species=="L. GREGALIS"]<-"L.GREGALIS"
  data.df$species[data.df$species=="C. NIVALIS"]<-"C.NIVALIS"
  data.df$species[data.df$species=="Neomys sp."]<-"NEOMYS"
  data.df$species[data.df$species=="N. anomalus"]<-"NEOMYS"
  data.df$species[data.df$species=="Talpa sp."]<-"Talpa"
  data.df$species[data.df$species=="S. araneus/coronatus"]<-"Sorex_araneus"
  data.df$species[data.df$species=="Sorex sp."]<-"Sorex_sp."
 
  data.df$species[data.df$species=="A. OECONOMUS"]<-"A.OECONOMUS"
  data.df$species[data.df$species=="M. ARVALIS"]<-"Microtus_arvalis"
  data.df$species[data.df$species=="M.ARVALIS"]<-"Microtus_arvalis"
  data.df$species[data.df$species=="M.ARVALIS.AGRESTIS"]<-"Microtus_arvalis.agrestis"
  data.df$species[data.df$species=="A._sapidus"]<-"Arvicola_sapidus"
  data.df$species[data.df$species=="ARVICOLA"]<-"Arvicola_sp."
  data.df$species[data.df$species=="A.OECONOMUS"]<-"Alexandromys_oeconomus"
  data.df$species[data.df$species=="C.NIVALIS"]<-"Chinomys_nivalis"
  data.df$species[data.df$species=="ELIO"]<-"Eliomys_quercinus"
  data.df$species[data.df$species=="L.GREGALIS"]<-"Lasiopodomys_gregalis"
  data.df$species[data.df$species=="S._minutus"]<-"Sorex_minutus"
  data.df$species[data.df$species=="Spermophilus_sp._ "]<-"Spermophilus_sp."
  data.df$species[data.df$species=="Talpa"]<-"Talpa_europaea"
  data.df$species[data.df$species=="Terricola_sp."]<-"Microtus_Terricola_sp."
  data.df$species[data.df$species=="M._T._pyrenaicus "]<-"Microtus_Terricola_gerbei"
  data.df$species[data.df$species=="M.AGRESTIS"]<-"Microtus_agrestis"
  data.df$species[data.df$species=="Microtus SP."]<-"Microtus_sp."
  data.df$species[data.df$species=="Microtus_SP."]<-"Microtus_sp."
  #data.df$species[data.df$species=="S. araneus/coronatus"]<-"Sorex_araneus"
  data.df$species[data.df$species=="Murid sp."]<-"Murid_sp."
  data.df$species[data.df$species=="Apodemus sp."]<-"Apodemus_sp."
  data.df$species[data.df$species=="A. amphibius"]<-"Arvicola_amphibius"
  data.df$species[data.df$species=="A. sylv/flavicol"]<-"Apodemus_sylvaticus"
 # data.df$species[data.df$species=="Sorex sp."]<-"Sorex_sp."
  data.df$species[data.df$species=="A. sylvaticus"]<-"Apodemus_sylvaticus"
  data.df$species[data.df$species=="Dicrostonyx torquatus"]<-"Dicrostonyx_torquatus"
  data.df$species[data.df$species=="Spermophilus sp. "]<-"Spermophilus_sp. "
  data.df$species[data.df$species=="C. glareolus"]<-"Myodes_glareolus"
  
  data.df$species[data.df$Genre=="batracien"]<-"Batracien"
  data.df$species[data.df$Genre=="serpent"]<-"serpent"
  data.df$species[data.df$Genre=="salamandra"]<-"salamandra"
  
 # assign(paste0(name_of_file,".2"), value=data.df,envir = globalenv())
  
  data.df <- data.df[!(data.df[,5]=='"RONGEUR"'  & data.df[,6]=="NOUVEL ENREGISTREMENT" & data.df[,7]=="m1inf"),] 
  
  assign(paste0("BDD2",".2"), value=data.df,envir = globalenv())
return(data.df)
}
