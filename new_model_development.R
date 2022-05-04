#setwd("C:/Marius Tita/OLD/marius/backup 27.11.2007/my doc/Directia administr. riscuri/Analize Risc/Web Plafon R/Risk applications/IFRS_9/2021_IFRS9/Model Nou")
library(magrittr)
library(glmnet)


set.seed(2022)

baza_macro <- readxl::read_excel("date_macro.xlsx",sheet = "new_data",range = "A1:I23") %>%
  dplyr::mutate(data_raport = as.Date.POSIXct(data_raport))


baza_updated <- readr::read_csv("baza_updated.csv") %>% dplyr::left_join( baza_macro, 
    #dplyr::select(data_raport, Crestere_PIB_3ani, Variatie_Robor3M_next_1Y_last_1Y,Crestere_3_Ani_lag_2Y,
    #Variatie_Robor3M_next_3Y_last_3Y,Crestere_PIB_1Y_next_1Y,Variatie_PIB1an_next_1Y_last_1Y),
    by = "data_raport") %>% dplyr::select(-min_cerere_plata,-mean_encode_categ_contaminata_1Y,
    -`Crestere 1Y last year`,
     -prob_1Y_CRC, -prob_1Y_no_crc,-prob_1Y,-stage,-mean_encode_categ_contaminata_3Y,
     -prob_3Y_CRC, -prob_3Y_no_crc, -prob_3Y, -mean_default_1Y_stage1, -mean_default_3Y_stage2) 

rm("baza_macro")

baza_updated <- baza_updated %>% dplyr::mutate(data_raport_6M = lubridate::`%m-%`(data_raport, months(6))) %>%
  dplyr::mutate(dplyr::across(data_raport_6M,.fns = ~ifelse(lubridate::month(.x)==12,
    lubridate::make_date(year = lubridate::year(.x),month = 12,day = 31),.x))) %>%
  dplyr::mutate(data_raport_6M = as.Date(data_raport_6M, origin = as.Date("1970-01-01"))) 




baza_updated <- baza_updated %>% dplyr::left_join(y = baza_updated %>% 
      dplyr::select(CUI, data_raport, total_incidente_6M = total_incidente, 
                    are_restante_peste_30_zile_6M = are_restante_peste_30_zile,
                    scor_serv_datorie_6M = scor_serv_datorie,
                    Este_in_interdictie_6M = Este_in_interdictie),
      by = c("CUI","data_raport_6M"="data_raport")) %>% dplyr::select(-data_raport_6M) %>% 
    dplyr::mutate(new_incidente_6M = total_incidente-total_incidente_6M) %>%
    dplyr::mutate(dplyr::across(new_incidente_6M,~ifelse(is.na(.x),0,.x))) %>%
    dplyr::mutate(new_Este_in_interdictie_6M = Este_in_interdictie-Este_in_interdictie_6M) %>%
    dplyr::mutate(dplyr::across(new_Este_in_interdictie_6M,~ifelse(is.na(.x),0,.x))) %>%
    dplyr::mutate(new_are_restante_peste_30_zile_6M = are_restante_peste_30_zile-are_restante_peste_30_zile_6M) %>%
    dplyr::mutate(dplyr::across(new_are_restante_peste_30_zile_6M,~ifelse(is.na(.x),0,.x))) %>%
  dplyr::mutate(new_scor_serv_datorie_6M = scor_serv_datorie-scor_serv_datorie_6M) %>%
  dplyr::mutate(dplyr::across(new_scor_serv_datorie_6M,~ifelse(is.na(.x),0,.x))) 



############################    DEVELOP 1Y CRC    ########################################################



baza_1Y_CRC <- baza_updated %>% dplyr::filter( data_raport <= as.Date("2020-12-31"),
        !is.na(scor_serv_datorie),!categorie_contaminata %in% c("insolventa", "cerere_plata")) %>%
            rsample::initial_split(prop = 0.70, strata = Default_1Y)

#readr::write_csv(x = rsample::training(baza_1Y_CRC), "Updated new model/train_1Y_CRC.csv")


glm_spec_new_model_1Y_CRC <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>% 
  parsnip::set_engine(engine = "glmnet") %>%
  parsnip::set_mode(mode = "classification")



glm_recipe_new_model_1Y_CRC <- recipes::recipe(formula = Default_1Y ~ ., data = rsample::training(baza_1Y_CRC) %>%
                                        dplyr::select(3:13,15:19,24:27)) %>%
  embed::step_woe(role = "predictor",outcome = "Default_1Y",prefix = "woe_1Y",categorie_contaminata) %>%
  recipes::step_rm(data_raport,A_fost_interdictie, new_are_restante_peste_30_zile_6M,new_Este_in_interdictie_6M,
                   total_incidente_majore,A_fost_interdictie,scor_serv_prel, 
                   new_scor_serv_datorie_6M, # it is selected by the model but decreases overall auc by about 0.2
                   Variatie_Robor3M_next_1Y_last_1Y,
                   "Crestere 1Y next year",
                   "Raport Crestere next year last year",
                   "Variatie_PIB1an_next_1Y_last_1Y") 

# View data after transformation with the above recipe
recipes::bake( recipes::prep( glm_recipe_new_model_1Y_CRC,training = rsample::training(baza_1Y_CRC) %>% 
             dplyr::mutate(Default_1Y=as.factor(Default_1Y))),new_data = rsample::training(baza_1Y_CRC) %>% 
       dplyr::mutate(Default_1Y=as.factor(Default_1Y))) %>% dplyr::group_by(woe_1Y_categorie_contaminata) %>%
  dplyr::summarise(dplyr::n())

glm_workflow_new_model_1Y_CRC <- workflows::workflow() %>% workflows::add_recipe(glm_recipe_new_model_1Y_CRC) %>% 
        workflows::add_model(glm_spec_new_model_1Y_CRC)


glm_initial_fit_1Y_CRC <- parsnip::fit(object = glm_workflow_new_model_1Y_CRC, data = rsample::training(baza_1Y_CRC) %>%
                                         dplyr::mutate(Default_1Y = as.factor(Default_1Y)))

# Select the third best penalty with auc of 0.862. The best is 0.006 with auc of 0.866
broom::tidy( glm_initial_fit_1Y_CRC,penalty=0.004)

#penalty_grid_new_model_1Y_CRC <- tibble::tibble(penalty = seq(from = 0, to=0.1, by=0.001))

#tune_res_new_model_1Y_CRC <- tune::tune_grid(object = glm_workflow_new_model_1Y_CRC,grid = penalty_grid_new_model_1Y_CRC, 
 #           resamples = rsample::training(baza_1Y_CRC) %>%  dplyr::mutate(Default_1Y = as.factor(Default_1Y)) %>% 
  #            rsample::vfold_cv(v = 10))

#autoplot(tune_res_new_model_1Y_CRC)
#select_best(tune_res_new_model_1Y_CRC, metric = "roc_auc")
#collect_metrics(x = tune_res_new_model_1Y_CRC) %>% dplyr::filter(.metric=="roc_auc") %>% dplyr::arrange(desc(mean)) %>% View()

glm_final_workflow_new_model_1Y_CRC <- tune::finalize_workflow( x = glm_workflow_new_model_1Y_CRC,
                                              parameters = data.frame(penalty=0.004))

#select_best(tune_res_new_model_1Y, metric = "roc_auc")) 

glm_final_fit_1Y_CRC <- parsnip::fit(object = glm_final_workflow_new_model_1Y_CRC,data = rsample::training(baza_1Y_CRC) %>%
                                       dplyr::mutate(Default_1Y = as.factor(Default_1Y)) )


pROC::auc(predictor = broom::augment(x = glm_final_fit_1Y_CRC,new_data = rsample::training(baza_1Y_CRC)) %>% dplyr::pull(.pred_1),
          response = rsample::training(baza_1Y_CRC) %>% dplyr::pull(Default_1Y))

temp_1Y_CRC <- baza_updated %>% dplyr::filter(data_raport <= as.Date("2018-12-31"),!is.na(scor_serv_datorie),
                                              !categorie_contaminata %in% c("insolventa","cerere_plata"))
temp_1Y_CRC <- temp_1Y_CRC %>%  dplyr::mutate(prob_1Y_CRC = broom::augment(x = glm_final_fit_1Y_CRC, 
                                                                    new_data = temp_1Y_CRC) %>%    dplyr::pull(.pred_1))

View(temp_1Y_CRC %>% dplyr::group_by(round(prob_1Y_CRC,3)) %>% dplyr::summarise(Default_3Y = mean(Default_3Y),
                                                                                Nr_beneficiari = dplyr::n())) 

threshold_1Y_CRC <- 0.033

rm("temp_1Y_CRC")

baza_updated <- baza_updated %>% dplyr::mutate(prob_1Y_CRC =  broom::augment(x = glm_final_fit_1Y_CRC, 
                     new_data = baza_updated) %>%    dplyr::pull(.pred_1)) 

#saveRDS(object = glm_final_fit_1Y_CRC,file = "Updated new model/model_1Y_crc.rds")


##############################  DEVELOP 1Y NO crc   ###############################


baza_1Y_NO_crc <- baza_updated %>% dplyr::filter(data_raport <= as.Date("2020-12-31"),is.na(scor_serv_datorie),
     !categorie_contaminata %in% c("insolventa","cerere_plata")) %>%   
  rsample::initial_split(prop = 0.7,strata = Default_1Y)

#readr::write_csv(x = rsample::training(baza_1Y_NO_crc), "Updated new model/train_1Y_NO_crc.csv")


glm_spec_new_model_1Y_NO_crc <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>% 
  parsnip::set_engine(engine = "glmnet") %>%
  parsnip::set_mode(mode = "classification")



glm_recipe_new_model_1Y_NO_crc <- recipes::recipe(formula = Default_1Y ~ ., data = rsample::training(baza_1Y_NO_crc) %>% 
                                           dplyr::select(9:11,13,15:19)) %>%
  #embed::step_woe(role = "predictor",outcome = "Default_1Y",prefix = "woe_1Y", categorie_contaminata) %>%
  recipes::step_mutate(role = "predictor", woe_1Y_categorie_contaminata = 
                ifelse(categorie_contaminata == "standard",0.373,
                       ifelse(categorie_contaminata=="instiintare_neplata",-2.48,NA_real_))) %>%
  recipes::step_rm(data_raport,categorie_contaminata,
                   Variatie_PIB1an_next_1Y_last_1Y, # not relevant
                   Variatie_Robor3M_next_1Y_last_1Y,`Crestere 1Y next year`, # not relevant or relevance too small
                   "Raport Crestere next year last year") # "wrong sign

# View encoding of categorie_contaminata
recipes::bake( recipes::prep( glm_recipe_new_model_1Y_NO_crc,training = rsample::training(baza_1Y_NO_crc) %>% 
             dplyr::mutate(Default_1Y=as.factor(Default_1Y))),new_data = rsample::training(baza_1Y_NO_crc) %>% 
       dplyr::mutate(Default_1Y=as.factor(Default_1Y))) %>% dplyr::group_by(woe_1Y_categorie_contaminata) %>%
  dplyr::summarise(dplyr::n())


glm_workflow_new_model_1Y_NO_crc <- workflows::workflow() %>% workflows::add_recipe(glm_recipe_new_model_1Y_NO_crc) %>% 
  workflows::add_model(glm_spec_new_model_1Y_NO_crc)


glm_initial_fit_1Y_NO_crc <- parsnip::fit(object = glm_workflow_new_model_1Y_NO_crc, data = rsample::training(baza_1Y_NO_crc) %>%
                            dplyr::mutate(Default_1Y = as.factor(Default_1Y)))

# 0 is the best penalty
broom::tidy( glm_initial_fit_1Y_NO_crc,penalty=0.0)

#penalty_grid_new_model_1Y_NO_crc <- tibble::tibble(penalty = seq(from = 0, to=0.04, by=0.001))

#tune_res_new_model_1Y_NO_crc <- tune::tune_grid(object = glm_workflow_new_model_1Y_NO_crc,grid = penalty_grid_new_model_1Y_NO_crc, 
 #  resamples = rsample::training(baza_1Y_NO_crc) %>%  dplyr::mutate(Default_1Y = as.factor(Default_1Y)) %>% 
  #  rsample::vfold_cv(v = 10))

#autoplot(tune_res_new_model_1Y_NO_crc)
#select_best(tune_res_new_model_1Y_NO_crc, metric = "roc_auc")

#collect_metrics(x = tune_res_new_model_1Y_NO_crc) %>% dplyr::filter(.metric=="roc_auc") %>% dplyr::arrange(desc(mean)) %>% View()

glm_final_workflow_new_model_1Y_NO_crc <- tune::finalize_workflow(x = glm_workflow_new_model_1Y_NO_crc,
                                                            parameters = data.frame(penalty=0.00))
 

glm_final_fit_1Y_NO_crc <- parsnip::fit(object = glm_final_workflow_new_model_1Y_NO_crc,data = rsample::training(baza_1Y_NO_crc) %>%
                                          dplyr::mutate(Default_1Y = as.factor(Default_1Y)) )


# Get global auc  for training/testing of model_1Y_NO_crc
pROC::auc(predictor = broom::augment(x = glm_final_fit_1Y_NO_crc,new_data = rsample::training(baza_1Y_NO_crc)) %>% dplyr::pull(.pred_1),
          response = rsample::training(baza_1Y_NO_crc) %>% dplyr::pull(Default_1Y))

# View for visual detecting of global stage2 threshold for NO_crc

temp_1Y_NO_crc <- baza_updated %>% dplyr::filter(data_raport <= as.Date("2018-12-31"),is.na(scor_serv_datorie),
                                                 !categorie_contaminata %in% c("insolventa","cerere_plata"))
temp_1Y_NO_crc <- temp_1Y_NO_crc %>%  dplyr::mutate(prob_1Y_NO_crc = broom::augment(x = glm_final_fit_1Y_NO_crc, 
             new_data = temp_1Y_NO_crc) %>%    dplyr::pull(.pred_1))


View(temp_1Y_NO_crc %>% #dplyr::mutate(group_col = ifelse(prob_1Y_NO_crc<0.1,"Under threshold%","Unde threshold")) %>%
       dplyr::group_by(group_col = round(prob_1Y_NO_crc,2)) %>%
       dplyr::group_by(group_col) %>% dplyr::summarise(Default_3Y = mean(Default_3Y),Nr_beneficiari = dplyr::n())) 

threshold_1Y_NO_crc <- 0.11

rm("temp_1Y_NO_crc")

baza_updated <- baza_updated %>% dplyr::mutate(prob_1Y_NO_crc = broom::augment(x = glm_final_fit_1Y_NO_crc, 
                          new_data = baza_updated) %>%    dplyr::pull(.pred_1)) 


baza_updated <- baza_updated %>% dplyr::mutate(stage = ifelse(
  categorie_contaminata %in% c("insolventa", "cerere_plata"), "stage3",
  ifelse( !is.na(scor_serv_datorie) & prob_1Y_CRC > threshold_1Y_CRC,  "stage2",
          ifelse( is.na(scor_serv_datorie) &  prob_1Y_NO_crc > threshold_1Y_NO_crc,  "stage2",
                  "stage1") ) ))

saveRDS(object = glm_final_fit_1Y_NO_crc,file = "Updated new model/model_1Y_NO_crc.rds")


########################### DEVELOP 3Y CRC  ##########################################

baza_3Y_CRC <- baza_updated %>% dplyr::filter( stage == "stage2",
                    data_raport <= as.Date("2018-12-31"),!is.na(scor_serv_datorie),
                    !categorie_contaminata %in% c("insolventa", "cerere_plata")) %>%
                        rsample::initial_split(prop = 0.7, strata = Default_3Y)

# Save training data for 3Y_CRC
#readr::write_csv(x = rsample::training(baza_3Y_CRC), "Updated new model/train_3Y_CRC.csv")

glm_spec_new_model_3Y_CRC <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>%
  parsnip::set_engine(engine = "glmnet") %>%
  parsnip::set_mode(mode = "classification")


glm_recipe_new_model_3Y_CRC <- recipes::recipe(formula = Default_3Y ~ ., data = rsample::training(baza_3Y_CRC) %>% 
                                        dplyr::select(3:12, 14:19,24:27)        ) %>%
  embed::step_woe(role = "predictor",outcome = 'Default_3Y',trained = FALSE, categorie_contaminata,
                  prefix = "woe_3Y") %>%
  recipes::step_rm(data_raport,scor_serv_prel, 
                   total_incidente_majore,A_fost_interdictie,  # these variables are never relevant for regression
                   total_incidente, new_are_restante_peste_30_zile_6M, # are not selected by the best penalty
                   "Raport Crestere next year last year",Variatie_Robor3M_next_1Y_last_1Y,Variatie_PIB1an_next_1Y_last_1Y,
                   Variatie_Robor3M_next_1Y_last_1Y,Variatie_PIB1an_next_1Y_last_1Y)

#View encoding categorie contaminata 3Y
recipes::bake( object = recipes::prep(x = glm_recipe_new_model_3Y_CRC,training = rsample::training(baza_3Y_CRC) %>%
                     dplyr::mutate(Default_3Y = as.factor(Default_3Y))) ,new_data =  rsample::training(baza_3Y_CRC) %>%
       dplyr::mutate(Default_3Y = as.factor(Default_3Y)) ) %>% 
  dplyr::group_by(woe_3Y_categorie_contaminata) %>% dplyr::summarise(dplyr::n())




glm_workflow_new_model_3Y_CRC <- workflows::workflow() %>% workflows::add_recipe(glm_recipe_new_model_3Y_CRC) %>% 
    workflows::add_model(glm_spec_new_model_3Y_CRC)


glm_initial_fit_3Y_CRC <- parsnip::fit(object = glm_workflow_new_model_3Y_CRC, data = rsample::training(baza_3Y_CRC) %>%
                                         dplyr::mutate(Default_3Y = as.factor(Default_3Y)))

# 0.00 is the best penalty
broom::tidy( glm_initial_fit_3Y_CRC,penalty=0.00)

penalty_grid_new_model_3Y_CRC <- tibble::tibble(penalty = seq(from = 0, to=0.08, by=0.001))

tune_res_new_model_3Y_CRC <- tune::tune_grid(object = glm_workflow_new_model_3Y_CRC,grid = penalty_grid_new_model_3Y_CRC, 
                                         resamples = rsample::training(baza_3Y_CRC) %>%  
                                             dplyr::mutate(Default_3Y = as.factor(Default_3Y)) %>% rsample::vfold_cv(v = 10))

collect_metrics(x = tune_res_new_model_3Y_CRC) %>% dplyr::filter(.metric=="roc_auc") %>% dplyr::arrange(desc(mean)) %>% View()


glm_final_workflow_new_model_3Y_CRC <- finalize_workflow(x = glm_workflow_new_model_3Y_CRC,
                                                         parameters = data.frame(penalty=0.000))


glm_final_fit_3Y_CRC <- parsnip::fit(object = glm_final_workflow_new_model_3Y_CRC,data = training(baza_3Y_CRC) %>%
                                       dplyr::mutate(Default_3Y = as.factor(Default_3Y)) )


pROC::auc(predictor = augment(x = glm_final_fit_3Y_CRC,new_data = rsample::testing(baza_3Y_CRC)) %>% dplyr::pull(.pred_1),
          response = rsample::testing(baza_3Y_CRC) %>% dplyr::pull(Default_3Y))


baza_updated <- baza_updated %>% dplyr::mutate(prob_3Y_CRC = augment(x = glm_final_fit_3Y_CRC, 
                              new_data = baza_updated) %>%    dplyr::pull(.pred_1))

#saveRDS(object = glm_final_fit_3Y_CRC,file = "Updated new model/model_3Y_CRC.rds")

######################################## DEVELOP 3Y NO crc ############################################

baza_3Y_NO_crc <- baza_updated %>% dplyr::filter( stage == "stage2",data_raport <= as.Date("2018-12-31"),
            is.na(scor_serv_datorie),!categorie_contaminata %in% c("insolventa", "cerere_plata") ) %>%
     rsample::initial_split(prop = 0.7, strata = Default_3Y)


glm_spec_new_model_3Y_NO_crc <- parsnip::logistic_reg( penalty = tune::tune(), mixture = 1) %>% 
  parsnip::set_engine(engine = "glmnet") %>%
  parsnip::set_mode(mode = "classification")


glm_recipe_new_model_3Y_NO_crc <- recipes::recipe(formula = Default_3Y ~ ., data = rsample::training(baza_3Y_NO_crc) %>% 
                                           dplyr::select(9:11,14:19)) %>%
  # categorie_contaminata is no longer a predictor here as almost all observations are instiintare_neplata
  #embed::step_woe(role = "predictor",outcome = "Default_3Y",prefix = "woe_3Y", categorie_contaminata) %>%
  #step_mutate(woe_3Y_categorie_contaminata = ifelse( categorie_contaminata=="standard",0.188,ifelse(
  # categorie_contaminata == "instiintare_neplata",-0.526,0))) %>%
  recipes::step_rm(data_raport, categorie_contaminata,expunere_mil_lei,
                   Variatie_Robor3M_next_1Y_last_1Y, #wrong_sign
                   Variatie_PIB1an_next_1Y_last_1Y,'Raport Crestere next year last year') # too little relevance



glm_workflow_new_model_3Y_NO_crc <- workflows::workflow() %>% workflows::add_recipe(glm_recipe_new_model_3Y_NO_crc) %>% 
  workflows::add_model(glm_spec_new_model_3Y_NO_crc)

glm_initial_fit_3Y_NO_crc <-  parsnip::fit( object = glm_workflow_new_model_3Y_NO_crc,
                    data = rsample::training(baza_3Y_NO_crc) %>% dplyr::mutate(Default_3Y = as.factor(Default_3Y)) )

# Penalty 0.044 is the best penalty
broom::tidy( glm_initial_fit_3Y_NO_crc,penalty=0.0)

penalty_grid_new_model_3Y_NO_crc <- tibble::tibble(penalty = seq(from = 0, to=0.05, by=0.001))

tune_res_new_model_3Y_NO_crc <- tune::tune_grid(object = glm_workflow_new_model_3Y_NO_crc,grid = penalty_grid_new_model_3Y_NO_crc,
                    resamples = rsample::training(baza_3Y_NO_crc) %>%  
                    dplyr::mutate(Default_3Y = as.factor(Default_3Y)) %>% rsample::vfold_cv(v = 10))

collect_metrics(x = tune_res_new_model_3Y_NO_crc) %>% dplyr::filter(.metric=="roc_auc") %>% 
  dplyr::arrange(desc(mean)) %>% View()


glm_final_workflow_new_model_3Y_NO_crc <- finalize_workflow(x = glm_workflow_new_model_3Y_NO_crc,
                                                            parameters = data.frame(penalty=0.044))


glm_final_fit_3Y_NO_crc <- parsnip::fit(object = glm_final_workflow_new_model_3Y_NO_crc,data = training(baza_3Y_NO_crc) %>%
                                          dplyr::mutate(Default_3Y = as.factor(Default_3Y)) )

pROC::auc(predictor = augment(x = glm_final_fit_3Y_NO_crc,new_data = training(baza_3Y_NO_crc)) %>% dplyr::pull(.pred_1),
          response = training(baza_3Y_NO_crc) %>% dplyr::pull(Default_3Y))



baza_updated <- baza_updated %>% dplyr::mutate(prob_3Y_NO_crc = augment(x = glm_final_fit_3Y_NO_crc,
                    new_data = baza_updated) %>%    dplyr::pull(.pred_1) )


saveRDS(object = glm_final_fit_3Y_NO_crc,file = "Updated new model/model_3Y_NO_crc")



############################UPDATE BAZA LUCRU ################################
baza_lucru_2010_2021 <- baza_updated %>% dplyr::mutate( woe_1Y_categorie_contaminata =
      ifelse( categorie_contaminata == "standard",  0.373,  
              ifelse( categorie_contaminata == "instiintare_neplata",  -2.48, NA_real_)  ),
    woe_3Y_categorie_contaminata = ifelse(  categorie_contaminata == "standard" &
        !is.na(scor_serv_datorie), 0.161, 
        ifelse( categorie_contaminata == "instiintare_neplata" &!is.na(scor_serv_datorie),
        -0.436,    NA_real_ )  )) %>%
  dplyr::filter(data_raport != as.Date("2021-06-30"))

### VIEWS
baza_dec_2021 <- baza_lucru_2010_2021 %>% dplyr::filter(data_raport==as.Date("2021-12-31")) %>%
  dplyr::select(-`Crestere 1Y next year`, -`ROBOR 3M mediu next year`, -prob_1Y_CRC,-prob_1Y_NO_crc,
                -prob_3Y_CRC, -prob_3Y_NO_crc, -stage)

coeficienti <- readRDS("coef_non_ifrs.rds") %>% dplyr::filter(FromDate == as.Date("2021-12-31"))

baza_dec_2021 <- baza_dec_2021 %>% dplyr::mutate(`Crestere 1Y next year` = -0.01, `ROBOR 3M mediu next year`=0.08)


baza_dec_2021 <- baza_dec_2021 %>% 
  dplyr::mutate(prob_1Y_CRC = augment(x = glm_final_fit_1Y_CRC, new_data = baza_dec_2021) %>%    dplyr::pull(.pred_1)) %>%
  dplyr::mutate(prob_1Y_NO_crc = augment(x = glm_final_fit_1Y_NO_crc, 
                                         new_data = baza_dec_2021) %>%    dplyr::pull(.pred_1)) %>%
  dplyr::mutate(prob_3Y_CRC = augment(x = glm_final_fit_3Y_CRC, 
                                      new_data = baza_dec_2021) %>%    dplyr::pull(.pred_1)) %>% 
  dplyr::mutate(prob_3Y_NO_crc = augment(x = glm_final_fit_3Y_NO_crc, new_data = baza_dec_2021) %>%   
                  dplyr::pull(.pred_1) ) %>%
  dplyr::mutate(prob_1Y = ifelse(is.na(scor_serv_datorie), prob_1Y_NO_crc, prob_1Y_CRC),
                prob_3Y = ifelse(is.na(scor_serv_datorie), prob_3Y_NO_crc, prob_3Y_CRC))



baza_dec_2021 <- baza_dec_2021 %>%  dplyr::mutate(stage = ifelse(
  categorie_contaminata %in% c("insolventa", "cerere_plata"), "stage3",
  ifelse( !is.na(scor_serv_datorie) & prob_1Y_CRC > threshold_1Y_CRC,   "stage2",
          ifelse(  is.na(scor_serv_datorie) &     prob_1Y_NO_crc > threshold_1Y_NO_crc,   "stage2", "stage1") ))) %>%
  dplyr::mutate(
    Provizion = ifelse(
      stage == "stage1",
      prob_1Y * (
        expunere_mil_lei * 1000000 -
          coeficienti$Ajustare_ctg * contragarantii) *
        coeficienti$Coef_trans_cereri_plata_plati *
        coeficienti$Coef_provizionare_plati,
      ifelse ( stage == "stage2",
               prob_3Y * (
                 expunere_mil_lei * 1000000 -
                   coeficienti$Ajustare_ctg * contragarantii) *
                 coeficienti$Coef_trans_cereri_plata_plati *
                 coeficienti$Coef_provizionare_plati,
               ifelse(
                 categorie_contaminata == "insolventa",
                 coeficienti$Coef_trans_cereri_plata_insolvente *
                   (
                     expunere_mil_lei * 1000000 -
                       coeficienti$Ajustare_ctg *contragarantii) *
                   coeficienti$Coef_trans_cereri_plata_plati *
                   coeficienti$Coef_provizionare_plati,
                 (
                   expunere_mil_lei * 1000000 - coeficienti$Ajustare_ctg * contragarantii
                 ) *
                   coeficienti$Coef_trans_cereri_plata_plati *
                   coeficienti$Coef_provizionare_plati
               )
      )
    )
  )

baza_dec_2021 %>% dplyr::group_by(stage) %>% dplyr::filter( stage!="stage3" ) %>%
  dplyr::summarise(Provizion= sum(Provizion), Expunere = sum(expunere_mil_lei))  %>%
  janitor::adorn_totals(where = "row")

#readr::write_csv(x = baza_lucru_2010_2021 %>% dplyr::select(-A_fost_interdictie,-scor_serv_prel,
  #        -c(4,15:16,19:23)), file = "new_updated_baza_lucru_2010_2021.csv" )

#readr::write_csv(x = testing(baza_3Y_NO_crc),file = "new_updated_test_3Y_NO_crc.csv")

readr::write_csv(x = baza_dec_2021 %>% dplyr::select(-total_incidente_majore,-A_fost_interdictie,-data_raport,
          -`Variatie_Robor3M_next_1Y_last_1Y`, -`Variatie_PIB1an_next_1Y_last_1Y`,
          -`Raport Crestere next year last year`, -scor_serv_prel),file = "test_dec_2021.csv")
