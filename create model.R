#  __  __         _     _   _____         _      _           
# |  \/  |___  __| |___| | |_   _| _ __ _(_)_ _ (_)_ _  __ _ 
# | |\/| / _ \/ _` / -_) |   | || '_/ _` | | ' \| | ' \/ _` |
# |_|  |_\___/\__,_\___|_|   |_||_| \__,_|_|_||_|_|_||_\__, |
#                                                      |___/ 
    
## Cargo los paquetes
#https://allisonhorst.github.io/palmerpenguins/
library(palmerpenguins)
library(tidymodels)

#============================================#
#== 1. Exploracion y limpieza de los datos ==#
#============================================#

## Leo las datos
penguins; nrow(penguins)

## Filtro los datos con las que tengan NA para las columnas que terminan en mm
colSums(is.na(penguins));(filtered_penguins <- penguins %>% drop_na(ends_with("mm")))

## Exploracion de los datos
filtered_penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, col = species)) +
  geom_point()

filtered_penguins %>% 
  select(species, ends_with("mm"), body_mass_g)
#----------------------------------------------------------------------------------------------------------------------------------

## Particiono los datos en conjunto de entrenamiento 25% y en un conjunto de prueba 25%
set.seed(123)
penguin_split <- initial_split(filtered_penguins, strata = species, prop = 0.75)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)

## Creo un remuestreo por bootstrap de los datos de entrenamiento, para evaluar nuestros modelos.
#set.seed(123);penguin_boot <- bootstraps(data = penguin_train, times = 10)

## Creo una particion aleatoria para la validacion crizada
set.seed(123);penguin_cv <-vfold_cv(data = penguin_train, v = 5)
penguin_cv


#============================================#
#== 2. Creación del Modelo                 ==#
#============================================#

##-- Ajuste del modelo forma 1 --##
#----------------------------------------------------------------------------------------------------------------------------------

model <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  fit(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = filtered_penguins)

model

predict(model, new_data = head(filtered_penguins), type = "prob")

## Exportar el modelo
readr::write_rds(model, here::here("models", "model.rds"))


##-- Ajuste del modelo forma 2
#----------------------------------------------------------------------------------------------------------------------------------

## Especificación del modelo
rf_spec <- rand_forest() %>% set_mode("classification") %>% set_engine("ranger")
rf_spec

## Creo el flujo de trabajo para el modelo
penguin_wf <- workflow() %>% add_formula(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g)
penguin_wf

rf_rs <- penguin_wf %>%
         add_model(rf_spec) %>%
         fit_resamples(
           resamples = penguin_cv, # Agrego las muestras por validacion cruzada
           metrics = metric_set(accuracy, roc_auc, f_meas), # Agrego las metricas que quiero calcular
           control = control_resamples(save_pred = TRUE)
          )
rf_rs

## Evaluar el modelo
collect_metrics(rf_rs)

## Calculo la matriz de cofusion
rf_rs %>% conf_mat_resampled(tidy = FALSE)

## Calculo la curva ROC para todas las especies
rf_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(species, .pred_Adelie:.pred_Gentoo) %>%
  autoplot()

## Ajusto el modelo final
penguin_final <- penguin_wf %>%
                 add_model(rf_spec) %>%
                 last_fit(split = penguin_split,
                          metrics = metric_set(accuracy, roc_auc, f_meas))
penguin_final

## Performance del modelo
collect_predictions(penguin_final) %>% conf_mat(species, .pred_class)
rf_model <- penguin_final$.workflow[[1]]

predict(penguin_final$.workflow[[1]], penguin_test[1, ], type = "prob") # type = "class"

if(!dir.exists("models")) dir.create(path = "models")
saveRDS(rf_model, here::here("models", "rf_model.rds"))
