## Práctica 1.
## Bibliotecas
if (! require(ggplot2) ) install.packages('ggplot2')
if (!require(tidyverse))  install.packages('tidyverse')
if (!require(dplyr)) install.packages('dplyr')
if (!require(visdat)) install.packages('visdat')
if (!require(skimr)) install.packages('skimr')
if (!require(insuranceData)) install.packages('insuranceData')

## Cargar los datos.
data(dataCar)

## Categoría 1: Género.
gen_no_rec <- dataCar |> 
  group_by(gender,veh_body) |>
  summarise(no_total = sum(numclaims)) |>
  arrange(desc(no_total))
gen_no_rec

gen_rec <- dataCar |> 
  group_by(gender,veh_body) |>
  summarise(monto_total = sum(claimcst0)) |>
  arrange(desc(monto_total))
gen_rec



## Categoría 2: Area.
ar_exp <- dataCar |> 
  group_by(area,veh_body) |>
  summarise(exp_by_veh = mean(exposure)) |>
  arrange(desc(exp_by_veh))
ar_exp



## Categoría 3: Edad del asegurado.
ed_tuvo_rec <- dataCar |> 
  group_by(agecat) |>
  summarise(no_total = sum(clm)) |>
  arrange(desc(no_total))
ed_tuvo_rec

ed_mod <- dataCar |> 
  group_by(agecat) |>
  summarise(tot_pol = n()) |>
  arrange(desc(tot_pol))
ed_mod



## Categoría 4: Edad del vehiculo
ant_exp <- dataCar |> 
  group_by(veh_age,veh_body) |>
  summarise(exp_promedio = mean(exposure)) |>
  arrange(desc(exp_promedio))
ant_exp

ant_max_mod <- dataCar |> 
  group_by(veh_age,veh_body) |>
  summarise(val_max = max(veh_value)) |>
  arrange(desc(val_max))
ant_max_mod



## Gráficas 
ggplot(gen_rec, aes(x=reorder(veh_body,-monto_total), y = monto_total, fill = gender))+
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(title = 'Montos Reclamados por Modelo',x='Tipo de Vehículos', y= 'Monto Total de las Reclamaciones')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

ggplot(gen_no_rec, aes(x=reorder(veh_body,-no_total), y = no_total, fill = gender))+
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(title = 'Reclamaciones por Modelo',x='Tipo de Vehículos', y= 'Número de Reclamaciones')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))
  
ggplot(ar_exp, aes(x=reorder(veh_body,-exp_by_veh), y = exp_by_veh))+
  geom_point(color='cyan4')+
  labs(title = 'Exposición por Modelo',x='Modelo del Vehículo', y= 'Exposición')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

ggplot(dataCar, aes(x=area,y=claimcst0))+ geom_point(color='cyan4')+
  labs(title = 'Reclamaciones por Área', x='Zona', y= 'Reclamaciones')+
  theme_minimal()

ggplot(ed_tuvo_rec, aes(x=agecat,y=no_total))+
  geom_bar(stat = 'identity', position = 'dodge', color='cyan4',fill = 'cyan',lwd=2)+ 
  geom_text(aes(label = no_total), vjust = 2, colour = "black")+
  labs(title = 'Pólizas con Reclamación por Grupo de Edad',x='Grupo', y= 'No. Total')+
  theme_minimal()

ggplot(ed_mod, aes(x=agecat,y=tot_pol))+ 
  geom_bar(stat = 'identity', position = 'dodge',color='magenta4',fill='magenta3',lwd=2)+
  geom_text(aes(label = tot_pol), vjust = 2, colour = "black")+
  labs(title = 'No. de Pólizas por Grupo de Edad', x='Grupo', y= 'No. de Pólizas')+
  theme_minimal()

ggplot(ant_exp, aes(x=veh_age,y=exp_promedio))+
  geom_point(color='cyan4')+
  labs(title = 'Exposición por Antigüedad del Modelo',x='Antigüedad', y= 'Exposición')+
  theme_minimal()

ggplot(dataCar, aes(x=veh_age,y=veh_value))+
  geom_point(color='cyan4')+
  labs(title = 'Valores de los Vehículos por Antigüedad',x='Antigüedad', y= 'Valor')+
  theme_minimal()
