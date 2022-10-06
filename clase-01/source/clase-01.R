## Eduard Martinez
## update: 04-10-2022

## instalar pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

## llamar/instalar librerias de la sesion
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       janitor, # clean_names and tabyl
       caret) # Classification And REgression Training

## leer conjutno de datos
db <- import("input/house_prices.rds") %>% as_tibble()

#### **Inspeccionar datos**

## print data
head(db)
tail(db)

## summary db
skim(db)

## summary var
summary(db$price)

#### **Visualizar datos**

## data + mapping
ggplot(data = db , mapping = aes(x = log(surface_total) , y = log(price)))

## + geometry
ggplot(data = db , mapping = aes(x = log(surface_total) , y = log(price))) +
geom_point(col = "red" , size = 0.5)

## by group
ggplot(data = db , 
       mapping = aes(x = log(surface_total) , y = log(price) , group=as.factor(l3) , color=as.factor(l3))) +
       geom_point()

## box_plot: estrato1 vs totalHoursWorked
box_plot <- ggplot(data=db , mapping = aes(log(price) , y = as.factor(l3))) + 
            geom_boxplot() 
box_plot

## add theme
box_plot +  labs(x="Log(Precio)" , y="Ciudad") + theme_test()

#### **3.4 Transformaciones**

##### **3.4.1 Centrado y escalado**
h_bathrooms = ggplot() + geom_histogram(data=db , aes(x=bathrooms) , fill="#99FF33" , alpha=0.5)
h_bathrooms

db = db %>% mutate(esc_bathrooms = scale(bathrooms))
h_bathrooms + geom_histogram(data=db , aes(x=esc_bathrooms) , fill="#FF0066" , alpha=0.5)

##### **3.4.2 Lidiar con outliers **
quantile(x=db$surface_total , na.rm=T)

IQR(x=db$surface_total , na.rm=T)

iqr <- IQR(x=db$surface_total , na.rm=T)

db_out <- db %>% subset(surface_total <= 2*iqr | is.na(surface_total)==T)

cat("¡Elimina las NA!")

quantile(x=db_out$surface_total , na.rm=T)

nrow(db) - nrow(db_out)

#### **3.5 Crear, modificar y eliminar variables**

##### **3.5.1 Variables dummy**
db = db %>% 
     mutate(surface_total_out = ifelse(test = surface_total > 4*iqr, 
                               yes = 1, 
                               no = 0))
table(db$surface_total_out)

##### **3.5.2 Variables categoricas**
q = quantile(db$surface_total_out , na.rm=T)
q
db = db %>% 
     mutate(surface_q = case_when(surface_total_out < q[2] ~ "Q-1", 
                                  surface_total_out >= q[2] & surface_total_out < q[3] ~ "Q-2", 
                                  surface_total_out >= q[3] & surface_total_out < q[4] ~ "Q-3", 
                                  surface_total_out >= q[4] ~ "Q-4"))
table(db$surface_q)

##### **3.4.4 Mantener solo algunas variables**

## select: delete a variable
head(db)
db %>% select(-V1)

## select variable: by patter name 
db %>% select(starts_with("l1"))

db %>% select(id,contains("surface"))

## select variable: by class
db %>% select_if(is.character)

