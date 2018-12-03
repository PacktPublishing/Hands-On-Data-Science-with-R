##Introduction
#install.packages("sparklyr")
#install.packages(c("dplyr", "sparklyr", "DAAG"))
#library(sparklyr); library(dplyr)
#spark_install()
##End introduction


##First Section
sc <- spark_connect(master = "local")

dt_sugar <- copy_to(sc, sugar, "SUGAR")
dt_stVincent <- copy_to(sc, stVincent, "STVINCENT")
dt <- spark_read_csv(sc, "avocado", path ="###avocado_file_path###")

dt_sugar %>% filter(trt == "Control")

dbGetQuery(sc, "SELECT trt FROM sugar")
##End First section



##Second Section
mod_stvincent <- dt_stVincent %>% select(code, id, harvwt) %>% 
  filter(harvwt > 15) %>% arrange(desc(id))

mod1_stvincent <- collect(mod_stvincent)
plot(mod1_stvincent$id, mod1_stvincent$harvwt)

mod1_stvincent <- dbGetQuery(sc, "SELECT *
  FROM (SELECT `code`, `id`, `harvwt`
  FROM `STVINCENT`) `tgssodfxtm`
  WHERE (`harvwt` > 15.0)
  ORDER BY `id` DESC")
plot(mod1_stvincent$id, mod1_stvincent$harvwt) 
##End second section



##Third Section
library(sparklyr)
library(dplyr)
library(car)

###SparkML
#sc <- spark_connect(master = 'local') #Only if you aren't connected
dt_chile <- copy_to(sc, Chile, 'chile')

dt_chile <- na.omit(dt_chile)
partitions <- dt_chile %>% sdf_partition(training = 0.7, test = .3, seed = 50)

chile_training <- partitions$training
chile_test <- partitions$test

dt_chile_ML <- chile_training %>% ml_decision_tree(vote ~ ., seed = 50)
dt_chile_pred <- sdf_predict(chile_test, dt_chile_ML)
ml_multiclass_classification_evaluator(dt_chile_pred)
###End SparkML

###h2o
library(sparklyr); library(rsparkling)
library(dplyr); library(h2o)
library(car)


#only if you started a new R session since previous codes
#sc <- spark_connect(master = 'local')
#dt_chile <- copy_to(sc, Chile, 'chile')
#dt_chile <- na.omit(dt_chile)
#partitions <- dt_chile %>% sdf_partition(training = 0.7, test = .3, seed = 50)

chile_training_h2o <- as_h2o_frame(sc, partitions$training, 
                                   strict_version_check = FALSE)
chile_test_h2o <- as_h2o_frame(sc, partitions$test, 
                               strict_version_check = FALSE)

dt_chile_ML <- h2o.gbm(y = "vote", training_frame = as.factor(chile_training_h2o)
                       , model_id = "ModelTree")
dt_chile_pred <- h2o.performance(dt_chile_ML, newdata = as.factor(chile_test_h2o))

h2o.mse(dt_chile_pred)
###End h2o
##End Third section


##Fourth section
library(sparklyr)
show_version <- function(sc) {
  spark_context(sc) %>% 
    invoke("version") 
}

sc <- spark_connect(master = "local")
show_version(sc)
##End Fourth section

