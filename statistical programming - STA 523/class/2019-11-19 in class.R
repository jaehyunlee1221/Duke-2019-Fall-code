#install.packages("sparklyr")
library(sparklyr)

spark_available_versions()
sparklyr::spark_get_java()
#spark_install(version = "2.4")

conf <- list(
  sparklyr.cores.local = 4,
  `sparklyr.shell.driver-memory` = "16G",
  spark.memory.fraction = 0.5
)

Sys.setenv(JAVA_HOME = "C:/Program Files/java/jdk1.8.0_121")

sc <- spark_connect(master = "local",
                    spark_home = "/data/spark/spark-2.4.0-bin-hadoop2.7/")

sc <- spark_connect(master = "local",
                    version = "2.4.3",
                    config = conf)

#add data

cab <- spark_read_csv(sc,name = "cab",
                      path = ``)