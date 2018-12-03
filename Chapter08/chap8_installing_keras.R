# Installing Python modules
# ONLY IF conda IS AVAILABLE
# if(!require('reticulate')){install.packages('reticulate')}
# library(reticulate)

# py_install('numpy')
# py_install('scipy')
# py_install('graphviz')
# py_install('keras')
# py_install('tensorflow')
# py_install('matplotlib')
# py_install('pydot-ng')
# py_install('h5py')
# ALTERNATIVE WAY
modules <- c('numpy','scipy','graphviz',
             'keras', 'tensorflow',
             'matplotlib','pydot-ng','h5py')
for(i in modules){
  system(paste('python -m pip install --upgrade', i, sep = ' '))
}
# Installing a newer version of reticulate
devtools::install_github('rstudio/reticulate')
# installing keras
install.packages('keras')
library(keras)
install_keras()