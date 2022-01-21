# Instalar un paquete .
install.packages (" ggpubr ")
# Primera forma de importar un paquete .
library ( ggpubr )
# Segunda forma de importar un paquete .
require ( ggplot2 )
# Importar un paquete , instal á ndolo de ser necesario .
if (! require ( dplyr )){
  install.packages (" dplyr ", dependencies = TRUE )
  require ( dplyr )
}