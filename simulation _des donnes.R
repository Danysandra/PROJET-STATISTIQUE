# importation  des données 
install.packages("readODS")
install.packages("TOSTER")
library(TOSTER)
library(readODS)

# importation de toutes les données
# La varaible Long contient prend au départ deux valeurs; 2 ou 3
dataset_base <- read_ods("dosages_préliminaires.ods")

# on extrait de toute cette base de départ les observations pour lesquelles la variable Long vaut 3 
dataset <- subset(dataset_base , Long ==2 , select = c(Groupe , Hb))

dataset$Groupe <- relevel(dataset$factor(Groupe), ref = "T-")
modele <- lm(Hb ~ factor(Groupe) , data =  dataset )
modele <- lm(Hb ~ factor(relevel(dataset$Groupe, ref = "T-")) , data =  dataset )
# notre modèle de regression a pour groupe de reférence T-
# paramètres de la simulation 

# il faut noter que la modalité de reférence est la modalité T-

alpha_ <- 8.838 # pour la modalité T-
beta11_ <- -6.0773 # pour la modalité T+
beta12_ <- -5.665 # pour la modalité P
sigma_ <- 2.148

# génération d'observations          
erreur <- rnorm(100 ,0 , sigma_)

# hémoglobine pour le groupe de reférence H moins
Hb_T_moins <- alpha_ + erreur[1:33] 
Hb_T_plus <- alpha_ + beta11_ + erreur[34:66]
Hb_P <- alpha_ + beta12_ + erreur[67:100]
     
    
tsum_TOST(m1= alpha_ ,  sd1 = sigma_ , n1= 33 , m2 = alpha_ +beta12_ , sd2 = sigma_  , n2 = 34 , hypothesis = "EQU" , low_eqbound =  -0.3 , high_eqbound = 0.3)

testequi <- TOSTtwo(m1= alpha_ , m2 = alpha_ + beta12_, sd1 = sigma_ , sd2 = sigma_ , n1= 60 , n2 = 60 , low_eqbound_d =  -0.9 , high_eqbound_d = 0.9 )
# 
# T_moins <- rnorm(56 , 8.838 , 3.249215) # 
# T_plus <- rnorm(56 , 8.838 , 0.415968)
# testvrai <- 0
# for( n in 140:200){
#   T_moins <- rnorm(n , 8.838 , 3.249215) #
#   T_plus <- rnorm(n , 8.838 , 0.415968)
#   test <- TOSTtwo(m1 =mean(T_moins) , m2 = mean(T_plus), sd1 = 3.2492153 , sd2 = 0.41596 , n1= n , n2 = n , low_eqbound_d =  -0.3 , high_eqbound_d = 0.3)
#   if (max(test$TOST_p1 , test$TOST_p2)< 0.05 ){
#     testvrai= testvrai+ 1
#   }
#   
# }
# puissance <- testvrai/60
# puissance
# 
# testequi&diff&TOST_t1
