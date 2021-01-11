

######################
# Fonction qui permet de déterminer les coordonnées du plan cartésien empruntées par Rémi dans le labyrinthe.
#
# ENTREES: 
#   posXinit:                Position initiale de Rémi en x.
#   posYinit:                Position initiale de Rémi en y.
#   directionRemi:           Dans le plan cartésien, vers quelle direction doit se diriger Rémi? 
#                            ("droite", "haut", "gauche", "bas")
#   distance1:               Lors du premier mouvement de Rémi, combien de mètres doit-il avancer?
#   distance2:               Lors du deuxième mouvement de Rémi, combien de mètres doit-il avancer?
#   changementdirection1:    Entre le premier et le deuxième mouvement, quel sera le changement de direction
#                            de Rémi? ("droite", "gauche")
#   nbmouvement:             Combien de fois Rémi fera le mouvement répétitif semblable à un L?
#   
# SORTIES:
#   xy:                      Les différentes coordonnées (x,y) empruntées par Rémi sous forme d'une matrice
#   
#######################


mouvement <- function(posXinit,posYinit,directionRemi,distance1,distance2,changementdirection1,nbmouvement){
  xy <- matrix(0, nrow = 2*nbmouvement+2, ncol = 2)  # Création d'une matrice des coordonnées (x,y)
  xy[1,] <- c(posXinit,posYinit)  # Position initiale dans le plan cartésien
  
  # Si Rémi regarde à (gauche/haut/droite/bas), alors l'angle doit être de...
  if(directionRemi == "gauche"){
    angle <- 3*pi/2
  } else if(directionRemi == "droite"){
    angle <- pi/2
  } else if(directionRemi == "haut"){
    angle <- 0
  } else if(directionRemi == "bas"){
    angle <- pi
  }
  
  # Si Rémi doit tourner à (droite/gauche) au premier tournant, alors l'angle sera modifier
  if(changementdirection1 == "droite"){
    angle2 <- pi/2
  } else if(changementdirection1 == "gauche"){
    angle2 <- -pi/2
  }
  
  # Pour chaque mouvement de style "L", on calcule la coordonnée de Rémi au creux et à la fin du "L"
  for (i in 1:nbmouvement){
    xy[2*i,] <- c(xy[2*i-1,1] + sin(angle)*distance1, xy[2*i-1,2] + cos(angle)*distance1)
    xy[2*i+1,] <- c(xy[2*i,1] + sin(angle + angle2)*distance2, xy[2*i,2] + cos(angle + angle2)*distance2)
  }
  
  # On ajoute la dernière ligne droite nécessaire
  xy[2*nbmouvement+2,] <- c(xy[2*nbmouvement+1,1] + sin(angle)*distance1, xy[2*nbmouvement+1,2] + cos(angle)*distance1)
  return(xy)
}


# Exemple pour la section 1:

section1 <- mouvement(0,0,"droite",50,30,"droite",2) # Besoin de 6 paires de coordonnées.

# Section 2:

section2 <- mouvement() # À REMPLIR

# Section 3:

section3 <- mouvement() # À REMPLIR

# Section 4:

# Comment déterminer la coordonnée de la dernière emplacement (FIN)?







