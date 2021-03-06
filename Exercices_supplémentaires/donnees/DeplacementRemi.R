

######################
# Fonction qui permet de d�terminer les coordonn�es du plan cart�sien emprunt�es par R�mi dans le labyrinthe.
#
# ENTREES: 
#   posXinit:                Position initiale de R�mi en x.
#   posYinit:                Position initiale de R�mi en y.
#   directionRemi:           Dans le plan cart�sien, vers quelle direction doit se diriger R�mi? 
#                            ("droite", "haut", "gauche", "bas")
#   distance1:               Lors du premier mouvement de R�mi, combien de m�tres doit-il avancer?
#   distance2:               Lors du deuxi�me mouvement de R�mi, combien de m�tres doit-il avancer?
#   changementdirection1:    Entre le premier et le deuxi�me mouvement, quel sera le changement de direction
#                            de R�mi? ("droite", "gauche")
#   nbmouvement:             Combien de fois R�mi fera le mouvement r�p�titif semblable � un L?
#   
# SORTIES:
#   xy:                      Les diff�rentes coordonn�es (x,y) emprunt�es par R�mi sous forme d'une matrice
#   
#######################


mouvement <- function(posXinit,posYinit,directionRemi,distance1,distance2,changementdirection1,nbmouvement){
  xy <- matrix(0, nrow = 2*nbmouvement+2, ncol = 2)  # Cr�ation d'une matrice des coordonn�es (x,y)
  xy[1,] <- c(posXinit,posYinit)  # Position initiale dans le plan cart�sien
  
  # Si R�mi regarde � (gauche/haut/droite/bas), alors l'angle doit �tre de...
  if(directionRemi == "gauche"){
    angle <- 3*pi/2
  } else if(directionRemi == "droite"){
    angle <- pi/2
  } else if(directionRemi == "haut"){
    angle <- 0
  } else if(directionRemi == "bas"){
    angle <- pi
  }
  
  # Si R�mi doit tourner � (droite/gauche) au premier tournant, alors l'angle sera modifier
  if(changementdirection1 == "droite"){
    angle2 <- pi/2
  } else if(changementdirection1 == "gauche"){
    angle2 <- -pi/2
  }
  
  # Pour chaque mouvement de style "L", on calcule la coordonn�e de R�mi au creux et � la fin du "L"
  for (i in 1:nbmouvement){
    xy[2*i,] <- c(xy[2*i-1,1] + sin(angle)*distance1, xy[2*i-1,2] + cos(angle)*distance1)
    xy[2*i+1,] <- c(xy[2*i,1] + sin(angle + angle2)*distance2, xy[2*i,2] + cos(angle + angle2)*distance2)
  }
  
  # On ajoute la derni�re ligne droite n�cessaire
  xy[2*nbmouvement+2,] <- c(xy[2*nbmouvement+1,1] + sin(angle)*distance1, xy[2*nbmouvement+1,2] + cos(angle)*distance1)
  return(xy)
}


# Exemple pour la section 1:

section1 <- round(mouvement(0,0,"droite",50,30,"droite",2)) # Besoin de 6 paires de coordonn�es.

# Section 2:

section2 <- mouvement() # � REMPLIR

# Section 3:

section3 <- mouvement() # � REMPLIR

# Section 4:

# Comment d�terminer la coordonn�e de la derni�re emplacement (FIN)?







