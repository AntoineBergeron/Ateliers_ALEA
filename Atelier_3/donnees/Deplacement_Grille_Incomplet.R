# Fonction qui permet de modifier la direction de marche de la personne
#
# ENTREES:
#   DirectionDepart:    la direction vers laquelle la personne se deplace pour le moment.
#   Probabilite:        un vecteur contenant les probabilites d'aller dans l'une ou l'autre des directions. 
#                       Probabilite = c(ProbabiliteGauche, ProbabiliteDroite)
#
# SORITES:
#   NouvelleDirection:  la direction vers laquelle la personne va se deplacer au prochain carrefour.
#
ChangerDirection = function(DirectionDepart, Probabilite){
  
  # A COMPLETER: Mettre la condition pour tourner a gauche
  if(){ # <-- Manque du code
    # On tourne a gauche
    NouvelleDirection = DirectionDepart - pi/2
  }
  else{
    # On tourne a droite
    NouvelleDirection = DirectionDepart + pi/2
  }
  
  return(NouvelleDirection)
  
}

# Fonction qui permet de calculer les nouvelles coordonnees de la personne suite a un deplacement
#
# ENTREES:
#   PositionCourante:   un vecteur contenant la position de la personne pour le moment.
#                       PositionCourante = c(PositionX, PositionY)
#   Direction:          la direction vers laquelle la personne va se deplacer.
#
# SORITES:
#   NouvellePosition:   un vecteur contenant la position de la personne apres son deplacement.
#                       NouvellePosition = c(NouvellePositionX, NouvellePositionY)
#
Deplacement = function(PositionCourante, Direction){
  
  # A COMPLETER: Calcul des nouvelles coordonnées
  NouvellePositionX =  + sin(Direction)*250 # <-- Manque du code
  NouvellePositionY =  + cos(Direction)*250 # <-- Manque du code
  
  # On place les nouvelles positions pour chacune des coordonnees dans un vecteur
  NouvellePosition = c(NouvellePositionX, NouvellePositionY)
  
  return(NouvellePosition)
}