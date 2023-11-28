#' Définir une fonction qui génère un échantillon aléatoire d'une distribution donnée
#' avec une taille et des paramètres spécifiés
#' @param dist une chaîne de caractères qui indique le nom de la distribution
#' @param size un entier positif qui indique la taille de l'échantillon
#' @param ... les paramètres de la distribution
#' @return un vecteur numérique qui contient l'échantillon aléatoire
echantillon <- function(dist, size, ...) {
  # Utiliser la fonction switch pour choisir la distribution
  switch(dist,
         "normale" = rnorm(size, ...), # Distribution normale
         "uniforme" = runif(size, ...), # Distribution uniforme
         "binomiale" = rbinom(size, ...), # Distribution binomiale
         "poisson" = rpois(size, ...), # Distribution de Poisson
         "exponentielle" = rexp(size, ...), # Distribution exponentielle
         stop("Distribution non reconnue") # Erreur si la distribution n'est pas reconnue
  )
}

#' Définir une fonction qui calcule la moyenne empirique d'un vecteur
#' @param x un vecteur numérique
#' @return un nombre réel qui représente la moyenne empirique
moyenne <- function(x) {
  mean(x) # Utiliser la fonction mean de R
}


#' Définir une fonction qui calcule la variance empirique d'un vecteur
#' @param x un vecteur numérique
#' @return un nombre réel qui représente la variance empirique
variance <- function(x) {
  var(x) # Utiliser la fonction var de R
}


#' Définir une fonction qui calcule l'écart-type empirique d'un vecteur
#' @param x un vecteur numérique
#' @return un nombre réel qui représente l'écart-type empirique
ecart_type <- function(x) {
  sd(x) # Utiliser la fonction sd de R
}

