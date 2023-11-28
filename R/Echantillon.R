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

