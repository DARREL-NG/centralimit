
#' Définir une fonction qui applique le théorème central limite à un échantillon
#' en calculant la distribution de la moyenne empirique
#' @param x un vecteur numérique qui représente l'échantillon
#' @param n un entier positif qui représente le nombre de sous-échantillons à tirer
#' @param m un entier positif qui représente la taille des sous-échantillons
#' @return une liste qui contient les éléments suivants:
#' - moyennes: un vecteur numérique qui contient les moyennes empiriques des sous-échantillons
#' - mu: un nombre réel qui représente la moyenne théorique de la distribution de la moyenne empirique
#' - sigma: un nombre réel qui représente l'écart-type théorique de la distribution de la moyenne empirique
#' - densite: une fonction qui calcule la densité de la distribution de la moyenne empirique
#'
#' @examples
#' echantillon_normale <- echantillon("normale", 1000, mean = 10, sd = 2)
#' #Appliquer le théorème central limite à cet échantillon avec n = 100 et m = 50
#' resultat <- theoreme_central_limite(echantillon_normale, 100, 50)
#' #Afficher les résultats
#' print(paste("La moyenne théorique de la distribution de la moyenne empirique est", resultat$mu))
#' print(paste("L'écart-type théorique de la distribution de la moyenne empirique est", resultat$sigma))
#' hist(resultat$moyennes, freq = FALSE, main = "Histogramme des moyennes empiriques", xlab = "Moyenne empirique")+
#' curve(resultat$densite(x), add = TRUE, col = "red", lwd = 2)
#'


theoreme_central_limite <- function(x, n, m) {
  # Vérifier que la taille de l'échantillon est supérieure ou égale à la taille des sous-échantillons
  if (length(x) < m) {
    stop("La taille de l'échantillon doit être supérieure ou égale à la taille des sous-échantillons")
  }
  # Tirer n sous-échantillons de taille m de l'échantillon x
  sous_echantillons <- replicate(n, sample(x, m, replace = TRUE))
  # Calculer les moyennes empiriques des sous-échantillons
  moyennes <- apply(sous_echantillons, 2, moyenne)
  # Calculer la moyenne théorique de la distribution de la moyenne empirique
  mu <- moyenne(x)
  # Calculer l'écart-type théorique de la distribution de la moyenne empirique
  sigma <- ecart_type(x) / sqrt(m)
  # Définir une fonction qui calcule la densité de la distribution de la moyenne empirique
  densite <- function(y) {
    loi_normale(y, mu, sigma)
  }
  # Retourner une liste qui contient les éléments calculés
  list(moyennes = moyennes, mu = mu, sigma = sigma, densite = densite)
}

