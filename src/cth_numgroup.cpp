#include <Rcpp.h> //utilisation du package Rcpp pour faire le lien entre du C++ et du R
using namespace Rcpp; 

// [[Rcpp::export]]
NumericVector cth_numgroup(NumericVector cvo) {
  //Fonction qui permet de numeroter dans l'ordre croissant 
  //un vecteur et de laisser 0 quand le vecteur initial vaut 0
  int nb = 1;
  NumericVector cvod(cvo.length()); //creation d'un vecteur de meme longueur que le vecteur cvo
  cvod[0] = nb;
  for(int i = 1; i<cvo.length(); i++){
    if(cvo[i-1]!=cvo[i]) nb++; //Si la valeur du vecteur est différente de la precedente alors on augmente nb de 1
    if(cvo[i]!=0) cvod[i] = nb; //si la valeur est differente de 0 alors on met dans le nouveau vecteur la valeur de nb sinon on laisse 0 
  }
  return cvod; //on retroune le vecteur
}