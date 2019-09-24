if(inherits(jdd,"data.frame")){
  my_data <- jdd
  choix_var_expliquer <- quanticondesshiny[1]
  valeur_proba <- 1
  couleur_basse <- "#03F7FF"
  couleur_haute <- "#F00000"
  choix_var_quanti <- colnames(jdd)[(sapply(jdd,is.numeric))]
  choix_var_quali <- colnames(jdd)[(sapply(jdd,is.factor))]
}

if(inherits(jdd,"condesshiny")){
  my_data <- jdd$donnees
  choix_var_expliquer <- jdd$explain
  valeur_proba <- jdd$proba
  couleur_basse <- jdd$col_basse
  couleur_haute <- jdd$col_haute
  choix_var_quanti <- jdd$var_quanti
  choix_var_quali <- jdd$var_quali
}

if(inherits(jdd,"condes")){
  my_data <- jdd$call$X
  choix_var_expliquer <- colnames(jdd$call$X)[jdd$call$num.var]
  valeur_proba <- jdd$call$proba
  couleur_basse <- "#03F7FF"
  couleur_haute <- "#F00000"
  choix_var_quanti <- colnames(jdd$call$X)[sapply(jdd$call$X,is.numeric) & colnames(jdd$call$X) != choix_var_expliquer]
  choix_var_quali <- colnames(jdd$call$X)[sapply(jdd$call$X,is.factor)]
}