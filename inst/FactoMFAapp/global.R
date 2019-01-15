#global AFM
quanti=names(which(sapply(x,is.numeric)))
quali=names(which(!(sapply(x,is.numeric))))
VariableChoices=quanti
nom=rownames(x)
num=c(1:length(nom))
QualiChoice=quali
IdChoices=c(1:length(VariableChoices))
Idqualisup=c(1:length(QualiChoice))
title1=gettext("Groups representation")
title2=gettext("Individual factor map")
title3=gettext("Correlation circle")
title4=gettext("Graph of the partial axes")
title5=gettext("Graph of the frequencies")
