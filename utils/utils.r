# Fonction pour vérifier le type d'un objet
check_type <- function(obj, type) {
    if (!is(obj, type)) {
        stop(paste("L'objet", obj, "doit être de type", type))
    }
}

# Fonction pour indiquer quelle est la première modalité
# rencontrée d'une variable catégorielle
modalite_reference <- function(data, variable) {
    modalites <- unique(data[[variable]])
    modalites <- sort(modalites)
    modalite_reference <- modalites[length(modalites)]
    return(modalite_reference)
}
