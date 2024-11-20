# Fonction pour vérifier le type d'un objet
check_type <- function(obj, type) {
    if (!is(obj, type)) {
        stop(paste("L'objet", obj, "doit être de type", type))
    }
}
