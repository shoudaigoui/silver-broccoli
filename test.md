_sinus-processus_

Pour la suite, il faudrait réfléchir aux spécifications de la mémoire.

Je pense que des mots de 32 bits devraient suffire. Ce n'est pas nécessaire d'aller au-dessus et ça prend de la mémoire inutilement,
mais en-dessous, on risque d'avoir des problèmes pour assurer la compatibilité MIPS.

La mémoire cache peut être utile, peut-être pour stocker l'heure et y avoir un accès rapide, justement. D'un autre coté, on pourrait
utiliser un registre spécial qui ne servirait qu'à stocker l'heure, ou du moins pour cette application.


PS : on a le droit d'aller après la ligne 80 pour ce projet ou pas ?
