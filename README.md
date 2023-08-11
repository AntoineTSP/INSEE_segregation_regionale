# AU33_Regional

@auteur : Antoine Klein

## AU33 : Extension à l'échelle régionale

## KIT AU33

Ce dépôt est une extension du kit AU33 interne à l'INSEE dispoible ici :
<https://gitlab.insee.fr/psar-au/groupe_au33/au33_dev>

Ce dernier permettait de mesurer les disparités spatiales selon les
revenus à l'infracommunal. Sa documentation se trouve par ici :
<http://psar-au.gitlab-pages.insee.fr/groupe_au33/documentation_AU33/>

## Description : Ce dépôt étend le kit à l'échelle de la région du Grand Est.

Le code permet de :\
-Définir la fenêtre temporelle d'étude (00a)\
-Définir le nombre de groupes sociaux (00a)\
-Définir le seuil du nombre de ménages minimum à retenir par maille
(00a)\
-Définir la maille élémentaire comme carreau de côté choisi par
l'utilisateur (00a)\

-Définir le teritoire d'étude en shapefile (shp) (04)\
-Récupérer des statistiques territoriales de la région (04)\
-Définir la coupe inter/intra avec un autre shapefile (04)\
-Affecter chaque ménage localisé à son carreau et effectue le calcul des
indices de Theil (04)\
-Garder trace de la proportion des ménages écartés par le filtre de
privacy (04)\

-Fournir des CSV qui contiennent les mesures de indices de Theil et une
colonne GEOMETRY pour effectuer les cartes sous QGIS (11)\
-Mesure de 2004 à 2019 de la ségrégation à l'échelle de la région (11)\
-Mesurer les composantes de ségrégation de chaque groupe social au cours
du temps (11)\
-Mesurer les contributions de ségrégation de chaque groupe social au
cours du temps (11)\
-Tracer les histogrammes de revenus de chaque sous-territoire (11)\
-Obtenir la part de population vivant dans le sous-territoire d'étude au
cours du temps (11)\
-Décomposer la ségrégation totale en composantes inter/intra au cours du
temps (11)\
-Décomposer la ségrégation totale en contributions inter/intra au cours
du temps (11)\
-Obtenir les évolutions en base 100 de chaque composante inter/intra
(11)\
-Comparer en base 100 les évolutions de la ségrégation totale au cours
du temps en fonctions de la taille des carreaux (11)\

-Itérer la coupe inter/intra en fonction du critère "toutes les communes
ayant moins de X habitants VS les autres" (11)\
-Récupérer sur chacune de ces itérations : (11)\
-Evolutions des médianes de revenu pour chaque sous-territoire (11)\
-Evolutions des médianes de revenu en base 100 pour chaque
sous-territoire (11)\
-Histogramme de revenu pour chaque sous territoire 2004 VS 2019 (11)\
-Part de la population vivant dans le territoire sous le seuil (11)\
-Comparer en fonction de ces seuils la décomposition de la ségrégation
totale en inter/intra (11)\

## Visuels


## Installation

La liste des packages utilisées est inscrites et importées dans (00a).
Toutes les dépendances sont compatibles avec AUSV3 !

## Usage

Ce dépôt reprend le dataset nettoyé du Kit33. Pour l'utiliser, vous
aurez besoin de demander les droits de lecture du coffre :
HAB-PSAR-AU-AU33-DEV.

Hyperparamètres à définir à l'usage :\

-Variable: ce qu'elle décrit \| sa classe \| mon utilisation \| fichier
R qui l'utilise\

-millesi : vecteur des années à étudier \| vecteur de chaînes de
character \| de "04" à "19" \| (00a)\
-nb_tranches : nombre de groupes sociaux \| integer \| 5 \| (00a)\
-nbmen_min : nombre de ménages minimum à garder dans chaque maille \|
integer \| 20 \| (00a)\
-liste_taille_carreaux : liste des côtés des carreaux à traiter \| list
of integer \| list(4000) \| (00a)\
-reg : code de la région à étudier \| character \| "44" \| (00a)\

-lmen: liste pour chaque année des tableaux de données des ménages \|
list of dataframe \| à récupérer du coffre \| (04)\
-dep_valid_met : liste des codes départementaux du territoire d'étude
pour le filtrage des ménages \| vector of integer
\|c(paste0("0",8),10,51,52,54,55,57,67,68,88) \| (04)\
-shapefile : contours bdtopo de toutes les communes \| fichier shapefile
\| commune_bdtopo_franceentiere_2022.shp \| (04)\
-com_rural : nomenclature des communes rurales \| fichier Excel \|
<https://www.insee.fr/fr/statistiques/5039991?sommaire=5040030> \| (04)\
-region: nomenclature des régions \| fichier csv \| v_region_2023.csv \|
(04)\
-Palette: liste de couleur pour les plots \| list of hexadimals \|
c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
\| (11)\
-Liste_seuil : liste des seuils de ménages tels que la région devient
coupée en "territoire de communes ayant moins de X ménages / territoire
de communes ayant plus que X ménages" \| list of integer \| (11)\

Sorties pertinentes du code :\
-sf_rural_region_inters: liste des contours des communes rurales de la
région \| shapefile \| à mettre sur QGIS pour observer \| (04)\
-territoire_rural/urbain : contour du territoire rural/urbain \|
shapefile \| QGIS pour observer \| (04)\
-liste_mediane_rural/urbaine/region : médiane des revenus des ménages de
2004 à 2019 vivant sur le territoire en question \| list of integer \| à
executer pour observer \| (04)\
-datadash : tableau qui contient les indices de Theil et toutes le
grandeurs de sorties \| list of dataframe \| à executer pour observer \|
(05)\
-data : tableau avec les grandeurs interessantes et une colonne avec la
géométrie pour QGIS \| dataframe \| à mettre sous QGIS pour observer \|
(11)\
-Statistiques : statistiques descriptives du filtre (20men/maille) \|
dataframe \| à ouvrir sur Open Office pour observer \| (11)\

## Support

Au moindre besoin, je suis joignable par mail jusqu'à fin août 2023 :
[antoine.klein\@insee.fr](mailto:antoine.klein@insee.fr){.email}

Au delà de cette date, veuillez me joindre au travers du mail de ma
formation :
[antoine.klein\@ensae.fr](mailto:antoine.klein@ensae.fr){.email}

## Roadmap

Futurs axes de travail: 
-Exécuter le code sur d'autres régions.
-Etablir des graphiques de comparaison inter-régions.

## Contribution

Ce dépot, lui-même extension du kit AU33, est ouvert à tout ajout !

## Auteur et remerciements

Dépôt crée par Antoine Klein, stagiaire ENSAE de la DR67 entre Juin et
Août 2023 Je remercie la DR67 de l'INSEE pour m'avoir fait confiance au
travers de ce stage d'été lors de ma 2A ENSAE. Merci à Vivien Heim pour
cette confiance, à Vincent Monchâtre pour la supervision du stage et à
Lionel Cacheux pour m'avoir aidé lors de la cartographie.

## Licence

MIT License

## Statut du projet

Projet toujours en cours d'élaboration
