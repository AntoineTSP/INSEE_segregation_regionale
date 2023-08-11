---
editor_options: 
  markdown: 
    wrap: 72
---

# AU33_Regional

@auteur : Antoine Klein

## AU33 : Extension � l'�chelle r�gionale

## KIT AU33

Ce d�p�t est une extension du kit AU33 interne � l'INSEE dispoible ici :
<https://gitlab.insee.fr/psar-au/groupe_au33/au33_dev>

Ce dernier permettait de mesurer les disparit�s spatiales selon les
revenus � l'infracommunal. Sa documentation se trouve par ici :
<http://psar-au.gitlab-pages.insee.fr/groupe_au33/documentation_AU33/>

## Description : Ce d�p�t �tend le kit � l'�chelle de la r�gion du Grand Est.

Le code permet de :\
-D�finir la fen�tre temporelle d'�tude (00a)\
-D�finir le nombre de groupes sociaux (00a)\
-D�finir le seuil du nombre de m�nages minimum � retenir par maille
(00a)\
-D�finir la maille �l�mentaire comme carreau de c�t� choisi par
l'utilisateur (00a)\

-D�finir le teritoire d'�tude en shapefile (shp) (04)\
-R�cup�rer des statistiques territoriales de la r�gion (04)\
-D�finir la coupe inter/intra avec un autre shapefile (04)\
-Affecter chaque m�nage localis� � son carreau et effectue le calcul des
indices de Theil (04)\
-Garder trace de la proportion des m�nages �cart�s par le filtre de
privacy (04)\

-Fournir des CSV qui contiennent les mesures de indices de Theil et une
colonne GEOMETRY pour effectuer les cartes sous QGIS (11)\
-Mesure de 2004 � 2019 de la s�gr�gation � l'�chelle de la r�gion (11)\
-Mesurer les composantes de s�gr�gation de chaque groupe social au cours
du temps (11)\
-Mesurer les contributions de s�gr�gation de chaque groupe social au
cours du temps (11)\
-Tracer les histogrammes de revenus de chaque sous-territoire (11)\
-Obtenir la part de population vivant dans le sous-territoire d'�tude au
cours du temps (11)\
-D�composer la s�gr�gation totale en composantes inter/intra au cours du
temps (11)\
-D�composer la s�gr�gation totale en contributions inter/intra au cours
du temps (11)\
-Obtenir les �volutions en base 100 de chaque composante inter/intra
(11)\
-Comparer en base 100 les �volutions de la s�gr�gation totale au cours
du temps en fonctions de la taille des carreaux (11)\

-It�rer la coupe inter/intra en fonction du crit�re "toutes les communes
ayant moins de X habitants VS les autres" (11)\
-R�cup�rer sur chacune de ces it�rations : (11)\
-Evolutions des m�dianes de revenu pour chaque sous-territoire (11)\
-Evolutions des m�dianes de revenu en base 100 pour chaque
sous-territoire (11)\
-Histogramme de revenu pour chaque sous territoire 2004 VS 2019 (11)\
-Part de la population vivant dans le territoire sous le seuil (11)\
-Comparer en fonction de ces seuils la d�composition de la s�gr�gation
totale en inter/intra (11)\

## Visuels


## Installation

La liste des packages utilis�es est inscrites et import�es dans (00a).
Toutes les d�pendances sont compatibles avec AUSV3 !

## Usage

Ce d�p�t reprend le dataset nettoy� du Kit33. Pour l'utiliser, vous
aurez besoin de demander les droits de lecture du coffre :
HAB-PSAR-AU-AU33-DEV.

Hyperparam�tres � d�finir � l'usage :\

-Variable: ce qu'elle d�crit \| sa classe \| mon utilisation \| fichier
R qui l'utilise\

-millesi : vecteur des ann�es � �tudier \| vecteur de cha�nes de
character \| de "04" � "19" \| (00a)\
-nb_tranches : nombre de groupes sociaux \| integer \| 5 \| (00a)\
-nbmen_min : nombre de m�nages minimum � garder dans chaque maille \|
integer \| 20 \| (00a)\
-liste_taille_carreaux : liste des c�t�s des carreaux � traiter \| list
of integer \| list(4000) \| (00a)\
-reg : code de la r�gion � �tudier \| character \| "44" \| (00a)\

-lmen: liste pour chaque ann�e des tableaux de donn�es des m�nages \|
list of dataframe \| � r�cup�rer du coffre \| (04)\
-dep_valid_met : liste des codes d�partementaux du territoire d'�tude
pour le filtrage des m�nages \| vector of integer
\|c(paste0("0",8),10,51,52,54,55,57,67,68,88) \| (04)\
-shapefile : contours bdtopo de toutes les communes \| fichier shapefile
\| commune_bdtopo_franceentiere_2022.shp \| (04)\
-com_rural : nomenclature des communes rurales \| fichier Excel \|
<https://www.insee.fr/fr/statistiques/5039991?sommaire=5040030> \| (04)\
-region: nomenclature des r�gions \| fichier csv \| v_region_2023.csv \|
(04)\
-Palette: liste de couleur pour les plots \| list of hexadimals \|
c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
\| (11)\
-Liste_seuil : liste des seuils de m�nages tels que la r�gion devient
coup�e en "territoire de communes ayant moins de X m�nages / territoire
de communes ayant plus que X m�nages" \| list of integer \| (11)\

Sorties pertinentes du code :\
-sf_rural_region_inters: liste des contours des communes rurales de la
r�gion \| shapefile \| � mettre sur QGIS pour observer \| (04)\
-territoire_rural/urbain : contour du territoire rural/urbain \|
shapefile \| QGIS pour observer \| (04)\
-liste_mediane_rural/urbaine/region : m�diane des revenus des m�nages de
2004 � 2019 vivant sur le territoire en question \| list of integer \| �
executer pour observer \| (04)\
-datadash : tableau qui contient les indices de Theil et toutes le
grandeurs de sorties \| list of dataframe \| � executer pour observer \|
(05)\
-data : tableau avec les grandeurs interessantes et une colonne avec la
g�om�trie pour QGIS \| dataframe \| � mettre sous QGIS pour observer \|
(11)\
-Statistiques : statistiques descriptives du filtre (20men/maille) \|
dataframe \| � ouvrir sur Open Office pour observer \| (11)\

## Support

Au moindre besoin, je suis joignable par mail jusqu'� fin ao�t 2023 :
[antoine.klein\@insee.fr](mailto:antoine.klein@insee.fr){.email}

Au del� de cette date, veuillez me joindre au travers du mail de ma
formation :
[antoine.klein\@ensae.fr](mailto:antoine.klein@ensae.fr){.email}

## Roadmap

Futurs axes de travail: 
-Ex�cuter le code sur d'autres r�gions.
-Etablir des graphiques de comparaison inter-r�gions.

## Contribution

Ce d�pot, lui-m�me extension du kit AU33, est ouvert � tout ajout !

## Auteur et remerciements

D�p�t cr�e par Antoine Klein, stagiaire ENSAE de la DR67 entre Juin et
Ao�t 2023 Je remercie la DR67 de l'INSEE pour m'avoir fait confiance au
travers de ce stage d'�t� lors de ma 2A ENSAE. Merci � Vivien Heim pour
cette confiance, � Vincent Monch�tre pour la supervision du stage et �
Lionel Cacheux pour m'avoir aid� lors de la cartographie.

## Licence

MIT License

## Statut du projet

Projet toujours en cours d'�laboration
