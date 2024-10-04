# Dashboard_Exploration
Application web intéractive développée en langage R et la librairie shiny, qui permet le chargement des fichiers de données de différents formats,le prétraitement ,l'exploration et la visualisation des données, réalisation des calculs statistiques ainsi qu'une partie d'apprentissage automatique .Et donc cet outil se base sur deux axes :Analyse descriptive(Univariée et Bivariée) et explicative des données (des modéles de machine Learning)

Importation du fichier de données :

           Importataion de différents types de fichiers (csv,txt,xlsx,data...) avec différents séparateurs (virgule,espace,tabulation,point,point-virgule)
           
L'onglet Dataset :
           Affichage de la table de données avec le nombre de lignes et de colonnes 
           
L'onglet prétraitement de données : c'est l'ensemble de méthodes ou de processus de traitement de données brutes afin de les rendre fiables ,exploitables et utiles  dans des éventuels modéles d'apprentissage automatique .

       Identification et traitement des valeurs abéranntes :l'utilisateur décide de garder ou supprimer l'observation
       
       Identification et traitement des valeurs manquantes :
       
               Variable quantitative : moyenne , médiane ,interpolation ou suppression de la variable
               
               Variable qualitative:mode , dernière valeur rencontrée , suppression de la variable
               
        Normalisation des données  :Min_Max , Z_score
        
        Réequilibrage des données :over_sampling ,under_sampling
        
        Dummification des variables catégorielles
        
Exploration des données :

        Summary détaillé du jeu de données 
        
        Heatmap de corrélation entre toute les variables du dataset importé
        
        Fouille des données : détail des types des variables et la possibiliter de les renommer 

        
L'onglet Tests statistiques sur les variables quantitatives:

        Tests statistiques: A-D Test,Shapiro,KS-Test,MV-Shapiro
        
        Corrélation:Covariance,KarlPearson,Spearman,Kendals
        

L'onglet Analyse Univariée :

                Quantitative:
                
                    Numérique (caractéristiques de tendance centrale et de dispersion) : moyenne, écart-type, variance, minimum, Q1, médiane, Q3, maximum
                    
                    Histogramme des éffectifs
                    
                    Boîte à moustache
                    
                    Diagramme de battons des éffectifs
                    
                    Courbe cumulative
                    
                Qualitative:
                
                    Numérique : modalités, effectifs, fréquences 
                    
                    Diagramme en secteurs
                    
                    Diagramme en colonnes 
                    
  L'onglet Analyse Bivariée:
  
                2 Quantitatives:
                
                      Nuage de points
                      
                      Histogramme
                      
                      Nuage de points et Histogramme
                      
                      Histogramme dos à dos
                      
                2 Qualitatives:
                
                      Diagramme en barres 
                      
                      Les indices Chi2(X2), Phi2 et Cramer
                      
                Quantitative vs qualitative :
                
                        Diagramme en barres
                        
                        Boite en paralléle
                        
                        
 L'onglet Machine Learning :
 
        Le dataset choisi :Employee Attrition qui fait partie des datasets proposés dans le module de Machine Learning et sur lequel entrain de travailler avec le langage Python, on s'est dit pourquoi pas faire son équivalent en langage R pour équilibrer entre les deux langages .En effet , on estime que notre choix est bon car il nous permet de faire le lien entre les différentes fonctions d'apprentissage automatique disponibles en Python et R .
        
        les modéles choisis : Random Forest ,Decision Tree , Logistic Regression , Naive Bayes ,LDA,KNN et SVM 
        
        pour chaque modéle , on calcule Precision , Recall et F-score , courbe PR et auc selon la précision et le Recall déja calculés
        
        A paramétrer : la portion du Test et du Train set+ over_sampling ,under samling ou sans-sampling 
        
 L'onglet Features Selection :
 
       Pour chaque modéle cité précédemment , on essaye de générer un summary (statistiques) sur l'étape d'apprentissage du modéle sur nos données , pour détérminer le poids de chaque Feature dans ce modéle et puis décider des variables les plus discriminantes (explicatives ou importantes).
       
       
       
       
Quelques remarques issues des analyses univariée et bivariée du jeu de données:

                      Notre dataset est désiquilibré , car 84% des observavtions appartiennet a la classe No et seulement 16% qui apparteiennet a la classe Yes

                      Les jeunes ont tendance à avoir des taux d'attrition plus élevés.

                      Le département des ventes a le taux d'attrition le plus élevé, les ressources humaines étant légèrement inférieures à celui-ci.

                      La proportion d'employés qui ont quitté était comparativement moindre parmi les employés senior. Peut-être que ces employés étaient à une autorité supérieure et étaient moins susceptibles de quitter ce poste.

                      Suivent les employés à mi-carrière, les niveaux d'attrition parmi les employés les plus récents et les moins expérimentés semblent être les plus élevés. Ces salariés sont peut-être partis pour des offres plus lucratives ou des études supérieures.

                      Une attrition majeure s'est produite parmi les employés à mi-carrière. Le changement de carrière et la croissance de carrière pourraient être l'une des possibilités d'un tel niveau d'attrition.

                      L'âge des employés varie entre 18 et 60 ans, ce qui semble être intuitivement vrai car l'âge de travail sans restriction commence principalement entre 18 et 60 ans. 

                      La plupart des employés restent plus près de l'emplacement du bureau.

                      Il y a une proportion plus élevée d'employés churner qui restent loin du bureau (Plus de 10 KM) que la proportion d'employés qui n'ont pas quitté l'entreprise et restent loin du bureau.

                      Parmi les employés occupant divers postes, la distance moyenne entre le travail et le domicile pour les employés des représentants de la santé, les employés des ressources humaines, les gestionnaires et les cadres commerciaux qui ont quitté l'entreprise est supérieure à la distance moyenne entre le travail et le domicile pour les employés qui ne sont pas partis .

Modéles de classifications :

   En se basant sur les différentes variables, en en convertissant la variable catégorielle Attrition en une variable numérique discrète, nous divisons notre base de données en deux sous-ensembles : entraînement et test. Nous utilisons un esemble de modéles de classifications  :
                      a.	Random Forest : le meilleur résultat obtenu en choisissant 14 voisons est un taux de bonne prédiction de 80% ;

                      b.	Logistic Regression : un taux de bonne prédiction 79% ;

                      c.	SVM : le meilleur résultat a été obtenu avec le kernel linear avec un taux de bonne prédiction de 85.31% ;

                      d.	Decision Tree : le meilleur résultat obtenu avec un taux de prédiction d’environ 70.00%.

                      e.    LDA,KNN,Naive Bayes  :les meilleurs résultats obtenu avec des taux de bonne prédiction qui varie entre 60-70%.

  Les meilleurs algorithmes sont le SVM et le Random Forest. 




# Lien de l'application déployée :
https://hamel-amir.shinyapps.io/Projet_web/



        

