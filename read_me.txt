Dashboard user guide :

* Pour se connecter à la machine virtuelle : 
ssh root@163.172.188.58

* Pour copier les données uniquement (à adapter selon ce qu'on souhaite copier):
scp -rv "data" "root@163.172.188.58:/home/jlalo/covid_dashboard"

* Après la copie des fichier sources penser a modifier les chemin du working directory pour le fichier __app__.R, tous les fichiers des source et à changer l'adresse ip de sortie en ce qui concerne le fichier __app__.R à 0.0.0.0

* Pour lancer le processus : CMD BATCH __app__.R &

* Pour se déconnecter de la machine virtuelle : 
logout

* Pour arrêter la machine virtuelle : 
halt
