# Ada France Web Site

[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-france/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-france/summary)

Ce projet contient les sources de l'application Ada France qui permet
au site [Ada France](https://www.ada-france.org) de diffuser des informations
et promouvoir le langage Ada.

# Version 1.22 - Under development
- Utilisation de AWA 2.6.0
- Suppression des scripts configure

# Version 1.21 - Oct 2023
- Utilisation de AWA 2.5.0

# Version 1.20 - Juin 2023
- Construction avec Alire
- Mise à jour de AWA

# Version 1.19 - Mai 2023
- Ajout du status `INACTIVE` pour marquer un membre qui n'est plus actif
  ou qui utilise une autre adresse mail
- Correction du mail pour changer son mot de passe

# Version 1.18 - Mars 2023

- Supression des liens externes morts et ajout de nouveaux sites,
- Meilleure présentation sur les mobiles,
- Génération des recus en asynchrone avec AWA.Jobs.

# Version 1.16 - Dec 2022

- L'authentification par Github ou Gitlab est possible pour les membres enregistrés
- Intégration de l'éditeur EasyMDE pour éditer des articles et wiki en Markdown

Cette version est basee sur SQLite et utilise Alire pour la construction
et gestion des dependances.

# Version 1.13 - Mar 2022

- Mise à jour pour le renouvellement des cotisations

# Compilation

## Setup

Pour une compilation sous Debian 12, la configuration suivante est nécessaire:

```
sudo apt-get install imagemagick unzip xsltproc \
  libmariadb-dev mariadb-server mariadb-client libsqlite3-dev \
  context context-modules libjpeg-turbo-progs closure-compiler
```

Notes:

* `imagemagick` est utilisé pour la conversion d'images,
* `context` est utilisé pour la génération des attestations
* `alire` 2.x est nécessaire et doit etre récupéré via https://ada-lang.io/

## Build

Récupération des sources et des sous-modules git:

```
git clone --recursive https://github.com/Ada-France/ada-france.git
cd ada-france
```

Ensuite lancer la compilation avec:
```
alr update
alr build -- -XSOCKET=openssl
```

# Lancement

## Setup

Pour configurer le serveur la première fois, lancer la commande:

```
  bin/adafr-server -v setup adafr
```

Faire le setup dans le navigateur en allant sur la page: http://localhost:8080/adafr/index.html
Le setup permet de:

* définir l'accès à la base de données,
* configurer les secrets pour OAuth2 (Google, Facebook),
* configurer les paramètres SMTP pour envoyer les mails

Une fois le setup terminé, l'application Ada France est lancée.

Pour re-faire une setup, supprimer le fichier `.initialized` et relancer la commande.

## Lancement normal:

Pour lancer l'application, utilisez:
```
   bin/adafr-server start --upload=./upload
```

Par défault, la base de donnée SQLite est créée avec l'utilisateur `admin` at `ada-france.org` et mot de passe `admin`.
Cet utilisateur n'existe pas en prod, inutile d'essayer!
