# Ada France Web Site

Ce projet contient les sources de l'application Ada France qui permet
au site [Ada France](https://www.ada-france.org) de diffuser des informations
et promouvoir le langage Ada.

# Version 1.18 - Mars 2023

- Supression des liens externes morts et ajout de nouveaux sites,
- Meilleure présentation sur les mobiles,
- Génération des recus en asynchrone avec AWA.Jobs.

# Version 1.16 - Dec 2022

- L'authentification par Github ou Gitlab est possible pour les membres enregistrés
- Intégration de l'éditeur EasyMDE pour éditer des articles et wiki en Markdown

# Version 1.13 - Mar 2022

- Mise à jour pour le renouvellement des cotisations

# Compilation

## Setup

Le serveur tourne sous Debian 10 at la configuration suivante est nécessaire avant de lancer
le `configure`:

```
sudo apt-get install gnat gprbuild libxmlada-dom8-dev \
   libaws18-dev libgpr2-dev imagemagick unzip xsltproc libmariadb-dev
```

## Build

Lancer la commande configure:
```
   ./configure
```

Ensuite lancer la compilation avec:
```
   make generate build
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

Une fois le setup terminée, l'application Ada France est lancée.

Pour re-faire une setup, supprimer le fichier `.initialized` et relancer la commande.

## Lancement normal:

Pour lancer l'application, utilisez:
```
   bin/adafr-server start --upload=./upload
```

Par défault, la base de donnée SQLite est créee avec l'utilisateur `admin` at `ada-france.org` et mot de passe `admin`.
Cet utilisateur n'existe pas en prod, inutile d'essayer!
