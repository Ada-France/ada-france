# Ada France Web Site

Ce projet contient les sources de l'application Ada France qui permet
au site [Ada France](https://www.ada-france.org) de diffuser des informations.

Cette version est basee sur SQLite et utilise Alire pour la construction
et gestion des dependances.

# Version 1.13 - Mar 2022

- Mise à jour pour le renouvellement des cotisations

# Compilation

Lancer la compilation avec:
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
