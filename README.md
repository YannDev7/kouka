# Kwuka

Mon compilateur passe tous les tests de la partie 1 et 2. Toutefois, mon inférence d'effets n'est pas générale (cf ci-dessous).

# Remarques:
- Pour run les tests de la partie 1.
> make test1
- Pour run les test de la partie 2.
> make test2
- on peut utiliser le compilateur ainsi:
> ./kokac.exe main.koka
- Un sous ensemble intéressant des tests que j'ai écrit se trouvent dans les dossiers tests/typing et tests/syntax. Ils sont préfixés de "Y-".
- le fichier **pp.ml** contient des pretty printer plus ou moins complets (typiquement, pour le typage, je me suis contenté d'afficher les types plutôt que l'ast typé)

# Lexer, parser:
- la fonction **next_tokens** correspond au premier lexer, sans l'identation.
- la fonction **next_token** correspond au deuxième lexer, qui gère l'identation.
- le token **ELIF** est transformé en **ELSE IF**.
- la fonction **emit** permet de push un token dans la Queue du deuxième lexer (et en particulier, elle permet de bien mettre à jour last à **IF** quand on push **ELIF**...)
- je me suis rendu compte que construire des strings en ocaml était un peu compliqué et j'ai découvert le module **Buffer**.
- la ref globale **c** permet de maintenir la colonne d'indentation actuelle i.e la colonne juste **avant** le prochain token à lire. En particulier, il faut la mettre à jour lors de la lecture des commentaires (sinon la colonne du prochain token non commenté ne sera pas la bonne).
- il me semble que l'algorithme d'indentation ne supporte pas l'utilisation de l'indentation et des blocs avec **{** dans le même programme.


- il y a des warning menhir (notamment parce que le token **ELIF** n'est pas utilisé dans **parser.mly**)
- il y avait un conflit sur file à cause de **separated_list** (essentiellement, menhir ne savait pas différencier SEMICOLON* SEMICOLON* de SEMICOLON*). Il est réglé en utilisant la construction **list**
- pour **funbody**, si les effets ne sont pas précisé, on rajoute l'effet **ff** pour le signaler au typage (cela permet de distinguer l'effet vide **<>** de l'absence d'effet). J'aurais aussi pu faire un **option**.
- il y avait des conflits sur **typ** et **atyp** ( () -> result avait deux dérivations possibles, l'une passant par (atyp) et l'autre non). Même soucis avec (typ) -> result. On règle le problème en traitant la liste à un seul élément à part (cela revient à remonter les **ARROW** dans la règle typ pour que menhir sache s'il faut continuer à lire les arguments ou lire le result).
- dans mon AST, une fonction **() -> result** a une liste d'argument vide. Ce cas est réglé à la main.
- **LPAR** est plus prioritaire que **bexpr_p** pour que **expr** lise d'abord une **bexpr** avant un **block**.
- dans **bexpr**, on peut soit lire un **atom** soit lire **bexpr op bexpr** et ensuite lire l'atom depuis le **bexpr**. **atom_p** permet de lire **atom** avant de lire **bexpr**.
- le problème du dangling else est résolu en disant que **THEN** doit être au-dessus de **ELSE** dans l'AST (i.e **THEN** a une priorité plus faible que **ELSE**)

# Typage:

- On utilise des **tvar** pour gérer l'inférence de certains types (la liste vide et les types de retour des fonctions). Note: mon implémentation est essentiellement un union find eco+ (on pourrait optimiser le code avec de la compression de chemins).