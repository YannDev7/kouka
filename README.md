# Kwuka - Yann Viegas

Mon compilateur passe tous les tests de la partie 1 et 2. Toutefois, mon inférence d'effets n'est pas générale (cf ci-dessous).

# Remarques:
- Pour run les tests de la partie 1.
> make test1
- Pour run les test de la partie 2.
> make test2
- on peut utiliser le compilateur ainsi:
> ./kokac.exe main.koka
- Un sous ensemble intéressant des tests que j'ai écrit se trouve dans les dossiers tests/typing et tests/syntax. Ils sont préfixés de "Y-".
- le fichier **pp.ml** contient des pretty printer plus ou moins complets (typiquement, pour le typage, je me suis contenté d'afficher les types plutôt que l'ast typé)

# Lexer, parser:
- la fonction **next_tokens** correspond au premier lexer, sans l'identation.
- la fonction **next_token** correspond au deuxième lexer, qui gère l'identation.
- le token **ELIF** est transformé en **ELSE IF**.
- la fonction **emit** permet de push un token dans la Queue du deuxième lexer (et en particulier, elle permet de bien mettre à jour last à **IF** quand on push **ELIF**...)
- je me suis rendu compte que construire des strings en ocaml était un peu compliqué et j'ai découvert le module **Buffer**.
- la ref globale **c** permet de maintenir la colonne d'indentation actuelle i.e la colonne juste **avant** le prochain token à lire. En particulier, il faut la mettre à jour lors de la lecture des commentaires (sinon la colonne du prochain token non commenté ne sera pas la bonne).
- il me semble que l'algorithme d'indentation ne supporte pas l'utilisation de l'indentation et des blocs avec **{** dans le même programme.
- les **ident** de koka sont un peu étranges. On peut notamment écrire
```
val a-a = 42;
val a = 0; 
val c = a-a + a; // c != a
```
- on accepte avec une regex un ensemble d'**ident** plus gros que voulu et l'on vérifie ensuite la validité manuellement.


- il y a des warning menhir (notamment parce que le token **ELIF** n'est pas utilisé dans **parser.mly**)
- il y avait un conflit sur file à cause de **separated_list** (essentiellement, menhir ne savait pas différencier SEMICOLON* SEMICOLON* de SEMICOLON*). Il est réglé en utilisant la construction **list**
- pour **funbody**, si les effets ne sont pas précisé, on rajoute l'effet **ff** pour le signaler au typage (cela permet de distinguer l'effet vide **<>** de l'absence d'effet). J'aurais aussi pu faire un **option**.
- il y avait des conflits sur **typ** et **atyp** ( () -> result avait deux dérivations possibles, l'une passant par (atyp) et l'autre non). Même soucis avec (typ) -> result. On règle le problème en traitant la liste à un seul élément à part (cela revient à remonter les **ARROW** dans la règle typ pour que menhir sache s'il faut continuer à lire les arguments ou lire le result).
- dans mon AST, une fonction **() -> result** a une liste d'argument vide. Ce cas est réglé à la main.
- **LPAR** est plus prioritaire que **bexpr_p** pour que **expr** lise d'abord une **bexpr** avant un **block**.
- dans **bexpr**, on peut soit lire un **atom** soit lire **bexpr op bexpr** et ensuite lire l'atom depuis le **bexpr**. **atom_p** permet de lire **atom** avant de lire **bexpr**.
- le problème du dangling else est résolu en disant que **THEN** doit être au-dessus de **ELSE** dans l'AST (i.e **THEN** a une priorité plus faible que **ELSE**)

# Typage:

- On utilise des **tvar** pour gérer l'inférence de certains types (la liste vide et les types de retour des fonctions). Note: mon implémentation est essentiellement un union find sans optimisation(on pourrait optimiser le code avec de la compression de chemins).
- Pour typer les fonctions anonymes, je crée une déclaration avec un identifiant unique **-anoi** et j'utilise la fonction qui type les déclarations.
- Je maintiens en permanence une pile des noms et des types de retour des déclarations imbriquées (puisque les **fn** imbriquées comptent comme des déclarations dans mon typage). Cela permet de gérer l'unification de **e** dans **return e** avec le type de retour de la fonction. L'effet **div** est ajouté dans la map **divg**, notamment grâce à **cur_id** qui contient le nom de la **vraie** déclaration en cours.
- Un problème que l'on peut rencontrer (voir "tests/typing/good/Y-println-rec-infer.koka"), est qu'au moment de typer print, le type de l'argument n'a pas encore été inféré et l'on ne sait pas s'il faut vérifier que c'est une string ou un int ou ... Une solution générale à laquelle j'ai pensé est de réaliser un calcul de point fixe. Autrement dit, on ne raise pas d'erreur puisque le type n'est pas encore inféré. On récupère le **typed_ast** avec type_file, et on le re-donne à type_file. On fait cela tant que de nouveaux types sont inférés. Si le type était inférable, il y aura un moment où l'on pourra typer **println(e)** puisque le type de **e** sera connu (et l'on pourra raise une erreur si besoin). Je n'ai pas implémenté cette solution pour éviter d'avoir trop de code à réécrire (mes fonctions prennent un **ast** en paramètre et pas un **typed_ast**). Comme les fonctions mutuellement récursives sont interdites, il me semble que deux passes de typage devraient suffire à typer la majorité des cas. Je remplace la deuxième passe par une liste **constraints**. Au moment de typer **println(e)**, je push la contrainte que le type de **e** doit être string,int ou bool. Une fois tout l'AST exploré, je vérifie que toutes les contraintes sont bien satisfaites (en fait, je vérifie pour chaque type qu'il match son ensemble de contraintes). C'est le travail de la fonction **resolve_const**. Remarque: je fais deux passes de **resolve_const** parce qu'on pourrait imaginer que certains types soient inférés via un unify avec un **TMAybe** lors de la résolution des contraintes (et si un autre type dépendait de ce **TMaybe**, il faut attendre avant de raise une erreur).
- Pour les effets, j'utilise aussi des variables d'effets **tef**. Je traite les effets essentiellement comme les types avec une fonction **unify_eff** définie dans **typing_utils.ml**. L'union d'effet est mal définie dans le cas de deux **tef** et j'en renvoie un arbitrairement (en pratique, j'ai essayé de faire en sorte que le plus cohérent soit renvoyé). La fonction union est donc fausse. Typiquement, si l'on fait un **while** dans une fonction récursive **f** qui utilisera un **println** plus tard, et que le **while** appelle **f**:  il faudrait donner l'effet **div** et **console** au **while** mais actuellement mon code donnerait seulement l'effet **div**.
- En pratique, ce n'est pas très grave puisque l'on s'intéresse seulement aux effets de **f**, qui eux seront normalement corrects. J'ai toutefois quelques idées pour régler le problème. La première serait d'utiliser un type **EUnion** pour rendre paresseux le calcul des unions de types. Le problème est déplacé à l'unification (s'il y a plusieurs **tef** de chaque côté de l'union). On peut alors s'en sortir par un calcul de point fixe analogue à ce qui a été décrit précédement. 
- Une autre solution possible serait d'autoriser des **tef** dans mes **Effset**. Au moment d'unifier **e1** et **e2**, pour chaque effet connu de **e1** qui n'est pas déjà dans le **ESet** de **e2**, on fixe un des **teff** de **e2** et on regarde ce qu'il se passe si on lui donne l'effet en question.
- il me semble aussi que je gère mal l'effet console quand il est dans un while
```
fun main(): <div> unit {
    while { True } { println(42); } 
 }
```

Essentiellement, c'est parce que le bloc
```
{
    fn () {
        println(42);
    }
}
```
n'aura pas l'effet console, seulement le type de retour de la fonction anonyme l'aura. Je peux régler le problème facilement (pour typer un appel de fonction, il suffit de regarder si parmi les arguments passés il y en a un dont le type est une fonction dont le type a un effet).

Je ne sais pas si ce comportement est attendu (d'après le **test while-1** je suppose que oui ?) ou non donc j'ignore temporairement ce problème.

- Un autre problème (mais je pense que c'est normal)
```fun pp_ls(ls: list<int>) {
  if True return 42
  return 42
}
```
 
est bien typé

alors que
```
fun pp_ls(ls: list<int>) {
  if True return 42
}```

ne l'est pas. Mais dans les faits il n'y a aucun moyen de savoir que la condition du if est tout le temps vraie.
