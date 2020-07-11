
/*Tía Agatha, el carnicero y Charles son las únicas personas que viven en la mansión Dreadbury.*/

viveEnLaMansion(tiaAgatha).
viveEnLaMansion(elCarnicero).
viveEnLaMansion(charles).
viveEnLaMansion(milhouse).

 %Charles odia a todas las personas de la mansión que no son odiadas por la tía Agatha.
odia(charles, OtraPersona):-
    not(odia(tiaAgatha,OtraPersona)).


% Agatha odia a todos los que viven en la mansión, excepto al carnicero.
odia(tiaAgatha, OtraPersona):-
    viveEnLaMansion(OtraPersona),
    OtraPersona \= elCarnicero.

%El carnicero odia a las mismas personas que odia tía Agatha.
odia(elCarnicero,UnaPersona):-
    odia(tiaAgatha, UnaPersona).


%Quien no es odiado por el carnicero y vive en la mansión, es más rico que tía Agatha
esMAsRico(UnaPersona, tiaAgatha):-
    not(odia(elCarnicero,UnaPersona)),
    viveEnLaMansion(UnaPersona).

%Quien mata es porque odia a su víctima y no es más rico que ella. Además, quien mata debe vivir en la mansión Dreadbury.
mata(Persona,OtraPersona):-
    odia(Persona,OtraPersona),
    not(esMAsRico(Persona,OtraPersona)),
    viveEnLaMansion(Persona).


/*mata(Alguien, tiaAgatha).
Alguien = tiaAgatha */

/*?- odia(Alguien, milhouse).
Alguien = elCarnicero ;
Alguien = elCarnicero.*/

/*odia(Alguien, charles).
Alguien = charles ;
Alguien = tiaAgatha ;
Alguien = elCarnicero ;
Alguien = elCarnicero.*/


/*dia(elCarnicero, _).
true .*/

/*Queremos saber si un libro está bueno, esto se cumple cuando:
- Es una novela policial y tiene menos de 12 capítulos.
- Es una novela de terror.
- Los libros con más de 10 cuentos siempre son buenos.
- Es un libro científico de fisicaCuantica.
- Es un best seller y el precio por página es menor a $50.
estaBueno/1
*/

estaBueno(Obra):-
    esNovelaPolicial(Artista,cantCapitulos),
    cantPaginas < 12


esNovelaPolicial