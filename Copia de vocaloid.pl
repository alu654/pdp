%vocaloid(Nombre, cancion que sabe cantar)
vocaloid(megurineLuka, nightFever).
vocaloid(megurineLuka, foreverYoung).
vocaloid(hatsuneMiku, tellYourWorld).
vocaloid(gumi, foreverYoung2).
vocaloid(gumi, tellYourWorld2).
vocaloid(seeU, novemberRain).
vocaloid(seeU, nightFever2).

%cancion(nombre, duracion en minutos)
cancion(nightFever, 4).
cancion(foreverYoung, 5).
cancion(tellYourWorld, 4).
cancion(foreverYoung2, 4).
cancion(tellYourWorld2, 5).
cancion(novemberRain, 6).
cancion(nightFever2, 5).

%concierto(nombre, pais, fama, tipoConcierto)%
concierto(mikuExpo, eeuu, 2000, gigante(2,6)).
concierto(magicalMirai, japon, 3000, gigante(3,10)).
concierto(vocalektVisions, eeuu, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, diminuto(4)).

%conoce(un cantante, otro cantante)
conoce(megurineLuka, hatsuneMiku).
conoce(megurineLuka, gumi).
conoce(gumi, seeU).
conoce(seeU, kaito).

% Punto a
esNovedoso(Vocaloid):-
    sabeAlMenosDosCanciones(Vocaloid), 
    duracionCanciones(Vocaloid, Duracion),
    Duracion < 15.

duracionCanciones(Vocaloid, Duracion):-
    vocaloid(Vocaloid, _),
    findall(Minutos, (vocaloid(Vocaloid, Cancion), cancion(Cancion, Minutos)), ListaMinutos),
    sumlist(ListaMinutos, Duracion).

sabeAlMenosDosCanciones(Vocaloid):-
    vocaloid(Vocaloid, Cancion1), 
    vocaloid(Vocaloid, Cancion2), 
    Cancion1 \= Cancion2.

% Punto b
esAcelerado(Vocaloid):-
    vocaloid(Vocaloid, _),
    forall(vocaloid(Vocaloid, Cancion), (cancion(Cancion, Minutos), Minutos < 4)).

% Punto 2
puedeParticipar(Vocaloid, Concierto):-
    concierto(Concierto, _, _, Condiciones),
    cumpleLasCondiciones(Vocaloid, Condiciones).

cumpleLasCondiciones(Vocaloid, gigante(CantMinimaCanciones, TiempoMinimo)):-
    cantidadCanciones(Vocaloid, Cantidad), Cantidad >= CantMinimaCanciones,
    duracionCanciones(Vocaloid, Duracion), Duracion >= TiempoMinimo.

cantidadCanciones(Vocaloid, Cant):-
    findall(Cancion, vocaloid(Vocaloid, Cancion), Lista),
    length(Lista, Cant).

cumpleLasCondiciones(Vocaloid, mediano(TiempoMax)):-
    duracionCanciones(Vocaloid, Dur), Dur < TiempoMax.

cumpleLasCondiciones(Vocaloid, diminuto(TiempoMinimo)):-
    vocaloid(Vocaloid, Cancion), cancion(Cancion, Minutos), Minutos > TiempoMinimo.

puedeParticipar(hatsuneMiku, _).

%Punto 3

masFamoso(Vocaloid):-
    nivelFama(Vocaloid, Fama1),
    forall(nivelFama(_, Fama2), Fama1 >= Fama2).

nivelFama(Vocaloid, Nivel):-
    cantNivelFama(Vocaloid, CantFama),
    cantidadCanciones(Vocaloid, CantCanciones), 
    Nivel is CantFama * CantCanciones.

cantNivelFama(Vocaloid, CantFama):-
    findall(Fama, famaConcierto(Vocaloid, Fama), CantidadesFama),
    sumlist(CantidadesFama, CantFama).

famaConcierto(Vocaloid, Fama):-
    puedeParticipar(Vocaloid, Concierto),
    concierto(Concierto, _, Fama, _).

% Punto 4
unicoParticipanteEntreConocidos(Cantante,Concierto):- 
    puedeParticipar(Cantante, Concierto),
	not((conocido(Cantante, OtroCantante), 
    puedeParticipar(OtroCantante, Concierto))).

    %Conocido directo
    conocido(Cantante, OtroCantante) :- 
    conoce(Cantante, OtroCantante).

    %Conocido indirecto
    conocido(Cantante, OtroCantante) :- 
    conoce(Cantante, UnCantante), 
    conocido(UnCantante, OtroCantante).
