%persona(Apodo, Edad, Peculiaridades).
persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).

%esSalaDe(NombreSala, Empresa).
esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).

esSalaDe(miseriaDeLaNoche, sKPista).
esSalaDe(estrellasDePelea, supercelula).
esSalaDe(probar, fff).


%terrorifica(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).

%esSalaDe(NombreSala, Empresa).
%sala(Nombre, Experiencia).
sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(probar, familiar(comics, 1)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).

sala(miseriaDeLaNoche, terrorifica(150, 21)).
sala(estrellasDePelea, familiar(videojuegos, 7)).

/*nivelDeDificultadDeLaSala/2: para cada sala nos dice su dificultad. Para las salas de experiencia
terrorífica el nivel de dificultad es la cantidad de sustos menos la edad mínima para ingresar.
Para las de experiencia familiar es 15 si es de una temática futurista y para cualquier otra temática es
la cantidad de habitaciones. El de las enigmáticas es la cantidad de candados que tenga.*/

nivelDeDificultadDeLaSala(Sala,Dificultad):-
    sala(Sala,Experiencia),
    infoDePelicula(Experiencia,Dificultad).

infoDePelicula(terrorifica(Susto, Edad),Dificultad):-
    Dificultad is Susto - Edad.

infoDePelicula(familiar(futurista, _),15).
infoDePelicula(familiar(Tematica, Habitaciones),Dificultad):-
    Tematica \=futurista,
    Habitaciones is Dificultad.

infoDePelicula(enigmatica(ListaCandados),Dificultad):-
    length(ListaCandados, Dificultad).


puedeSalir(Persona,Sala):-
    nivelDeDificultadDeLaSala(Sala,1),
    persona(Persona, _ , Peculiaridades),
    not(esClaustrofobia(Peculiaridades)).

puedeSalir(Persona,Sala):-
    persona(Persona, Edad, Peculiaridades),
    nivelDeDificultadDeLaSala(Sala,Dificultad),
    Dificultad < 5,
    Edad > 13,
    not(esClaustrofobia(Peculiaridades)).

esClaustrofobia(Peculiaridades):-
    member(clautrofobia, Peculiaridades).

/*tieneSuerte/2: una persona tiene suerte en una sala si puede salir de ella aún sin tener ninguna peculiaridad.*/
%persona(Apodo, Edad, Peculiaridades).
tieneSuerte(Persona,Sala):-
    puedeSalir(Persona,Sala),
    persona(Persona, _, []).

%esSalaDe(NombreSala, Empresa).
%sala(Nombre, Experiencia).
/*esMacabra/1: una empresa es macabra si todas sus salas son de experiencia terrorífica.*/
esMacabra(Empresa):-
    esSalaDe(_, Empresa),
    forall(esSalaDe(NombreSala, Empresa),sala(NombreSala, terrorifica(_,_))).
    
/*empresaCopada/1: una empresa es copada si no es macabra y el promedio de dificultad de sus salas es menor a 4.*/
%esSalaDe(NombreSala, Empresa).
%sala(Nombre, Experiencia).probar
empresaCopada(Empresa):-
    esSalaDe(_, Empresa),
    not(esMacabra(Empresa)),
    findall(Dificultad,(esSalaDe(NombreSala, Empresa),nivelDeDificultadDeLaSala(NombreSala,Dificultad)), ListaDeDificultad),
    sumlist(ListaDeDificultad, CantidadDeDificultad),
    length(ListaDeDificultad,CantidadDeSalas),
    (CantidadDeDificultad / CantidadDeSalas) < 4.


nivelDeDificultadDeLaSalaDeLaEmpresa(Empresa,Dificultad):-
esSalaDe(NombreSala, Empresa),
nivelDeDificultadDeLaSala(NombreSala,Dificultad).

/*¡Cada vez hay más salas y empresas! Conozcámoslas para agregarlas a nuestro sistema:

La empresa supercelula es dueña de salas de escape familiares ambientadas en videojuegos. La sala estrellasDePelea 
cuenta con 7 habitaciones pero lamentablemente no sabemos la cantidad que tiene su nueva sala choqueDeLaRealeza.

La empresa SKPista (fanática de un famoso escritor) es la dueña de una única sala terrorífica para mayores de 21 
llamada miseriaDeLaNoche que nos asegura 150 sustos.
La nueva empresa que se suma a esta gran familia es Vertigo, pero aún no cuenta con salas.*/

%esSalaDe(NombreSala, Empresa).
%sala(Nombre, Experiencia).

%Lo agregue en la base de conocimiento