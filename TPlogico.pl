% PUNTO 1)

esCandidato(frank).
esCandidato(claire).
esCandidato(garrett).
esCandidato(peter).
esCandidato(jackie).
esCandidato(linda).
esCandidato(catherine).
esCandidato(seth).
esCandidato(heather).

esDelPartido(frank, rojo).
esDelPartido(claire, rojo).
esDelPartido(garrett, azul).
esDelPartido(jackie, amarillo).
esDelPartido(linda, azul).
esDelPartido(catherine, rojo).
esDelPartido(seth, amarillo).
esDelPartido(heather, amarillo).

edad(frank, 50).
edad(claire, 52).
edad(garrett, 64).
edad(peter, 26).
edad(jackie, 38).
edad(linda, 30).
edad(catherine, 59).
edad(heather, 51).

sePostulaEn(azul, buenosAires).
sePostulaEn(azul, chaco).
sePostulaEn(azul, tierraDelFuego).
sePostulaEn(azul, sanLuis).
sePostulaEn(azul, neuquen).
sePostulaEn(rojo, buenosAires).
sePostulaEn(rojo, santaFe).
sePostulaEn(rojo, cordoba).
sePostulaEn(rojo, chubut).
sePostulaEn(rojo, tierraDelFuego).
sePostulaEn(rojo, sanLuis). 
sePostulaEn(amarillo, chaco).
sePostulaEn(amarillo, formosa).
sePostulaEn(amarillo, tucuman).
sePostulaEn(amarillo, salta).
sePostulaEn(amarillo, santaCruz).
sePostulaEn(amarillo, laPampa).
sePostulaEn(amarillo, corrientes).
sePostulaEn(amarillo, misiones).
sePostulaEn(amarillo, buenosAires).

cantidadDeHabitantes(buenosAires, 15355000).
cantidadDeHabitantes(chaco, 1143201).
cantidadDeHabitantes(tierraDelFuego, 160720).
cantidadDeHabitantes(sanLuis, 489255).
cantidadDeHabitantes(neuquen, 637913).
cantidadDeHabitantes(santaFe, 3397532).
cantidadDeHabitantes(cordoba, 3567654).
cantidadDeHabitantes(chubut, 577466).
cantidadDeHabitantes(formosa, 527895).
cantidadDeHabitantes(tucuman, 1687305).
cantidadDeHabitantes(salta, 1333365).
cantidadDeHabitantes(santaCruz, 273964).
cantidadDeHabitantes(laPampa, 349299).
cantidadDeHabitantes(corrientes, 992595).
cantidadDeHabitantes(misiones, 1189446).

% No se muestra el partido de Peter porque no lo dice. Dice de qué partido no da información concreta.
% No se muestra el partido violeta porque, para cualquier candidato, ninguno pertenece a él.
% No aparece Formosa en la lista del partido Rojo porque no se presenta, así como hay otras más de 10 provincias que aparecen.

% Punto 2)
esPicante(Provincia):-
    cantidadDeHabitantes(Provincia, Cantidad),
    Cantidad > 1000000,
    sePostulaEn(UnPartido, Provincia),
    sePostulaEn(OtroPartido, Provincia),
    UnPartido \= OtroPartido.

% Punto 3)
intencionDeVotoEn(buenosAires, rojo, 40).
intencionDeVotoEn(buenosAires, azul, 30).
intencionDeVotoEn(buenosAires, amarillo, 30).
intencionDeVotoEn(chaco, rojo, 50).
intencionDeVotoEn(chaco, azul, 20).
intencionDeVotoEn(chaco, amarillo, 0).
intencionDeVotoEn(tierraDelFuego, rojo, 40).
intencionDeVotoEn(tierraDelFuego, azul, 20).
intencionDeVotoEn(tierraDelFuego, amarillo, 10).
intencionDeVotoEn(sanLuis, rojo, 50).
intencionDeVotoEn(sanLuis, azul, 20).
intencionDeVotoEn(sanLuis, amarillo, 0).
intencionDeVotoEn(neuquen, rojo, 80).
intencionDeVotoEn(neuquen, azul, 10).
intencionDeVotoEn(neuquen, amarillo, 0).
intencionDeVotoEn(santaFe, rojo, 20).
intencionDeVotoEn(santaFe, azul, 40).
intencionDeVotoEn(santaFe, amarillo, 40).
intencionDeVotoEn(cordoba, rojo, 10).
intencionDeVotoEn(cordoba, azul, 60).
intencionDeVotoEn(cordoba, amarillo, 20).
intencionDeVotoEn(chubut, rojo, 15).
intencionDeVotoEn(chubut, azul, 15).
intencionDeVotoEn(chubut, amarillo, 15).
intencionDeVotoEn(formosa, rojo, 0).
intencionDeVotoEn(formosa, azul, 0).
intencionDeVotoEn(formosa, amarillo, 0).
intencionDeVotoEn(tucuman, rojo, 40).
intencionDeVotoEn(tucuman, azul, 40).
intencionDeVotoEn(tucuman, amarillo, 20).
intencionDeVotoEn(salta, rojo, 30).
intencionDeVotoEn(salta, azul, 60).
intencionDeVotoEn(salta, amarillo, 10).
intencionDeVotoEn(santaCruz, rojo, 10).
intencionDeVotoEn(santaCruz, azul, 20).
intencionDeVotoEn(santaCruz, amarillo, 30).
intencionDeVotoEn(laPampa, rojo, 25).
intencionDeVotoEn(laPampa, azul, 25).
intencionDeVotoEn(laPampa, amarillo, 40).
intencionDeVotoEn(corrientes, rojo, 30).
intencionDeVotoEn(corrientes, azul, 30).
intencionDeVotoEn(corrientes, amarillo, 10).
intencionDeVotoEn(misiones, rojo, 90).
intencionDeVotoEn(misiones, azul, 0).
intencionDeVotoEn(misiones, amarillo, 0).

leGanaA(Ganador, Perdedor, Provincia):-
    esDelPartido(Ganador, PartidoGan),
    esDelPartido(Perdedor, PartidoPer),
    sePostulaEn(PartidoGan, Provincia),
    not(sePostulaEn(PartidoPer, Provincia)).

leGanaA(Ganador, Perdedor, Provincia):-
    esDelPartido(Ganador, PartidoGan),
    esDelPartido(Perdedor, PartidoPer),
    sePostulaEn(PartidoGan, Provincia),
    sePostulaEn(PartidoPer, Provincia),
    PartidoGan \= PartidoPer,
    intencionDeVotoEn(Provincia, PartidoGan, PorcentajeGan),
    intencionDeVotoEn(Provincia, PartidoPer, PorcentajePer),
    PorcentajeGan > PorcentajePer.

leGanaA(Ganador, Perdedor, Provincia):-
    esDelPartido(Ganador, Partido),
    esDelPartido(Perdedor, Partido),
    sePostulaEn(Partido, Provincia).

% Punto 4)
elGranCandidato(UnCandidato):- 
    esDelPartido(UnCandidato, UnPartido),
    sePostulaEn(UnPartido, Provincia),
    leGanaA(UnCandidato, _, Provincia),
    esDelPartido(OtroCandidato, UnPartido),
    edad(UnCandidato, EdadUnCandidato),
    edad(OtroCandidato, EdadOtroCandidato),
    EdadUnCandidato \= EdadOtroCandidato.

% Porque al consultar ?- elGranCandidato(UnCandidato). 
% La única respuesta es UnCandidato = frank .
% Está relacionado con el concepto de unificación y de ligar a una variable a un único candidato que cumple el predicado.

% Punto 5) 

partidoPierdeEn(Partido, Provincia):-
    intencionDeVotoEn(Provincia, Partido, Porcentaje),
    intencionDeVotoEn(Provincia, OtroPartido, _),
    Partido \= OtroPartido,
    forall(intencionDeVotoEn(Provincia, OtroPartido, OtroPorcentaje), Porcentaje =< OtroPorcentaje).

ajusteConsultora(Partido, Provincia, RealPorcentaje):-
    intencionDeVotoEn(Provincia, Partido, Porcentaje),
    partidoPierdeEn(Partido, Provincia),
    RealPorcentaje is Porcentaje + 5.

ajusteConsultora(Partido, Provincia, RealPorcentaje):-
    intencionDeVotoEn(Provincia, Partido, Porcentaje),
    not(partidoPierdeEn(Partido, Provincia)),
    RealPorcentaje is Porcentaje - 20.

