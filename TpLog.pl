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

esPicante(Provincia):-
    sePostulaEn(Candidato, Provincia),
    sePostulaEn(OtroCandidato, Provincia),
    Candidato \= OtroCandidato
    cantidadDeHabitantes(Provincia, CantHab),
    CantHab > 1000000.


