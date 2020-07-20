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
tieneMasDeUnMillonHabitantes(Provincia):-
    cantidadDeHabitantes(Provincia, Cantidad),
    Cantidad > 1000000.

sePresentanPorLoMenosDosPartidos(Provincia):-
    sePostulaEn(UnPartido, Provincia),
    sePostulaEn(OtroPartido, Provincia),
    UnPartido \= OtroPartido.

esPicante(Provincia):-  
    tieneMasDeUnMillonHabitantes(Provincia),
    sePresentanPorLoMenosDosPartidos(Provincia).


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
    esGanador(Provincia, PartidoGan, PartidoPer).

esGanador(Provincia, _, PartidoPer):-
    not(sePostulaEn(PartidoPer, Provincia)).

esGanador(Provincia, PartidoGan, PartidoPer):-
    sePostulaEn(PartidoPer, Provincia),
    PartidoGan \= PartidoPer,
    intencionDeVotoEn(Provincia, PartidoGan, PorcentajeGan),
    intencionDeVotoEn(Provincia, PartidoPer, PorcentajePer),
    PorcentajeGan > PorcentajePer.

esGanador(_, Partido, Partido).

% Punto 4)

sePostulanEn(Partido, OtroPartido, Provincia):-
    sePostulaEn(Partido, Provincia),
    sePostulaEn(OtroPartido, Provincia),
    Partido \= OtroPartido.

ganaEnTodasLasProvinciasElPartido(Partido):-
    forall(sePostulanEn(Partido, OtroPartido, Provincia), esGanador(Provincia, Partido, OtroPartido)).

edadDeOtroCandidatoDelPartido(Partido, UnCandidato, Edad):-
    esDelPartido(OtroCandidato, Partido),
    edad(OtroCandidato, Edad),
    UnCandidato \= OtroCandidato.

esElMenorDelPartido(UnCandidato, Partido):-
    esDelPartido(UnCandidato, Partido),
    edad(UnCandidato, EdadUnCandidato),
    forall(edadDeOtroCandidatoDelPartido(Partido, UnCandidato, EdadOtroCandidato), EdadUnCandidato < EdadOtroCandidato ).

elGranCandidato(UnCandidato):-
    esElMenorDelPartido(UnCandidato, Partido),
    ganaEnTodasLasProvinciasElPartido(Partido).

% LO RESOLVIMOS CON AYUDA DE GUS. Al final lo resolvimos adoptando la idea de que el partido es el que 
% gana en una provincia a otro partido. Después, si tienen ganas, nos explican cómo se resuelve con
% leGanaA
        
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
    actualizacionDePorcentaje(Partido, Provincia, RealPorcentaje, Porcentaje).

actualizacionDePorcentaje(Partido, Provincia, RealPorcentaje, Porcentaje):-
    partidoPierdeEn(Partido, Provincia),
    RealPorcentaje is Porcentaje + 5.

actualizacionDePorcentaje(Partido, Provincia, RealPorcentaje, Porcentaje):-
    not(partidoPierdeEn(Partido, Provincia)),
    RealPorcentaje is Porcentaje - 20.

% Punto 6) 

%inflacion(CotaInferior, CotaSuperior).
%construir(ListaDeObras).
%nuevosPuestosDeTrabajo(Cantidad).

%edilicio(Edilicio, Cantidad).

promete(azul, inflacion(2, 4)).
promete(amarillo, inflacion(1, 15)).
promete(rojo, inflacion(10, 30)).

promete(rojo, nuevosPuestosDeTrabajo(800000)).
promete(amarillo, nuevosPuestosDeTrabajo(10000)).

promete(azul, construir([edilicio(hospitales, 1000), edilicio(jardines, 100), edilicio(escuelas, 5)])).
promete(amarillo, construir([edilicio(hospitales, 100), edilicio(universidades, 1), edilicio(comisarias, 200)])).

% Cambiamos la base de datos con gus. Dejamos comentado cómo lo teniamos antes, que lógicamente está bien 
% Pero nos daba resultados no deseados en las consultas del punto 8

%promete(Partido, construir(ListaDeObras)):-
%    ediliciosPrometidos(Partido, _),
%    findall(edilicio(Edilicio, Cantidad), ediliciosPrometidos(Partido, edilicio(Edilicio, Cantidad)), ListaDeObras).

%ediliciosPrometidos(azul, edilicio(hospitales, 1000)).
%ediliciosPrometidos(azul, edilicio(jardines, 100)).
%ediliciosPrometidos(azul, edilicio(escuelas, 5)).
%ediliciosPrometidos(amarillo, edilicio(hospitales, 100)).
%ediliciosPrometidos(amarillo, edilicio(universidades, 1)).
%ediliciosPrometidos(amarillo, edilicio(comisarias, 200)).

% Punto 7)

% influenciaDePromesas(Promesa, Intencion).

/*Para la inflación, la intención de votos 
disminuirá de manera directamente proporcional al promedio de las cotas de la promesa realizada.*/

esEdilicioEducativo(jardines).
esEdilicioEducativo(escuelas).
esEdilicioNecesario(hospitales).
esEdilicioNecesario(comisarias).
esEdilicioNecesario(universidades).

influenciaDePromesas(inflacion(CotaInferior, CotaSuperior), Intencion):-
    Intencion is (CotaInferior + CotaSuperior) / (-2).

influenciaDePromesas(nuevosPuestosDeTrabajo(Cantidad), Intencion):-
    Cantidad > 50000,
    Intencion is 3.

influenciaDePromesas(nuevosPuestosDeTrabajo(Cantidad), 0):-
    Cantidad =< 50000.

influenciaDePromesas(construir(ListaDeObras), Sumatoria):-
    findall(Intencion, intencionPorObras(construir(ListaDeObras), Intencion), Porcentajes),
    sum_list(Porcentajes, Sumatoria).
    
intencionPorObras(construir([]), 0).

intencionPorObras(construir(ListaDeObras), 2):-
    member(edilicio(hospitales, _), ListaDeObras).

intencionPorObras(construir(ListaDeObras), Intencion):-
    member(edilicio(Edilicio, Cantidad), ListaDeObras),
    esEdilicioEducativo(Edilicio),
    Intencion is Cantidad / 10.

intencionPorObras(construir(ListaDeObras), 2):-
    member(edilicio(comisarias, 200), ListaDeObras).

intencionPorObras(construir(ListaDeObras), 0):-
    member(edilicio(universidades, _), ListaDeObras).

intencionPorObras(construir(ListaDeObras), -1):-
    member( edilicio(Edilicio, _), ListaDeObras),
    not(esEdilicioEducativo(Edilicio)),
    not(esEdilicioNecesario(Edilicio)).

% Punto 8)

porcentajePorPromesa(Partido, Intencion):-
    promete(Partido, Promesa),
    influenciaDePromesas(Promesa, Intencion).

promedioDeCrecimiento(Partido, Promedio):-
    promete(Partido, _),
    findall(Intencion, porcentajePorPromesa(Partido, Intencion), TodasLasIntenciones),
    sum_list(TodasLasIntenciones, Promedio).


