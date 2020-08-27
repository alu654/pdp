
%vende(Empresa, EstiloDeViaje, Destino).
vende(mcs, crucero(10), rioDeJaneiro).
vende(mcs, crucero(20), mykonos).
vende(mcs, crucero(366), madagascar).
vende(vacaviones, allInclusive(burjAlArab), dubai).
vende(vacaviones, allInclusive(wyndhamPlayaDelCarmen), playaDelCarmen).
vende(moxileres, mochila([carpa, bolsaDeDormir, linterna]), elBolson).
vende(moxileres, mochila([camara, cantimplora, protectorSolar, malla]), puntaDelDiablo).
vende(tuViaje, clasico(primavera, avion), madrid).
vende(tuViaje, clasico(verano, micro), villaGesell).

%crucero(CantidadDeDias).
%allInclusive(Hotel).
%mochila(Objetos).
%clasico(Temporada, MedioDeTransporte).
%continente(Destino, Continente).
continente(rioDeJaneiro, sudAmerica).
continente(mykonos, europa).
continente(dubai, asia).
continente(playaDelCarmen, centroAmerica).
continente(puntaDelDiablo, sudAmerica).
continente(sydney, oceania).
continente(madagascar, africa).
continente(madrid, europa).

%moneda(Destino, Moneda).
moneda(rioDeJaneiro, real).
moneda(miami, dolar).
moneda(shenzhen, renminbi).
moneda(madagascar, ariaryMalgache).

%cambioAPesos(Moneda, Converasion).
cambioAPesos(real, 11).
cambioAPesos(dolar, 44).
cambioAPesos(pesoMexicano, 2).
cambioAPesos(ariaryMalgache, 0.012).

%viajaA/2: una empresa viaja a un continente si vende al menos un destino que quede en ese continente.
%vende(Empresa, EstiloDeViaje, Destino).
%continente(Destino, Continente).
viajaA(Empresa,Continente):-
    vende(Empresa, _, Destino),
    continente(Destino, Continente).

%esMasEconomico/2: que relaciona dos destinos y nos dice si el primero tiene una conversión a pesos argentinos menor que el segundo.
%moneda(Destino, Moneda).
%cambioAPesos(Moneda, Converasion).
esMasEconomico(UnDestino, OtroDestino):-
    cambioAPesosDelDestino(UnDestino, UnCambio),
    cambioAPesosDelDestino(OtroDestino, OtroCambio),
    UnCambio < OtroCambio.

cambioAPesosDelDestino(UnDestino, Cambio):-
    moneda(UnDestino, Moneda),
    cambioAPesos(Moneda, Cambio).


%%conviene/1: un destino conviene cuando su moneda vale menos que un peso argentino y no queda en Europa

conviene(UnDestino):-
    valeMenosDeUnPeso(UnDestino),
    not(quedaEnEuropa(UnDestino)).

valeMenosDeUnPeso(UnDestino):-
    cambioAPesosDelDestino(UnDestino, Cambio),
    Cambio < 1.

quedaEnEuropa(UnDestino):-
    continente(UnDestino, europa).

/*%elDestinoMasPopular/2: el destino más popular de una empresa es aquel que es más económico que todos los 
%demás destinos que vende.*/
%moneda(Destino, Moneda).
%cambioAPesos(Moneda, Converasion).
%vende(Empresa, EstiloDeViaje, Destino).
elDestinoMasPopular(Empresa,Destino):-
    vende(Empresa, _, Destino),
    forall((vende(Empresa, _, Destino1),Destino \=Destino1 ), esMasEconomico(Destino,Destino1)).
    
    






    


