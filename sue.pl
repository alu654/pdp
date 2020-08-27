%vende(Empresa, EstiloDeViaje, Destino).
vende(mcs, crucero(10), rioDeJaneiro).
vende(mcs, crucero(20), mykonos).
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

%cambioAPesos(Moneda, Conversion).
cambioAPesos(real, 11).
cambioAPesos(dolar, 44).
cambioAPesos(pesoMexicano, 2).
cambioAPesos(ariaryMalgache, 0.012).


%viajaA/2: una empresa viaja a un continente si vende al menos un destino que quede en ese continente.
%vende(Empresa, EstiloDeViaje, Destino).
%continente(Destino, Continente).
viajaA(Empresa,Continente):-
    vende(Empresa,_,Destino),
    continente(Destino,Continente).
