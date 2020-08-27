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


/*esMasEconomico/2: que relaciona dos destinos y nos dice si el primero tiene una conversión a pesos 
argentinos menor que el segundo.*/
esMasEconomico(Destino1,Destino2):-
    cambioAPesosDelDestino(Destino1,Moneda1),
    cambioAPesosDelDestino(Destino2,Moneda2),
    Moneda1 < Moneda2.

cambioAPesosDelDestino(UnDestino, Cambio):-
    moneda(UnDestino, Moneda),
    cambioAPesos(Moneda, Cambio).

/*conviene/1: un destino conviene cuando su moneda vale menos que un peso argentino y no queda en Europa.*/
conviene(Destino):-
    moneda(Destino,Moneda),
    Moneda < 1,
    not(viajaAeuropa(Destino)).

viajaAeuropa(Destino):-
    continente(Destino,europa).

/*elDestinoMasPopular/2: el destino más popular de una empresa es aquel que es más económico que todos los demás
destinos que vende.*/
%vende(Empresa, EstiloDeViaje, Destino).
%moneda(Destino, Moneda).

%cambioAPesos(Moneda, Conversion).
elDestinoMasPopular(Empresa,Destino):-
    vende(Empresa,_,Destino1),
    forall((vende(Empresa,_,Destino1),Destino \= Destino1), esMasEconomico(Destino,Destino1)).
    
    
    
    
/*ventaExtravagante/1: una venta a un destino es extravagante cuando el destino no conviene y además:
es un viaje en un crucero con duración de más de 1 año;
es un viaje con mochila pero está vacía;
es un viaje en un all inclusive en el Burj Al Arab o el Pangu.
Un viaje clásico nunca es extravagante.*/

ventaExtravagante(Destino):-
    vende(_,InfoDestino,Destino),
    not(conviene(Destino)),
    elementos(InfoDestino),
    not(elementos(InfoDestino)).


elementos(clasico([])).
elementos(crucero(Dias)):-
    Dias > 365.

elementos(mochila([])).

elementos(allInclusive(burjAlArab)).
elementos(allInclusive(pangu)).
/*nivelExtravagancia/2: el nivel se calcula como la cantidad de ventas extravagante de la empresa multiplicada por 8.
*/
nivelExtravagancia(Empresa,Nivel):-
    vende(Empresa,_,Destino),
    findall(Destino, ventaExtravaganteDeLaEmpresa(Empresa,Destino), CantidadDeNivel),
    length(CantidadDeNivel, Total),
    Nivel is Total * 8.
    
ventaExtravaganteDeLaEmpresa(Empresa,Destino):-
    vende(Empresa,_,Destino),
    ventaExtravagante(Destino).

   
    
    




