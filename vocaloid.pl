%canta(nombreCancion, cancion)%
canta(megurineLuka, cancion(nightFever, 4)).
canta(megurineLuka, cancion(foreverYoung, 5)).
canta(hatsuneMiku, cancion(tellYourWorld, 4)).
canta(gumi, cancion(foreverYoung, 4)).
canta(gumi, cancion(tellYourWorld, 5)).
canta(seeU, cancion(novemberRain, 6)).
canta(seeU, cancion(nightFever, 5)).


% cuando saben al menos 2 canciones y el tiempo total que duran todas las canciones debería ser menor a 15.
sabe(Cantante):-
    sabeAlMenosDosCanciones(Cantante),
    tiempoTotalCanciones(Cantante, Tiempo),
    Tiempo < 15.


sabeAlMenosDosCanciones(Cantante):-
    canta(Cantante, UnaCancion),
	canta(Cantante, OtraCancion),
	UnaCancion \= OtraCancion.

tiempoTotalCanciones(Cantante, TiempoTotal):-
    findall(Tiempo, tiempoDeCancion(Cantante, TiempoCancion), Tiempos),
    sumlist(Tiempos,TiempoTotal).

tiempoDeCancion(Cantante,TiempoCancion):-  
      canta(Cantante,Cancion),
      tiempo(Cancion,TiempoCancion).

tiempo(cancion(_, Tiempo), Tiempo).


/*Hay algunos vocaloids que simplemente no quieren cantar canciones largas porque no les gusta, es por eso que se pide saber
 si un cantante es acelerado, 
condición que se da cuando todas sus canciones duran 4 minutos o menos. Resolver sin usar forall/2.*/

acelerado(Cantante):-
    vocaloids(Cantante),
    not((tiempoDeCancion(Cantante,Tiempo),Tiempo > 4)).

vocaloid(Cantante):-
     canta(Cantante, _).
        

%concierto(nombre, pais, fama, tipoConcierto)%
concierto(mikuExpo, eeuu, 2000, gigante(2,6)).
concierto(magicalMirai, japon, 3000, gigante(3,10)).
concierto(vocalektVisions, eeuu, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, diminuto(4)).

%%%%%%%%%%%%%%%%%%%%%%%2
puedeParticipar(hatsuneMiku,Concierto):-
	concierto(Concierto, _, _, _).

puedeParticipar(Cantante, Concierto):-
	vocaloid(Cantante),
	Cantante \= hatsuneMiku,
	concierto(Concierto, _, _, Requisitos),
	cumpleRequisitos(Cantante, Requisitos).

/*gigante del cual se sabe la cantidad mínima de canciones que el cantante tiene que saber y
 además la duración total de todas las canciones tiene que ser mayor a una cantidad dada.
*/


cumpleRequisitos(Cantante, gigante(CantCanciones, TiempoMinimo)):-
    cantidadCanciones(Cantante,Cantidad),
    Cantidad >= CantCanciones.

%mediano sólo pide que la duración total de las canciones del cantante sea menor a una cantidad determinada.
cumpleRequisitos(Cantante, mediano(TiempoTotal):-
    tiempoTotalCanciones(Cantante, Tiempo),
    TiempoTotal > Tiempo.

%pequeño el único requisito es que alguna de las canciones dure más de una cantidad dada.
cumpleRequisitos(Cantante, diminuto(TiempoTotal):-
    canta(Cantante, Cancion),
	tiempo(Cancion, Tiempo),
	Tiempo > TiempoMinimo.


cantidadCanciones(Cantante,Cantidad):-
    findall(Cancion, canta(Cantante, Cancion), Canciones),
    length(Canciones, Cantidad).


/*Conocer el vocaloid más famoso, es decir con mayor nivel de fama. El nivel de fama de un vocaloid se calcula 
como la fama total que
le dan los conciertos en los cuales puede participar multiplicado por la cantidad de canciones que sabe cantar.
*/
nivelFamoso(Cantante, Nivel):- 
    nivelDeFama(Cantante,FamaTotal),
    cantidadCanciones(Cantante, Cantidad), 
    Nivel is FamaTotal * Cantidad.

nivelDeFama(Cantante,FamaTotal):-
    vocaloid(Cantante),
    findall(Fama, famaConcierto(Cantante, Fama), CantidadesFama), 	
    sumlist(CantidadesFama, FamaTotal).
    
famaConcierto(Cantante, Fama):-
    puedeParticipar(Cantante,Concierto),
    fama(Concierto, Fama).

fama(Concierto,Fama):- 
concierto(Concierto,_,Fama,_).





