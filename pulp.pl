
personaje(pumkin,     ladron([licorerias, estacionesDeServicio])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)). 
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).

pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).

amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).

%encargo(Solicitante, Encargado, Tarea). 
%las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).

caracteristicas(vincent,  [negro, muchoPelo, tieneCabeza]).
caracteristicas(jules,    [tieneCabeza, muchoPelo]).
caracteristicas(marvin,   [negro]).

sonPareja(UnPersonaje, OtroPersonaje):-
    pareja(UnPersonaje, OtroPersonaje).

sonPareja(UnPersonaje, OtroPersonaje):-
    pareja(OtroPersonaje, UnPersonaje).

sonAmigos(UnPersonaje, OtroPersonaje):-
    amigo(UnPersonaje, OtroPersonaje).

sonAmigos(UnPersonaje, OtroPersonaje):-
    amigo(OtroPersonaje, UnPersonaje).



/*1. esPeligroso/1. Nos dice si un personaje es peligroso. Eso ocurre cuando:
realiza alguna actividad peligrosa: ser matón, o robar licorerías. 
tiene empleados peligrosos
*/
esPeligroso(Personaje):-
    personaje(Personaje,Info),
    esLadroOMaton(Info).

esPeligroso(Personaje):-
    trabajaPara(Personaje,Empleador),
    esPeligroso(Empleador).

esLadroOMaton(ladron(Info)):-
    member(licorerias, Info).

esLadroOMaton(mafioso(_)).

% 2
/*duoTemible/2 que relaciona dos personajes cuando son peligrosos y
 además son pareja o amigos. Considerar que Tarantino también nos dió los siguientes hechos:*/

 duoTemible(Personaje,OtroPersonaje):-
    sonAmigos(Personaje,OtroPersonaje),
    esPeligroso(Personaje),
    esPeligroso(OtroPersonaje).
% 3
/* estaEnProblemas/1: un personaje está en problemas cuando 
el jefe es peligroso y le encarga que cuide a su pareja
o bien, tiene que ir a buscar a un boxeador. 
Además butch siempre está en problemas. */

%encargo(Solicitante, Encargado, Tarea).

estaEnProblemas(butch).

estaEnProblemas(Jefe):-
    trabajaPara(Jefe,Encargado),
    esPeligroso(Jefe),
    tareaEncargo(Jefe,Encargado).

tareaEncargo(UnJefe, UnPersonaje):-
    pareja(UnJefe, SuPareja),
    encargo(UnJefe, UnPersonaje, cuidar(SuPareja)).

tareaEncargo(UnJefe, UnPersonaje):-
    encargo(UnJefe, UnPersonaje, buscar(OtroPersonaje, _)),
    personaje(OtroPersonaje, boxeador).

/* sanCayetano/1:  es quien a todos los que tiene cerca les da trabajo (algún encargo). 
Alguien tiene cerca a otro personaje si es su amigo o empleado.
*/

sanCayetano(UnPersonaje):-
    trabajaPara(UnPersonaje, _),
    daTrabajoAAmigosOEmpleados(UnPersonaje).

sanCayetano(UnPersonaje):-
    amigo(UnPersonaje, _),
    daTrabajoAAmigosOEmpleados(UnPersonaje).

daTrabajoAAmigosOEmpleados(UnPersonaje):-
    forall(esAmigoOEmpleado(UnPersonaje, OtroPersonaje), encargo(UnPersonaje, OtroPersonaje, _)).

esAmigoOEmpleado(UnPersonaje, OtroPersonaje):-
    sonAmigos(UnPersonaje, OtroPersonaje).

esAmigoOEmpleado(UnPersonaje, OtroPersonaje):-
    trabajaPara(UnPersonaje, OtroPersonaje).


% 5 
/*masAtareado/1. Es el más atareado aquel que tenga más encargos que cualquier otro personaje.*/
masAtareado(Empleado):-
   tieneMasTrabajo(Empleado,Cantidad),
   forall((tieneMasTrabajo(EmpleadoOtro,CatidadOtra),Empleado \= EmpleadoOtro),Cantidad >= CatidadOtra).
   

tieneMasTrabajo(Empleado,CantidadTotal):-
   encargo(_,Empleado,_),
   findall(Tarea, encargo(_,Empleado,Tarea), ListadeTarea),
   length(ListadeTarea, CantidadTotal).
   
/*personajesRespetables/1: genera la lista de todos los personajes respetables. 
Es respetable cuando su actividad tiene un nivel de respeto mayor a 9. Se sabe que:
Las actrices tienen un nivel de respeto de la décima parte de su cantidad de peliculas.
Los mafiosos que resuelven problemas tienen un nivel de 10 de respeto, los matones 1 y los capos 20.
Al resto no se les debe ningún nivel de respeto
*/
personajesRespetables(Lista):-
    findall(Personaje,esPersonajeRespetable(Personaje),Lista).
   
    
esPersonajeRespetable(Personaje):-
    nivelDeRespeto(Personaje, UnNivel),
    UnNivel > 9.

nivelDeRespeto(actriz(ListaDePeliculas), UnNivel):-
    length(ListaDePeliculas, CantidadDePeliculas),
    UnNivel is CantidadDePeliculas / 10.
nivelDeRespeto(mafioso(resuelveProblemas), 10).
nivelDeRespeto(mafioso(maton), 1).
nivelDeRespeto(mafioso(capo), 20).
nivelDeRespeto(OtraProfesion, 0):-
    OtraProfesion \= mafioso,
    OtraProfesion \= actriz.

/*7. hartoDe/2: un personaje está harto de otro, cuando todas las tareas asignadas al primero requieren
 interactuar con el segundo (cuidar, buscar o ayudar) o un amigo del segundo. Ejemplo:*/

 hartoDe(Jefe,Empleado):-
    encargo(Jefe, Empleado, _),
    forall(encargo(Jefe,Empleado,Info),obtenerInfo(Jefe,Info)).

obtenerInfo(Jefe,cuidar(Jefe)).
obtenerInfo(Jefe,cuidar(buscar(Jefe))).
obtenerInfo(Jefe,ayudar(Jefe)).

