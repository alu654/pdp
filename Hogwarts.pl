% Hogwarts


/*Harry es sangre mestiza, y se caracteriza por ser corajudo, amistoso, orgulloso e inteligente. 
Odiaría que el sombrero lo mande a Slytherin.
Draco es sangre pura, y se caracteriza por ser inteligente y orgulloso, 
pero no es corajudo ni amistoso. Odiaría que el sombrero lo mande a Hufflepuff.*/
%Hermione es sangre impura, y se caracteriza por ser inteligente, orgullosa y responsable

/*Para Gryffindor, lo más importante es tener coraje.
Para Slytherin, lo más importante es el orgullo y la inteligencia.
Para Ravenclaw, lo más importante es la inteligencia y la responsabilidad.
Para Hufflepuff, lo más importante es ser amistoso.*/

%mago(Nombre,Sangre).
%caracterizacion(Mago,Personalidad).

mago(harry, mestiza).
mago(draco, pura).
mago(hermione, impura).


caracterizacion(harry, corajudo).
caracterizacion(harry, amistoso).
caracterizacion(harry, orgulloso).
caracterizacion(harry, inteligente).
caracterizacion(draco, inteligente).
caracterizacion(draco, orgulloso).
caracterizacion(hermione, inteligente).
caracterizacion(hermione, orgullosa).
caracterizacion(hermione, responsable).

%sombrero(casa, lo mas importante)
sombrero(gryffindor, corajudo).
sombrero(slytherin,orgulloso).
sombrero(slytherin,inteligente).
sombrero(ravenclaw,inteligente).
sombrero(ravenclaw,responsable).
sombrero(hufflepuff,amistoso).


odia(harry,slytherin).
odia(draco,hufflepuff).

%Saber si una casa permite entrar a un mago, lo cual se cumple para cualquier mago y cualquier casa excepto 
%en el caso de Slytherin, que no permite entrar a magos de sangre impura.

casa(hufflepuff).
casa(ravenclaw).
casa(slytherin).
casa(gryffindor).

permiteEntrar(Casa, _):-
    casa(Casa),
    Casa \= slytherin.
permiteEntrar(slytherin, Mago):-
     mago(Mago,TipoDeSangre),
     TipoDeSangre \= impura.

/*Saber si un mago tiene el carácter apropiado para una casa, lo cual se cumple para cualquier mago si sus características
 incluyen todo lo que se busca para los integrantes de esa casa, independientemente de si la casa le permite la entrada.*/

tieneCaracter(Casa,Mago):-
    caracterizacion(Mago, Personalidad),
    sombrero(Casa,Personalidad).

/*Determinar en qué casa podría quedar seleccionado un mago sabiendo que tiene que tener el carácter adecuado para la casa,
la casa permite su entrada y además el mago no odiaría que lo manden a esa casa. 
Además Hermione puede quedar seleccionada en Gryffindor, porque al parecer encontró una forma de hackear al sombrero.*/

quedaSeleccionado(gryffindor,hermione).
quedaSeleccionado(Casa,Mago):-
    tieneCaracter(Casa,Mago),
    odia(Mago,Casa).

/*Definir un predicado cadenaDeAmistades/1 que se cumple para una lista de magos si todos ellos se caracterizan por ser amistosos 
y cada uno podría estar en la misma casa que el siguiente. No hace falta que sea inversible, se consultará de forma individual.*/

cadenaDeAmistades(Mago):-
    amistoso(Mago),
    mago(OtroMago,_),
    viveenLaMismaCasa(Mago,OtroMago).
    

amistoso(Mago):-
    caracterizacion(Mago,amistoso).

viveenLaMismaCasa(Mago1,OtroMago):-
    mago(Mago1,_),
    mago(OtroMago,_),
    Mago1 \=OtroMago.

    


