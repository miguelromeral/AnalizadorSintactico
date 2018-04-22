%----------------------------------------------------
% Diseñado por: Miguel Angel Garcia Romeral
%----------------------------------------------------


% Reglas gramaticales

% ORACIONES:

oracion(o(GN,GV)) --> g_nominal(GN), g_verbal(GV).
oracion(o(GV)) --> g_verbal(GV).
%Una oración puede tener una subordinada en el sujeto, por lo que habrá que tratarlo en or_sub_aux.
oracion(S) --> or_sub_aux(S).
%Una oración compuesta es una oración que tiene una subordinada y otra oración o al revés.
oracion(ocm(S,conj(y),O)) --> or_sub_aux(S),conjuncion(conj(y)),oracion(O).
%Una oración puede ser la unión de varias oraciones.
oracion(O) --> oracion_coordinada(O).

% ORACIÓN SUBORDINADA EN EL SUJETO:

or_sub_aux(o(GN,GV)) --> g_nominal_con_subordinada(GN),g_verbal(GV).

% SINTAGMA NOMINAL CON ORACIÓN SUBORDINADA:

g_nominal_con_subordinada(gn(N,O)) --> g_nominal(N),oracion_subordinada(O).

% ORACIÓN SUBORDINADA:

oracion_subordinada(or(conj(que),GV)) --> conjuncion(conj(que)),g_verbal(GV).

% SINTAGMA NOMINAL:

g_nominal(gn(N)) --> nombre(N).
% Un sujeto puede ser múltiple, los nombres se unen con 'conj(y)'.
g_nominal(gn(N,GN)) --> nombre(N),gn_comp(GN).
g_nominal(gn(D,N)) --> determinante(D),nombre(N).
% Un sujeto puede ser múltiple, los nombres se unen con 'conj(y)'.
g_nominal(gn(D,N,GN)) --> determinante(D),nombre(N),gn_comp(GN).
g_nominal(gn(N,A)) --> nombre(N),g_adjetival(A).
% Puede tener varios comlpementos que indentifiquen al sujeto, ejemplo: "el perro de el jardin"
g_nominal(gn(D,N,P)) --> determinante(D),nombre(N),g_prep_aux(P).
% Un sintagma adjetivla puede estar delante o detrás del sustantivo.
g_nominal(gn(D,N,A)) --> determinante(D),nombre(N),g_adjetival(A).
g_nominal(gn(D,A,N)) --> determinante(D),g_adjetival(A),nombre(N).

% COMBINACIÓN DE SUJETOS MÚLTIPLES:
% Unidos mediante "y" pudiendo ser varios los sintagmas nominales.

gn_comp(^(conj(y),GN)) --> conjuncion(conj(y)),g_nominal(GN).
gn_comp(^(conj(y),GN,O)) --> conjuncion(conj(y)),g_nominal(GN),gn_comp(O).

% SINTAGMA VERBAL:
% Puede ser unicamente un verbo o la union de varios sintagmas.

g_verbal(gv(V)) --> v_compuesto(V).
g_verbal(gv(V,S)) --> v_compuesto(V),sintagmas_aux(S).

% CONTINUACIÓN DE SINTAGMAS EN UNO VERBAL:
% uno o más sintagmas. '^' indica que el nivel del sintagma debería estar más arriba.

sintagmas_aux(^(S)) --> sintagmas(S).
sintagmas_aux(^(S,S2)) --> sintagmas(S),sintagmas_aux(S2).

% SINTAGMAS EN UNO VERBAL:

sintagmas(S) --> g_nominal(S).
sintagmas(S) --> g_adjetival(S).
sintagmas(S) --> g_adverbial(S).
sintagmas(S) --> g_preposicional(S).
sintagmas(S) --> oracion_subordinada(S).

% CONTINUACIÓN DEL SINTAGMA PREPOSICIONAL
% Un SP puede estar unido a otro SP.

g_prep_aux(GP) --> g_preposicional(GP).
g_prep_aux(^(GP1,GP2)) --> g_preposicional(GP1),g_prep_aux(GP2).

% ORACIONES COORDINADAS:
% Dos o más oraciones separadas por conjuncinoes.

oracion_coordinada(oc(O1,conj(y),O2)) --> oracion(O1),conjuncion(conj(y)),oracion(O2).
oracion_coordinada(oc(O1,conj(y),O2)) --> oracion(O1),conjuncion(conj(y)),oracion_coordinada(O2).

% SINTAGMA PREOPOSICIONAL

g_preposicional(gp(P,GN)) --> preposicion(P),g_nominal(GN).
g_preposicional(gp(P,GN)) --> preposicion(P),g_verbal(GN).

% SINTAGMA ADVERBIAL

g_adverbial(gadv(A)) --> adverbio(A).
g_adverbial(gadv(C,A)) --> cuantificador(C),adverbio(A).

% SINTAGMA ADJETIVAL

g_adjetival(gadj(A)) --> adjetivo(A).
g_adjetival(gadj(C,A)) --> cuantificador(C),adjetivo(A).

% VERBO: puede ser simple o compuesto.

v_compuesto(V) --> verbo(V).
v_compuesto(vc(V1,V2)) --> verbo(V1),verbo(V2).

% Diccionario

cuantificador(ct(X)) --> [X],{ct(X)}, !.
ct(bastante).
ct(mas).
ct(muy).

determinante(det(X)) --> [X],{det(X)}, !.
det(el).
det(la).
det(las).
det(los).
det(mi).
det(un).
det(una).

adjetivo(adj(X)) --> [X],{adj(X)}, !.
adj(agil).
adj(alta).
adj(arabe).
adj(delicado).
adj(español).
adj(españoles).
adj(fritas).
adj(gran).
adj(grande).
adj(gris).
adj(libres).
adj(mayor).
adj(mejor).
adj(menor).
adj(moreno).
adj(negro).
adj(once).
adj(pasado).
adj(pequeno).
adj(potente).
adj(roja).
adj(rojas).
adj(sintactico).

adverbio(adv(X)) --> [X],{adv(X)}, !.
adv(ahora).
adv(aqui).
adv(bien).
adv(mal).
adv(no).
adv(perfectamente).
adv(solamente).
adv(todavia).

verbo(v(X)) --> [X],{v(X)}, !.
v(afirmo).
v(aseguro).
v(ama).
v(bebe).
v(beben).
v(canta).
v(come).
v(comen).
v(comer).
v(debera).
v(demostrar).
v(dijo).
v(eligen).
v(era).
v(es).
v(escala).
v(escribir).
v(estudia).
v(funciona).
v(gano).
v(ha_trabajado).
v(haber_ganado).
v(intentan).
v(ir).
v(juega).
v(juegan).
v(lanzar).
v(lee).
v(marcar).
v(mucho).
v(pasar).
v(prefiere).
v(programar).
v(queria).
v(quiere).
v(sabe).
v(salta).
v(sean).
v(se_merece).
v(son).
v(sirve).
v(tiene).
v(toma).
v(recoge).
v(va).
v(venir).

conjuncion(conj(X)) --> [X],{conj(X)}, !.
conj(aunque).
conj(e).
conj(mientras).
conj(pero).
conj(que).
conj(y).
conj(-).

preposicion(prep(X)) --> [X],{prep(X)}, !.
prep(a).
prep(con).
prep(de).
prep(desde).
prep(en).
prep(para).
prep(por).

nombre(n(X)) --> [X],{n(X)}, !.
nombre(np(X)) --> [X],{np(X)}, !.
n(alcalde).
n(alumno).
n(año).
n(analizador).
n(baño).
n(caballo).
n(cafe).
n(campeones).
n(carrera).
n(ceramica).
n(cerveza).
n(clase).
n(copa).
n(cosa).
n(cuchillo).
n(derecho).
n(dias).
n(diez).
n(documentos).
n(equipo).
n(españoles).
n(filosofia).
n(gato).
n(herramienta).
n(hombre).
n(informatica).
n(java).
n(jardin).
n(kiwis).
n(liga).
n(linea).
n(manzana).
n(manzanas).
n(mesa).
n(metros).
n(mujer).
n(nacion).
n(niños).
n(novela).
n(paella).
n(patatas).
n(pelota).
n(penalti).
n(penaltis).
n(periodico).
n(perro).
n(php).
n(practica).
n(procesador).
n(profesor).
n(punto).
n(raton).
n(raza).
n(rocodromo).
n(ronda).
n(tardes).
n(tenedor).
n(textos).
n(tiros).
n(triples).
n(universidad).
n(vecino).
n(vecinos).
n(zumo).

np(españa).
np(hector).
np(irene).
np(juan).
np(maria).
np(miguel).
np(pedro).
np(ramos).
np(talavera).