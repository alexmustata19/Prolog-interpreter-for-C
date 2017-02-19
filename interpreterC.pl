valnum(X):- char_type(X, alnum), char_type(X, ascii).
vother(X):- member(X, [';','<','+','-','*','(',')','{','}']).
validc(X):- valnum(X) ; vother(X) ;  X == '='.

lparseq(['='|L],'==',L).
lparseq([X|L],'=',[X|L]):-dif(X,'=').
lparseq([],'=',[]).

lparsealn([X|L],L2,R,L3):- valnum(X), lparsealn(L, [X|L2], R, L3).
lparsealn([X|L],L2,R,[X|L]):- \+valnum(X), reverse(L2, L3), atom_chars(R, L3).
lparsealn([],L2,R,[]):- reverse(L2, L3), atom_chars(R, L3).

lparse2(['='|L],L2,L3):- lparseq(L,R,L4), lparse2(L4,[R|L2],L3).
lparse2([X|L],L2,L3):- valnum(X),lparsealn(L,[X],R,L4), lparse2(L4,[R|L2],L3).
lparse2([X|L],L2,L3):- vother(X), lparse2(L,[X|L2],L3).
lparse2([X|L],L2,L3):- \+validc(X), lparse2(L,L2,L3).
lparse2([],L2,L3):- reverse(L2,L3).

lparse(S, L):- atom_chars(S, L2), lparse2(L2,[],L),!.

/*Predicatul filtreaza lista de numerele primite in formatul cu ghilimele,
 *apoi verifca progamul din lista obtinuta.
 *Daca programul nu este valid atunci exista o eroare de sintaxa
 *si rezultatul este e.
 *Daca programul este corect, dar apare o problema la executie atunci
 *rezultatul este e.
 *Daca programul s-a executat cu succes, dar nu a intors un rezultat
 *atunci rezultatul este x.
 *Altfel, inseamna ca programul este valid, s-a executat cu succes si
 *a intors un rezultat care se leaga la R.
 */
parseInputAux(L,R):-
	filtreaza(L, Ready),
	(verificaProgram(Ready, Program),
	 executaProgram(Program, [], _, Gasit, Rez),
	 ( Gasit = false -> R = x; R = Rez); R = e), !.

parseInput(F,R):-read_file_to_string(F,S,[]), lparse(S,L), parseInputAux(L,R), !.


esteNumar(Nr, Rez) :- atom_number(Nr, Rez).

/*Predicatul realizeaza trecerea numerelor primite in formatul
 *'123' in formatul normal de numere 123.
 */
filtreaza(L, R) :- filtreazaAux(L, [], Rez), reverse(Rez, R).

filtreazaAux([] ,Acc, Acc) :- !.
filtreazaAux([H|T] ,Acc, Accprim) :-
	(esteNumar(H,R) -> filtreazaAux(T, [R|Acc], Accprim);
			   filtreazaAux(T, [H|Acc], Accprim)).

/*Predicat utilizat la obtinerea valorii unei variabile dintr-un context.
 *Daca aceasta nu exista in context atunci acesta esueaza.
 */
getFromContext(Nume, [pair(N,V) | T], Val) :-
	(Nume = N -> Val = V;
	             getFromContext(Nume, T, Val) ), !.

/*Predicatul este utilizat atat pentru updatarea unei variabile intr-un context,
 *cat si la adaugarea unei perechi noi (variabila, valoare).
 */
updateInContext(H,[],[H]).
updateInContext(pair(Var,Val),[pair(OldVar, OldVal) | T], C) :-
	(Var = OldVar -> append([pair(OldVar,Val)], T, C);
	                 updateInContext(pair(Var,Val), T,R), append([pair(OldVar, OldVal)], R, C) ).

/*Predicatul leaga elementul de pe pozitia indicata la Elem,
 *iar daca lista este prea scurta acesta esueaza.
 */
getElementAt([H|T] , Curent, Pozitie, Elem) :-
	(Curent = Pozitie -> Elem = H;
	                     Curent1 is Curent +1,
	                     getElementAt(T, Curent1, Pozitie, Elem) ).

/*Predicatul leaga indexul primei aparitii a unui element cerut la Gasit.
 * Daca elementul nu exista in lista predicatul esueaza.
 */
getFirstIndexOf([H|T], Curent, Cautat, Gasit) :-
	(Cautat = H -> Gasit = Curent;
			       Curent1 is Curent + 1,
			       getFirstIndexOf(T, Curent1, Cautat, Gasit) ).

/*Predicatul leaga la R sublista obtinuta din elementele de la pozitia Start
 *pana la End inclusiv. Daca End este mai mare decat lungimea listei
 *atunci predicatul esueaza.
 */
subList([H|T], Curent, Start, End, R) :-
	(Curent < Start -> Curent1 is Curent + 1, subList(T, Curent1, Start, End, R);
			   (Curent < End -> Curent2 is Curent + 1, subList(T, Curent2, Start, End, R1), append([H],R1,R);
					    Curent = End -> R = [H];
			                                    fail) ).

/*Predicatul leaga R la lista obtinuta pana la pozitia PanaCand.
 *Daca PanaCand este mai mare decat lungimea listei atunci
 *predicatul esueaza.
 */
listUntil(_, Curent, Curent, []) :- !.
listUntil([H|T], Curent, PanaCand, R) :-
	   Curent < PanaCand,
	   Curent1 is Curent + 1,
	   listUntil(T, Curent1, PanaCand, Rez),
	   append([H],Rez,R).

/*Predicatul leaga R la lista obtinuta cu toate elementele
 *de la pozitia Start pana la sfarsitul listei. Daca lista este
 *prea scurta atunci se intoarce lista vida.
 */
listFrom([_|T], Curent, Start, R) :-
	Curent < Start,
	Curent1 is Curent + 1,
	listFrom(T, Curent1, Start, R), !.
listFrom(L,_,_,L).

/*Set de predicate ce exprima operatiile ce se pot regasi
 * intr-o expresie.
 */
inmultire(E1,E2,R) :- Rez is E1 * E2, R = Rez.

adunare(E1, E2, R) :- Rez is E1 + E2, R = Rez.

scadere(E1, E2, R) :- Rez is E1 - E2, R = Rez.

comparatie(E1,E2,R) :- E1 < E2 ,!,R=1.
comparatie(_,_,0).

egalitate(E1,E2,R) :- E1 = E2, !, R=1.
egalitate(_,_,0).

/*Predicatul primeste o lista ce reprezinta o expresie, un operator,
 *un context si realizeaza declansarea calculului primului operator de
 *tipul primit din expresie prin aplicarea lui pe operandul dinainte
 *de el si pe operandul de dupa el. Apoi, se leaga la R o lista ce
 *contine expresia nealterata, cu exceptia faptului ca in locul de unde
 *s-au extras operatorul si operanzii acum se afla rezultatul.
 *Predicatul se ocupa cu realizarea unui calcul efectiv intre doi
 *operanzi.
 */
proceseazaExpresie(L, Op, C, R) :-
	(Op = * -> Operatie = inmultire;
		   (Op = - -> Operatie = scadere;
			      (Op = + -> Operatie = adunare;
					 (Op = < -> Operatie = comparatie;
						    (Op = == -> Operatie = egalitate;
								fail) ) ) ) ),
	getFirstIndexOf(L, 0, Op, Poz),
	PozM1 is Poz - 1,
	PozP1 is Poz + 1,
	getElementAt(L, 0, PozM1, E1),
	getElementAt(L, 0, PozP1, E2),
	(number(E1) -> Elem1 = E1;
	               getFromContext(E1, C, Elem1)),
	(number(E2) -> Elem2 = E2;
	               getFromContext(E2, C, Elem2)),
	call(Operatie, Elem1, Elem2, Rez),
	listUntil(L, 0, PozM1, L1),
	PozPrim is PozP1 + 1,
	listFrom(L, 0, PozPrim, L2),
	append(L1,[Rez],Lprim),
	append(Lprim,L2, Lnou),
	evalueazaExpresie(Lnou, C, R).

/*Predicatul evalueaza o expresie reprezentata sub forma de lista.
 *Se primeste un context si rezultatul se va lega la o variabila
 *neinstantiata. Predicatul se foloseste de predicatul
 *proceseazaExpresie pentru a realiza calculul.
 *Predicatul se ocupa efectiv cu alegerea operatiei de
 *prioritate maxima din expresie.
 */
evalueazaExpresie([H], _, H) :- number(H), !.
evalueazaExpresie([Var], C, Val) :- getFromContext(Var, C, Val), !.
evalueazaExpresie(L, C, R) :-
	(member(*,L) -> proceseazaExpresie(L,*,C,R);
		        (member(-,L) -> proceseazaExpresie(L,-,C,R);
					(member(+,L) -> proceseazaExpresie(L,+,C,R);
							(member(<,L), member(==,L) -> getFirstIndexOf(L, 0, <, Poz1),
										      getFirstIndexOf(L, 0, ==, Poz2),
										      (Poz1 < Poz2 -> proceseazaExpresie(L,<,C,R);
										                      proceseazaExpresie(L,==,C,R)
										      );
										      (member(<,L) -> proceseazaExpresie(L,<,C,R);
												      (member(==,L) -> proceseazaExpresie(L,==,C,R);
														       fail
												      )
										      )
							)
					)
			)
	) ,!.

/*Predicatul verifica daca o lista este compusa
 *numai din litere.
 */
numaiLitere([]) :- !.
numaiLitere([H|T]) :- char_type(H, alpha), numaiLitere(T).

/*O baza de cunostinte pentru identificarea
 *operatorilor valizi.
 */
operator(*).
operator(-).
operator(+).
operator(<).
operator(==).

/*O baza de cunostinte pentru identificarea
 *numelor ce nu pot fi variabile.
 */
numeRezervat(if).
numeRezervat(then).
numeRezervat(else).
numeRezervat(while).
numeRezervat(return).

/*Predicat ce verifica daca un simbol este o variabila valida
 *prin descompunerea acestuia intr-o lista de caractere si
 *verificarea ca acestea sunt numai litere. De asemenea,
 *simbolul nu trebuie sa fie nume rezervat.
 */
verificaVariabila(Var) :- not(numeRezervat(Var)) ,atom_chars(Var,L), numaiLitere(L).

/*Predicat ce verifica daca un simbol este un operand valid.
 *Se verifaca daca acesta este numar sau variabila.
 */
verificaOperand(Var) :- (number(Var) ; verificaVariabila(Var)).

/*Predicatul verifica daca o lista este o reprezentare
 *valida a unei expresii. Pentru acest lucru trebuie ca
 *expresia sa inceapa si sa se termine cu un operand si
 *in lista sa exista o alternanta operand-operator.
 */
verificaExpresie([H], Curent) :-
	Rest is Curent mod 2,
	Rest = 0,
	verificaOperand(H), !.

verificaExpresie([H|T], Curent) :-
	Rest is Curent mod 2,
	Curent1 is Curent + 1,
	( Rest = 0 -> verificaOperand(H);
	              operator(H) ),
	verificaExpresie(T, Curent1), !.

/*Predicatul realizeaza legarea indexului de dupa acolada pereche
 *la o variabila neinstantiata prin numararea tipurilor de acolade
 *peste care se trece. Daca acolada pereche nu exista in sirul de
 *caractere reprezentat ca lista atunci predicatul esueaza.
 */
indexOfAcoladaPereche(_, 0, Curent, Curent) :- !.
indexOfAcoladaPereche([H|T], NrAcolade, Curent, Index) :-
	Curent1 is Curent + 1,
       ( H = '{' -> NrAcoladeP is NrAcolade + 1, indexOfAcoladaPereche(T, NrAcoladeP, Curent1, Index);
		    ( H = '}' -> NrAcoladeM is NrAcolade - 1, indexOfAcoladaPereche(T, NrAcoladeM, Curent1, Index);
				 indexOfAcoladaPereche(T, NrAcolade, Curent1, Index))).

/*Predicatul realizeaza verificarea corectitudinii sintactice
 *a programului, iar daca programul este valid atunci se va
 *intoarce o structura tip arbore sub care va fi reprezentat
 *programul, altfel predicatul esueaza.
 *
 *La fiecare pas daca programul este corect, dar mai exista
 *text neconsumat la parsare inseamna ca exista o secventiere
 *intre programul parsat si un altul.
 *
 *Un program de tip ; este valid mereu.
 */
verificaProgram([;], punctSiVirgula) :- !.

/*Corectitudinea unui program ; urmat de alt program T implica
 *corectitudinea programului T.
 */
verificaProgram([;|T], Program) :-
	verificaProgram(T, P),
	Program = secventa(punctSiVirgula, P), !.

/*Corectitudinea unui program return implica existenta ; dupa
 *expresie si corectitudinea expresiei.
 */
verificaProgram([return|T], Program) :-
	getFirstIndexOf(T, 0, ;, Index),
	listUntil(T, 0, Index, Expresie),
	verificaExpresie(Expresie, 0),
	Index1 is Index + 1,
	listFrom(T, 0, Index1, Rest),
	( Rest = [] -> Program = return(Expresie);
		       verificaProgram(Rest, Program1),
		       Program = secventa(return(Expresie), Program1)), !.

/*Corectitudinea unui program de tip atribuire implica faptul ca
 *atribuirea trebuie sa se faca unei variabile, expresia ce se atribuie
 *trebuie sa fie corecta si trebuie sa existe ; dupa expresie.
 */
verificaProgram([Var, = | T], Program) :-
	getFirstIndexOf(T, 0, ;, Index),
	listUntil(T, 0, Index, Expresie),
	verificaVariabila(Var),
	verificaExpresie(Expresie, 0),
	Index1 is Index + 1,
	listFrom(T, 0, Index1, Rest),
	(Rest =[] -> Program = atribuire(Var, Expresie);
		     verificaProgram(Rest, Program1),
	             Program = secventa(atribuire(Var, Expresie), Program1)), !.

/*Corectitudinea unui program de tip while implica indeplinirea
 *urmatoarelor conditii:
 *->sa existe ( dupa while;
 *->intre while si ( sa nu existe nimic;
 *->sa existe ) pereche pentru (;
 *->intre paranteze sa se afle o expresie valida;
 *->intre ) si { sa nu se afle nimic;
 *->sa existe } pereche pentru {;
 *->intre acolade sa existe un program valid.
 */
verificaProgram([while|T], Program) :-
	getFirstIndexOf(T, 0, '(', IndexPD),
	IndexPD = 0,
	getFirstIndexOf(T, 0, ')', IndexPI),
	IndexStart is IndexPD + 1,
	IndexEnd is IndexPI - 1,
	subList(T, 0, IndexStart, IndexEnd, Expresie),
	verificaExpresie(Expresie, 0),
	getFirstIndexOf(T, 0, '{', IndexAD),
	IndexVerificare is IndexPI + 1,
	IndexVerificare = IndexAD,
	IndexSelectie is IndexAD + 1,
	listFrom(T, 0, IndexSelectie, CautareAI),
	indexOfAcoladaPereche(CautareAI, 1, IndexSelectie, IndexAI),
	IndexSelectie2 is IndexAI - 2,
	subList(T, 0, IndexSelectie, IndexSelectie2, ProgramBucla),
	verificaProgram(ProgramBucla, Bucla),
	listFrom(T, 0, IndexAI, Rest),
	(Rest = [] -> Program = while(Expresie, Bucla);
	              verificaProgram(Rest, Program1),
	              Program = secventa(while(Expresie, Bucla), Program1)),!.

/*Corectitudinea unui program de tip if implica indeplinirea
 *urmatoarelor conditii:
 *->dupa if sa existe (;
 *->intre if si ( sa nu existe nimic;
 *->sa existe ) pereche pentru (;
 *->intre paranteze sa se afle o expresie valida;
 *->dupa ) sa urmeze then, fara nimic intre;
 *->dupa then sa urmeze {, fara nimic intre;
 *->sa existe } pereche pentru {;
 *->intre acoladele pentru prima ramura sa existe un program valid;
 *->dupa } sa existe else, fara nimic intre;
 *->dupa else sa existe {, fara nimic intre;
 *->sa existe } pereche pentru {;
 *->intre acoladele pentru a doua ramura sa existe un program valid.
 */
verificaProgram([if|T], Program) :-
	getFirstIndexOf(T, 0, '(', IndexPD),
	IndexPD = 0,
	getFirstIndexOf(T, 0, ')', IndexPI),
	IndexStart is IndexPD + 1,
	IndexEnd is IndexPI - 1,
	subList(T, 0, IndexStart, IndexEnd, Expresie),
	verificaExpresie(Expresie, 0),
	IndexThen is IndexPI + 1,
	getFirstIndexOf(T, 0, then, IndexThen2),
	IndexThen = IndexThen2,
	getFirstIndexOf(T, 0, '{', IndexAD),
	IndexVerificareAcolada is IndexThen + 1,
	IndexVerificareAcolada = IndexAD,
	IndexSelectie is IndexAD + 1,
	listFrom(T, 0, IndexSelectie, CautareAI),
	indexOfAcoladaPereche(CautareAI, 1, IndexSelectie, IndexAI),
	IndexSelectie2 is IndexAI - 2,
	subList(T, 0, IndexSelectie, IndexSelectie2, ProgramCorpTrue),
	verificaProgram(ProgramCorpTrue, CorpTrue),
	getElementAt(T, 0, IndexAI, Else),
	Else = else,
	IndexAD2 is IndexAI + 1,
        getElementAt(T, 0, IndexAD2, AD2),
	AD2 = '{',
	AD2plus1 is IndexAD2 + 1,
	listFrom(T, 0, AD2plus1, CautareAI2),
	indexOfAcoladaPereche(CautareAI2, 1, AD2plus1, AI2),
	AI2minus2 is AI2 - 2,
	subList(T, 0, AD2plus1, AI2minus2, ProgramCorpFalse),
	verificaProgram(ProgramCorpFalse, CorpFalse),
	listFrom(T, 0, AI2, Rest),
	(Rest = [] -> Program = if(Expresie, CorpTrue, CorpFalse);
	              verificaProgram(Rest, Program1),
		      Program = secventa(if(Expresie, CorpTrue, CorpFalse), Program1)), !.

/*Predicatul realizeaza executarea diferitelor tipuri de program.
 *Aceste primeste un program si un context si la trei variabile
 *neinstantiate se leaga contextul obtinut in urma executarii
 *programului, faptul daca s-a intalnit un program return, si daca
 *s-a intalnit return valoarea intoarsa de acesta.
 *
 *Un program de tip ; nu modifica contextul si nici nu returneaza ceva.
 */
executaProgram(punctSiVirgula, C, C, false, false) :- !.

/*Programul de tip secventa se executa prin executarea primei parti
 *din secventa. Daca aceasta intoarce ceva atunci secventa va intoarce
 *direct acelasi lucru, daca nu atunci se executa si a doua parte din
 *secventa si se intoarce starea data de executia acesteia.
 *In cadrul acestui program, contextul pentru partea a doua este
 *contextul ce a rezultat in urma rularii primei parti.
 */
executaProgram(secventa(Program1, Program2), C, C2, Gasit, Rez) :-
	executaProgram(Program1, C, C1, Gasit1, Rez1),
	( Gasit1 = true -> Gasit = true, Rez = Rez1;
			   executaProgram(Program2, C1, C2, Gasit2, Rez2),
	                   ( Gasit2 = true -> Gasit = true, Rez = Rez2;
			                      Gasit = false, Rez = false)), !.
/*Un program de tip atribuire se executa calculand expresia si
 *modificand corespunzator variabila careia i s-a facut atribuirea
 *in contextul primit, acesta reprezentand noul context.
 */
executaProgram(atribuire(Variabila, Expresie), C, Cnou, false, false) :-
	evalueazaExpresie(Expresie, C, Rez),
	updateInContext(pair(Variabila, Rez), C, Cnou), !.

/*Un program de tip if se executa determinand intai valoarea de
 *adevar a expresiei si apoi executand ramura corespunzatoare.
 */
executaProgram(if(Conditie, CorpTrue, CorpFalse), C, Cnou, Gasit, Rez) :-
	evalueazaExpresie(Conditie, C, Test),
	(Test = 0 -> executaProgram(CorpFalse, C, Cnou, Gasit, Rez);
	             executaProgram(CorpTrue, C, Cnou, Gasit, Rez)), !.

/*Un program de tip while se executa evaluand mai intai conditia,
 *daca aceasta este adevarata se executa corpul buclei.
 *Daca s-a intalnit return in bucla se stopeaza eventualul ciclu.
 *Daca nu conditia se reevalueaza folosind contextul ce reiese din
 *executia buclei, daca aceasta este inca adevarata se trimite
 *printr-un apel recursiv din nou programul while in executie.
 */
executaProgram(while(Conditie, Bucla), C, C2, Gasit, Rez) :-
	evalueazaExpresie(Conditie, C, Test1),
	(Test1 = 0 -> Gasit = false, Rez = false;
	             executaProgram(Bucla, C, C1, Gasit1, Rez1),
	             ( Gasit1 = true -> Gasit = true, Rez = Rez1, C2 = C1;
		                        evalueazaExpresie(Conditie, C1, Test2),
					(Test2 = 0 -> Gasit = false, Rez = false, C2 = C1;
					              executaProgram(while(Conditie, Bucla), C1, C2, Gasit, Rez)))), !.

/*Un program de tip return se executa prin calcularea expresiei,
 *intoarcerea aceluiasi context, semnalarea faptului ca s-a
 *intalnit return si intoarcerea rezultatului.
 */
executaProgram(return(Expresie), C , C, true, Rez) :- evalueazaExpresie(Expresie, C, Rez), !.














