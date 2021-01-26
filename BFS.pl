:-include('isimler.pl').%pl dpsyalarý okuma
:-include('fiiller.pl').

yol(Liste, Sonuc) :-%hedef yolunu yazdýrmak için kurallar
    yol(Liste,[], Sonuc).
yol([], Rota, Rota).
yol([H|T], RT, RL) :-
     yol(T, [H|RT], RL).

iliski2(A,B,C) :- iliski(A,B,C).
iliski2(A,B,C) :- iliski(C,B,A).

solve( Start, Solution) :-%breadthfirst giriþ kural(Seçilmemiþ iliþkiler için)
    breadthfirst( [ [Start] ], Solution).

breadthfirst( [ [Node | Path] |_], [Node | Path] ) :-%bfs recursive kuralýný durdurma kuralý(Seçilmemiþ iliþkiler için)
    goal(Node).

breadthfirst( [ [N | Path] | Paths], Solution) :-%bfs kuralý(Seçilmemiþ iliþkiler için)
    bagof([M,N|Path],
    (iliski2( N,_,M), \+ member( M, [N | Path] ) ), NewPaths),%ilgi iliskileri bulma
    append(Paths, NewPaths, Pathsl), !,%ilgili iliskileri path atma
    breadthfirst( Pathsl, Solution);
    breadthfirst( Paths, Solution).
solve1( Start,R,Solution) :-%breadthfirst giriþ kural(Seçilmiþ iliþkiler için)
    breadthfirst1( [ [Start] ],R,Solution).

breadthfirst1( [ [Node | Path] |_],R ,[Node | Path] ) :-%bfs recursive kuralýný durdurma kuralý(Seçilmiþ iliþkiler için)
    goal(Node).

breadthfirst1( [ [N | Path] | Paths],R ,Solution) :-%bfs kuralý(Seçilmiþ iliþkiler için)
    bagof([M,N|Path],
    ((member(Key,R),iliski2( N,Key,M)), \+ member( M, [N | Path] ) ), NewPaths),%iliþkileri seçmek için (member(Key,R),iliski2( N,Key,M)ekstra olarak.
    append(Paths, NewPaths, Pathsl), !,
    breadthfirst1( Pathsl,R,Solution);
    breadthfirst1( Paths,R,Solution).


basla(X, Start,Finish ):-%Kullanýcý ilk bilgileri girdikten sonra çözümlerin dallanmasý,hedefin belirlenmesi,iliþkilerin yazýlmasý
	assert(goal(Finish)),
        (  X=0 ->%if -else tipindekikural
        solve(Start,Cozum),
        yol(Cozum,Liste),
	write('Liste:'),
	write(Liste),nl,
	length(Liste,L),
	write('Uzunluk:'),
	write(L),nl,
	retract(goal(Finish))
           ;write('Ýstenen iliskiyi yaziniz :'), read(C),
            append([],[C],K),
            Y is X-1,
            loop(Y,K,Start,Finish),%iliþkileri yazmak için döngü
           retract(goal(Finish))
            ).


kullanici() :-%kullanicidan girdi alma
  write('kac tane iliski cesidi olsun?(hepsi icin 0 tuslayiniz'),nl,
  read(X),
  write('hangi kelime ile baslasin'),nl,
  read(Y),
  write('hangi kelime ile bitsin'),nl,
  read(Z),
  basla(X,Y,Z).




loop(0,Ste,Start,Finish):-%girilen iliskiler bittiðinde yapýlacak fonksiyonun çaðrýlmasý
    solve1(Start,Ste,Cozum), yol(Cozum,Liste),write('Liste:'),
	write(Liste),nl,
	length(Liste,L),
	write('Uzunluk:'),
	write(L).

loop(N,Ste,Start,Finish) :- N>0, write('Ýstenen iliskiyi yaziniz :'), read(X),append(Ste,[X],P),M is N-1,loop(M,P,Start,Finish).%Ýliski seçme döngüsü





