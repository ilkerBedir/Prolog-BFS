:-include('isimler.pl').%pl dpsyalar� okuma
:-include('fiiller.pl').

yol(Liste, Sonuc) :-%hedef yolunu yazd�rmak i�in kurallar
    yol(Liste,[], Sonuc).
yol([], Rota, Rota).
yol([H|T], RT, RL) :-
     yol(T, [H|RT], RL).

iliski2(A,B,C) :- iliski(A,B,C).
iliski2(A,B,C) :- iliski(C,B,A).

solve( Start, Solution) :-%breadthfirst giri� kural(Se�ilmemi� ili�kiler i�in)
    breadthfirst( [ [Start] ], Solution).

breadthfirst( [ [Node | Path] |_], [Node | Path] ) :-%bfs recursive kural�n� durdurma kural�(Se�ilmemi� ili�kiler i�in)
    goal(Node).

breadthfirst( [ [N | Path] | Paths], Solution) :-%bfs kural�(Se�ilmemi� ili�kiler i�in)
    bagof([M,N|Path],
    (iliski2( N,_,M), \+ member( M, [N | Path] ) ), NewPaths),%ilgi iliskileri bulma
    append(Paths, NewPaths, Pathsl), !,%ilgili iliskileri path atma
    breadthfirst( Pathsl, Solution);
    breadthfirst( Paths, Solution).
solve1( Start,R,Solution) :-%breadthfirst giri� kural(Se�ilmi� ili�kiler i�in)
    breadthfirst1( [ [Start] ],R,Solution).

breadthfirst1( [ [Node | Path] |_],R ,[Node | Path] ) :-%bfs recursive kural�n� durdurma kural�(Se�ilmi� ili�kiler i�in)
    goal(Node).

breadthfirst1( [ [N | Path] | Paths],R ,Solution) :-%bfs kural�(Se�ilmi� ili�kiler i�in)
    bagof([M,N|Path],
    ((member(Key,R),iliski2( N,Key,M)), \+ member( M, [N | Path] ) ), NewPaths),%ili�kileri se�mek i�in (member(Key,R),iliski2( N,Key,M)ekstra olarak.
    append(Paths, NewPaths, Pathsl), !,
    breadthfirst1( Pathsl,R,Solution);
    breadthfirst1( Paths,R,Solution).


basla(X, Start,Finish ):-%Kullan�c� ilk bilgileri girdikten sonra ��z�mlerin dallanmas�,hedefin belirlenmesi,ili�kilerin yaz�lmas�
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
           ;write('�stenen iliskiyi yaziniz :'), read(C),
            append([],[C],K),
            Y is X-1,
            loop(Y,K,Start,Finish),%ili�kileri yazmak i�in d�ng�
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




loop(0,Ste,Start,Finish):-%girilen iliskiler bitti�inde yap�lacak fonksiyonun �a�r�lmas�
    solve1(Start,Ste,Cozum), yol(Cozum,Liste),write('Liste:'),
	write(Liste),nl,
	length(Liste,L),
	write('Uzunluk:'),
	write(L).

loop(N,Ste,Start,Finish) :- N>0, write('�stenen iliskiyi yaziniz :'), read(X),append(Ste,[X],P),M is N-1,loop(M,P,Start,Finish).%�liski se�me d�ng�s�





