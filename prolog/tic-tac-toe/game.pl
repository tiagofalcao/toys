#!/usr/bin/swipl -f -q
% Tic-Tac-Toe
%
% Require Prolog with XPCE support.
% Play with mouse, or type "jogo(X,Y)" where X,Y e {0,1,2}
% The game will restart with a click after the end.
% This not use the best algorithm. Tic-Tac-Toe is a limited game, with public solutions.
% The idea is have fun, you can win this game!
%
% Copyright:
% Tiago Falcão
% Ricardo Saffi


:- use_module(library(pce)).


simbol(p,X):- new(X, text('X')),send(X,font, font(arial, bold, 72)).
simbol(j,O):- new(O, text('O')),send(O,font, font(arial, bold, 72)).

set_pos(P,0,0):- tabuleiro(b,K1,K2,K3,K4,K5,K6,K7,K8),retract(tabuleiro(b,K1,K2,K3,K4,K5,K6,K7,K8)),
                  assert(tabuleiro(P,K1,K2,K3,K4,K5,K6,K7,K8)).

set_pos(P,1,0):- tabuleiro(K0,b,K2,K3,K4,K5,K6,K7,K8),retract(tabuleiro(K0,b,K2,K3,K4,K5,K6,K7,K8)),
                  assert(tabuleiro(K0,P,K2,K3,K4,K5,K6,K7,K8)).

set_pos(P,2,0):- tabuleiro(K0,K1,b,K3,K4,K5,K6,K7,K8),retract(tabuleiro(K0,K1,b,K3,K4,K5,K6,K7,K8)),
                  assert(tabuleiro(K0,K1,P,K3,K4,K5,K6,K7,K8)).

set_pos(P,0,1):- tabuleiro(K0,K1,K2,b,K4,K5,K6,K7,K8),retract(tabuleiro(K0,K1,K2,b,K4,K5,K6,K7,K8)),
                  assert(tabuleiro(K0,K1,K2,P,K4,K5,K6,K7,K8)).

set_pos(P,1,1):- tabuleiro(K0,K1,K2,K3,b,K5,K6,K7,K8),retract(tabuleiro(K0,K1,K2,K3,b,K5,K6,K7,K8)),
                  assert(tabuleiro(K0,K1,K2,K3,P,K5,K6,K7,K8)).

set_pos(P,2,1):- tabuleiro(K0,K1,K2,K3,K4,b,K6,K7,K8),retract(tabuleiro(K0,K1,K2,K3,K4,b,K6,K7,K8)),
                  assert(tabuleiro(K0,K1,K2,K3,K4,P,K6,K7,K8)).

set_pos(P,0,2):- tabuleiro(K0,K1,K2,K3,K4,K5,b,K7,K8),retract(tabuleiro(K0,K1,K2,K3,K4,K5,b,K7,K8)),
                  assert(tabuleiro(K0,K1,K2,K3,K4,K5,P,K7,K8)).

set_pos(P,1,2):- tabuleiro(K0,K1,K2,K3,K4,K5,K6,b,K8),retract(tabuleiro(K0,K1,K2,K3,K4,K5,K6,b,K8)),
                  assert(tabuleiro(K0,K1,K2,K3,K4,K5,K6,P,K8)).

set_pos(P,2,2):- tabuleiro(K0,K1,K2,K3,K4,K5,K6,K7,b),retract(tabuleiro(K0,K1,K2,K3,K4,K5,K6,K7,b)),
                  assert(tabuleiro(K0,K1,K2,K3,K4,K5,K6,K7,P)).

joga(P,X,Y):- set_pos(P,X,Y), XX is X * 100 + 30, YY is Y * 100 + 13, simbol(P,S),
              send(@janela,display,S,point(XX,YY)). 

jogo(_,_):- limpar(true),restart.
jogo(X,Y):- write((j,X,Y)),restart,joga(j,X,Y),acabou(j),jogue.




jogue:-limpar(true).
jogue:-jogando,acabou(p).

% Onde matar
complete(P,0,0):-tabuleiro(b,P,P,_,_,_,_,_,_).
complete(P,1,0):-tabuleiro(P,b,P,_,_,_,_,_,_).
complete(P,2,0):-tabuleiro(P,P,b,_,_,_,_,_,_).

complete(P,0,1):-tabuleiro(_,_,_,b,P,P,_,_,_).
complete(P,1,1):-tabuleiro(_,_,_,P,b,P,_,_,_).
complete(P,2,1):-tabuleiro(_,_,_,P,P,b,_,_,_).

complete(P,0,2):-tabuleiro(_,_,_,_,_,_,b,P,P).
complete(P,1,2):-tabuleiro(_,_,_,_,_,_,P,b,P).
complete(P,2,2):-tabuleiro(_,_,_,_,_,_,P,P,b).

complete(P,0,0):-tabuleiro(b,_,_,P,_,_,P,_,_).
complete(P,0,1):-tabuleiro(P,_,_,b,_,_,P,_,_).
complete(P,0,2):-tabuleiro(P,_,_,P,_,_,b,_,_).

complete(P,1,0):-tabuleiro(_,b,_,_,P,_,_,P,_).
complete(P,1,1):-tabuleiro(_,P,_,_,b,_,_,P,_).
complete(P,1,2):-tabuleiro(_,P,_,_,P,_,_,b,_).

complete(P,2,0):-tabuleiro(_,_,b,_,_,P,_,_,P).
complete(P,2,1):-tabuleiro(_,_,P,_,_,b,_,_,P).
complete(P,2,2):-tabuleiro(_,_,P,_,_,P,_,_,b).

complete(P,0,0):-tabuleiro(b,_,_,_,P,_,_,_,P).
complete(P,1,1):-tabuleiro(P,_,_,_,b,_,_,_,P).
complete(P,2,2):-tabuleiro(P,_,_,_,P,_,_,_,b).

complete(P,2,0):-tabuleiro(_,_,b,_,P,_,P,_,_).
complete(P,1,1):-tabuleiro(_,_,P,_,b,_,P,_,_).
complete(P,0,2):-tabuleiro(_,_,P,_,P,_,b,_,_).

% Ganho
jogando:-complete(p,X,Y),joga(p,X,Y).

% Defendo
jogando:-complete(j,X,Y),joga(p,X,Y).

jogando:-ganhador(p),joga(p,0,0).
jogando:-ganhador(p),joga(p,2,2).
jogando:-ganhador(p),joga(p,0,2).
jogando:-ganhador(p),joga(p,2,0).
jogando:-ganhador(p),joga(p,1,1).

jogando:-joga(p,1,0).
jogando:-joga(p,0,1).
jogando:-joga(p,2,1).
jogando:-joga(p,1,2).

jogando:-not(jogador(p)),joga(p,1,1).

jogando:-not(jogador(p)),joga(p,0,0).
jogando:-not(jogador(p)),joga(p,2,2).
jogando:-not(jogador(p)),joga(p,0,2).
jogando:-not(jogador(p)),joga(p,2,0).



acabou(K):-tabuleiro(K,K,K,_,_,_,_,_,_),resultado(K).
acabou(K):-tabuleiro(_,_,_,K,K,K,_,_,_),resultado(K).
acabou(K):-tabuleiro(_,_,_,_,_,_,K,K,K),resultado(K).
acabou(K):-tabuleiro(K,_,_,K,_,_,K,_,_),resultado(K).
acabou(K):-tabuleiro(_,K,_,_,K,_,_,K,_),resultado(K).
acabou(K):-tabuleiro(_,_,K,_,_,K,_,_,K),resultado(K).
acabou(K):-tabuleiro(K,_,_,_,K,_,_,_,K),resultado(K).
acabou(K):-tabuleiro(_,_,K,_,K,_,K,_,_),resultado(K).
acabou(_):-not(tabuleiro(b,_,_,_,_,_,_,_,_)),not(tabuleiro(_,b,_,_,_,_,_,_,_)),not(tabuleiro(_,_,b,_,_,_,_,_,_)),not(tabuleiro(_,_,_,b,_,_,_,_,_)),not(tabuleiro(_,_,_,_,b,_,_,_,_)),not(tabuleiro(_,_,_,_,_,b,_,_,_)),not(tabuleiro(_,_,_,_,_,_,b,_,_)),not(tabuleiro(_,_,_,_,_,_,_,b,_)),not(tabuleiro(_,_,_,_,_,_,_,_,b)),resultado(n).
acabou(_).


mens(p,'Eu Ganhei !!! :P').
mens(j,'Você Ganhou !!! :(').
mens(n,'Deu Velha !!! :/').
resultado(P):- mens(P,M), new(@resultado, text(M)),send(@resultado,font, font(times, bold, 36)),send(@janela,display,@resultado,point(20,310)),retract(limpar(_)),assert(limpar(true)),retract(ganhador(_)),assert(ganhador(P)).

limpa:-limpar(true),free(@h1),free(@h2),free(@v1),free(@v2),free(@g00),free(@g01),free(@g02),free(@g10),free(@g11),free(@g12),free(@g20),free(@g21),free(@g22),free(@resultado),free(@janela),tabuleiro(K0,K1,K2,K3,K4,K5,K6,K7,K8),retract(tabuleiro(K0,K1,K2,K3,K4,K5,K6,K7,K8)),assert(tabuleiro(b,b,b,b,b,b,b,b,b)).
prestart:-new(@janela, picture('Jogo da Velha')),
          send(@janela,display,new(@h1,line(10,100,300,100))),
          send(@janela,display,new(@h2,line(10,200,300,200))),
          send(@janela,display,new(@v1,line(100,10,100,300))),
          send(@janela,display,new(@v2,line(200,10,200,300))),
          send(@janela,size,size(300,350)),
          send(@janela,scrollbars,none),
          send(@janela, display,new(@g00, box(100,100)), point(0,0)),
          send(@g00, pen,0),
          send(@g00, recogniser,click_gesture(left,'',single,message(@prolog, jogo,0,0))),
          send(@janela, display,new(@g01, box(100,100)), point(0,100)),
          send(@g01, pen,0),
          send(@g01, recogniser,click_gesture(left,'',single,message(@prolog, jogo,0,1))),
          send(@janela, display,new(@g02, box(100,100)), point(0,200)),
          send(@g02, pen,0),
          send(@g02, recogniser,click_gesture(left,'',single,message(@prolog, jogo,0,2))),
          send(@janela, display,new(@g10, box(100,100)), point(100,0)),
          send(@g10, pen,0),
          send(@g10, recogniser,click_gesture(left,'',single,message(@prolog, jogo,1,0))),
          send(@janela, display,new(@g11, box(100,100)), point(100,100)),
          send(@g11, pen,0),
          send(@g11, recogniser,click_gesture(left,'',single,message(@prolog, jogo,1,1))),
          send(@janela, display,new(@g12, box(100,100)), point(100,200)),
          send(@g12, pen,0),
          send(@g12, recogniser,click_gesture(left,'',single,message(@prolog, jogo,1,2))),
          send(@janela, display,new(@g20, box(100,100)), point(200,0)),
          send(@g20, pen,0),
          send(@g20, recogniser,click_gesture(left,'',single,message(@prolog, jogo,2,0))),
          send(@janela, display,new(@g21, box(100,100)), point(200,100)),
          send(@g21, pen,0),
          send(@g21, recogniser,click_gesture(left,'',single,message(@prolog, jogo,2,1))),
          send(@janela, display,new(@g22, box(100,100)), point(200,200)),
          send(@g22, pen,0),
          send(@g22, recogniser,click_gesture(left,'',single,message(@prolog, jogo,2,2))),
          send(@janela, open).
start(j):-limpa,prestart,retract(limpar(true)),assert(limpar(false)).
start(n):-start(j).
start(p):-start(j),jogue.
start(_).
restart:-ganhador(P),start(P).

r:-ganhador(p),limpa,prestart,jogue.
r:-limpa,prestart.

c:-tabuleiro(K0,K1,K2,K3,K4,K5,K6,K7,K8),write((K0,K1,K2,K3,K4,K5,K6,K7,K8)).

?-assert(ganhador(j)).

?-assert(limpar(false)).

?-assert(tabuleiro(b,b,b,b,b,b,b,b,b)).

?-prestart.

