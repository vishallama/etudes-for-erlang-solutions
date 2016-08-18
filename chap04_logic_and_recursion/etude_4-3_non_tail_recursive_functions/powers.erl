%% @author Vishal Lama
%% @doc Functions for raising a number to an integer power and finding the
%% Nth root of a number using Newton's method.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(powers).
-export([raise/2]).

%% @doc Raise a number X to an integer power N.
%% Any number to the power of 0 equals 1.
%% Any number to the power of 1 equals that number itself.
%% When N is positive, X^N is equal to X times X^(N-1).
%% When N is negative, X^N is equal to 1.0 / X^(-N).

-spec(raise(number(), integer()) -> number()).

raise(X, N) ->
  if
    N == 0 -> 1;
    N == 1 -> X;
    N > 0 -> X * raise(X, N-1);
    N < 0 -> 1.0 / raise(X, -N)
  end.

