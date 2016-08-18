%% @author Vishal Lama
%% @doc Functions for raising a number to an integer power and finding the
%% Nth root of a number using Newton's method.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(powers).
-export([raise/2, nth_root/2]).

%% @doc Raise a number X to an integer power N.
%% Any number to the power of 0 equals 1.
%% Any number to the power of 1 equals that number itself.
%% When N is positive, X^N is equal to X times X^(N-1).
%% When N is negative, X^N is equal to 1.0 / X^(-N).

-spec(raise(number(), integer()) -> number()).

raise(_, 0) -> 1;
raise(X, N) when N < 0 -> 1.0 / raise(X, -N);
raise(X, N) when N > 0 -> raise(X, N, 1).

raise(_, 0, Accumulator) -> Accumulator;
raise(X, N, Accumulator) -> raise(X, N-1, X * Accumulator).


%% @doc Find the nth root of a given number.

-spec(nth_root(number(), integer()) -> number()).

nth_root(X, N) -> nth_root(X, N, X / 2.0).

%% @doc Helper function to find an nth_root by passing an approximation
%% from one call to the next.
%% If the difference between current and next approximation is less than
%% 1.0e-8, return the next approximation; otherwise, return
%% nth_root(X, N, NextApproximation).

nth_root(X, N, A) ->
  io:format("Current guess is: ~w ~n", [A]),

  F = raise(A, N) - X,
  Fprime = N * raise(A, N-1),
  Next = A - F / Fprime,
  Change = abs(Next - A),

  if
    Change < 1.0e-8 -> Next;
    true -> nth_root(X, N, Next)
  end.

