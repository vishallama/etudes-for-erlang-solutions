%% @author Vishal Lama
%% @doc Functions for calculating basic statistics on a list of numbers.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(stats).
-export([minimum/1, maximum/1, range/1]).

%% @doc Returns the minimum item in a list of numbers. Fails when given an
%% empty list.

-spec(minimum(list(number())) -> number()).

minimum(List) -> minimum(List, hd(List)).

minimum([], Current) ->
  Current;
minimum([H|T], Current) when H < Current ->
  minimum(T, H);
minimum([_|T], Current) ->
  minimum(T, Current).

%% @doc Returns the maximum item in a list of numbers. Fails when given an
%% empty list.

-spec(maximum(list(number())) -> number()).

maximum(List) -> maximum(List, hd(List)).

maximum([], Current) ->
  Current;
maximum([H|T], Current) when H > Current ->
  maximum(T, H);
maximum([_|T], Current) ->
  maximum(T, Current).

%% @doc Returns the mininum and maximum items in a list of numbers in the
%% form [mininum, maximum]. Fails when given an empty list.

-spec(range(list(number())) -> list(number())).

range(List) ->
  Minimum = minimum(List),
  Maximum = maximum(List),
  [Minimum, Maximum].

