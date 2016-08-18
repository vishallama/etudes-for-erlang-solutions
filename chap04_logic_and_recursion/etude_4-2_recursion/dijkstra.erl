%% @author Vishal Lama
%% @doc Recursive function for calculating the GCD of any two non-negative
%% integers using Djikstra's algorithm.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(dijkstra).
-export([gcd/2]).

%% @doc Calculates the greatest common divisor (GCD) of two non-negative
%% integers. Uses Djikstra's algorithm, which does not require any division.

-spec(gcd(integer(), integer()) -> integer()).

gcd(M, N) when M >= 0, N >= 0 ->
  if
    M == 0 -> N;
    N == 0 -> M;
    M == N -> M;
    M > N -> gcd(M-N, N);
    M < N -> gcd(M, N-M)
  end.

