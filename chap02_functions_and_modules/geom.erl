%% @author Vishal Lama
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2016 by Vishal Lama
%% @version 0.1

-module(geom).
-export([area/2]).

%% @doc Calculates the area of a rectangle, given its length and width.
%% Returns the product of length and width.

-spec(area(number(), number()) -> number()).

area(Length, Width) -> Length * Width.

