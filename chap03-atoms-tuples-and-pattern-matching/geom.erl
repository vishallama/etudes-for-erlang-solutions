%% @author Vishal Lama
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(geom).
-export([area/1]).

%% @doc Calculates the area of a geometric shape, given a tuple containing
%% a shape and two of its dimensions.
%% Calls a private function.

-spec(area({atom(), number(), number()}) -> number()).

area({Shape, Dimension1, Dimension2}) ->
  area(Shape, Dimension1, Dimension2).

%% @doc Calculates the area of a geometric shape, given the shape and its
%% two dimensions. For a rectangle shape, it returns the product of its
%% arguments, for a triangle one half the product of its arguments, and for
%% an ellipse math:pi() times the product of its arguments.

area(rectangle, L, W) when L >=0, W >= 0 ->
  L * W;
area(triangle, B, H) when B >=0, H >= 0 ->
  B * H / 2.0;
area(ellipse, A, B) when A >=0, B >= 0 ->
  math:pi() * A * B;
area(_, _, _) ->
  0.

