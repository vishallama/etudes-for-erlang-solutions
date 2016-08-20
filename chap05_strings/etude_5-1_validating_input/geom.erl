%% @author Vishal Lama
%% @doc Functions for calculating areas of geometric shapes
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(geom).
-export([area/3]).

%% @doc Calculates the area of a geometric shape, given the shape and two of
%% its dimensions. For a rectangle it returns the products of its arguments,
%% for a triangle one half the product of its arguments, and for an ellipse
%% math:pi() times the product of the arguments.

-spec(area(atom(), number(), number()) -> number()).

area(Shape, A, B) when A >= 0, B >= 0 ->
  case Shape of
    rectangle -> A * B;
    triangle -> A * B / 2.0;
    ellipse -> math:pi() * A * B
  end.

