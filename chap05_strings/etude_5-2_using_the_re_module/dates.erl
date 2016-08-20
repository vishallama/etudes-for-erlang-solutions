%% @author Vishal Lama
%% @doc Functions for splitting a date into a list of year, month, and day.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(dates).
-export([date_parts/1]).

%% @doc Takes a string in ISO date format (yyyy-mm-dd), and returns a list
%% of integers in the form [year, month, day].

-spec(date_parts(string()) -> list()).

date_parts(DateStr) ->
  [YearStr, MonthStr, DayStr] = re:split(DateStr, "-", [{return, list}]),
  [
   element(1, string:to_integer(YearStr)),
   element(1, string:to_integer(MonthStr)),
   element(1, string:to_integer(DayStr))
  ].

