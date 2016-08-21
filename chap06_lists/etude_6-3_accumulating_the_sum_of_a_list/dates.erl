%% @author Vishal Lama
%% @doc Functions for splitting a date into a list of year, month, and day.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(dates).
-export([date_parts/1, julian/1, is_leap_year/1]).

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

%% @doc Takes a string in ISO date format (yyyy-mm-dd) and returns the day
%% of the year (Julian date).

-spec(julian(string()) -> integer()).

julian(DateStr) ->
  DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
  [Year, Month, Day] = date_parts(DateStr),
  julian(Year, Month, Day, DaysPerMonth, 0).

%% @doc Helper function that recursively accumulates the number of days up
%% to the specified date.

-spec(julian(
        integer(),
        integer(),
        integer(),
        list(integer()),
        integer()) -> integer()).

julian(Y, M, D, DaysPerMonth, Days) when M > (13 - length(DaysPerMonth)) ->
  julian(Y, M, D, tl(DaysPerMonth), Days + hd(DaysPerMonth));

julian(Y, M, D, _, Days) ->
  case M > 2 andalso is_leap_year(Y) of
    true -> Days + D + 1;
    false -> Days + D
  end.

%% @doc Given a year, returns true if it is a leap year, false otherwise.

-spec(is_leap_year(integer()) -> boolean()).

is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0).

