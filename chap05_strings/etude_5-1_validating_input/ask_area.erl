%% @author Vishal Lama
%% @doc Functions for calculating the areas of geometric shapes
%% based on dimensions provided by user
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(ask_area).
-export([area/0]).

%% @doc Requests a character for the name of a shape, numbers for its
%% dimensions, and calculates its corresponding area.
%% The characters are R for rectangle, T for triangles, and E for ellipse.
%% The characters may be in lowercase.

-spec(area() -> number()).

area() ->
  Answer = io:get_line("R)ectangle, T)riangle, or E)llipse > "),
  Shape = char_to_shape(hd(Answer)),
  Dimensions = case Shape of
                 rectangle ->
                   get_dimensions("width", "height");
                 triangle ->
                   get_dimensions("base", "height");
                 ellipse ->
                   get_dimensions("major axis", "minor axis");
                 unknown ->
                   {error, "Unknown shape " ++ [hd(Answer)]}
               end,

  Area = calculate(Shape, element(1, Dimensions), element(2, Dimensions)),
  Area.

%% @doc Given a character, returns an atom representing the corresponding
%% shape (or the atom unknown if a bad character is given.)

-spec(char_to_shape(char()) -> atom()).

char_to_shape(Char) ->
  LowerChar = string:to_lower(Char),

  case LowerChar of
    $r -> rectangle;
    $t -> triangle;
    $e -> ellipse;
    _   -> unknown
  end.

%% @doc Present a prompt, and get a number (integer or float) from the user.
%% If a number is not provided, present the prompt again.

-spec(get_number(string()) -> number()).

get_number(Prompt) ->
  Response = io:get_line("Enter " ++ Prompt ++ " > "),
  case string:to_float(Response) of
    {error, no_float} ->
      case string:to_integer(Response) of
        {error, no_integer} -> get_number(Prompt);
        {Value, _} -> Value
      end;
    {Value, _} -> Value
  end.

%% @doc Get the correct dimensions for a geometric shape. Inputs are the
%% two prompts, and output is a tuple {Dimension1, Dimension2}.

-spec(get_dimensions(string(), string()) -> {number(), number()}).

get_dimensions(Prompt1, Prompt2) ->
  N1 = get_number(Prompt1),
  N2 = get_number(Prompt2),
  {N1, N2}.

%% @doc Calculate area of a geometric shape, given its shape and dimensions.
%% TODO: Handle all errors appropriately.

calculate(Shape, A, B) ->
  case Shape of
    unknown -> io:format("Please enter R, T, or E. ~n");
    _ ->
      if
        is_number(A), is_number(B) -> geom:area(Shape, A, B);
        true -> io:format("Your dimensions must both be numbers ~n")
      end
  end.

