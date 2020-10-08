-module(card).
-export([get_card/2]).

% get_card() will return a hand 

get_card([H|[]], Card) ->
   Binary_card = erlang:list_to_binary(H),
   Rank = get_rank(binary_part(Binary_card,{0,1})),
   Suit = get_suit(binary_part(Binary_card,{1,1})),
   lists:reverse(lists:sort(Card ++ [[Rank, Suit]]));

get_card([H|T], Card) ->
   Binary_card = erlang:list_to_binary(H),
   Rank = get_rank(binary_part(Binary_card,{0,1})),
   Suit = get_suit(binary_part(Binary_card,{1,1})),
   get_card(T, Card ++ [[Rank, Suit]]).

get_rank(Rank) ->
   case Rank of 
     <<"A">> -> 14;
     <<"K">> -> 13;
     <<"Q">> -> 12; 
     <<"J">> -> 11;
     <<"T">> -> 10;
      _ ->  erlang:binary_to_integer(Rank)
   end.

get_suit(Suit) ->
   case Suit of 
     <<"C">> -> 1; 
     <<"D">> -> 2;
     <<"H">> -> 3;
     <<"S">> -> 4 
   end.