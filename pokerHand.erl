-module(pokerHand).
-export([straight/2, flush/2, straight_flush/1, highest_card/2,
         royal_flush/1, duplicates/2, four_of_kind/1, full_house/1,
         three_of_kind/1, two_pair/1, one_pair/1]).


% ---------------------------------------------

royal_flush(Card) ->
    Is_Ace = lists:nth(1,lists:nth(1, Card)),
    Is_straight = straight(Card, false),
    Is_flush = flush(Card, false),
    if (Is_Ace == 14) and (Is_straight == true) and (Is_flush == true) -> royal_flush;
    true -> false
    end.

% -----------------------------------------------------

straight_flush(Card) ->
     Is_straight = straight(Card, false),
     Is_flush = flush(Card, false),
     if (Is_flush == true) and (Is_straight == true) -> straight_flush;
     true -> false
     end.

% -----------------------------------------------------

four_of_kind(Card) ->
     Duplicates = duplicates(Card, #{}),
     Check_dup = check_dup(maps:to_list(Duplicates), 4),
     if Check_dup == true -> four_of_kind;
     true -> false
     end.

% ------------------------------------------------------

three_of_kind(Card) ->
     Duplicates = duplicates(Card, #{}),
     Check_three = check_dup(maps:to_list(Duplicates), 3),
     Check_one = check_dup(maps:to_list(Duplicates), 1),
     if (Check_three == true) and (Check_one == true) -> three_of_kind;
     true -> false
    end.
% -------------------------------------------------------

two_pair(Card) ->
     Duplicates = duplicates(Card, #{}),
     Check_pair = check_pair(maps:to_list(Duplicates), 2, 0),
     if Check_pair == true -> two_pair;
     true -> false
    end.
% ------------------------------------------------------

one_pair(Card) ->
     Duplicates = duplicates(Card, #{}),
     Check_pair = check_pair(maps:to_list(Duplicates), 1, 0),
     if Check_pair == true -> one_pair;
     true -> false
    end.

% --------------------------------------------------------

full_house(Card) ->
    Duplicates = duplicates(Card, #{}),
    Check_for_three = check_dup(maps:to_list(Duplicates), 3),
    Check_for_two = check_dup(maps:to_list(Duplicates), 2),
    if (Check_for_three == true) and (Check_for_two == true) -> full_house;
    true -> false
    end.
 
% ---------------------------------------------------------------

straight([H|T], Last_card) when Last_card == false ->
    straight(T, lists:nth(1, H));

straight([], _) -> straight;

straight([H|T], Last_card) ->
      Temp = lists:nth(1, H) + 1,
      if Last_card == Temp ->
          straight(T, lists:nth(1, H));
      true -> false
      end.

% ---------------------------------------------------------------

flush([H|T], Last_card) when Last_card == false ->
    flush(T, lists:nth(2, H));

flush([], _) -> flush;

flush([H|T], Last_card) ->
      Temp = lists:nth(2, H),
      if Last_card == Temp ->
          flush(T, lists:nth(2, H));
      true -> false
      end.

% ---------------------------------------------------------------

highest_card([], []) -> tie;

highest_card([P1_H|P1_T], [P2_H|P2_T]) ->
        P1_Rank = lists:nth(1, P1_H),
        P2_Rank = lists:nth(1, P2_H),
        if P1_Rank /= P2_Rank ->
                if P1_Rank > P2_Rank -> true;
                true -> false
                end;
        true -> highest_card(P1_T, P2_T)
        end.

% ----------------------------------------------------------------

duplicates([], Acc) -> Acc;

duplicates([H|T], Acc) ->
     Key = lists:nth(1,H),
     Val = maps:get(Key,Acc, 0),
    if Val == 0 ->
          duplicates(T, maps:put(Key, 1, Acc));
    true -> 
        NewVal = Val + 1, 
        duplicates(T, maps:put(Key, NewVal, Acc))
    end.

% ------------------------------------------------------------

check_dup([], _) -> false;

check_dup([H|T], N) ->
   {_, Val} = H,
   if Val == N -> true;
   true -> check_dup(T, N)
   end.
% -----------------------------------------------------------------

check_pair([], Max, N) when N == Max-> true;

check_pair([], Max, N) when N /= Max-> false;

check_pair([H|T], Max, N) ->
   {_, Val} = H,
   if Val == 2 -> check_pair(T, Max, N + 1);
   true -> check_pair(T, Max, N)
   end.