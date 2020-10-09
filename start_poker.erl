-module(start_poker). 
-export([start/0]). 
-define(RULES, [four_of_kind, royal_flush, straight_flush, full_house,
         flush, straight,three_of_kind,two_pair,one_pair]).
-define(MOD, pokerHand).

% --------------------------------------------------

start() -> 
   {ok, File} = file:open("poker.txt",[read]),
   {ok, Txt} = file:read(File, 1024 * 1024), 
   NewList = re:split(Txt,(["\r\n"]),[{return,list}]),
   io:fwrite("~nPlayer1 win = ~p~n",[wining_count_player1(NewList, 0)]).

% ---------------------------------------------------

wining_count_player1([[]], Acc) -> Acc;

wining_count_player1([H|T], Acc) when H /= [] ->
    NewList = string:tokens(H, " "),
    Player1 = card:get_card(lists:sublist(NewList, 5), []),
    Player2 = card:get_card(lists:sublist(NewList, 6, 10), []),
    Hand_Type1 = get_Hand(?RULES, ?MOD,Player1),
    Hand_Type2 = get_Hand(?RULES, ?MOD, Player2),
    Player1_Hand = poker_hand_rank:get_val(Hand_Type1),
    Player2_Hand = poker_hand_rank:get_val(Hand_Type2),
    if Player1_Hand /= Player2_Hand ->
      Val = if Player1_Hand > Player2_Hand ->
               1;
            true ->
              0
            end;
    true -> 
        Is_player1_has_highCard = pokerHand:highest_card(Player1,Player2),
        Val = if Is_player1_has_highCard == true ->
                1;
              true -> 0
              end
    end,
   wining_count_player1(T, Acc + Val).

% -------------------------------------------------

get_Hand([], _,_) -> high_card;

get_Hand([H|T], Mod,P) ->
    V = get_Hand_type(H, Mod, P),
    if V /= false -> V;
    true -> get_Hand(T, Mod, P)
    end.

% ---------------------------------------------------
get_Hand_type(Rule, Mod, P) -> 
    case Rule of
      royal_flush ->
             Mod:royal_flush(P);

      straight_flush ->
           Mod:straight_flush(P);
      
      four_of_kind ->
           Mod:four_of_kind(P);
      
      full_house ->
          Mod:full_house(P);
      
      flush ->
         Mod:flush(P, false);

      straight ->
          Mod:straight(P, false);
      
      three_of_kind ->
         Mod:three_of_kind(P);

      two_pair -> 
           Mod:two_pair(P);

      one_pair -> 
        Mod:one_pair(P)
   end.
   