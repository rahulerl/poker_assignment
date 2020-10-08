-module(poker_hand_rank).
-export([get_val/1]).

get_val(PokerHand) ->

  case PokerHand of 
     royal_flush -> 100;
     straight_flush -> 90;
     four_of_kind -> 80;
     full_house -> 70;
     flush -> 60;
     straight -> 50;
     three_of_kind -> 40;
     two_pair -> 30;
     one_pair -> 20;
     high_card -> 10
  end.