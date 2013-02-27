-module(battleships).
-include_lib("eunit/include/eunit.hrl").
-export([new/0, command/2, draw/1]).

-record(state, {size, ships, bombs}).

new() ->
    #state{size = 10, ships = [], bombs = []}.

command({place_ship, Coords}, #state{ships = Ships} = State) ->
    State#state{ships = lists:keystore(Coords, 1, Ships, {Coords})};

command({fire, Coords}, #state{bombs = Bombs} = State) ->
    State#state{bombs = lists:keystore(Coords, 1, Bombs, {Coords})}.


hits(State) ->
    ordsets:size(
      ordsets:intersection(
        ordsets:from_list(State#state.ships),
        ordsets:from_list(State#state.bombs))).















draw(State) ->
    lists:flatten(
      lists:map(
        fun (X) ->
                lists:map(
                  fun (Y) ->
                          case lists:keymember({X, Y}, 1, State#state.ships) of
                              true ->
                                  case lists:keymember({X, Y}, 1, State#state.bombs) of
                                      true ->
                                          "X";
                                      false ->
                                          "S"
                                  end;
                              false ->
                                  case lists:keymember({X, Y}, 1, State#state.bombs) of
                                      true ->
                                          "O";
                                      false ->
                                          " "
                                  end
                          end
                  end, lists:seq(1, State#state.size)) ++ "~n"
        end, lists:seq(1, State#state.size))).


draw_fun(State) ->
    Symbol = fun (X, Y) ->
                     Ship = lists:keymember({X, Y}, 1, State#state.ships),
                     Bomb = lists:keymember({X, Y}, 1, State#state.bombs),

                     if
                         Ship andalso Bomb ->
                             "X";
                         Ship ->
                             "S";
                         Bomb ->
                             "O";
                         Y =:= State#state.size ->
                             " ~n";
                         true ->
                             " "
                     end
             end,

    lists:flatten(
      [Symbol(X, Y) || X <- lists:seq(1, State#state.size),
                       Y <- lists:seq(1, State#state.size)]).
    
                     


do_apply(Commands, State) ->
    lists:foldl(fun (Command, S) ->
                        command(Command, S)
                end, State, Commands).





%%
%% TESTS
%%

sample_commands() ->
    [
     {place_ship, {1, 1}},
     {place_ship, {1, 10}},
     {place_ship, {5, 5}},
     {place_ship, {10, 10}},
     {place_ship, {10, 1}},
     {fire, {7, 3}},
     {fire, {7, 9}},
     {fire, {5, 5}}
    ].


hit_test() ->
    ?assertEqual(1, hits(do_apply([{place_ship, {1, 1}},
                                   {place_ship, {1, 2}},
                                   {fire, {1, 1}}], new()))).

draw_test() ->
    error_logger:info_msg(draw(do_apply(sample_commands(), new()))).

draw_fun_test() ->
    error_logger:info_msg(draw_fun(do_apply(sample_commands(), new()))).



