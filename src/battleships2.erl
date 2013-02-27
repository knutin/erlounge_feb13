-module(battleships2).
-include_lib("eunit/include/eunit.hrl").
-export([new/0, command/2, draw/1]).

-record(state, {ships = [], bombs = []}).

new() ->
    #state{}.

command({place_ship, _}, #state{ships = Ships})
  when length(Ships) > 10 ->
    throw(too_many_ships);

command({_, {X, Y}}, _) when X > 9 orelse Y > 9 orelse X < 0 orelse Y < 0 ->
    throw(coordinates_out_of_bounds);

command({place_ship, Coords}, #state{ships = Ships} = State) ->
    case lists:member(Coords, Ships) of
        true ->
            throw(ship_already_on_map);
        false ->
            State#state{ships = [Coords | Ships]}
    end;


command({fire, _}, #state{bombs = Bombs}) when length(Bombs) > 25 ->
    throw(out_of_bombs);

command({fire, Coords}, #state{bombs = Bombs} = State) ->
    State#state{bombs = [Coords | Bombs]}.


draw(State) ->
    EmptyGrid = list_to_tuple([erlang:make_tuple(10, " ") || _ <- lists:seq(0, 9)]),

    ShipGrid = lists:foldl(fun ({X, Y}, G) ->
                                   Row = erlang:element(Y, G),
                                   erlang:setelement(Y, G, erlang:setelement(X, Row, "S"))
                           end, EmptyGrid, State#state.ships),
    lists:foldl(fun ({X, Y}, G) ->
                        Row = erlang:element(Y, G),
                        Char = case erlang:element(X, Row) of
                            "S" -> "X";
                            " " -> "~"
                        end,
                        erlang:setelement(Y, G, erlang:setelement(X, Row, Char))
                end, ShipGrid, State#state.bombs).


hits(State) ->
    ordsets:size(
      ordsets:intersection(
        ordsets:from_list(State#state.bombs),
        ordsets:from_list(State#state.ships))).


%%
%% TESTS
%%

do_apply(Commands, State) ->
    lists:foldl(fun command/2, State, Commands).


round_test() ->
    Commands = [{place_ship, {5, 5}},
                {place_ship, {1, 1}},
                {fire, {5, 5}},
                {fire, {6, 6}}],

    Grid = lists:map(fun (Row) -> tuple_to_list(Row) ++ "\n" end,
                     tuple_to_list(draw(do_apply(Commands, new())))),
    error_logger:info_msg(Grid),
    
    ?assertEqual(1, hits(do_apply(Commands, new()))).
       
