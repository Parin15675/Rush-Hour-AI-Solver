

:- module(logic, [
    valid_move/4,
    within_board/4,
    blocked/5,
    prolog_a_star/4
]).

:- use_module(library(heaps)).
:- use_module(library(lists)).

grid_size(6).

valid_direction(h, left).
valid_direction(h, right).
valid_direction(v, up).
valid_direction(v, down).

direction_delta(left, -1, 0).
direction_delta(right, 1, 0).
direction_delta(up, 0, -1).
direction_delta(down, 0, 1).

within_board(X, Y, Len, h) :-
    grid_size(Size),
    X >= 0,
    Y >= 0,
    X + Len =< Size,
    Y < Size.
within_board(X, Y, Len, v) :-
    grid_size(Size),
    X >= 0,
    Y >= 0,
    X < Size,
    Y + Len =< Size.

car_occupies(car(_, X, Y, h, Len), CellX, CellY) :-
    between(0, Len - 1, Offset),
    CellX is X + Offset,
    CellY is Y.
car_occupies(car(_, X, Y, v, Len), CellX, CellY) :-
    between(0, Len - 1, Offset),
    CellX is X,
    CellY is Y + Offset.

blocked(State, CarId, Dir, X, Y) :-
    member(car(CarId, X0, Y0, Orient, Len), State),
    direction_delta(Dir, DX, DY),
    NX is X0 + DX,
    NY is Y0 + DY,
    \+ can_place(State, car(CarId, X, Y, Orient, Len), NX, NY).

can_place(State, car(CarId, _, _, Orient, Len), NX, NY) :-
    within_board(NX, NY, Len, Orient),
    \+ collision(State, CarId, Orient, Len, NX, NY).

collision(State, CarId, Orient, Len, NX, NY) :-
    member(car(Other, OX, OY, OOrient, OLen), State),
    Other \= CarId,
    car_occupies(car(Other, OX, OY, OOrient, OLen), CellX, CellY),
    car_occupies(car(CarId, NX, NY, Orient, Len), CellX, CellY).

replace_car([], _, _, []).
replace_car([car(CarId, _, _, _, _) | Tail], CarId, NewCar, [NewCar | Tail]) :- !.
replace_car([Head | Tail], CarId, NewCar, [Head | Updated]) :-
    replace_car(Tail, CarId, NewCar, Updated).

max_slide(State, car(CarId, X, Y, Orient, Len), Dir, MaxSteps) :-
    direction_delta(Dir, DX, DY),
    find_max(State, car(CarId, X, Y, Orient, Len), DX, DY, 1, 0, MaxSteps).

find_max(State, car(CarId, X, Y, Orient, Len), DX, DY, Step, Acc, Max) :-
    NX is X + DX * Step,
    NY is Y + DY * Step,
    can_place(State, car(CarId, X, Y, Orient, Len), NX, NY),
    NextStep is Step + 1,
    find_max(State, car(CarId, X, Y, Orient, Len), DX, DY, NextStep, Step, Max).
find_max(_, _, _, _, _, Max, Max).

valid_move(State, CarId, Dir, NewStateSorted) :-
    member(car(CarId, X, Y, Orient, Len), State),
    valid_direction(Orient, Dir),
    max_slide(State, car(CarId, X, Y, Orient, Len), Dir, Max),
    Max > 0,
    between(1, Max, Step),
    direction_delta(Dir, DX, DY),
    NX is X + DX * Step,
    NY is Y + DY * Step,
    replace_car(State, CarId, car(CarId, NX, NY, Orient, Len), TempState),
    sort(TempState, NewStateSorted).

prolog_a_star(State, Path, Cost, NodesExpanded) :-
    sort(State, SortedState),
    heuristic(SortedState, H),
    empty_heap(Empty),
    add_to_heap(Empty, H, node(SortedState, [], 0), OpenHeap),
    a_star_loop(OpenHeap, [], [state_cost(SortedState, 0)], 0, RevPath, RawCost, NodesExpanded),
    reverse(RevPath, Path),
    normalize_cost(RawCost, Cost).

normalize_cost(inf, inf).
normalize_cost(Value, Value) :-
    Value \= inf.

a_star_loop(OpenHeap, _, _, NodesAcc, [], inf, NodesAcc) :-
    heap_size(OpenHeap, Size),
    Size =:= 0,
    !.
a_star_loop(OpenHeap, Closed, Costs, NodesAcc, Path, Cost, NodesExpanded) :-
    get_from_heap(OpenHeap, _, node(State, PathSoFar, GCost), RestHeap),
    (   state_in_set(State, Closed)
    ->  a_star_loop(RestHeap, Closed, Costs, NodesAcc, Path, Cost, NodesExpanded)
    ;   goal_state(State)
    ->  Path = PathSoFar,
        Cost = GCost,
        NodesExpanded = NodesAcc
    ;   NodesNext is NodesAcc + 1,
        ClosedNext = [State|Closed],
        findall(move(CarId, Dir, NewState),
                valid_move(State, CarId, Dir, NewState),
                Moves),
        process_moves(Moves, GCost, PathSoFar, Costs, RestHeap, OpenNext, CostsNext),
        a_star_loop(OpenNext, ClosedNext, CostsNext, NodesNext, Path, Cost, NodesExpanded)
    ).

process_moves([], _, _, Costs, OpenHeap, OpenHeap, Costs).
process_moves([move(CarId, Dir, NewState)|Rest], GCost, PathSoFar, CostsIn, OpenIn, OpenOut, CostsOut) :-
    heuristic(NewState, HCost),
    NewG is GCost + 1,
    (   state_cost(NewState, CostsIn, ExistingG),
        ExistingG =< NewG
    ->  process_moves(Rest, GCost, PathSoFar, CostsIn, OpenIn, OpenOut, CostsOut)
    ;   Priority is NewG + HCost,
        NewPath = [move(CarId, Dir, NewState)|PathSoFar],
        add_to_heap(OpenIn, Priority, node(NewState, NewPath, NewG), OpenTemp),
        update_costs(NewState, NewG, CostsIn, CostsTemp),
        process_moves(Rest, GCost, PathSoFar, CostsTemp, OpenTemp, OpenOut, CostsOut)
    ).

heuristic(State, Heuristic) :-
    (   red_car(State, car(r, X, Y, h, Len))
    ->  grid_size(Size),
        Distance is Size - (X + Len),
        StartColumn is X + Len,
        blocking_cars(State, Y, StartColumn, Blocking),
        Heuristic is Distance + Blocking
    ;   red_car(State, _)
    ->  grid_size(Size),
        Heuristic is Size * Size
    ;   Heuristic = 0
    ).

goal_state(State) :-
    red_car(State, car(r, X, _, h, Len)),
    grid_size(Size),
    X + Len >= Size.

red_car(State, car(r, X, Y, Orient, Len)) :-
    member(car(r, X, Y, Orient, Len), State),
    !.

blocking_cars(State, Row, StartColumn, Count) :-
    findall(Id,
            (   member(car(Id, X, Y, Orient, Len), State),
                Id \= r,
                blocks_exit(car(Id, X, Y, Orient, Len), Row, StartColumn)
            ),
            Blockers),
    sort(Blockers, Unique),
    length(Unique, Count).

blocks_exit(car(_, X, Y, h, Len), Row, StartColumn) :-
    Y =:= Row,
    X + Len > StartColumn.
blocks_exit(car(_, X, Y, v, Len), Row, StartColumn) :-
    X >= StartColumn,
    Y =< Row,
    Row < Y + Len.

state_in_set(State, [Head|_]) :-
    State == Head,
    !.
state_in_set(State, [_|Tail]) :-
    state_in_set(State, Tail).

state_cost(State, [state_cost(Key, Cost)|_], Cost) :-
    State == Key,
    !.
state_cost(State, [_|Tail], Cost) :-
    state_cost(State, Tail, Cost).

update_costs(State, Cost, CostsIn, [state_cost(State, Cost)|CostsFiltered]) :-
    remove_cost(State, CostsIn, CostsFiltered).

remove_cost(State, [], []) :-
    !.
remove_cost(State, [state_cost(Key, _)|Tail], Tail) :-
    State == Key,
    !.
remove_cost(State, [Head|Tail], [Head|Filtered]) :-
    remove_cost(State, Tail, Filtered).
