

:- module(logic, [
    valid_move/5,
    within_board/4,
    blocked/5,
    prolog_a_star/4,
    configure_board/5,
    heuristic/2,
    goal_state/1
]).

:- set_prolog_stack(global, limit(512_000_000)).
:- set_prolog_stack(local,  limit(128_000_000)).
:- set_prolog_stack(trail,  limit(128_000_000)).

row_penalty_weight(3).
blocking_penalty_weight(2).

:- use_module(library(heaps)).
:- use_module(library(lists)).
:- use_module(library(assoc)).

:- dynamic grid_size/2.
:- dynamic exit_row/1.
:- dynamic exit_column/1.
:- dynamic exit_any_row/1.
:- dynamic pole/2.

grid_size(6, 6).
exit_row(2).
exit_column(6).
exit_any_row(false).

configure_board(Width, Height, ExitSpec, ExitCol, Poles) :-
    retractall(grid_size(_, _)),
    assertz(grid_size(Width, Height)),
    retractall(exit_row(_)),
    retractall(exit_column(_)),
    retractall(exit_any_row(_)),
    assertz(exit_column(ExitCol)),
    assert_exit_rows(ExitSpec),
    retractall(pole(_, _)),
    assert_poles(Poles).

assert_exit_rows(any) :-
    assertz(exit_any_row(true)).
assert_exit_rows(List) :-
    is_list(List),
    assertz(exit_any_row(false)),
    assert_exit_row_list(List).
assert_exit_rows(Row) :-
    number(Row),
    assertz(exit_any_row(false)),
    assertz(exit_row(Row)).

assert_exit_row_list([]) :-
    assertz(exit_row(0)).
assert_exit_row_list([Row | Rest]) :-
    assertz(exit_row(Row)),
    assert_exit_row_list(Rest).

assert_poles([]).
assert_poles([pole(X, Y) | Rest]) :-
    assertz(pole(X, Y)),
    assert_poles(Rest).
assert_poles([X-Y | Rest]) :-
    assertz(pole(X, Y)),
    assert_poles(Rest).

heading_axis(n, v).
heading_axis(s, v).
heading_axis(e, h).
heading_axis(w, h).

valid_direction(Heading, left) :-
    heading_axis(Heading, h).
valid_direction(Heading, right) :-
    heading_axis(Heading, h).
valid_direction(Heading, up) :-
    heading_axis(Heading, v).
valid_direction(Heading, down) :-
    heading_axis(Heading, v).

direction_delta(left, -1, 0).
direction_delta(right, 1, 0).
direction_delta(up, 0, -1).
direction_delta(down, 0, 1).

within_board(X, Y, Len, Heading) :-
    heading_axis(Heading, h),
    grid_size(Width, Height),
    X >= 0,
    Y >= 0,
    X + Len =< Width,
    Y < Height.
within_board(X, Y, Len, Heading) :-
    heading_axis(Heading, v),
    grid_size(Width, Height),
    X >= 0,
    Y >= 0,
    X < Width,
    Y + Len =< Height.

cell_within_board(X, Y) :-
    grid_size(Width, Height),
    X >= 0,
    X < Width,
    Y >= 0,
    Y < Height.

car_occupies(car(_, X, Y, Heading, Len), CellX, CellY) :-
    heading_axis(Heading, h),
    Upper is Len - 1,
    between(0, Upper, Offset),
    CellX is X + Offset,
    CellY is Y.
car_occupies(car(_, X, Y, Heading, Len), CellX, CellY) :-
    heading_axis(Heading, v),
    Upper is Len - 1,
    between(0, Upper, Offset),
    CellX is X,
    CellY is Y + Offset.

blocked(State, CarId, Dir, X, Y) :-
    member(car(CarId, X0, Y0, Heading, Len), State),
    direction_delta(Dir, DX, DY),
    NX is X0 + DX,
    NY is Y0 + DY,
    \+ can_place(State, car(CarId, X, Y, Heading, Len), NX, NY).

can_place(State, car(CarId, _, _, Heading, Len), NX, NY) :-
    within_board(NX, NY, Len, Heading),
    \+ collision(State, CarId, Heading, Len, NX, NY).

collision(_, _, Heading, Len, NX, NY) :-
    car_occupies(car(_, NX, NY, Heading, Len), CellX, CellY),
    pole(CellX, CellY).
collision(State, CarId, Heading, Len, NX, NY) :-
    member(car(Other, OX, OY, OHeading, OLen), State),
    Other \= CarId,
    car_occupies(car(Other, OX, OY, OHeading, OLen), CellX, CellY),
    car_occupies(car(CarId, NX, NY, Heading, Len), CellX, CellY).

replace_car([], _, _, []).
replace_car([car(CarId, _, _, _, _) | Tail], CarId, NewCar, [NewCar | Tail]) :- !.
replace_car([Head | Tail], CarId, NewCar, [Head | Updated]) :-
    replace_car(Tail, CarId, NewCar, Updated).

max_slide(State, car(CarId, X, Y, Heading, Len), Dir, MaxSteps) :-
    direction_delta(Dir, DX, DY),
    find_max(State, car(CarId, X, Y, Heading, Len), DX, DY, 1, 0, MaxSteps).

find_max(State, car(CarId, X, Y, Heading, Len), DX, DY, Step, _Acc, Max) :-
    NX is X + DX * Step,
    NY is Y + DY * Step,
    can_place(State, car(CarId, X, Y, Heading, Len), NX, NY),
    NextStep is Step + 1,
    find_max(State, car(CarId, X, Y, Heading, Len), DX, DY, NextStep, Step, Max).
find_max(_, _, _, _, _, Max, Max).

center_times_two(car(_, X, Y, Heading, Len), CenterX2, CenterY2) :-
    heading_axis(Heading, h),
    CenterX2 is X * 2 + Len - 1,
    CenterY2 is Y * 2.
center_times_two(car(_, X, Y, Heading, Len), CenterX2, CenterY2) :-
    heading_axis(Heading, v),
    CenterX2 is X * 2,
    CenterY2 is Y * 2 + Len - 1.

anchor_from_center(CenterX2, CenterY2, Heading, Len, X, Y) :-
    heading_axis(Heading, h),
    X is (CenterX2 - (Len - 1) + 1) // 2,
    Y is (CenterY2 + 1) // 2.
anchor_from_center(CenterX2, CenterY2, Heading, Len, X, Y) :-
    heading_axis(Heading, v),
    X is (CenterX2 + 1) // 2,
    Y is (CenterY2 - (Len - 1) + 1) // 2.

front_position(e, X, Y, Len, FrontX, Y) :-
    FrontX is X + Len - 1.
front_position(w, X, Y, _, X, Y).
front_position(n, X, Y, _, X, Y).
front_position(s, X, Y, Len, X, FrontY) :-
    FrontY is Y + Len - 1.

anchor_from_front(e, FrontX, FrontY, Len, NX, FrontY) :-
    NX is FrontX - (Len - 1).
anchor_from_front(w, FrontX, FrontY, _, FrontX, FrontY).
anchor_from_front(n, FrontX, FrontY, _, FrontX, FrontY).
anchor_from_front(s, FrontX, FrontY, Len, FrontX, NY) :-
    NY is FrontY - (Len - 1).

forward_delta(e, 1, 0).
forward_delta(w, -1, 0).
forward_delta(n, 0, -1).
forward_delta(s, 0, 1).

min_three(A, B, C, Min) :-
    Temp is min(A, B),
    Min is min(Temp, C).

max_three(A, B, C, Max) :-
    Temp is max(A, B),
    Max is max(Temp, C).

occupied_cell(State, X, Y, CarId) :-
    member(car(CarId, OX, OY, Heading, Len), State),
    car_occupies(car(CarId, OX, OY, Heading, Len), X, Y).

front_cell_free(State, CarId, X, Y) :-
    cell_within_board(X, Y),
    \+ pole(X, Y),
    (   occupied_cell(State, X, Y, Other)
    ->  Other = CarId
    ;   true
    ).

rotation_direction(n, rotate_cw, e).
rotation_direction(e, rotate_cw, s).
rotation_direction(s, rotate_cw, w).
rotation_direction(w, rotate_cw, n).
rotation_direction(n, rotate_ccw, w).
rotation_direction(w, rotate_ccw, s).
rotation_direction(s, rotate_ccw, e).
rotation_direction(e, rotate_ccw, n).

slide_move(State, CarId, Dir, Step, NewState) :-
    member(car(CarId, X, Y, Heading, Len), State),
    valid_direction(Heading, Dir),
    max_slide(State, car(CarId, X, Y, Heading, Len), Dir, Max),
    Max > 0,
    Step is 1,
    direction_delta(Dir, DX, DY),
    NX is X + DX * Step,
    NY is Y + DY * Step,
    replace_car(State, CarId, car(CarId, NX, NY, Heading, Len), NewState).

rotation_move(State, CarId, Dir, NewState) :-
    CarId = r,
    member(car(CarId, X, Y, Heading, Len), State),
    rotation_direction(Heading, Dir, NewHeading),
    forward_delta(NewHeading, FX, FY),
    front_position(Heading, X, Y, Len, FrontX0, FrontY0),
    FrontX is FrontX0 + FX,
    FrontY is FrontY0 + FY,
    front_cell_free(State, CarId, FrontX, FrontY),
    anchor_from_front(NewHeading, FrontX, FrontY, Len, NX, NY),
    within_board(NX, NY, Len, NewHeading),
    \+ collision(State, CarId, NewHeading, Len, NX, NY),
    replace_car(State, CarId, car(CarId, NX, NY, NewHeading, Len), NewState).

valid_move(State, CarId, Dir, Step, NewState) :-
    slide_move(State, CarId, Dir, Step, NewState).
valid_move(State, CarId, Dir, 1, NewState) :-
    rotation_move(State, CarId, Dir, NewState).

prolog_a_star(State, Path, Cost, NodesExpanded) :-
    sort(State, SortedState),
    heuristic(SortedState, H),
    empty_heap(Empty),
    add_to_heap(Empty, H, node(SortedState, [], 0), OpenHeap),
    empty_assoc(Closed0),
    empty_assoc(Costs0),
    state_signature(SortedState, Key),
    put_assoc(Key, Costs0, 0, Costs),
    a_star_loop(OpenHeap, Closed0, Costs, 0, RevPath, RawCost, NodesExpanded),
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
    state_signature(State, Key),
    (   get_assoc(Key, Closed, _)
    ->  a_star_loop(RestHeap, Closed, Costs, NodesAcc, Path, Cost, NodesExpanded)
    ;   goal_state(State)
    ->  Path = PathSoFar,
        Cost = GCost,
        NodesExpanded = NodesAcc
    ;   NodesNext is NodesAcc + 1,
        put_assoc(Key, Closed, true, ClosedNext),
        findall(move(CarId, Dir, Step, NewState),
                valid_move(State, CarId, Dir, Step, NewState),
                Moves),
        process_moves(Moves, GCost, PathSoFar, Costs, RestHeap, OpenNext, CostsNext),
        a_star_loop(OpenNext, ClosedNext, CostsNext, NodesNext, Path, Cost, NodesExpanded)
    ).

process_moves([], _, _, Costs, OpenHeap, OpenHeap, Costs).
process_moves([move(CarId, Dir, Step, NewState)|Rest], GCost, PathSoFar, CostsIn, OpenIn, OpenOut, CostsOut) :-
    heuristic(NewState, HCost),
    NewG is GCost + Step,
    state_signature(NewState, Key),
    (   get_assoc(Key, CostsIn, ExistingG),
        ExistingG =< NewG
    ->  process_moves(Rest, GCost, PathSoFar, CostsIn, OpenIn, OpenOut, CostsOut)
    ;   Priority is NewG + HCost,
        NewPath = [move(CarId, Dir, Step, NewState)|PathSoFar],
        add_to_heap(OpenIn, Priority, node(NewState, NewPath, NewG), OpenTemp),
        put_assoc(Key, CostsIn, NewG, CostsTemp),
        process_moves(Rest, GCost, PathSoFar, CostsTemp, OpenTemp, OpenOut, CostsOut)
    ).

heuristic(State, Heuristic) :-
    (   red_car(State, car(r, X, Y, Heading, Len))
    ->  (   heading_axis(Heading, h)
        ->  exit_column(ExitCol),
            RawDistance is ExitCol - (X + Len),
            (   RawDistance > 0 -> Distance is RawDistance ; Distance is 0 ),
            exit_target_row(Y, TargetRow, RowDiff),
            row_penalty_weight(RowWeight),
            RowPenalty is RowDiff * RowWeight,
            StartColumn is X + Len,
            blocking_cars(State, TargetRow, StartColumn, Blocking),
            blocking_penalty_weight(BlockWeight),
            Heuristic is Distance + RowPenalty + Blocking * BlockWeight
        ;   grid_size(W, H),
            (   W > H -> MaxSpan is W ; MaxSpan is H ),
            Heuristic is MaxSpan * MaxSpan
        )
    ;   Heuristic = 0
    ).

goal_state(State) :-
    red_car(State, car(r, X, Y, Heading, Len)),
    Heading = e,
    exit_column(ExitCol),
    (   exit_any_row(true)
    ;   exit_row(Row),
        Y =:= Row
    ),
    X + Len >= ExitCol.

red_car(State, car(r, X, Y, Heading, Len)) :-
    member(car(r, X, Y, Heading, Len), State),
    !.

exit_target_row(Y, Y, 0) :-
    exit_any_row(true),
    !.
exit_target_row(Y, Row, Distance) :-
    findall(R, exit_row(R), Rows),
    Rows \= [],
    closest_row(Rows, Y, Row, Distance).

closest_row([Row | Rest], Y, BestRow, Distance) :-
    Temp is abs(Y - Row),
    closest_row(Rest, Y, Row, Temp, BestRow, Distance).
closest_row([], _, BestRow, Distance, BestRow, Distance).
closest_row([Row | Rest], Y, CurrentRow, CurrentDistance, BestRow, Distance) :-
    Temp is abs(Y - Row),
    (   Temp < CurrentDistance
    ->  NextRow = Row,
        NextDistance = Temp
    ;   NextRow = CurrentRow,
        NextDistance = CurrentDistance
    ),
    closest_row(Rest, Y, NextRow, NextDistance, BestRow, Distance).

blocking_cars(State, Row, StartColumn, Count) :-
    exit_column(ExitCol),
    findall(Id,
            (   member(car(Id, X, Y, Heading, Len), State),
                Id \= r,
                blocks_exit(car(Id, X, Y, Heading, Len), Row, StartColumn, ExitCol)
            ),
            Blockers),
    sort(Blockers, Unique),
    length(Unique, Count).

blocks_exit(car(_, X, Y, Heading, Len), Row, StartColumn, ExitCol) :-
    heading_axis(Heading, h),
    Y =:= Row,
    X < ExitCol,
    X + Len > StartColumn.
blocks_exit(car(_, X, Y, Heading, Len), Row, StartColumn, ExitCol) :-
    heading_axis(Heading, v),
    X >= StartColumn,
    X < ExitCol,
    Y =< Row,
    Row < Y + Len.

state_signature(State, Signature) :-
    maplist(car_signature, State, Parts),
    atomic_list_concat(Parts, ';', Signature).

car_signature(car(Id, X, Y, Orient, Len), Atom) :-
    format(atom(Atom), '~w:~d:~d:~w:~d', [Id, X, Y, Orient, Len]).




