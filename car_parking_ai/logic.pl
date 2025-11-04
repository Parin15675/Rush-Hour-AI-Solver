% Knowledge base that validates Rush Hour moves on a 6x6 board.

:- module(logic, [
    valid_move/4,
    within_board/4,
    blocked/5
]).

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

