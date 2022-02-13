:- initialization(initialize_vertices).

:- dynamic(total_vertices/1).
:- dynamic(v/4).

iterations(52).

rank :-
    initialize_vertices,
    iterations(Iterations),
    rank(0, Iterations),
    forall(
        v(prev, V, Rank, _),
        format('~a = ~w~n', [V, Rank])
    ).

rank(Iterations, Iterations) :-
    !.
rank(Iterations0, Iterations) :-
    forall(
        v(prev, V, _, _),
        rank(V)
    ),
    update_ranks,
    Iterations1 is Iterations0 + 1,
    rank(Iterations1, Iterations).

initialize_vertices :-
    load_files(['graph.pl']),
    retractall(v(_, _, _, _)),
    retractall(total_vertices(_)),
    forall(
        edge(X, Y),
        initialize_vertex(edge(X, Y))
    ),
    aggregate_all(count, v(prev, _, _, _), N),
    assertz(total_vertices(N)),
    Seed is 1 / N,
    forall(
        v(prev, V, _, _),
        (   retract(v(prev, V, _, _)),
            initialize_score(V, Seed)
        )
    ).

initialize_vertex(edge(X, Y)) :-
    (   \+ v(prev, X, _, _)
    ->  initialize_score(X, 0)
    ;   true
    ),
    (   \+ v(prev, Y, _, _)
    ->  initialize_score(Y, 0)
    ;   true
    ).

initialize_score(V, Seed) :-
    outgoing_links(V, N),
    assertz(v(prev, V, Seed, N)).

outgoing_links(X, N) :-
    aggregate_all(count, edge(X, _), N0),
    outgoing_links_(N0, N).

outgoing_links_(0, N) :-
    total_vertices(N0),
    N is N0 - 1,
    !.
outgoing_links_(N, N).

incoming_vertex(U, v(prev, V, Score, N)) :-
    edge(V, U),
    v(prev, V, Score, N).

% The rank of U, is the sum of all ranks of V linking to U, divided by the number of links from V.
rank(U) :-
    T = total(0),
    forall(incoming_vertex(U, V), add_vertex_rank(T, V)),
    T = total(NewRank),
    v(prev, U, _, Outgoing),
    assertz(v(curr, U, NewRank, Outgoing)).

add_vertex_rank(Total, v(prev, _, Rank, N)) :-
    Total = total(T),
    Score is T + Rank / N,
    nb_setarg(1, Total, Score).

update_ranks :-
    forall(
        retract(v(curr, V, NewRank, N)),
        (   retract(v(prev, V, _, _)),
            assertz(v(prev, V, NewRank, N))
        )
    ).
