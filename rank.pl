:- initialization(initialize_vertices).

:- dynamic(total_vertices/1).

iterations(52).

rank :-
    initialize_vertices,
    iterations(Iterations),
    rank(0, Iterations),
    findall(v(prev, V, Rank, _), v(prev, V, Rank, _), Vs0),
    sort(Vs0, Vs),
    forall(
        member(v(prev, V, Rank, _), Vs),
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
    setof(X, Y^edge(X, Y), Xs),
    setof(Y, X^edge(X, Y), Ys),
    append(Xs, Ys, Vertices0),
    sort(Vertices0, Vertices),
    length(Vertices, N),
    assertz(total_vertices(N)),
    Seed is 1 / N,
    forall(member(V, Vertices), initialize_score(V, Seed)).

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
    findall(V, incoming_vertex(U, V), IncomingVertices),
    sum_vertices(IncomingVertices, NewRank),
    v(prev, U, _, Outgoing),
    assertz(v(curr, U, NewRank, Outgoing)).

sum_vertices(Vertices, Score) :-
    sum_vertices_(Vertices, 0, Score).

sum_vertices_([], Score, Score).
sum_vertices_([v(prev, _, Rank, N)|Rest], Score0, Score) :-
    VertexScore is Rank / N,
    Score1 is Score0 + VertexScore,
    sum_vertices_(Rest, Score1, Score).

update_ranks :-
    forall(
        v(curr, V, NewRank, N),
        (   retract(v(curr, V, _, _)),
            retract(v(prev, V, _, _)),
            assertz(v(prev, V, NewRank, N))
        )
    ).
