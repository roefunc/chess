# Chess server and client

A simple chess engine written in Haskell, exported as a JSON HTTP API. The chess engine's evaluation function is parameterised by a set of coefficients to a list of sub functions. The coefficients are determined through a simple genetic algortihm.

This is really simple: it uses no heuristics or even a transposition table yet, so it's not particularly fast yet. It is also missing castling, promotion and _en passant_. It parallelises linearly up to (at least) 4 cores in my experience.

The JavaScript client is in ES6 using a simple custom 2D graphics engine on top of an HTML Canvas element. There is some React for the dynamic scoreboard, which is (putting it politely) a work in progress.

This was all written in order to learn Haskell and Javascript, so don't laugh at the code.
