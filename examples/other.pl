% other example process definitions

defproc(loop, 0 ? (a1 > loop)).
defproc(loop1, a1 > loop1).
defproc(loop3, (a1 > loop3) ? 0).
