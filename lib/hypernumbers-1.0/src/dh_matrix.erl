-module(dh_matrix).

-export([ multiply/2 ]).

multiply(M1, M2) -> 
    NRows = length(M1), 
    mmult(length(M2), NRows, NRows,[], M1, M2).

sumprod(0, _, _, Sum, _, _) -> Sum;
sumprod(I, C, R, Sum, M1, M2) -> 
    NewSum = Sum + (lists:nth(I, lists:nth(R,M1)) * lists:nth(C, lists:nth(I,M2))),
    sumprod(I-1, C, R, NewSum, M1, M2).

rowmult(_, 0, _, L, _, _) -> L;
rowmult(I, C, R, L, M1, M2) -> 
    SumProd = sumprod(I, C, R, 0, M1, M2),
    rowmult(I, C-1, R, [SumProd|L], M1, M2).

mmult(_, _, 0, MM, _, _) -> MM;
mmult(I, C, R, MM, M1, M2) ->
    NewRow = rowmult(I, C, R, [], M1, M2),
    mmult(I, C, R-1, [NewRow|MM], M1, M2).
