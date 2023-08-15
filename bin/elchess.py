#!/usr/bin/env python3

import chess
import chess.svg
import sys
import os

fen = sys.argv[1]
image_file = sys.argv[2]

try:
    board = chess.Board(fen)
    boardsvg = chess.svg.board(board=board)

    with open(image_file, 'w') as f:
        f.write(boardsvg)

except Exception as e:
    raise e
