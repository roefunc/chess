"use strict"

let c = document.getElementById("myCanvas");

let cw = CanvasWrapper ( { canvas: c });

let build_game = function() {
  let pieces = [];
  for (let n = 0 ; n < 8 ; ++n) {
    pieces.push(Pawn({col:n, row:6, side:"white"}, board));
    pieces.push(Pawn({col:n, row:1, side:"black"}, board));
  }
  pieces.push(Queen({col:3, row:7, side:"white"}, board));
  pieces.push(Queen({col:3, row:0, side:"black"}, board));
  pieces.push(King({col:4, row:7, side:"white"}, board));
  pieces.push(King({col:4, row:0, side:"black"}, board));
  pieces.push(Knight({col:1, row:7, side:"white"}, board));
  pieces.push(Knight({col:6, row:7, side:"white"}, board));
  pieces.push(Knight({col:1, row:0, side:"black"}, board));
  pieces.push(Knight({col:6, row:0, side:"black"}, board));
  pieces.push(Bishop({col:2, row:7, side:"white"}, board));
  pieces.push(Bishop({col:5, row:7, side:"white"}, board));
  pieces.push(Bishop({col:2, row:0, side:"black"}, board));
  pieces.push(Bishop({col:5, row:0, side:"black"}, board));
  pieces.push(Rook({col:0, row:7, side:"white"}, board));
  pieces.push(Rook({col:7, row:7, side:"white"}, board));
  pieces.push(Rook({col:0, row:0, side:"black"}, board));
  pieces.push(Rook({col:7, row:0, side:"black"}, board));
  return pieces;
}

let dummy_board = Board({square_size:60}, cw);
let board = Board({square_size:Math.floor(Math.min(c.height,c.width)/8)}, cw);
board.start();
cw.render();
