"use strict"
/* @flow */
import { ImageObject } from './items'
import { start_animation, move_animation, sine_interpolator, default_animation_spec, slow_animation_spec } from './animation'

export let square = function(c: number, r : number) : Square {
  return {col:c,row:r, toString: function() { return "(" + this.col + "," + this.row + ")" }};
};

class Square {
  col: number;
  row: number;
}

class Move {
  start: Square;
  end: Square;
}
export let move = function(s: Square,e: Square) : Move {
  return {start:s, end:e};
};

export let Piece = function(spec, type, b) {
  let that = {};
  let board = b;

  that.col = spec.col;
  that.row = spec.row;
  that.type = type;
  that.side = spec.side;

  let get_image_from_spec = function(spec, type) {
    return type + "_" + spec.side + ".png";
  };

  let drawable = ImageObject({src:get_image_from_spec(spec, type), l:that.col*board.square_size, t:that.row*board.square_size,r:(that.col+1)*board.square_size, b:(that.row+1)*board.square_size, draggable:true});
  drawable.add_change_listener(that);

  that.changed = function(item, user_change) {
    if (user_change && !drawable.is_being_dragged() && drawable.enabled) {
      snap_to_square();
    }
  }

  that.attach_board = function(b) {
    board = b;
  };

  that.animate_to_square = function(square, callback) {
    let spec = slow_animation_spec;
    if (typeof callback === "function") {
      spec = Object.create(default_animation_spec);
      spec.callback = callback;
    }
    start_animation(drawable, spec, move_animation(square.col*board.square_size, square.row*board.square_size));
  }

  let snap_to_square = function() {
    let center = drawable.center();
    console.log(center);
    let new_col = Math.floor(center.x/board.square_size);
    let new_row = Math.floor(center.y/board.square_size);
    if (!that.move_to_square(new_col, new_row)) {
      start_animation(drawable, default_animation_spec, move_animation(that.col*board.square_size, that.row*board.square_size));
    }
  };

  that.move_to_square = function(new_col,new_row) {
    if (new_col !== that.col || new_row !== that.row) {
      return board.make_move(move(square(that.col, that.row), square(new_col, new_row)), that.side);
    }
    return false;
  };

  that.get_drawable = function() {
    return drawable;
  }

  return that;
};

export let piece_from_string = function(str) {
  let spec = JSON.parse(str);
  return Piece(spec, str.type);
}

export let pieces_from_string = function(pieces, canvas, board) {
  if (pieces) {
    return pieces.map(function(p) {
      let piece = Piece(p, p.type, board);
      canvas.add_item(piece.get_drawable());
      return piece;
    });
  } else {
    return []
  }
}

export let Pawn = function(spec, board) {
  return Piece(spec, "pawn", board);
}
export let Rook = function(spec, board) {
  return Piece(spec, "rook", board);
}
export let Knight = function(spec, board) {
  return Piece(spec, "knight", board);
}
export let Bishop = function(spec, board) {
  return Piece(spec, "bishop", board);
}
export let Queen = function(spec, board) {
  return Piece(spec, "queen", board);
}
export let King = function(spec, board) {
  return Piece(spec, "king", board);
}
