"use strict"
/* @flow */

import { CanvasWrapper } from './canvas';
import { CheckerBoard, Rect } from './items';
import { StatusDisplay } from './status_display';
import { Move, Piece, Square, move, square, pieces_from_string } from './chess_pieces';
import { start_animation, move_animation, sine_interpolator } from './animation'

export const TURN_TAKEN = "turn_taken";
export const START_THINKING = "start_thinking";
export const STOP_THINKING = "stop_thinking";

export let opp_colour = (colour: string) : string => {
  if (colour == "white") {
    return "black";
  }
  return "white";
}
export class Board {
  _square_size: number;
  pieces: Piece[];
  canvas: CanvasWrapper;
  request_active: boolean;
  board_size: number;
  checker_board: CheckerBoard;
  frame: Rect;
  status_display: StatusDisplay;
  allowed_moves: Move[];

  constructor(spec, canvas_wrapper: CanvasWrapper) {
    this._square_size = spec.square_size;
    this.allowed_moves = [];
    this.pieces = [];
    this.canvas = canvas_wrapper;
    this.request_active = false;
    this.board_size = Math.min(spec.width, spec.height);
    this.checker_board = CheckerBoard({l:0,t:0,r:this.board_size,b:this.board_size,light_colour:"#FFFFFF",dark_colour:"#efb276", cols:8, rows:8});
    this.frame = Rect({l:0,t:0,r:this.board_size,b:this.board_size, stroke_colour:"#000000"})
    this.status_display = spec.use_status_display ? new StatusDisplay({l:this.board_size,t:0,r:spec.width-1,b:spec.height-1}, this.canvas) : undefined;
    this.event_listener = undefined;
  }

  start() {
    this.do_request("initial", "test");
  }

  request_pending() {
    this.request_active = true;
    this.canvas.disable_all_events();
  }

  request_complete() {
    this.request_active = false;
    this.canvas.enable_all_events();
  }

  do_request(path: string, content: string, callback) {
    this.request_pending();
    let xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = () => {
      if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
        this.set_game_state(xmlhttp.responseText);
        this.request_complete();
        if (typeof(callback) == "function") {
          callback();
        }
      }
    };
    xmlhttp.open("POST", "http://127.0.0.1:8000/chess/" + path, true);
    xmlhttp.send(content);
  }

  get square_size() : number {
    return this._square_size;
  }

  moves_from_array(moves : Move[]) {
    return moves.map(function(m) {
      return move(square(m[0][1],m[0][0]),square(m[1][1],m[1][0]));
    });
  }

  move_from_array(m) : Move {
    return move(square(m[0][1],m[0][0]),square(m[1][1],m[1][0]));
  }

  array_from_move(m: Move) {
    return [[m.start.row,m.start.col],[m.end.row,m.end.col]]
  }

  set_game_state(str : string) {
    console.log(str);
    const spec = JSON.parse(str);
    this.move_piece(spec.move, () => {
      this.clear();
      this.canvas.add_item(this.checker_board);
      this.canvas.add_item(this.frame);
      this.pieces = pieces_from_string(spec.pieces, this.canvas, this);
      console.log(this.pieces);
      this.allowed_moves = this.moves_from_array(spec.allowedMoves);
    });
  };

  move_piece(move_array, callback) {
    if (move_array !== undefined) {
      let move = this.move_from_array(move_array);
      let piece = this.piece_on_square(move.start);
      if (piece !== undefined) {
        piece.animate_to_square(move.end, callback);
      } else {
        callback();
      }
    } else {
      callback();
    }
  }

  move_equals(move) {
    return function(m) {
      return (m.start.col === move.start.col && m.start.row === move.start.row &&
          m.end.col === move.end.col && m.end.row == move.end.row);
    }
  }



  piece_on_square(square) {
    return this.pieces.find(function(piece) {
      return (piece.col == square.col) && (piece.row == square.row);
    });
  }

  event(name) {
    if (this.event_listener) {
      this.event_listener.event(name);
    }
  }

  make_move(move, colour) {
    if (this.request_active || !this.allowed_moves.some(this.move_equals(move))) {
      return false;
    }
    this.event(TURN_TAKEN);
    console.log("Moved piece from " + move.start + " to " + move.end);
    let makeMoveMessage = JSON.stringify({move:this.array_from_move(move), gameState:this.pieces, side:opp_colour(colour)});
    console.log(makeMoveMessage);
    this.do_request("makemove", makeMoveMessage, () => {
      let message = JSON.stringify({depth:3,gameState:this.pieces, side:opp_colour(colour)});
      this.event(START_THINKING);
      this.do_request("getmove", message, () => {
        this.event(STOP_THINKING);
        this.event(TURN_TAKEN);
      });
  });
    return true;
  }

  clear() {
    this.canvas.clear();
    this.pieces = [];
  }
}
