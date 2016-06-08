"use strict"
/* @flow */
import { Pieces } from './chess_pieces';
import { Store, create_reducer } from './store';
import { opp_colour } from './board';

const initial_state = { player_turn:"white", pieces_taken:[], turn_number:1, thinking:false};

const TAKE_TURN: string = "take_turn";
const TAKE_PIECE: string = "take_piece";
const START_THINKING: string = "start_thinking";
const STOP_THINKING: string = "stop_thinking";

export const takeTurnAction = {
  type:TAKE_TURN
};

export const takePieceAction = (piece) => {
  return {
    type:TAKE_PIECE,
    piece:piece
  }
};

export const startThinkingAction = {
  type:START_THINKING
};

export const stopThinkingAction = {
  type:STOP_THINKING
};

class GameState {
  player_turn: string;
  pieces_taken: Piece[];
  turn_number: number;
  constructor(spec = initial_state) {
    this.player_turn = spec.player_turn;
    this.pieces_taken = spec.pieces_taken;
    this.turn_number = spec.turn_number;
    this.thinking = spec.thinking;
  }
}

let player_turn = (state, action) => {
  switch (action.type) {
    case TAKE_TURN:
      return opp_colour(state.player_turn);
    default:
      return state.player_turn;
  }
}

let thinking = (state, action) => {
  switch (action.type) {
    case START_THINKING:
      return true;
    case STOP_THINKING:
      return false;
    default:
      return state.thinking;
    }
}

let turn_number = (state, action) => {
  switch (action.type) {
    case TAKE_TURN:
      return state.turn_number + 1;
    default:
      return state.turn_number;
  }
}

let pieces_taken = (state, action) => {
  switch (action.type) {
    case TAKE_PIECE:
      return state.pieces_taken.slice(0).push(action.piece);
    default:
      return state.pieces_taken;
  }
}

let game_reducer = create_reducer({
  player_turn,
  pieces_taken,
  turn_number,
  thinking
});

const GameStore = () => new Store(initial_state, game_reducer);

export default GameStore;
