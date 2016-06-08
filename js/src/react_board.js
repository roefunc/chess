"use strict"
/* @flow */
import React from 'react';
import { Board, TURN_TAKEN, START_THINKING, STOP_THINKING } from './board';
import { CanvasWrapper } from './canvas';
import { takeTurnAction, startThinkingAction, stopThinkingAction } from './game_store';

export default class ChessBoard extends React.Component {

  canvas: canvas;
  canvas_wrapper: CanvasWrapper;
  board: Board;

  constructor(props) {
    super(props);
  }

  componentDidMount() {
    this.canvas = document.getElementById("myCanvas");
    this.canvas_wrapper = CanvasWrapper ( { canvas: this.canvas });
    this.board = new Board({square_size:Math.floor(Math.min(this.canvas.height,this.canvas.width)/8), width:this.canvas.width,height:this.canvas.height}, this.canvas_wrapper);
    this.board.event_listener = this;
    this.board.start();
    this.canvas_wrapper.render();
  }

  event(name) {
    if (this.props.dispatcher) {
      switch (name) {
        case TURN_TAKEN:
          this.props.dispatcher.dispatch(takeTurnAction);
          break;
        case START_THINKING:
          this.props.dispatcher.dispatch(startThinkingAction);
          break;
        case STOP_THINKING:
          this.props.dispatcher.dispatch(stopThinkingAction);
          break;
      }
    }
  }

  render() {
    return <canvas id="myCanvas" width="512" height="512" />
  }
}
