"use strict"
/* @flow */
import React from 'react';
import ChessBoard from './react_board';
import GameStore from './game_store';
import ChessGameStatus from './react_game_status';

export default class ChessGame extends React.Component {
  game_store : GameStore;

  constructor(props) {
    super(props);
    this.game_store = GameStore();
    this.game_store.subscribe(this);
  }

  componentWillMount() {
    this.setState(this.game_store.state)
  }

  notify(state) {
    this.setState(state);
  }

  render() {
    return (
      <div>
        <ChessBoard dispatcher={this.game_store}/>
        <ChessGameStatus player_turn={this.state.player_turn} turn_number={this.state.turn_number} dispatcher={this.game_store} thinking={this.state.thinking}/>
      </div>
    );
  }
}
