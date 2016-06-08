"use strict"
/* @flow */
import React from 'react';

export default class ChessGameStatus extends React.Component {
  render() {
    return (<div>
        <p>Player: {this.props.player_turn} {this.props.thinking ? "Thinking..." : ""}</p>
        <p>Turn: {this.props.turn_number}</p>
      </div>
    );
  }
}
