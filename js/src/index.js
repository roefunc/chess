"use strict"
/* @flow */

import App from './app'
import React from 'react'
import ReactDOM from 'react-dom'
import { Router, Route, IndexRoute, useRouterHistory } from 'react-router'
import { createHashHistory } from 'history'
import ChessGame from './react_chess_game'

const appHistory = useRouterHistory(createHashHistory)({queryKey: false});

ReactDOM.render(
  <ChessGame />,
  document.getElementById("app")
);
