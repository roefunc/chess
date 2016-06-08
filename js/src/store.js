"use strict"
/* @flow */
export const create_reducer = (spec) => {
  return (state, action) => {
    let internal_spec = Object.assign({}, spec);
    let o = {};
    for (let p in internal_spec) {
      if (spec.hasOwnProperty(p)) {
        o[p] = spec[p](state, action);
      }
    }
    return o;
  }
}

export class Store {
  constructor(initial_state, reducer) {
    this.state = initial_state;
    this.listeners = [];
    this.reducer = reducer;
  }

  subscribe(listener) {
    console.log(`Store subscription: ${listener}`);
    this.listeners.push(listener);
  }

  unsubscribe(listener) {
    let index = this.listeners.indexOf(listener);
    if (index !== -1) {
      this.listeners.splice(index,1);
    }
  }

  notify() {
    for (let l of this.listeners) {
      l.notify(this.state);
    }
  }

  dispatch(action) {
    console.log(`Dispatch action ${action}`);
    this.state = this.reducer(this.state, action);
    this.notify();
    return this.state;
  }
}
