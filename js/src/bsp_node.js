"use strict"
/* @flow */
export class BspNode {
  left : number;
  right : number;
  top : number;
  bottom : number;
  items: Item[];
  child_nodes: BspNode[];

  constructor(spec) {
    this._left = spec.left;
    this._top = spec.top;
    this._right = spec.right;
    this._bottom = spec.bottom;
    this.items = [];
    this.child_nodes = [];
  }

  add_item(item) {
    let added = false;
    if (this.contains_item(item)) {
      for (let n in this.child_nodes) {
        if (this.child_nodes.hasOwnProperty(n) && this.child_nodes[n].add_item(item)) {
          added = true;
          break;
        }
      }
      if (!added) {
        this.items.push(item);
        this.split_if_necessary();
        added = true;
      }
      return added;
    }
    return false;
  }

  left() {
    return this._left;
  }
  right() {
    return this._right;
  }
  top() {
    return this._top;
  }
  bottom() {
    return this._bottom;
  }

  is_large_enough() {
    return ((this._right - this._left) > 10) && ((this._bottom - this._top) > 10);
  }

  split_if_necessary() {
    if (this.items.length > 10 && this.child_nodes.length == 0 && this.is_large_enough()) {
      this.child_nodes.push(new BspNode({ left:this._left, right:(this._left+this._right)/2, top:this._top, bottom:(this._top+this._bottom)/2}));
      this.child_nodes.push(new BspNode({ left:(this._left+this._right)/2, right:this._right, top:this._top, bottom:(this._top+this._bottom)/2}));
      this.child_nodes.push(new BspNode({ left:this._left, right:(this._left+this._right)/2, top:(this._top+this._bottom)/2, bottom:this._bottom}));
      this.child_nodes.push(new BspNode({ left:(this._left+this._right)/2, right:this._right, top:(this._top+this._bottom)/2, bottom:this._bottom}));
      let items_copy = this.items.slice(0);
      this.items = [];
      items_copy.forEach((item) => {
        this.add_item(item);
      });
    }
  }

  contains_item(item) {
    return item.left() >= this._left && item.right() <= this._right && item.top() >= this._top && item.bottom() <= this._bottom;
  }

  contains_point(x,y) {
    return x >= this._left && x < this._right && y >= this._top && y < this._bottom;
  }

  collect_items(x,y,acc) {
    if (this.contains_point(x,y)) {
      this.items.forEach(function(item) {
        if (item.contains_point(x,y)) {
          acc.push(item);
        }
      });
      this.child_nodes.forEach(function(node) {
        node.collect_items(x,y,acc);
      });
    }
  }

  get_items_at_point(x,y) {
    let items_found = [];
    this.collect_items(x,y,items_found);
    return items_found;
  }

  rects_intersect(first, second) {
    if (first.left() <= second.right() && first.right() >= second.left()) {
      return (first.top() <= second.bottom() && first.bottom() >= second.top());
    }
    return false;
  }

  get_items_in_rect(rect) {
    let items_found = [];
    this.collect_items_in_rect(rect, items_found);
    return items_found;
  }

  collect_items_in_rect(rect, acc) {
    if (this.rects_intersect(this,rect)) {
      this.items.forEach((item) => {
        if (this.rects_intersect(rect, item)) {
          acc.push(item);
        }
      });
      this.child_nodes.forEach(function(node) {
        node.collect_items_in_rect(rect, acc);
      });
    }
  }

  remove_item(item) {
    let index = this.items.indexOf(item);
    if (index !== -1) {
      this.items.splice(index,1);
      return true;
    }
    for (let n in this.child_nodes) {
      if (this.child_nodes.hasOwnProperty(n) && this.child_nodes[n].remove_item(item)) {
        return true;
      }
    }
    return false;
  }
}
