"use strict"
/* @flow */
import { BspNode } from './bsp_node'

export let CanvasWrapper = function(spec) {
  let canvas = spec.canvas;
  let context = canvas.getContext("2d");
  let items = [];
  let items_registered_for_mouse_events = [];
  let bsp_node = new BspNode({left:0,right:canvas.width, top:0, bottom:canvas.height});
  let events_enabled = true;
  let that = {};

  let convert_to_local_coords = function(event) {
    return {x:event.x - canvas.getBoundingClientRect().left, y: event.y - canvas.getBoundingClientRect().top};
  };

  let get_item_clicked_on_and_do = function(event, func) {
    if (!events_enabled) {
      return;
    }
    let local_coords = convert_to_local_coords(event);
    for (let n in items_registered_for_mouse_events) {
      if (items_registered_for_mouse_events.hasOwnProperty(n)) {
        func(items_registered_for_mouse_events[n], local_coords);
      }
    }
    let items_clicked_on = bsp_node.get_items_at_point(local_coords.x, local_coords.y);
    if (items_clicked_on[items_clicked_on.length-1]) {
      func(items_clicked_on[items_clicked_on.length-1], local_coords);
    }
  };

  that.enable_all_events = function() {
    events_enabled = true;
  }

  that.disable_all_events = function() {
    events_enabled = false;
  }

  that.register_for_mouse_events = function(item) {
    items_registered_for_mouse_events.push(item);
  }

  that.unregister_for_mouse_events = function(item) {
    for (let n in items_registered_for_mouse_events) {
      if (items_registered_for_mouse_events.hasOwnProperty(n) && items_registered_for_mouse_events[n] === item) {
        items_registered_for_mouse_events.splice(n, 1);
        break;
      }
    }
  }

  that.on_mouse_down = function(event) {
    get_item_clicked_on_and_do(event, function(item, coords) {
      if (item.on_mouse_down) {
        item.on_mouse_down(coords.x, coords.y);
      }
    });
  };

  that.on_mouse_up = function(event) {
    get_item_clicked_on_and_do(event, function(item, coords) {
      if (item.on_mouse_down) {
        item.on_mouse_up(coords.x, coords.y);
      }
    });
  };

  that.on_mouse_move = function(event) {
    get_item_clicked_on_and_do(event, function(item, coords) {
      if (item.on_mouse_down) {
        item.on_mouse_move(coords.x, coords.y);
      }
    });
  };

  that.on_mouse_double_click = function(event) {
    get_item_clicked_on_and_do(event, function(item, coords) {
      if (item.on_mouse_double_click) {
        item.on_mouse_double_click(coords.x, coords.y);
      }
    });
  };

  that.add_item = function(item) {
    if (item.add_change_listener !== undefined) {
      item.add_change_listener(that);
    }
    if (item.set_event_register !== undefined) {
      item.set_event_register(that);
    }
    bsp_node.add_item(item);
    items.push(item);
    draw_item(item);
  };

  let draw_item = function(item) {
    context.save();
    item.configure_context(context);
    item.render(context);
    context.restore();
  }

  that.changed = function(item, user_change, rect) {
    if (rect){
      console.log("Changed rect ()" + rect.left() + "," + rect.top() + "," + rect.right() + "," + rect.bottom());
    }
    bsp_node.remove_item(item);
    bsp_node.add_item(item);
    let items_changed = undefined;
    if (rect !== undefined) {
      items_changed = bsp_node.get_items_in_rect(rect);
      console.log("Items changed" + items_changed);
    }
    that.render(items_changed, rect);
  };

  that.render = function(items_changed, rect) {
    let items_to_render = items_changed ? items_changed : items;
    context.save();
    if (rect) {
      context.beginPath();
      context.moveTo(rect.left(), rect.top());
      context.lineTo(rect.right(), rect.top());
      context.lineTo(rect.right(), rect.bottom());
      context.lineTo(rect.left(), rect.bottom());
      context.clip();
      context.clearRect(rect.left(), rect.top(), rect.right()-rect.left(), rect.bottom()-rect.top());
    } else {
      context.clearRect(0, 0, canvas.width, canvas.height);
    }
    for (let item in items_to_render) {
      if (items.hasOwnProperty(item)) {
        draw_item(items_to_render[item]);
      }
    }
    context.restore();
  }

  that.remove_item = function(item) {
    if (!bsp_node.remove_item(item)) {
      throw "Didn't remove item" + item;
    }
    let index = items.indexOf(item);
    if (index !== -1) {
      items.splice(index,1);
      item.remove_change_listener(that);
      if (item.set_event_register !== undefined) {
        item.set_event_register(undefined);
      }
      that.render();
    } else {
      throw "Didn't remove item" + item;
    }
  }

  that.clear = function() {
    items.slice().forEach(function(item) {
      that.remove_item(item);
    });
  };

  canvas.addEventListener("mousedown", that.on_mouse_down);
  canvas.addEventListener("mouseup", that.on_mouse_up);
  canvas.addEventListener("mousemove", that.on_mouse_move);
  canvas.addEventListener("dblclick", that.on_mouse_double_click);

  return that;
};
