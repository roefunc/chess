/* @flow */
import { extendsObject, checkThis } from './utils'
import { ImageCache } from './image_cache'

let Item = function(spec, that) {
  that = checkThis(this, Item);
  if (spec.draggable) {
    make_draggable(that);
  }
  that.l = spec.l;
  that.t = spec.t;
  that.r = spec.r;
  that.b = spec.b;
  that.change_listeners = [];
  that.enabled = true;
  that._opacity = 1.0;
  return that;
}

Item.prototype = {
  top: function() {
    return this.t;
  },

  left: function() {
    return this.l;
  },

  bottom: function() {
    return this.b;
  },

  right: function() {
    return this.r;
  },

  configure_context: function(context) {
    context.globalAlpha = this.opacity;
  },

  get opacity() {
    return this._opacity;
  },

  set opacity(o: number) {
    this._opacity = o;
    this.changed();
  },

  center : function() {
    return {x:this.l+(this.r-this.l)/2,y:this.t+(this.b-this.t)/2};
  },

  contains_point : function(x:number,y:number) {
    return x >= this.l && x < this.r && y >= this.t && y < this.b;
  },

  add_change_listener : function(listener) {
    this.change_listeners.push(listener);
  },

  remove_change_listener : function(listener) {
    let index = this.change_listeners.indexOf(listener);
    if (index !== undefined) {
      this.change_listeners.splice(index,1);
    }
  },

  changed : function(user_change, rect) {
    let that = this
    this.change_listeners.forEach(function(listener) {
      listener.changed(that, user_change, rect);
    });
  },

  enable : function () {
    this.enabled = true;
  },

  disable : function () {
    this.enabled = false;
  },

  move_rect: function(x:number, y:number) {
    let l = Math.min(this.l, x);
    let r = Math.max(this.r, x+this.r-this.l);
    let t = Math.min(this.t, y);
    let b = Math.max(this.b, y+this.b-this.t);
    return {
      left: function() {
        return l;
      },
      right: function() {
        return r;
      },
      top: function() {
        return t;
      },
      bottom: function() {
        return b;
      }
    }
  },

  move : function(x_in:number, y_in:number, user_change: boolean) {
    let x = Math.floor(x_in);
    let y = Math.floor(y_in);
    if (x !== this.l || y !== this.t) {
      let change_rect = this.move_rect(x, y);
      let width = this.r - this.l;
      let height = this.b - this.t;
      this.l = x;
      this.t = y;
      this.r = this.l + width;
      this.b = this.t + height;
      this.changed(user_change, change_rect);
    }
  },

  set_size : function(w:number,h:number) {
    if ((w !== (this.r-this.l)) || (h !== (this.b-this.t))) {
      this.r = this.l + w;
      this.b = this.t + h;
      this.changed(false);
    }
  },

  toString : function() {
    return "Item: left=" + this.l + " right=" + this.r + " top=" + this.t + " bottom=" + this.b;
  }
}

let make_draggable = function(item) {
  item.click_down = false;
  item.click_x_offset = 0;
  item.click_y_offset = 0;
  item.event_register = undefined;

  item.set_event_register = function(register) {
    this.event_register = register;
  };

  item.on_mouse_down = function(x,y) {
    if (this.enabled) {
      if (this.event_register !== undefined) {
        this.event_register.register_for_mouse_events(this);
      }
      this.click_down = true;
      this.click_x_offset = x - this.left();
      this.click_y_offset = y - this.top();
    }
  };

  item.on_mouse_up = function(x,y) {
    if (this.enabled) {
      this.click_down = false;
      if (this.event_register !== undefined) {
        this.event_register.unregister_for_mouse_events(this);
      }
      this.changed(true);
    }
  };

  item.on_mouse_move = function(x,y) {
    if (this.enabled) {
      if (this.click_down) {
        this.move(x - this.click_x_offset, y - this.click_y_offset, true);
      }
    }
  };

  item.is_being_dragged = function() {
    if (this.enabled) {
      return this.click_down;
    }
  }
}

export let Rect = function(spec) {
  let that = checkThis(this, Rect);
  Item.call(that, spec);
  that.fill_colour = spec.fill_colour;
  that.stroke_colour = spec.stroke_colour;
  return that;
}

extendsObject(Rect, Item, {
  configure_context:function(context) {
    if (this.fill_colour !== undefined) {
      context.fillStyle = this.fill_colour;
    }
    if (this.stroke_colour !== undefined) {
      context.strokeStyle = this.stroke_colour;
    }
  },

  render:function(context) {
    if (this.fill_colour !== undefined) {
      context.fillRect(this.left(), this.top(), this.right() - this.left(), this.bottom() - this.top());
    }
    if (this.stroke_colour !== undefined) {
      context.strokeRect(this.left(), this.top(), this.right() - this.left(), this.bottom() - this.top());
    }
  }
});

export let Text = function(spec) {
  let that = checkThis(this, Text);
  Item.call(that, spec);
  that.text = spec.text;
  that.colour = spec.colour;
  that.font = spec.font;
  return that;
}

extendsObject(Text, Item, {
  configure_context: function(context) {
    context.fillStyle = this.colour;
    context.font = this.font;
    context.globalAlpha = this.opacity;
  },

  render: function(context) {
    context.fillText(this.text, this.left(), this.top());
  }
});

let image_cache = ImageCache({base_path:"images/"});

export let ImageObject = function(spec) {
  let that = checkThis(this, ImageObject);
  Item.call(that, spec);
  that.image = undefined;
  image_cache.get_image_from_cache(spec.src, that.image_loaded.bind(that));
  return that;
}

extendsObject(ImageObject, Item, {
  image_loaded:function(newImage) {
    this.image = newImage;
    this.set_size(this.image.naturalWidth, this.image.naturalHeight);
    this.changed(false);
  },

  render:function(context) {
    if (this.image !== undefined) {
      context.drawImage(this.image, this.left(), this.top());
    }
  }
});

export let CheckerBoard = function(spec) {
  let that = checkThis(this, CheckerBoard);
  Item.call(that, spec);
  that.rows = spec.rows;
  that.cols = spec.cols;
  that.colours = [spec.dark_colour, spec.light_colour];
  return that;
}

extendsObject(CheckerBoard, Item, {
  render: function(context) {
    let r;
    let c;
    let square_width = Math.floor((this.right()-this.left()) / this.cols);
    let square_height = Math.floor((this.bottom()-this.top()) / this.rows);

    let rmod, cmod;
    for (r = 0 ; r < this.rows ; ++r) {
      rmod = r % 2;
      for (c = 0 ; c < this.cols ; ++c) {
        let colour = this.colours[(c+rmod) % 2];
        context.fillStyle = colour;
        context.fillRect(square_width*c,square_height*r, square_width, square_height);
      }
    }
  }
});
