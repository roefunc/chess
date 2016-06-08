"use strict"
/* @flow */


export let linear_interpolator = function(start : number, end : number, percentage : number) {
  return start + ((end-start) * percentage/100);
};

export let sine_return_interpolator = function(start : number, end : number, percentage) : number {
  return start + (end - start)*Math.sin(Math.PI*percentage/100);
}

export let sine_interpolator = function(start : number, end : number, percentage) : number {
  return start + (end - start)*Math.sin(Math.PI*percentage/200);
}

export let default_animation_spec = { framerate:60, curve:sine_interpolator, duration:100 };
export let slow_animation_spec = { framerate:60, curve:sine_interpolator, duration:300 };

export let loop_animation = function(item, spec, animation) {
  let spec_copy = Object.create(spec);
  let stopped = true;
  let that = {};

  spec_copy.callback = function() {
    if (!stopped) {
      start_animation(item, spec_copy, animation);
    }
  }

  that.stop = function() {
    stopped = true;
  }

  that.start = function() {
    if (stopped) {
      stopped = false;
      spec_copy.callback();
    }
  }
  return that;
}

export let start_animation = function(item, spec, animation_creator) {
  if (typeof item.disable == "function") {
    item.disable();
  }
  let animation = animation_creator(item, spec);
  let number_of_frames = spec.duration * spec.framerate / 1000;
  let delay = 1000 / spec.framerate;
  let current_frame = 0;

  let interval_id = setInterval(function() {
    animation(Math.ceil(100 * current_frame / number_of_frames));
    ++current_frame;
    if (current_frame > number_of_frames) {
      if (typeof spec.callback === "function") {
        spec.callback();
      }
      clearInterval(interval_id);
      if (typeof item.enable == "function") {}
      item.enable();
    }
  }
  , delay);

}

export let property_animation = function(property, end_val : number) {
  return function(item, spec) {
    let start_val = item[property];
    return function(percentage) {
      let new_val = spec.curve(start_val, end_val, percentage);
      item[property] = new_val;
    }
  }
}

export let move_animation = function(end_x : number, end_y : number) {
  return function(item, spec) {
    let start_x = item.left();
    let start_y = item.top();
    return function(percentage) {
      let x = spec.curve(start_x, end_x, percentage);
      let y = spec.curve(start_y, end_y, percentage);
      console.log("Animating to " + x + "," + y);
      item.move(x, y);
    }
  }
}
