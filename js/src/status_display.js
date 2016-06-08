import { Rect, Text } from './items';
import { loop_animation, property_animation, sine_return_interpolator } from './animation';

let common_margin = 20;

export class StatusDisplay  {
  canvas: CanvasWrapper;
  frame: Rect;
  is_thinking: boolean;

  constructor(spec, canvas_wrapper: CanvasWrapper) {
    this.canvas = canvas_wrapper;
    this.frame = Rect({l:spec.l,r:spec.r,t:spec.t,b:spec.b, stroke_colour:"#FF00000"});
    this.is_thinking = false;
    this.thinking_text = Text({l:spec.l + common_margin, r:spec.l + common_margin,
    t: (spec.b + spec.t)/2, b:(spec.b + spec.t)/2 + 10,
    text:"Thinking...", colour:"#000000",
    font:"32px sans-serif"});
    this.thinking_animation = loop_animation(thinking_text, {duration:2000,framerate:30,curve:sine_return_interpolator}, property_animation("opacity", 1.0));
  }

  add_to_canvas(canvas_wrapper : CanvasWrapper) {
    this.canvas = canvas_wrapper;
    this.canvas.add_item(frame);
  }

  set_thinking(thinking: boolean) {
    this.is_thinking = thinking;
    if (this.is_thinking) {
      this.thinking_text.opacity = 0.3;
      this.canvas.add_item(thinking_text);
      this.thinking_animation.start();
    } else {
      this.thinking_animation.stop();
      this.canvas.remove_item(thinking_text);
    }
  }
}
