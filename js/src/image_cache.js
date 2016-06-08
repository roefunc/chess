/* @flow */

let ImageCacheEntry = function(spec) {
  let that = {};
  let loaded = false;
  let onload_listeners = [];
  let image = new Image();

  that.add_onload_listener = function(callback) {
    if (loaded) {
      callback(image);
    } else {
      onload_listeners.push(callback);
    }
  }

  let onload = function() {
    loaded = true;
    onload_listeners.forEach(function(callback) {
      callback(image);
    });
    onload_listeners = [];
  };

  that.get_image = function() {
      return image;
  };

  image.onload = onload;
  image.src = spec.image_name;

  return that;
};

export let ImageCache = function(spec) {
  let that = {};
  let base_path = spec.base_path || "";
  let images = {};

  that.get_image_from_cache = function(image_name, callback) {
    let full_name = base_path ? base_path + image_name : image_name;
    let cache_entry = images[image_name];
    if (cache_entry !== undefined) {
      console.log("Retrieving image " + image_name + " from cache");
      cache_entry.add_onload_listener(callback);
    } else {
      let new_cache_entry = ImageCacheEntry({image_name:full_name});
      new_cache_entry.add_onload_listener(callback);
      images[image_name] = new_cache_entry;
    }
  }
  return that;
};
