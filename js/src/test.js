var val = function (num) {
  var value = num;
  var that = {}

  that.get_value = function () {
    return value;
  }
  that.set_value = function (v) {
    value = v;
  }
  return that;
};

var v = val(1);

document.write("<h1>" + v.get_value() + "</h1>");
v.set_value(2);
document.write("<h1>" + v.get_value() + "</h1>");

