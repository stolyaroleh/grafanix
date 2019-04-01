"use strict";

exports.getItemImpl = function (just, nothing, key) {
  var item = window.sessionStorage.getItem(key);
  if (item === null) {
    return nothing;
  }
  return just(item);
};
exports.setItemImpl = function (key, item) {
  try {
    window.sessionStorage.setItem(key, item);
  }
  catch (DOMException) {}  // Item is too big
}
exports.removeItemImpl = function (key) {
  window.sessionStorage.removeItem(key);
}
