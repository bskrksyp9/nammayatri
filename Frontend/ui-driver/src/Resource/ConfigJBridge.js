const callbackMapper = require('presto-ui').callbackMapper;
exports.getKeyInSharedPrefKeysConfig = function (key) {
    return JBridge.getKeysInSharedPrefs(key);
  };

exports.getValueToLocalNativeStoreConfig = function (key) {
  return JBridge.getKeyInNativeSharedPrefKeys(key);
}

exports["getZoneTagConfig'"] = function (key) {
  if (key in zoneConfig){
    return zoneConfig[key];
  }
  console.error("not found in zoneConfig");
  return {};
}

const zoneConfig = {
  "METRO_PICKUP" : {
    "backgroundColor" : "#2194FF",
    "text" : "Metro Pickup",
    "imageUrl" : "ic_metro_white",
    "cancelText" : "ZONE_CANCEL_TEXT_PICKUP",
    "cancelConfirmImage" : "ic_cancelride_metro_pickup,https://assets.juspay.in/nammayatri/images/driver/ic_cancelride_metro_pickup.png"
  },
  "METRO_DROP" : {
    "backgroundColor" : "#2194FF",
    "text" : "Metro Drop",
    "imageUrl" : "ic_metro_white",
    "cancelText" : "ZONE_CANCEL_TEXT_DROP",
    "cancelConfirmImage" : "ic_cancelride_metro_drop,https://assets.juspay.in/nammayatri/images/driver/ic_cancelride_metro_drop.png"
  },
  "HOSPITAL" : {
    "backgroundColor" : "#FADFDF",
    "text" : "Hospital Ride",
    "imageUrl" : "",
    "cancelText" : "",
    "cancelConfirmImage" : ""
  },
  "AIRPORT" : {
    "backgroundColor" : "",
    "text" : "",
    "imageUrl" : "",
    "cancelText" : "",
    "cancelConfirmImage" : ""
  },
  "SCHOOL" : {
    "backgroundColor" : "",
    "text" : "",
    "imageUrl" : "",
    "cancelText" : "",
    "cancelConfirmImage" : ""
  },
  "RAILWAY" : {
    "backgroundColor" : "",
    "text" : "",
    "imageUrl" : "",
    "cancelText" : "",
    "cancelConfirmImage" : ""
  }
}