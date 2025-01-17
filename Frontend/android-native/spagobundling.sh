#!/bin/bash
echo " ---------- Customer prod:android :- --------------"
cd atlas-ui-customer
npm run spago:prod:android
cd ..
echo " ---------- Copy index_bundle.js Customer :- --------------"
mkdir -p "app/src/user/js/juspay"
rm -rf app/src/user/js/juspay/index_bundle.js
cp atlas-ui-customer/dist/android/index_bundle.js app/src/user/js/juspay
echo " ---------- Driver prod:android :- --------------"
cd atlas-ui-driver
npm run spago:prod:android
cd ..
echo " ---------- Copy index_bundle.js Driver :- --------------"
mkdir -p "app/src/driver/js/juspay"
rm -rf app/src/driver/js/juspay/index_bundle.js
cp atlas-ui-driver/dist/index_bundle.js app/src/driver/js/juspay
echo " ---------- CreateJSAFiles :- --------------"
rm -rf app/src/driver/assets/juspay/v1-index_bundle.jsa
rm -rf app/src/user/assets/juspay/v1-index_bundle.jsa
rm -rf app/src/main/assets/juspay/v1-config.jsa
./gradlew createJSAFiles