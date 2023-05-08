package in.juspay.mobility.common;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_AUDIO;
import static android.Manifest.permission.READ_MEDIA_IMAGES;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.Manifest;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.PorterDuff;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.icu.util.Calendar;
import android.location.Address;
import android.location.Geocoder;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.provider.MediaStore;
import android.provider.Settings;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.webkit.ConsoleMessage;
import android.webkit.JavascriptInterface;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.DatePicker;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.NumberPicker;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.browser.customtabs.CustomTabsIntent;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.res.ResourcesCompat;
import androidx.core.location.LocationManagerCompat;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.ResolvableApiException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResponse;
import com.google.android.gms.location.LocationSettingsStatusCodes;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Dot;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.MapStyleOptions;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.gms.tasks.Task;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;

public class MobilityCommonBridge extends HyperBridge {

    public static final int REQUEST_CONTACTS = 7;
    public static final int LOCATION_PERMISSION_REQ_CODE = 1;
    public static final int REQUEST_CALL = 8;
    protected static final int STORAGE_PERMISSION = 67;
    //Constants
    private static final String LOCATE_ON_MAP = "LocateOnMap";
    private static final String CURRENT_LOCATION = "ny_ic_customer_current_location";
    private static final String CURRENT_LOCATION_LATLON = "Current Location";
    private static final int LOCATION_RESOLUTION_REQUEST_CODE = 21345;
    private static final int DATEPICKER_SPINNER_COUNT = 3;
    //Maps
    protected static JSONObject markers = new JSONObject();
    protected static GoogleMap googleMap;
    // CallBacks
    protected static String storeLocateOnMapCallBack = null;
    protected static String storeInternetActionCallBack = null;
    protected static String storeDashboardCallBack = null;
    private static Marker userPositionMarker;
    private final FusedLocationProviderClient client;
    protected Polyline polyline = null;
    protected HashMap<String, JSONObject> markersElement = new HashMap<>();
    //Location
    protected double lastLatitudeValue;
    protected double lastLongitudeValue;
    //LOG_TAGS
    protected String MAPS = "MAPS";
    protected String LOCATION = "LOCATION";
    protected String UTILS = "UTILS";
    protected String OTHERS = "OTHERS";
    protected String DTUTILS = "DTUTILS";
    protected String OVERRIDE = "OVERRIDE";
    protected String CALLBACK = "CALLBACK";
    protected String phoneNumber;
    protected String invoice;
    CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
    private int lastFocusedEditView;
    // Others
    private LottieAnimationView animationView;


    public MobilityCommonBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        client = LocationServices.getFusedLocationProviderClient(bridgeComponents.getContext());
    }

    public static boolean isClassAvailable(String className) {
        try {
            Class.forName(className);
            return true;
        } catch (Exception ignored) {
        }
        return false;
    }

    @Override
    public void reset() {

    }

    // region Store and Trigger CallBack
    @JavascriptInterface
    public void storeCallBackInternetAction(String callback) {
        storeInternetActionCallBack = callback;
    }

    public void callInternetActionCallBack(String isPermission) {
        if (storeInternetActionCallBack != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeInternetActionCallBack, isPermission);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }
    // endregion

    // region Location
    @JavascriptInterface
    public void currentPosition(String str) {
        Log.i(LOCATION, "Fetching Current Position");
        showLocationOnMap();
    }

    @JavascriptInterface
    public void requestLocation() {
        if (!isLocationPermissionEnabled()) {
            requestPermission();
        }
        resolvableLocationSettingsReq();
    }

    public void requestPermission() {
        try {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{ACCESS_FINE_LOCATION}, LOCATION_PERMISSION_REQ_CODE);
            }
        } catch (Exception e) {
            Log.e(LOCATION, "Exception in request permission", e);
        }
    }

    @JavascriptInterface
    public boolean isLocationEnabled() {
        LocationManager locationManager = (LocationManager) bridgeComponents.getContext().getSystemService(Context.LOCATION_SERVICE);
        return locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
    }

    @JavascriptInterface
    public boolean isLocationPermissionEnabled() {
        return (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED);
    }

    private void showLocationOnMap() {
        ExecutorManager.runOnMainThread(() -> {
            if (!isLocationPermissionEnabled()) return;
            updateLastKnownLocation(null, true);
        });
    }

    @SuppressLint("MissingPermission")
    protected void updateLastKnownLocation(String callback, boolean animate) {
        if (!isLocationPermissionEnabled()) return;
        if (bridgeComponents.getActivity() != null) {
            client.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                    .addOnSuccessListener(bridgeComponents.getActivity(), location -> {
                        if (location != null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            setKeysInSharedPrefs("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                            if (callback != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                        callback, lat, lng);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                            if (animate && googleMap != null) {
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                } else {
                                    if (storeLocateOnMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                            }
                        } else getLastKnownLocationFromClientFallback(callback, animate);
                    })
                    .addOnFailureListener(bridgeComponents.getActivity(), e -> {
                        Log.e(LOCATION, "Current position not known");
                        getLastKnownLocationFromClientFallback(callback, animate);
                    });
        }

    }

    @SuppressLint("MissingPermission")
    private void getLastKnownLocationFromClientFallback(String callback, boolean animate) {
        if (!isLocationPermissionEnabled()) return;
        if (bridgeComponents.getActivity() != null) {
            if (client != null) 
                client.getLastLocation()
                    .addOnSuccessListener(bridgeComponents.getActivity(), location -> {
                        if (location != null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            setKeysInSharedPrefs("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                            setKeysInSharedPrefs("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                            if (callback != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                        callback, lat, lng);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                            if (animate && googleMap != null) {
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                } else {
                                    if (storeLocateOnMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                            }
                        }
                    })
                    .addOnFailureListener(bridgeComponents.getActivity(), e -> Log.e(LOCATION, "Last and current position not known"));
        }
    }

    @JavascriptInterface
    public void initiateLocationServiceClient() {
        if (!isLocationPermissionEnabled()) return;
        resolvableLocationSettingsReq();
    }

    private void resolvableLocationSettingsReq() {
        LocationRequest locationRequest = createLocReq();

        LocationSettingsRequest.Builder lBuilder = new LocationSettingsRequest.Builder()
                .addLocationRequest(locationRequest)
                .setAlwaysShow(true);

        Task<LocationSettingsResponse> task =
                LocationServices.getSettingsClient(bridgeComponents.getContext()).checkLocationSettings(lBuilder.build());

        task.addOnCompleteListener(task1 -> {
            try {
                task1.getResult(ApiException.class);
            } catch (ApiException exception) {
                switch (exception.getStatusCode()) {
                    case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
                        try {
                            if (bridgeComponents.getActivity() != null) {
                                ResolvableApiException resolvable = (ResolvableApiException) exception;
                                resolvable.startResolutionForResult(bridgeComponents.getActivity(), LOCATION_RESOLUTION_REQUEST_CODE);
                            }
                        } catch (IntentSender.SendIntentException e) {
                            // Ignore the error.
                        } catch (ClassCastException e) {
                            // Ignore, should be an impossible error.
                        }
                        break;
                    case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
                        // Sadly this change is not available in this particular device :(
                        break;
                }
            }
        });
    }

    private LocationRequest createLocReq() {
        return new LocationRequest.Builder(Priority.PRIORITY_HIGH_ACCURACY)
                .setIntervalMillis(1000)
                .setMinUpdateIntervalMillis(500)
                .build();
    }

    @JavascriptInterface
    public void getCurrentPosition(String callback) {
        if (!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, false);
    }

    @JavascriptInterface
    public void isMockLocation(String callback) {
        if (!isLocationPermissionEnabled()) return;
        if (client != null)
            client.getLastLocation()
                .addOnSuccessListener(activity, location -> {
                    boolean isMock = false;
                    if (Build.VERSION.SDK_INT <= 30) {
                        isMock = location.isFromMockProvider();
                        //methodName = "isFromMockProvider";
                    } else if(Build.VERSION.SDK_INT >= 31) {
                        isMock = location.isMock();
                        //methodName = "isMock";
                    }
                    if (callback != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                        String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                callback, isMock);
                        dynamicUI.addJsToWebView(js);
                    }
                })
                .addOnFailureListener(activity, e -> Log.e(LOG_TAG, "Last and current position not known"));
    }
    //endregion

    // region Maps
    @JavascriptInterface
    public void removeMarker(final String title) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (markers.has(title)) {
                    Marker m = (Marker) markers.get(title);
                    m.setVisible(false);
                    Log.i(MAPS, "Removed marker " + title);
                }
            } catch (Exception e) {
                Log.e(MAPS, "Remove Marker error " + title, e);
            }
        });
    }

    @JavascriptInterface
    public void animateCamera(final double lat, final double lng, final float zoom) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (googleMap != null) {
                    LatLng latLngObj = new LatLng(lat, lng);
                    googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLngObj, zoom));
                    Log.i(MAPS, "Animated Camera");
                }
            } catch (Exception e) {
                Log.e(MAPS, "Error while animating camera");
            }
        });
    }

    @JavascriptInterface
    public void upsertMarker(final String title, final String lat, final String lng, final int markerSize, final float anchorV, final float anchorV1) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (lat != null && lng != null) {
                    double latitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lat);
                    double longitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lng);
                    LatLng latLngObj = new LatLng(latitude, longitude);
                    Marker markerObject;
                    if (markers.has(title)) {
                        markerObject = (Marker) markers.get(title);
                        markerObject.setPosition(latLngObj);
                        markerObject.setFlat(true);
                        markerObject.setVisible(true);
                        markerObject.hideInfoWindow();
                        Log.i(MAPS, "Marker position updated for " + title);
                    } else {
                        MarkerOptions markerOptionsObj = makeMarkerObject(title, latitude, longitude, markerSize, anchorV, anchorV1);
                        if (markerOptionsObj != null && googleMap != null) {
                            markerObject = googleMap.addMarker(markerOptionsObj);
                            markers.put(title, markerObject);
                            if (markerObject != null) {
                                markerObject.setPosition(latLngObj);
                                markerObject.setVisible(true);
                                markerObject.setFlat(true);
                                markerObject.hideInfoWindow();
                            }
                            if (title.equals(CURRENT_LOCATION)) {
                                userPositionMarker = markerObject;
                            }
                            Log.i(MAPS, "New marker created and updated for " + title);
                        }
                    }
                }
            } catch (Exception e) {
                Log.i(MAPS, "Marker creation error for " + title, e);
            }
        });
    }

    private MarkerOptions makeMarkerObject(final String title, final double lat, final double lng, final int markerSize, final float anchorV, final float anchorV1) {
        try {
            MarkerOptions markerOptions = new MarkerOptions()
                    .position(new LatLng(lat, lng))
                    .title(title)
                    .anchor(anchorV, anchorV1);
            if (!title.equals(LOCATE_ON_MAP)) {
                Bitmap smallMarker = constructBitmap(markerSize, title);
                markerOptions.icon(BitmapDescriptorFactory.fromBitmap(smallMarker));
            }
            return markerOptions;
        } catch (Exception e) {
            Log.e(MAPS, "MARKER obj creation", e);
            return null;
        }
    }

    protected Bitmap constructBitmap(final int markerSize, final String title) {
        @SuppressLint("DiscouragedApi") int imageID = bridgeComponents.getContext().getResources().getIdentifier(title, "drawable", bridgeComponents.getContext().getPackageName());
        @SuppressLint("UseCompatLoadingForDrawables") BitmapDrawable bitmapdraw = (BitmapDrawable) bridgeComponents.getContext().getResources().getDrawable(imageID);
        Bitmap b = bitmapdraw.getBitmap();
        float maximum = Math.max(b.getWidth(), b.getHeight());
        float multiplier = markerSize / maximum;
        int markerWidth = Math.round(b.getWidth() * multiplier);
        int markerHeight = Math.round(b.getHeight() * multiplier);
        Log.i(MAPS, "real width and height of " + title + b.getWidth() + " , " + b.getHeight());
        Log.i(MAPS, "after width and height of " + title + markerWidth + " , " + markerHeight);
        return Bitmap.createScaledBitmap(b, markerWidth, markerHeight, false);
    }

    @JavascriptInterface
    public void openNavigation(double slat, double slong, double dlat, double dlong) {
        try {
            if (bridgeComponents.getActivity() != null) {
                Uri googleMapsURI = Uri.parse("google.navigation:q=" + dlat + "," + dlong);
                Intent mapIntent = new Intent(Intent.ACTION_VIEW, googleMapsURI);
                mapIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                mapIntent.setPackage("com.google.android.apps.maps");
                bridgeComponents.getActivity().startActivity(mapIntent);
            }
        } catch (Exception e) {
            Log.e(MAPS, "Can't open google maps", e);
        }
    }

    @JavascriptInterface
    public void getLocationName(String latitude, String longitude, String defaultText, String callback) {
        if (!isLocationPermissionEnabled()) return;

        updateLastKnownLocation(null, false);

        if (defaultText.equals(CURRENT_LOCATION_LATLON)) {
            latitude = String.valueOf(lastLatitudeValue);
            longitude = String.valueOf(lastLongitudeValue);
        }

        Geocoder geocoder = new Geocoder(bridgeComponents.getActivity(), Locale.getDefault());
        StringBuilder returnedAddressStrBuilder;
        try {
            List<Address> addresses = geocoder.getFromLocation(Double.parseDouble(latitude), Double.parseDouble(longitude), 1);
            if (addresses != null && addresses.size() > 0) {
                returnedAddressStrBuilder = new StringBuilder();
                Address returnedAddress = addresses.get(0);
                for (int i = 0; i <= returnedAddress.getMaxAddressLineIndex(); i++) {
                    returnedAddressStrBuilder.append(returnedAddress.getAddressLine(i)).append(",");
                }
                Log.d(MAPS, "getLocationName:" + returnedAddressStrBuilder);
            } else {
                returnedAddressStrBuilder = new StringBuilder(defaultText);
                Log.e(MAPS, "Can't fetch current Address");
            }
            if (callback != null) {
                String returnedAddressStr = String.valueOf(returnedAddressStrBuilder);
                returnedAddressStr = returnedAddressStr.replaceAll("'", "");
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                        callback, latitude, longitude, returnedAddressStr);
                Log.d(MAPS, "getCurrent___Position___inside if" + latitude + longitude);
                bridgeComponents.getJsCallback().addJsToWebView(javascript);
            }
        } catch (Exception e) {
            Log.e(MAPS, "Exception occurred in getting Location Name " + e.getMessage());
            e.printStackTrace();
        }
    }

    @SuppressLint({"MissingPermission", "PotentialBehaviorOverride"})
    private void getMapAsync(SupportMapFragment mapFragment, boolean isEnableCurrentLocation, final String mapType, final String callback, final String pureScriptId, final float zoom) {
        if (bridgeComponents.getActivity() != null) {
            mapFragment.getMapAsync(googleMap -> {
                MobilityCommonBridge.googleMap = googleMap;
                googleMap.setMinZoomPreference(7.0f);
                googleMap.setMaxZoomPreference(googleMap.getMaxZoomLevel());
                googleMap.getUiSettings().setRotateGesturesEnabled(false);
                googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                if (!isLocationPermissionEnabled()) {
                    googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
                markers = new JSONObject();
                markersElement.put(pureScriptId, markers);
                googleMap.setOnMarkerClickListener(marker -> {
                    marker.hideInfoWindow();
                    return true;
                });
                try {
                    if (mapType.equals(LOCATE_ON_MAP)) {
                        upsertMarker(LOCATE_ON_MAP, String.valueOf(lastLatitudeValue), String.valueOf(lastLongitudeValue), 160, 0.5f, 0.9f);
                        MobilityCommonBridge.googleMap.setOnCameraMoveListener(() -> {
                            try {
                                double lat = (googleMap.getCameraPosition().target.latitude);
                                double lng = (googleMap.getCameraPosition().target.longitude);
                                upsertMarker(LOCATE_ON_MAP, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f);
                            } catch (Exception e) {
                                Log.i(MAPS, "Marker creation error for ", e);
                            }
                        });
                        MobilityCommonBridge.googleMap.setOnCameraIdleListener(() -> {
                            if (callback != null) {
                                double lat = (googleMap.getCameraPosition().target.latitude);
                                double lng = (googleMap.getCameraPosition().target.longitude);
                                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "LatLon", lat, lng);
                                Log.e(MAPS, javascript);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                        });
                    }
                    setMapCustomTheme();
                    if (lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        MobilityCommonBridge.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, zoom));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                if (callback != null) {
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "MAP", "READY", "LOADED");
                    Log.e(MAPS, javascript);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });
        }
    }

    public void setMapCustomTheme() { // TODO Check for grey boxes and update the json for the same -- SHAILESH GAHLAWAT
        boolean success;
        try {
            success = googleMap.setMapStyle(
                    MapStyleOptions.loadRawResourceStyle(
                            bridgeComponents.getContext(), R.raw.map_style_retro));
            if (!success) {
                Log.e(MAPS, "Style parsing failed.");
            }
        } catch (Resources.NotFoundException e) {
            Log.e(MAPS, "Can't find style. Error: ", e);
        }
    }

    @JavascriptInterface
    @SuppressLint("MissingPermission")
    public void enableMyLocation(boolean isEnableCurrentLocation) {
        try {
            ExecutorManager.runOnMainThread((() -> {
                if (googleMap != null && isLocationPermissionEnabled()) {
                    googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
            }));
        } catch (Exception e) {
            Log.i(MAPS, "Enable My Location on GoogleMap error", e);
        }
    }

    @JavascriptInterface
    public void reallocateMapFragment(final String pureScriptId) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (bridgeComponents.getActivity() != null) {
                    SupportMapFragment mapFragment = (SupportMapFragment) ((FragmentActivity) bridgeComponents.getActivity()).getSupportFragmentManager()
                            .findFragmentById(Integer.parseInt(pureScriptId));
                    if (mapFragment != null) {
                        mapFragment.getMapAsync(googleMap -> {
                            MobilityCommonBridge.googleMap = googleMap;
                            googleMap.getUiSettings().setRotateGesturesEnabled(false);
                            googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                            markers = markersElement.get(pureScriptId);
                        });
                    }
                }
            } catch (Exception e) {
                Log.e("FAILED WHILE REALLOCATING", e.toString());
            }
        });
    }

    @JavascriptInterface
    public void drawRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth, String type, String sourceName, String destinationName) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                PolylineOptions polylineOptions = new PolylineOptions();
                int color = Color.parseColor(trackColor);
                try {
                    System.out.println("inside_drawRoute_try");
                    JSONObject jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    if (coordinates.length() <= 1) {
                        JSONObject coordinate = (JSONObject) coordinates.get(0);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        upsertMarker("ny_ic_auto_map", String.valueOf(lat), String.valueOf(lng), 90, 0.5f, 0.5f);
                        animateCamera(lat, lng, 20.0f);
                        return;
                    }
                    JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                    JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length() - 1);
                    double sourceLat = sourceCoordinates.getDouble("lat");
                    double sourceLong = sourceCoordinates.getDouble("lng");
                    double destLat = destCoordinates.getDouble("lat");
                    double destLong = destCoordinates.getDouble("lng");
                    if (sourceLat != 0.0 && sourceLong != 0.0 && destLat != 0.0 && destLong != 0.0) {
                        moveCamera(sourceLat, sourceLong, destLat, destLong, coordinates);
                    }
                    if (isActual) {
                        for (int i = coordinates.length() - 1; i >= 0; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            polylineOptions.add(new LatLng(lat, lng));
                        }
                    } else {
                        LatLng fromPointObj = new LatLng(sourceLat, sourceLong);
                        LatLng toPointObj = new LatLng(destLat, destLong);
                        polylineOptions.add(toPointObj);
                        polylineOptions.add(fromPointObj);
                    }

                    polyline = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);
                    LatLng sourceLatLng = new LatLng(sourceLat, sourceLong);
                    LatLng destLatLng = new LatLng(destLat, destLong);

                    if (destMarker != null && !destMarker.equals("")) {
                        List<LatLng> points = polylineOptions.getPoints();
                        LatLng dest = points.get(0);
                        MarkerOptions markerObj = new MarkerOptions()
                                .title(destMarker)
                                .position(dest)
                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destinationName, destMarker)));

                        Marker tempmarker = googleMap.addMarker(markerObj);
                        markers.put(destMarker, tempmarker);

                    }
                    if (type.equals("DRIVER_LOCATION_UPDATE")) {
                        System.out.println("inside insert marker");
                        List<LatLng> points = polylineOptions.getPoints();
                        LatLng source = points.get(points.size() - 1);
                        upsertMarker("ny_ic_auto_map", String.valueOf(source.latitude), String.valueOf(source.longitude), 90, 0.5f, 0.5f);
                        Marker currMarker = (Marker) markers.get("ny_ic_auto_map");
                        int index = polyline.getPoints().size() - 1;
                        float rotation = (float) SphericalUtil.computeHeading(polyline.getPoints().get(index), polyline.getPoints().get(index - 1));
                        if (rotation != 0.0) currMarker.setRotation(rotation);
                        currMarker.setAnchor(0.5f, 0.5f);
                        markers.put("ny_ic_auto_map", currMarker);
                    } else if (sourceMarker != null && !sourceMarker.equals("")) {
                        System.out.println("sourcelatlong: " + sourceLatLng);
                        System.out.println("destlatlong: " + destLatLng);
                        List<LatLng> points = polylineOptions.getPoints();
                        LatLng source = points.get(points.size() - 1);
                        MarkerOptions markerObj = new MarkerOptions()
                                .title(sourceMarker)
                                .position(source)
                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceName, sourceMarker)));
                        Marker tempmarker = googleMap.addMarker(markerObj);
                        markers.put(sourceMarker, tempmarker);
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @SuppressLint({"UseCompatLoadingForDrawables", "SetTextI18n"})
    protected Bitmap getMarkerBitmapFromView(String locationName, String imageName) {
        Context context = bridgeComponents.getContext();
        @SuppressLint("InflateParams") View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(R.layout.marker_label_layout, null);
        TextView label = customMarkerView.findViewById(R.id.marker_text);
        if (locationName.equals("")) {
            label.setVisibility(View.GONE);
        } else {
            if (locationName.length() <= 27) {
                label.setText(locationName);
            } else {
                label.setText(locationName.substring(0, 17) + "...");
            }
        }
        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);
        try {
            if (imageName.equals("ny_ic_dest_marker")) {
                pointer.setImageDrawable(ResourcesCompat.getDrawable(context.getResources(), R.drawable.ny_ic_dest_marker, context.getTheme()));
            } else {
                pointer.setImageDrawable(ResourcesCompat.getDrawable(context.getResources(), R.drawable.ny_ic_src_marker, context.getTheme()));
            }
        } catch (Exception e) {
            Log.e("Exception in rendering Image", e.toString());
        }
        customMarkerView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
        customMarkerView.layout(0, 0, customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight());
        customMarkerView.buildDrawingCache();
        Bitmap returnedBitmap = Bitmap.createBitmap(customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight(),
                Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(returnedBitmap);
        canvas.drawColor(Color.WHITE, PorterDuff.Mode.SRC_IN);
        Drawable drawable = customMarkerView.getBackground();
        if (drawable != null)
            drawable.draw(canvas);
        customMarkerView.draw(canvas);
        return returnedBitmap;
    }

    @JavascriptInterface
    public void removeAllPolylines(String str) {
        removeMarker("ny_ic_auto_map");
        removeMarker("ny_ic_src_marker");
        removeMarker("ny_ic_dest_marker");
        ExecutorManager.runOnMainThread(() -> {
            if (polyline != null) {
                polyline.remove();
                polyline = null;
            }
        });
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final JSONArray json_coordinates) {
        ExecutorManager.runOnMainThread(() -> {
            double source_lat, source_lng, destination_lat, destination_lng;

            Log.i(MAPS, "json_coordinates" + json_coordinates);
            ArrayList<Double> all_latitudes = new ArrayList<>();
            ArrayList<Double> all_longitudes = new ArrayList<>();
            for (int i = 0; i < json_coordinates.length(); i++) {
                JSONObject each_json_coordinates;
                try {
                    each_json_coordinates = (JSONObject) json_coordinates.get(i);
                    double lon = each_json_coordinates.getDouble("lng");
                    double lat = each_json_coordinates.getDouble("lat");
                    all_latitudes.add(lat);
                    all_longitudes.add(lon);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
            Log.i(MAPS, "all_latitudes" + (all_latitudes));
            Log.i(MAPS, "all_longitudes" + (all_longitudes));
            double minimum_latitude = Collections.min(all_latitudes);
            double maximum_latitude = Collections.max(all_latitudes);
            double minimum_longitude = Collections.min(all_longitudes);
            double maximum_longitude = Collections.max(all_longitudes);
            Log.i(MAPS, String.valueOf(minimum_latitude));
            Log.i(MAPS, String.valueOf(maximum_latitude));

            if (source_latitude <= destination_latitude) {
                source_lat = minimum_latitude - 1.3 * (maximum_latitude - minimum_latitude);
                destination_lat = maximum_latitude + 0.1 * (maximum_latitude - minimum_latitude);
            } else {
                source_lat = maximum_latitude + 0.1 * (maximum_latitude - minimum_latitude);
                destination_lat = minimum_latitude - 1.3 * (maximum_latitude - minimum_latitude);
            }
            if (source_longitude <= destination_longitude) {
                source_lng = minimum_longitude - 0.09 * (maximum_longitude - minimum_longitude);
                destination_lng = maximum_longitude + 0.09 * (maximum_longitude - minimum_longitude);
            } else {
                source_lng = maximum_longitude + 0.09 * (maximum_longitude - minimum_longitude);
                destination_lng = minimum_longitude - 0.09 * (maximum_longitude - minimum_longitude);
            }
            Log.i(MAPS, "Coordinates Points" + json_coordinates);
            if (googleMap != null) {
                try {
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                    if (json_coordinates.length() < 5) {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400));
                    } else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150));
                    }
                } catch (IllegalArgumentException e) {
                    Log.i(MAPS, "Exception in Move camera" + e);
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                    if (json_coordinates.length() < 5) {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400));
                    } else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150));
                    }
                } catch (Exception e) {
                    Log.i(MAPS, "Exception in Move camera" + e);
                }
            }
        });
    }

    public Polyline setRouteCustomTheme(PolylineOptions options, int color, String style, final int width) {
        PatternItem DOT = new Dot();
        PatternItem GAP = new Gap(10);
        PatternItem DASH = new Dash(20);
        options.width(width);
        List<PatternItem> PATTERN_POLYLINE_DOTTED = Arrays.asList(GAP, DOT);
        List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Collections.singletonList(DASH);
        options.color(color);
        switch (style) {
            case "DASH":
                options.pattern(PATTERN_POLYLINE_DOTTED_DASHED);
                break;
            case "DOT":
                options.pattern(PATTERN_POLYLINE_DOTTED);
                break;
            default:
                break;
        }
        return googleMap.addPolyline(options);
    }
    // endregion

    @JavascriptInterface
    public void showMap(final String pureScriptId, boolean isEnableCurrentLocation, final String mapType, final float zoom, final String callback) {
        try {
            ExecutorManager.runOnMainThread(() -> {
                if (bridgeComponents.getActivity() != null) {
                    SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                    FragmentManager supportFragmentManager = ((FragmentActivity) bridgeComponents.getActivity()).getSupportFragmentManager();
                    FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                    fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                    fragmentTransaction.commitAllowingStateLoss();
                    getMapAsync(mapFragment, isEnableCurrentLocation, mapType, callback, pureScriptId, zoom);
                }
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    }

    // region Shared Preference Utils
    @JavascriptInterface
    public void setKeysInSharedPrefs(String key, String value) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getSdkName(), Context.MODE_PRIVATE);
        sharedPref.edit().putString(key, value).apply();
    }

    @JavascriptInterface
    public String getKeysInSharedPref(String key) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getSdkName(), Context.MODE_PRIVATE);
        return sharedPref.getString(key, "__failed");
    }

    @JavascriptInterface
    public String getKeyInNativeSharedPrefKeys(String key) {
        return getKeysInSharedPref(key);
    }

    @JavascriptInterface
    public void setEnvInNativeSharedPrefKeys(String key, String value) {
        setKeysInSharedPrefs(key,value);
    }

    //endregion

    // region DATE / TIME UTILS
    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.N)
    public void timePicker(final String callback) {
        ExecutorManager.runOnMainThread(() -> {
            final Calendar c = Calendar.getInstance();
            int hour = c.get(Calendar.HOUR_OF_DAY);
            int minute = c.get(Calendar.MINUTE);
            Log.e(DTUTILS, "Time picker called");
            TimePickerDialog timePickerDialog = new TimePickerDialog(bridgeComponents.getActivity(), (timePicker, hourOfDay, minute1) -> {
                if (callback != null) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d);",
                            callback, hourOfDay, minute1);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            }, hour, minute, false);
            timePickerDialog.show();
        });
    }

    @SuppressLint("DiscouragedApi")
    private void reOrderSpinners(DatePickerDialog dialog, char[] dateOrder) {
        if (!dialog.isShowing()) {
            return;
        }

        final int yearId = Resources.getSystem().getIdentifier("year", "id", "android");
        final int monthId = Resources.getSystem().getIdentifier("month", "id", "android");
        final int dayId = Resources.getSystem().getIdentifier("day", "id", "android");
        final int layoutId = Resources.getSystem().getIdentifier("pickers", "id", "android");

        final NumberPicker yearSpinner = dialog.findViewById(yearId);
        final NumberPicker monthSpinner = dialog.findViewById(monthId);
        final NumberPicker daySpinner = dialog.findViewById(dayId);
        final LinearLayout layout = dialog.findViewById(layoutId);

        layout.removeAllViews();
        for (int i = 0; i < DATEPICKER_SPINNER_COUNT; i++) {
            switch (dateOrder[i]) {
                case 'y':
                    layout.addView(yearSpinner);
                    setImeOptions(yearSpinner, i);
                    break;
                case 'm':
                    layout.addView(monthSpinner);
                    setImeOptions(monthSpinner, i);
                    break;
                case 'd':
                    layout.addView(daySpinner);
                    setImeOptions(daySpinner, i);
                    break;
                default:
                    throw new IllegalArgumentException("Invalid DateOrder");
            }
        }
    }

    @SuppressLint("DiscouragedApi")
    private void setImeOptions(NumberPicker spinner, int spinnerIndex) {
        final int imeOption;
        if (spinnerIndex < DATEPICKER_SPINNER_COUNT - 1) {
            imeOption = EditorInfo.IME_ACTION_NEXT;
        } else {
            imeOption = EditorInfo.IME_ACTION_DONE;
        }
        int idPickerInput = Resources.getSystem().getIdentifier("numberpicker_input", "id", "android");
        TextView input = spinner.findViewById(idPickerInput);
        input.setImeOptions(imeOption);
    }

    private static class DatePickerLabels {
        private static final String MAXIMUM_PRESENT_DATE = "MAXIMUM_PRESENT_DATE";
        private static final String MINIMUM_EIGHTEEN_YEARS = "MINIMUM_EIGHTEEN_YEARS";
        private static final String MIN_EIGHTEEN_MAX_SIXTY_YEARS = "MIN_EIGHTEEN_MAX_SIXTY_YEARS";
        private static final String MAX_THIRTY_DAYS_FROM_CURRENT_DATE = "MAX_THIRTY_DAYS_FROM_CURRENT_DATE";
    }
    //endregion

    // region OTHER UTILS

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.N)
    @SuppressLint("DiscouragedApi")
    public void datePicker(final String callback, String label) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                final Calendar c = Calendar.getInstance();
                int mYear = c.get(Calendar.YEAR);
                int mMonth = c.get(Calendar.MONTH);
                int mDate = c.get(Calendar.DATE);
                int datePickerTheme = AlertDialog.THEME_HOLO_LIGHT;
                if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.N) datePickerTheme = 0;

                DatePickerDialog datePickerDialog = new DatePickerDialog(bridgeComponents.getActivity(), datePickerTheme, (datePicker, year, month, date) -> {
                    if (callback != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d,%d);",
                                callback, year, month, date);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                }, mYear, mMonth, mDate) {

                    final int month = getContext().getResources().getIdentifier("month", "id", "android");
                    final String[] monthNumbers =
                            {
                                    "Jan (01)",
                                    "Feb (02)",
                                    "Mar (03)",
                                    "April (04)",
                                    "May (05)",
                                    "June (06)",
                                    "July (07)",
                                    "Aug (08)",
                                    "Sept (09)",
                                    "Oct (10)",
                                    "Nov (11)",
                                    "Dec (12)"
                            };

                    @Override
                    public void onDateChanged(@NonNull DatePicker view, int y, int m, int d) {
                        super.onDateChanged(view, y, m, d);
                        try {
                            if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                                if (month != 0) {
                                    NumberPicker monthPicker = findViewById(month);
                                    if (monthPicker != null) {
                                        monthPicker.setDisplayedValues(monthNumbers);
                                    }
                                }
                            }
                        } catch (Exception e) {
                            Log.e(DTUTILS, "Error in onDateChanged : " + e);
                        }
                    }

                    @Override
                    protected void onCreate(Bundle savedInstanceState) {
                        super.onCreate(savedInstanceState);
                        try {
                            if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                                if (month != 0) {
                                    NumberPicker monthPicker = findViewById(month);
                                    if (monthPicker != null) {
                                        monthPicker.setDisplayedValues(monthNumbers);
                                    }
                                }
                            }
                        } catch (Exception e) {
                            Log.e(DTUTILS, "Error in Date onCreate : " + e);
                        }
                    }
                };

                switch (label) {
                    case DatePickerLabels.MINIMUM_EIGHTEEN_YEARS:
                        Calendar maxDateDOB = Calendar.getInstance();
                        maxDateDOB.set(Calendar.DAY_OF_MONTH, mDate);
                        maxDateDOB.set(Calendar.MONTH, mMonth);
                        maxDateDOB.set(Calendar.YEAR, mYear - 18);
                        datePickerDialog.getDatePicker().setMaxDate(maxDateDOB.getTimeInMillis());
                        break;
                    case DatePickerLabels.MAXIMUM_PRESENT_DATE:
                        datePickerDialog.getDatePicker().setMaxDate(System.currentTimeMillis() - 1000);
                        break;
                }
                if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N)
                    datePickerDialog.setTitle(bridgeComponents.getContext().getString(R.string.select_date));
                else datePickerDialog.setTitle("");
                datePickerDialog.show();
                final char[] dateOrder =
                        {
                                'd',
                                'm',
                                'y'
                        };
                try {
                    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N)
                        reOrderSpinners(datePickerDialog, dateOrder);
                } catch (Exception e) {
                    Log.e(DTUTILS, "Error in reOrdering spinners : " + e);
                }
            }
        });
    }
    //endregion

    // region OTHER UTILS
    @JavascriptInterface
    public String getVersionName() {
        PackageManager manager = bridgeComponents.getContext().getPackageManager();
        PackageInfo info = new PackageInfo();
        try {
            info = manager.getPackageInfo(bridgeComponents.getContext().getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            Log.e(OTHERS, "Exception in get version name" + e);
        }
        return info.versionName;
    }

    @JavascriptInterface
    public int getVersionCode() {
        PackageManager manager = bridgeComponents.getContext().getPackageManager();
        PackageInfo info = new PackageInfo();
        try {
            info = manager.getPackageInfo(bridgeComponents.getContext().getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            Log.e(OTHERS, "Exception in get version code" + e);
        }
        return info.versionCode;
    }

    @JavascriptInterface
    public void toggleLoader(final boolean visible) {
        ExecutorManager.runOnMainThread(() -> {
            if (bridgeComponents.getActivity() != null) {
                @SuppressLint("InflateParams") View loader = bridgeComponents.getActivity().getLayoutInflater().inflate(R.layout.loader, null);
                if (loader != null) {
                    if (visible) {
                        loader.setVisibility(View.VISIBLE);
                        loader.setOnClickListener(v -> { // Added this to prevent invisible touches through the loader
                            System.out.println("LOADER CLICKED");
                        });
                    } else {
                        loader.setVisibility(View.GONE);
                    }
                }
            }
        });
    }

    @JavascriptInterface
    public void loaderText(final String mainMsg, final String subMsg) {
        ExecutorManager.runOnMainThread(() -> {
            @SuppressLint("InflateParams") View loader = LayoutInflater.from(bridgeComponents.getContext()).inflate(R.layout.loader, null, false);
            TextView mainloaderText = loader.findViewById(R.id.loaderMainText);
            TextView subloaderText = loader.findViewById(R.id.loaderSubText);
            mainloaderText.setText(mainMsg);
            subloaderText.setText(subMsg);
        });
    }

    @JavascriptInterface
    public void hideKeyboardOnNavigation(boolean permission) {
        if (bridgeComponents.getActivity() != null) {
            View view = bridgeComponents.getActivity().getCurrentFocus();
            if (view == null) {
                view = new View(bridgeComponents.getActivity());
            }
            InputMethodManager imm = (InputMethodManager) bridgeComponents.getContext().getSystemService(Activity.INPUT_METHOD_SERVICE);
            imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
        }
    }

    @SuppressLint("MissingPermission")
    @JavascriptInterface
    public boolean isNetworkAvailable() {
        ConnectivityManager connectivityManager = (ConnectivityManager) bridgeComponents.getContext().getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo activeNetworkInfo = null;
        if (connectivityManager != null) {
            activeNetworkInfo = connectivityManager.getActiveNetworkInfo();
        }
        return activeNetworkInfo != null && activeNetworkInfo.isConnected();
    }

    @SuppressLint("MissingPermission")
    @JavascriptInterface
    public boolean isInternetAvailable() {
        return ((ConnectivityManager) bridgeComponents.getContext().getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo() != null;
    }

    @JavascriptInterface
    public void attach(String id, String argumentsJson, String callbackFunctionName) {
//        switch (id) {
//            case "SMS_RETRIEVER":
//                detach(new String[]{id});
//                 if(browserFragment != null) {
//                     JuspayDuiHook juspayDuiHook = new OtpUtils(browserFragment, callbackFunctionName);
//                     if (juspayDuiHook != null) {
//                         super.listenerMap.put(id, juspayDuiHook);
//                         juspayDuiHook.attach(activity);
//                     }
//                 }
//                break;
//            default:
//                super.attach(id, argumentsJson, callbackFunctionName);
//        }TODO Handle OTPUtils

    }

    @JavascriptInterface
    public void showDialer(String phoneNum) {
        Intent intent = new Intent();
        intent.setAction(Intent.ACTION_CALL);
        phoneNumber = phoneNum;
        if (bridgeComponents.getActivity() != null && ContextCompat.checkSelfPermission(bridgeComponents.getActivity(), Manifest.permission.CALL_PHONE) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.CALL_PHONE}, REQUEST_CALL);
        } else {
            phoneNumber = "tel:" + phoneNum;
            intent.setData(Uri.parse(phoneNumber));
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);

        }
    }

    @JavascriptInterface
    public void openUrlInApp(String url) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                Intent httpIntent = new Intent(Intent.ACTION_VIEW);
                httpIntent.setData(Uri.parse(url));
                httpIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                bridgeComponents.getContext().startActivity(httpIntent);
            } catch (Exception e) {
                Log.e(UTILS, "Exception occurred while calling WebView", e);
            }
        });
    }

    @JavascriptInterface
    public void requestKeyboardShow(final String id) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (bridgeComponents.getActivity() != null) {
                    int currentId = Integer.parseInt(id);
                    InputMethodManager inputMethodManager = (InputMethodManager) bridgeComponents.getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
                    View editText = bridgeComponents.getActivity().findViewById(currentId);
                    View prevEditText = null;
                    if (lastFocusedEditView != -1) {
                        prevEditText = bridgeComponents.getActivity().findViewById(lastFocusedEditView);
                    }
                    if (inputMethodManager != null && editText != null) {
                        if (prevEditText != null && lastFocusedEditView != currentId) {
                            prevEditText.clearFocus();
                        }
                        editText.requestFocus();
                        inputMethodManager.showSoftInput(editText, InputMethodManager.SHOW_IMPLICIT);
                    }
                    if (currentId != lastFocusedEditView) {
                        lastFocusedEditView = Integer.parseInt(id);
                    }
                }
            } catch (Exception e) {
                Log.e(UTILS, "Keyboard Exception" + e);
            }
        });
    }

    @JavascriptInterface
    public void goBackPrevWebPage(String id) {
        if (bridgeComponents.getActivity() != null) {
            WebView webView = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
            ExecutorManager.runOnMainThread(() -> {
                if (webView == null) return;
                if (webView.canGoBack()) {
                    webView.post(webView::goBack);
                } else {
                    if (storeDashboardCallBack != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                storeDashboardCallBack, "TRUE");
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                }
            });
        }
    }

    @JavascriptInterface
    public void startLottieProcess(String rawJson, String id, boolean repeat, float speed, String scaleType) {
        if (bridgeComponents.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    animationView = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                    animationView.setAnimationFromJson(getJsonFromResources(rawJson), null);
                    animationView.setRepeatCount(ValueAnimator.INFINITE);
                    animationView.setSpeed(speed);
                    animationView.playAnimation();
                    switch (scaleType) {
                        case "MATRIX":
                            animationView.setScaleType(ImageView.ScaleType.MATRIX);
                            break;
                        case "FIT_XY":
                            animationView.setScaleType(ImageView.ScaleType.FIT_XY);
                            break;
                        case "FIT_START":
                            animationView.setScaleType(ImageView.ScaleType.FIT_START);
                            break;
                        case "FIT_END":
                            animationView.setScaleType(ImageView.ScaleType.FIT_END);
                            break;
                        case "CENTER":
                            animationView.setScaleType(ImageView.ScaleType.CENTER);
                            break;
                        case "CENTER_CROP":
                            animationView.setScaleType(ImageView.ScaleType.CENTER_CROP);
                            break;
                        case "CENTER_INSIDE":
                            animationView.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
                            break;
                        default:
                            animationView.setScaleType(ImageView.ScaleType.FIT_CENTER);
                            break;
                    }
                } catch (Exception e) {
                    Log.d(UTILS, "exception in startLottieAnimation", e);
                }
            });
        }
    }

    @SuppressLint("DiscouragedApi")
    private String getJsonFromResources(String rawJson) {
        Writer writer = new StringWriter();
        if (bridgeComponents.getActivity() != null) {
            InputStream inputStreams = bridgeComponents.getActivity().getResources().openRawResource(bridgeComponents.getActivity().getResources().getIdentifier(rawJson, "raw", bridgeComponents.getActivity().getPackageName()));
            char[] buffer = new char[1024];
            try {
                Reader reader = new BufferedReader(new InputStreamReader(inputStreams, StandardCharsets.UTF_8));
                int n;
                while ((n = reader.read(buffer)) != -1) {
                    writer.write(buffer, 0, n);
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                try {
                    inputStreams.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return writer.toString();
    }

    @JavascriptInterface
    public void shareTextMessage(String title, String message) {
        ExecutorManager.runOnMainThread(() -> {
            Intent sendIntent = new Intent();
                sendIntent.setAction(Intent.ACTION_SEND);
                sendIntent.putExtra(Intent.EXTRA_TEXT, message);
                sendIntent.putExtra(Intent.EXTRA_TITLE, title);
                Bitmap thumbnailBitmap = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_icon);
                Uri thumbnailUri = getImageUri(context, thumbnailBitmap);
                ClipData clipData = ClipData.newUri(context.getContentResolver(), "Thumbnail Image", thumbnailUri);
                sendIntent.setClipData(clipData);
                sendIntent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                sendIntent.setType("text/plain");
                Intent shareIntent = Intent.createChooser(sendIntent, null);
                activity.startActivity(shareIntent);
        });
    }

    private Uri getImageUri(Context context, Bitmap bitmap) {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        bitmap.compress(Bitmap.CompressFormat.PNG, 100, bytes);
        String path = MediaStore.Images.Media.insertImage(context.getContentResolver(), bitmap, "Thumbnail Image", null);
        return Uri.parse(path);
    }

    @JavascriptInterface
    public void shareImageMessage(String message, String imageName) {
        ExecutorManager.runOnMainThread(() -> {
            Intent sendIntent = new Intent();
            @SuppressLint("DiscouragedApi") int image = bridgeComponents.getContext().getResources().getIdentifier(imageName, "drawable", bridgeComponents.getContext().getPackageName());
            @SuppressLint("UseCompatLoadingForDrawables") BitmapDrawable bitmapDrawable = (BitmapDrawable) bridgeComponents.getContext().getResources().getDrawable(image);
            Bitmap bitmap = bitmapDrawable.getBitmap();
            Uri uri = Uri.parse(MediaStore.Images.Media.insertImage(bridgeComponents.getContext().getContentResolver(), bitmap, "qrCode", null));
            sendIntent.setAction(Intent.ACTION_SEND);
            sendIntent.putExtra(Intent.EXTRA_STREAM, uri);
            sendIntent.putExtra(Intent.EXTRA_TEXT, message);
            sendIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            sendIntent.setType("image/*");
            Intent shareIntent = Intent.createChooser(sendIntent, null);
            shareIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(shareIntent);
        });
    }

    @JavascriptInterface
    public void adjustViewWithKeyboard(String flag) {
        ExecutorManager.runOnMainThread(() -> {
            if (bridgeComponents.getActivity() != null) {
                if (flag.equals("true"))
                    bridgeComponents.getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);
                else
                    bridgeComponents.getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_NOTHING);
            }
        });
    }

    @JavascriptInterface
    public void copyToClipboard(String inputText) {
        if (bridgeComponents.getActivity() != null) {
            ClipboardManager clipboard = (ClipboardManager) bridgeComponents.getActivity().getSystemService(Context.CLIPBOARD_SERVICE);
            ClipData clip = ClipData.newPlainText("Text", inputText);
            clipboard.setPrimaryClip(clip);
        }
    }

    @JavascriptInterface
    public void initialWebViewSetUp(String callback, String id) {
        storeDashboardCallBack = callback;
        Context context = bridgeComponents.getContext();
        Activity activity = bridgeComponents.getActivity();
        if (activity != null) {
            ExecutorManager.runOnMainThread(new Runnable() {
                @Override
                public void run() {
                    WebView webView = (WebView) activity.findViewById(Integer.parseInt(id));
                    if (webView == null) return;
                    webView.setWebChromeClient(new WebChromeClient() {
                        @Override
                        public boolean onConsoleMessage(ConsoleMessage consoleMessage) {
                            String message = consoleMessage.message();
                            if (message.contains("Write permission denied")) {
                                // Handle the error here
                                ClipboardManager clipboard = (ClipboardManager) context.getSystemService(Context.CLIPBOARD_SERVICE);
                                ClipData clip = ClipData.newPlainText("MyLbl", "https://nammayatri.in/link/rider/SJ8D");
                                clipboard.setPrimaryClip(clip);
                            }
                            return super.onConsoleMessage(consoleMessage);
                        }
                    });
                    webView.setWebViewClient(new WebViewClient() {
                        @Override
                        public boolean shouldOverrideUrlLoading(WebView view, String url) {
                            if (url.startsWith("intent://")) {
                                try {
                                    Intent intent = Intent.parseUri(url, Intent.URI_INTENT_SCHEME);
                                    if (intent != null) {
                                        PackageManager packageManager = context.getPackageManager();
                                        ResolveInfo info = packageManager.resolveActivity(intent, PackageManager.MATCH_DEFAULT_ONLY);
                                        if (info != null) {
                                            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                            context.startActivity(intent);
                                            return true;
                                        }
                                    }
                                } catch (URISyntaxException e) {
                                    e.printStackTrace();
                                }
                            }
                            if (url.startsWith("tg:") || url.startsWith("https://www.facebook.com") || url.startsWith("https://www.twitter.com/") || url.startsWith("https://www.linkedin.com") || url.startsWith("https://api.whatsapp.com") || url.contains("YATRI.pdf") || url.startsWith("https://telegram.me/")) {
                                try {
                                    CustomTabsIntent.Builder builder = new CustomTabsIntent.Builder();
                                    CustomTabsIntent customTabsIntent = builder.build();
                                    customTabsIntent.intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                    customTabsIntent.launchUrl(context, Uri.parse(url));
                                    return true;
                                } catch (Exception e) {
                                    Toast.makeText(context, "Looks like there is no app or web browser installed on your device", Toast.LENGTH_SHORT).show();
                                    return true;
                                }
                            }
                            return false;
                        }
                    });
                }
            });
        }
    }

    @JavascriptInterface
    public void minimizeApp() {
        Intent startMain = new Intent(Intent.ACTION_MAIN);
        startMain.addCategory(Intent.CATEGORY_HOME);
        startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        bridgeComponents.getContext().startActivity(startMain);
    }

    @JavascriptInterface
    public void closeApp() {
        if (bridgeComponents.getActivity() != null) {
            bridgeComponents.getActivity().finish();
        }
    }

    @JavascriptInterface
    public void toast(String msg) {
        Toast.makeText(bridgeComponents.getContext(), msg, Toast.LENGTH_SHORT).show();
    }

    @SuppressLint("SetTextI18n")
    public void enablePermissionFromSettings(@NonNull String permission, String permissionName) {
        if (bridgeComponents.getActivity() != null) {

            if (ActivityCompat.shouldShowRequestPermissionRationale(bridgeComponents.getActivity(), permission)) {
                setKeysInSharedPrefs(permission, "false");
            } else {
                if (getKeysInSharedPref(permission).equals("false")) {
                    try {
                        LayoutInflater inflater = LayoutInflater.from(bridgeComponents.getContext());
                        View permissionStepsView = inflater.inflate(R.layout.permission_steps_layout, null);
                        TextView stepText = permissionStepsView.findViewById(R.id.step_text);

                        AlertDialog.Builder builder = new AlertDialog.Builder(bridgeComponents.getContext());
                        stepText.setText("3. Tap on " + permissionName);
                        builder.setTitle("Permission Required")
                                .setCancelable(true)
                                .setView(permissionStepsView)
                                .setPositiveButton("Go to settings", (dialog, which) -> {
                                    Intent settingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                                    settingsIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                    Uri uri = Uri.fromParts("package", bridgeComponents.getContext().getPackageName(), null);
                                    settingsIntent.setData(uri);
                                    bridgeComponents.getContext().startActivity(settingsIntent);
                                });
                        AlertDialog alert = builder.create();
                        alert.show();
                    } catch (Exception e) {
                        Log.d(OVERRIDE, e.toString());
                    }
                }
            }
        }
    }

    @JavascriptInterface
    public void scrollToBottom(final String id) {
        try {
            if (bridgeComponents.getActivity() != null) {
                ScrollView scrollView = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                if (scrollView != null) {
                    scrollView.fullScroll(View.FOCUS_DOWN);
                }
            }
        } catch (Exception e) {
            Log.e(OTHERS, "Error in scroll to Bottom : " + e);
        }
    }

    protected boolean isStoragePermissionGiven() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
            return (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED);
        } else {
            return true;
        }
    }

    protected void requestStoragePermission() {
        if (bridgeComponents.getActivity() != null) {
            ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE}, STORAGE_PERMISSION);
        }
    }
    // endregion
}