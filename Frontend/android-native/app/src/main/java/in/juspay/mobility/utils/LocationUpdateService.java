/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.KeyguardManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.BitmapFactory;
import android.location.Location;
import android.location.LocationManager;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.PowerManager;
import android.os.SystemClock;
import android.provider.Settings;
import android.service.notification.StatusBarNotification;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.location.LocationManagerCompat;
import androidx.work.WorkManager;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.BuildConfig;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class LocationUpdateService extends Service {
    private static final String LOG_TAG = "LocationServices";
    private String LOCATION_DESCRIPTION = "LOCATION_IS_UPDATING";
    private String LOCATION_UPDATES = "LOCATION_UPDATES";
    private String MAX_LIMIT_TO_STORE_LOCATION_PT = "MAX_LIMIT_TO_STORE_LOCATION_PT";
    private String NO_OF_LOCATION_PT_TO_REMOVE = "NO_OF_LOCATION_PT_TO_REMOVE";
    private static final String LOCATION_PAYLOAD = "LOCATION_PAYLOAD";
    private static String LAST_LOCATION_TIME;
    public static AlarmManager alarmManager;
    final int notificationServiceId = 15082022; // ARDU pilot launch date : DDMMYYYY
    final int alertNotificationId = 07102022;
    private static final int REQUEST_CODE_ALARM = 28022001;
    FusedLocationProviderClient fusedLocationProviderClient;
    LocationRequest locationRequest;
    LocationCallback locationCallback;
    double lastLatitudeValue;
    double lastLongitudeValue;
    double prevLat;
    double prevLon;
    boolean updated;
    private SharedPreferences sharedPrefs;
    private static Boolean isLocationUpdating = false;
    private Timer timer;
    private String driverStatus;
    private String locationGFrequency, locationTFrequency;
    private String minimumDisplacement;
    private String gpsMethodSwitch;
    private Context context;
    private int delayForG = 500000, delayForT = 20000;
    private int delayForGNew = 500000, delayForTNew = 20000;
    private int maximumLimit = 100;
    private int noOfPointsToRemove = 10;
    private float minDispDistanceNew = 25.0f, minDispDistance = 25.0f;
    private TimerTask timerTask;
    // JSONArray payload = new JSONArray();
    CancellationTokenSource cancellationTokenSource;
    static JSONArray metaDataForLocation;
    static JSONObject deviceManufacturer;
    static JSONObject deviceModel;
    static JSONObject batteryPercentage;
    static JSONObject isOnCharge;
    static JSONObject triggerFunction;
    static JSONObject androidVersion;
    enum LocationSource {
        CurrentLocation,
        LastLocation;
    }
    enum TriggerFunction {
        TimerTask,
        GoogleCallback;
    }


    private static ArrayList<LocationUpdateService.UpdateTimeCallback> timeUpdateCallback = new ArrayList<>();

    public interface UpdateTimeCallback{
        public void timeUpdateFlow(String time, String lat, String lng);
    }

    public  static void registerCallback(LocationUpdateService.UpdateTimeCallback timeUpdateCallback){
        LocationUpdateService.timeUpdateCallback.add(timeUpdateCallback);
    }

    public static void deRegisterCallback(LocationUpdateService.UpdateTimeCallback timeUpdateCallback){
        LocationUpdateService.timeUpdateCallback.remove(timeUpdateCallback);
    }

    String versionName = BuildConfig.VERSION_NAME;

    @Override
    public void onCreate()
    {
        super.onCreate();
        boolean updated = updateConfigVariables();
        initialiseJSONObjects();
        context = getApplicationContext();
        isLocationUpdating = false;
        this.startForeground(notificationServiceId, createNotification());
        fusedLocationProviderClient = LocationServices.getFusedLocationProviderClient(this);
        updated = false;
        timer = new Timer();
        resetTimer(delayForGNew, minDispDistanceNew, delayForTNew);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId)
    {
        /* Start the service if the driver is active*/
        startForeground(notificationServiceId, createNotification());
        initialiseJSONObjects();
        updateDeviceDetails();
        boolean updated = updateConfigVariables();
        logEventForHealthCheck(intent);
        if ( delayForGNew != delayForG || minDispDistanceNew != minDispDistance || delayForTNew != delayForT){
            resetTimer(delayForGNew, minDispDistanceNew, delayForTNew);
        } else if( !isLocationUpdating){
            startLocationUpdates(delayForGNew, minDispDistanceNew, delayForTNew);
        }
        return START_STICKY;
    }

    private void updateDeviceDetails(){
        try {
            updateJSONObject(deviceManufacturer, "deviceManufacturer", Build.MANUFACTURER);
            updateJSONObject(androidVersion, "androidVersion", Build.VERSION.SDK_INT);
            updateJSONObject(deviceModel, "deviceModel", Build.MODEL);
            metaDataForLocation.put(deviceManufacturer);
            metaDataForLocation.put(deviceModel);
            metaDataForLocation.put(androidVersion);
        } catch (JSONException e) {
            e.printStackTrace();
            Log.d("LOG_TAG", "Unable to put data in metaData " + e);
        }
    }

    private void initialiseJSONObjects(){
        metaDataForLocation = new JSONArray();
        deviceManufacturer = new JSONObject();
        deviceModel = new JSONObject();
        batteryPercentage = new JSONObject();
        isOnCharge = new JSONObject();
        triggerFunction = new JSONObject();
        androidVersion = new JSONObject();
    }

    private void updateJSONObject(JSONObject obj, String key, String value) throws JSONException {
        obj.put("key",key);
        obj.put("value",value);
    }

    private void updateJSONObject(JSONObject obj, String key, int value) throws JSONException {
        obj.put("key",key);
        obj.put("value",value);
    }

    private void updateJSONObject(JSONObject obj, String key, boolean value) throws JSONException {
        obj.put("key",key);
        obj.put("value",value);
    }

    private boolean updateConfigVariables(){
        try{
            sharedPrefs = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            driverStatus = sharedPrefs.getString("DRIVER_STATUS", "__failed");

            maximumLimit = Integer.parseInt(sharedPrefs.getString(MAX_LIMIT_TO_STORE_LOCATION_PT, "100"));
            noOfPointsToRemove = Integer.parseInt(sharedPrefs.getString(NO_OF_LOCATION_PT_TO_REMOVE, "1"));
            // UPDATE FOR GOOGLE CALLBACK FREQUENCY
            locationGFrequency = sharedPrefs.getString("RIDE_G_FREQUENCY", null);
            delayForGNew = locationGFrequency != null ? Integer.parseInt(locationGFrequency) : 50000;

            // UPDATE FOR TIMER TASK FREQUENCY
            locationTFrequency = sharedPrefs.getString("RIDE_T_FREQUENCY", null);
            delayForTNew = locationTFrequency != null ? Integer.parseInt(locationTFrequency) : 20000;

            //UPDATE FOR GOOGLE MIN DISPLACEMENT VALUE
            minimumDisplacement = sharedPrefs.getString("DRIVER_MIN_DISPLACEMENT", null);
            minDispDistanceNew = minimumDisplacement != null ? Float.parseFloat(minimumDisplacement) : 30.0f;
            String gpsMethod = sharedPrefs.getString("GPS_METHOD", null);
            gpsMethodSwitch = gpsMethod != null ? gpsMethod : "CURRENT";

            cancellationTokenSource = new CancellationTokenSource();
            return true;
        }catch (Exception exception){
            return false;
        }
    }

    private void resetTimer(int delayInMilliS, float minDisplacement, int delayForTinMillis)
    {
        cancelTimer();
        if (fusedLocationProviderClient != null && locationCallback != null) {
            fusedLocationProviderClient.removeLocationUpdates(locationCallback);
        }
        startLocationUpdates(delayInMilliS, minDisplacement, delayForTinMillis);
    }

    @Override
    public void onDestroy ()
    {
        super.onDestroy();
        isLocationUpdating = false;
        cancelTimer();
        if (fusedLocationProviderClient != null && locationCallback != null) {
            fusedLocationProviderClient.removeLocationUpdates(locationCallback);
        }
        stopForeground(true);
        stopSelf();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

// MAIN FUNCTIONS

    @SuppressLint("MissingPermission")
    private void startLocationUpdates(int delayInMilliS, float minDisplacement, int delayInTMillis)
    {
        if (!isLocationUpdating) {
            delayForG = delayInMilliS;
            minDispDistance = minDisplacement;
            delayForT = delayInTMillis;
            isLocationUpdating = true;
            checkLocation(); // Checking the location permissions and status(on or off)

            // Starting fusedLocationProviderClient callback
            LocationCallback lc = getLocationCallback();
            LocationRequest lr = createLocationRequest(delayForGNew, minDispDistanceNew);
            fusedLocationProviderClient.requestLocationUpdates(lr, lc, Looper.getMainLooper());

            TimerTask tt = createTimer();
            if (timer != null) {
                timer.scheduleAtFixedRate(tt, 0, delayForT);
            }
        }
    }

    /* To update driver status if location is disabled. To prevent false location updates*/
    private void updateDriverStatus(Boolean status) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        executor.execute(() ->
        {
            StringBuilder result = new StringBuilder();
            System.out.println("updateDriverStatus");
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            String bundle_version = sharedPref.getString("BUNDLE_VERSION","null");
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
            try
            {
                //endPoint for driver status
                String orderUrl = baseUrl + "/driver/setActivity?active=" + status;
                Log.d(LOG_TAG, "orderUrl " + orderUrl);
                //Http connection to make API call
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", versionName);
                connection.setRequestProperty("token", token);
                connection.setRequestProperty("x-bundle-version", bundle_version);
                connection.setRequestProperty("x-device", deviceDetails);
                connection.setDoOutput(true);
                connection.connect();

                // validating the response code
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;
                Log.d(LOG_TAG, "respCode "+ respCode);

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    Log.d(LOG_TAG, "in error "+ respReader);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    Log.d(LOG_TAG, "in 200 "+ respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                updateStorage("LOCATION_STATUS","PAUSE");
                updateStorage("DRIVER_STATUS","__failed");
                Log.d(LOG_TAG, "in result "+ result);
                showAlertNotification(); // To notify the driver that he is offline.
                cancelTimer();
                LocationUpdateAlarm locationUpdateAlarm = new LocationUpdateAlarm();
//                locationUpdateAlarm.stopAlarm(context);
                WorkManager mWorkManager = WorkManager.getInstance(context);
                mWorkManager.cancelAllWorkByTag(context.getString(R.string.location_update));
                onDestroy();
                executor.shutdown();
            }
            catch (Exception error)
            {
                Log.d(LOG_TAG, "Catch in updateDriverStatus : "+error);
            }
        });
    }

    /*Location update API call*/
    private void callDriverCurrentLocationAPI(double latitude, double longitude, float accuracy, String locTime, String log, String locationSource, String triggerFunctionValue) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() ->
        {
            StringBuilder result = new StringBuilder();
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);

            try {
                BatteryManager bm = (BatteryManager) context.getSystemService(BATTERY_SERVICE);
                int batteryPercentageValue = bm.getIntProperty(BatteryManager.BATTERY_PROPERTY_CAPACITY);
                boolean isCharging = bm.isCharging();
                updateJSONObject(batteryPercentage, "batteryPercentage", batteryPercentageValue);
                updateJSONObject(isOnCharge, "isOnCharge", isCharging);
                updateJSONObject(triggerFunction, "triggerFunction", triggerFunctionValue);
            } catch (JSONException e) {
                e.printStackTrace();
                Log.d("LOG_TAG", "Json exception while putting data in metaData" + e);
            }

            String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            String makeNullAPICall = sharedPref.getString("MAKE_NULL_API_CALL", "NO");
            String demoModePassword = sharedPref.getString("DEMO_MODE_PASSWORD", "null");
            String isDemoModeEnabled = sharedPref.getString("IS_DEMOMODE_ENABLED", "null");
            String bundle_version = sharedPref.getString("BUNDLE_VERSION","null");
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
            String bufferedLocationObjects = sharedPref.getString(LOCATION_PAYLOAD,null);
            JSONArray locationPayload = null;
            if(bufferedLocationObjects!= null)
            {
                try { locationPayload = new JSONArray(bufferedLocationObjects);
                    if (locationPayload.length() >= maximumLimit && noOfPointsToRemove < maximumLimit)
                    {
                        for(int i =0; i<noOfPointsToRemove; i++)
                        {
                            locationPayload.remove(i);
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                    Log.d("LOG_TAG", "Unable to parse buffered Locations from sharedPref" + e);
                    locationPayload = new JSONArray();
                }
            }
            else
            {
                locationPayload = new JSONArray();
            }
            JSONArray metaData = null;
            try {
                metaData = new JSONArray(metaDataForLocation.toString());
            } catch (JSONException e) {
                metaData = new JSONArray();
            }
            metaData.put(batteryPercentage);
            metaData.put(isOnCharge);
            metaData.put(triggerFunction);
            try {
                // endPoint for location update
                if (!token.equals("__failed")) {
                    String orderUrl = baseUrl + "/driver/location";
                    System.out.println("LOCATION_UPDATE: Log by " + log + "baseUrl - " + orderUrl);

                    final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                    f.setTimeZone(TimeZone.getTimeZone("UTC"));
                    String getCurrTime = f.format(new Date());


                    // Http connection for making API call
                    HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                    if (connection instanceof HttpsURLConnection)
                        ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                    connection.setRequestMethod("POST");
                    connection.setRequestProperty("Content-Type", "application/json");
                    connection.setRequestProperty("x-client-version", versionName);
                    connection.setRequestProperty("source", log);
                    connection.setRequestProperty("token", token);
                    connection.setRequestProperty("x-bundle-version", bundle_version);
                    connection.setRequestProperty("x-device", deviceDetails);
                    connection.setDoOutput(true);

                    // Request Body for API call
                    JSONObject point = new JSONObject();
                    JSONObject locationData = new JSONObject();
                    if (isDemoModeEnabled.equals("true")){
                        switch (demoModePassword){
                            case "7891234" :
                                point.put("lat", 13.311895563147432);
                                point.put("lon", 76.93981481869986);
                                break;
                            case "8917234" :
                                point.put("lat", 13.260559676317829);
                                point.put("lon", 76.4785809882692);
                                break;
                            case "9178234" :
                                point.put("lat", 13.160550263780683);
                                point.put("lon", 76.66727044721313);
                                break;
                            case "1789234" :
                                point.put("lat", 12.522069908884921);
                                point.put("lon", 76.89518072273476);
                                break;
                            default:
                                point.put("lat", latitude);
                                point.put("lon", longitude);
                                updateStorage(LAST_LOCATION_TIME,locTime);
                                break;
                        }
                    }
                    else {
                        if(latitude == 0.0 || longitude == 0.0){
                           if (makeNullAPICall.equals("NO")) return;
                           point.put("lat", null);
                           point.put("lon", null);
                        } else{
                            point.put("lat", latitude);
                            point.put("lon", longitude);
                            updateStorage(LAST_LOCATION_TIME,locTime);
                        }

                    }

                    locationData.put("pt", point);
                    locationData.put("ts", locTime);
                    locationData.put("acc", accuracy);
                    locationData.put("source", locationSource);
                    if(metaData.length()!=0) locationData.put("metaData",metaData);
                    if(locationData == null) return;
                    if(!locationData.has("pt")) return;
                    locationPayload.put(locationData);
                    if (locationPayload==null) return;
                    updateStorage(LOCATION_PAYLOAD,locationPayload.toString());
//                    payload.put(locationData);
//                    if(payload == null) return;
                    Log.d("LOG_TAG", "Location payload for API call" + locationPayload);
                    System.out.println("LOCATION_UPDATE: PAYLOAD CREATED :- "+ locationPayload);
                    Log.d(LOG_TAG, "condition 1  ::: " + (token.equals("__failed")));
                    Log.d(LOG_TAG, "condition 2  ::: " + !(token.equals("__failed")));
                    Log.d(LOG_TAG, "condition 3  ::: " + sharedPref.getString("REGISTERATION_TOKEN", "null"));
                    OutputStream stream = connection.getOutputStream();
                    stream.write(locationPayload.toString().getBytes());
                    connection.connect();

                    int respCode = connection.getResponseCode();
                    InputStreamReader respReader;
                    Log.d(LOG_TAG, "respCode " + respCode);

                    if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                        respReader = new InputStreamReader(connection.getErrorStream());
                        System.out.println("LOCATION_UPDATE: ERROR API respReader :- " + respReader);
                        Log.d(LOG_TAG, "in error " + respReader);
                    } else {
                        respReader = new InputStreamReader(connection.getInputStream());
                        Log.d(LOG_TAG, "in 200 " + respReader);
                        System.out.println("LOCATION_UPDATE: SUCCESS API respReader :- " + respReader);
//                        payload = new JSONArray();
                        updateStorage(LOCATION_PAYLOAD,new JSONArray().toString());
                        for (int i = 0; i < timeUpdateCallback.size(); i++) {
                            timeUpdateCallback.get(i).timeUpdateFlow(getCurrTime, String.valueOf(latitude), String.valueOf(longitude));
                        }
                    }
                    BufferedReader in = new BufferedReader(respReader);
                    String inputLine;
                    while ((inputLine = in.readLine()) != null) {
                        result.append(inputLine);
                    }
                    System.out.println("LOCATION_UPDATE: API result :- " + result);
                    Log.d(LOG_TAG, "in result OVERALL " + result.toString());
                }
            } catch (Exception ignored) {
                Log.d(LOG_TAG, "Catch in callDriverCurrentLocationAPI : " + ignored);
            }

            handler.post(() -> {
                try {
                    JSONObject resp = new JSONObject(String.valueOf(result));
                    if (resp.has("errorCode") && resp.get("errorCode").equals("INVALID_TOKEN")) {
                        System.out.println("Inisde Invalid token " + resp.get("errorCode"));
                        updateStorage("REGISTERATION_TOKEN", "__failed");
                        if (timer != null)
                            timer.cancel();
                        //cancel alarmManager
                        LocationUpdateAlarm locationUpdateAlarm = new LocationUpdateAlarm();
                        locationUpdateAlarm.stopAlarm(this);
                        onDestroy();
                        executor.shutdown();
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            });
        });
    }

    // HELPER FUNCTIONS
    /* creates location request */
    private LocationRequest createLocationRequest (int intervalForLocationUpdate, float minDispDistance) {
        LocationRequest mLocationRequest = new LocationRequest.Builder(Priority.PRIORITY_HIGH_ACCURACY)
                .setIntervalMillis(intervalForLocationUpdate)
                .setMinUpdateDistanceMeters(minDispDistance)
                .build();
        return mLocationRequest;
    }

    /*Creating channel for sticky notification*/
    private void createNotificationChannel(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = LOCATION_SERVICE ;
            String description = LOCATION_DESCRIPTION;
            NotificationChannel channel = new NotificationChannel(LOCATION_UPDATES, name, NotificationManager.IMPORTANCE_MIN);
            channel.setDescription(description);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    /* returns notification for foreground services */
    private Notification createNotification()
    {
        createNotificationChannel();
        Intent notificationIntent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, LOCATION_UPDATES)
                        .setContentTitle("Updating")
                        .setContentText(getString(R.string.your_location_is_being_updated))
                        .setSmallIcon(R.drawable.ny_ic_launcher)
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    /* Creating alert notification to notify that he is offline */
    private void showAlertNotification()
    {
        System.out.println("Notification");
        Intent notificationIntent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, alertNotificationId, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context,"General") ;
        mBuilder.setLargeIcon(BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_launcher));
        mBuilder.setContentTitle(getString(R.string.we_made_you_offline))
                .setSmallIcon((R.drawable.ny_ic_launcher))
                .setContentText(getString(R.string.location_is_turned_off_permission_is_disabled))
                .setAutoCancel(true)
                .setPriority(NotificationCompat.PRIORITY_MAX);
        mBuilder.setContentIntent(pendingIntent);
        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        notificationManager.notify(alertNotificationId, mBuilder.build());

        startGPSListeningService();
    }

    private void startGPSListeningService() {
        Intent gpsListeningService = new Intent(this, GpsListeningService.class);
        gpsListeningService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            this.getApplicationContext().startForegroundService(gpsListeningService);
        }else {
            this.startService(gpsListeningService);
        }
    }

    /* creates the Location callback  */
    private LocationCallback getLocationCallback ()
    {
        System.out.println("LOCATION_UPDATE: Created Location CallBack");
        locationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(LocationResult locationResult) {
                super.onLocationResult(locationResult);
                Location lastLocation = locationResult.getLastLocation();
                if (lastLocation != null) {
                    updated = true;
                    Log.e("startLocationUpdates", lastLocation.getLatitude() + "/" + lastLocation.getLongitude());
                    Double lat = lastLocation.getLatitude();
                    Double lng = lastLocation.getLongitude();
                    float acc = lastLocation.getAccuracy();
                    lastLatitudeValue = lat;
                    lastLongitudeValue = lng;
                    long locTimeMilliSeconds = lastLocation.getTime();
                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                    Date locTime = new Date(locTimeMilliSeconds);
                    String thisLocationTimeStamp = sdf.format(locTime);
                    boolean isLocationUpdateValid = compareCurrentAndLastTimestamp(thisLocationTimeStamp, sdf);
                    if(isLocationUpdateValid)
                    {
                        updateStorage("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                        updateStorage("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                        callDriverCurrentLocationAPI(lat, lng, acc, thisLocationTimeStamp, "fused_location_callback", LocationSource.LastLocation.toString(), TriggerFunction.GoogleCallback.toString());
                    }
                    prevLat = lastLatitudeValue;
                    prevLon = lastLongitudeValue;
                }
            }
        };
        return locationCallback;
    }
    /* check all the cases of location permission */
    private void checkLocation()
    {
        if (ActivityCompat.checkSelfPermission(context, ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            showAlertNotification();
            onDestroy();
            return;
        }
        else if (!isLocationEnabled())
        {
            updateDriverStatus(false);
            return;
        }
        else
        {
            NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
            StatusBarNotification[] currentNotifications = notificationManager.getActiveNotifications();
            for (StatusBarNotification currentNotification : currentNotifications) {
                if (currentNotification.getId() == alertNotificationId)
                {
                    notificationManager.cancel(alertNotificationId);
                }
            }
        }
    }

    /*  returns true if location enabled
        return false or if location disabled    */
    private boolean isLocationEnabled ()
    {
        LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        return locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
    }

    /* create timer task */
    @SuppressLint("MissingPermission")
    private TimerTask createTimer()
    {
        System.out.println("LOCATION_UPDATE: Created Timer");
        timer = new Timer();
        /* triggering the location update explicitly if we are not getting any updates for 5sec */
        timerTask = new TimerTask() {
            @Override
            public void run() {
                driverStatus = sharedPrefs!=null ? sharedPrefs.getString("DRIVER_STATUS", "__failed") : "";
//                    if (updated == true && lastUpdatedTime != null && checkIfUpdateRequired(lastUpdatedTime)) { NEED TO HANDLE THIS MORE CONFIDENTLY
//                        if (timer != null)
//                            timer.cancel();
//                        updated = false;
//                    } else
                if (timerTask != null) {
                    System.out.println("LOCATION_UPDATE: Created Timer where not null");
                    System.out.println("Timer now is : " + timer);
                    timer = new Timer();
                    checkLocation();
                    if(gpsMethodSwitch.equals("CURRENT")){
                        System.out.println("LOCATION_UPDATE: CURRENT LOCATION FETCHED BY GPS");
                        fusedLocationProviderClient.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                                .addOnSuccessListener(new OnSuccessListener<Location>() {
                                    @Override
                                    public void onSuccess(Location location) {
                                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                                        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                                        if (location != null) {
                                            long locTimeMilliSeconds = location.getTime();
                                            Date locTime = new Date(locTimeMilliSeconds);
                                            String thisLocationTimeStamp = sdf.format(locTime);
                                            boolean isLocationUpdateValid = compareCurrentAndLastTimestamp(thisLocationTimeStamp, sdf);
                                            if(isLocationUpdateValid)
                                            {
                                                updateStorage("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                                                updateStorage("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                                                callDriverCurrentLocationAPI(location.getLatitude(), location.getLongitude(), location.getAccuracy(), thisLocationTimeStamp, "timer_task", LocationSource.CurrentLocation.toString(), TriggerFunction.TimerTask.toString());
                                            }
                                        }
                                       else {
                                           System.out.println("LOCATION_UPDATE: CURRENT LOCATION IS NULL");
                                           callDriverCurrentLocationAPI(0.0,0.0, 0, sdf.format(new Date()), "timer_task_null_location", LocationSource.CurrentLocation.toString(), TriggerFunction.TimerTask.toString() );
                                       }
                                    }
                                })
                                .addOnFailureListener(new OnFailureListener() {
                                    @Override
                                    public void onFailure(@NonNull Exception e) {
                                        e.printStackTrace();
                                    }
                                });
                    }else{
                        System.out.println("LOCATION_UPDATE: LAST KNOWN LOCATION FETCHED BY GPS");
                        fusedLocationProviderClient.getLastLocation()
                                .addOnSuccessListener(new OnSuccessListener<Location>() {
                                    @Override
                                    public void onSuccess(Location location) {
                                        if (location != null) {
                                            updateStorage("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                                            updateStorage("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                                            long locTimeMilliSeconds = location.getTime();
                                            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                                            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                                            Date locTime = new Date(locTimeMilliSeconds);
                                            String thisLocationTimeStamp = sdf.format(locTime);
                                            boolean isLocationUpdateValid = compareCurrentAndLastTimestamp(thisLocationTimeStamp, sdf);
                                            if(isLocationUpdateValid)
                                            {
                                                callDriverCurrentLocationAPI(location.getLatitude(), location.getLongitude(), location.getAccuracy(), thisLocationTimeStamp, "COMING FROM TIMER", LocationSource.LastLocation.toString(), TriggerFunction.TimerTask.toString());
                                            }
                                        }
                                    }
                                })
                                .addOnFailureListener(new OnFailureListener() {
                                    @Override
                                    public void onFailure(@NonNull Exception e) {
                                        e.printStackTrace();
                                    }
                                });
                    }

                    System.out.println("Inside else of handler");
                }
            }
        };
        return timerTask;
    }

    // to cancel timer
    private void cancelTimer()
    {
        System.out.println("LOCATION_UPDATE: CANCEL TIMER CALLED INSIDE");
        if (timer != null){
            timer.cancel();
            timer.purge();
        }
        if (timerTask != null)
            timerTask.cancel();
        timer = null;
        timerTask = null;
        isLocationUpdating = false;
    }

    private boolean isScreenLocked()
    {
        Context context = getApplicationContext();
        PowerManager powerManager = (PowerManager)context.getSystemService(Context.POWER_SERVICE);
        KeyguardManager myKM = (KeyguardManager) context.getSystemService(Context.KEYGUARD_SERVICE);
        boolean isPhoneLocked = myKM.inKeyguardRestrictedInputMode();
        return  isPhoneLocked || !(Build.VERSION.SDK_INT < 20? powerManager.isScreenOn():powerManager.isInteractive());
    }

    private String getLastLocationTimeStamp ()
    {
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        return sharedPref.getString(LAST_LOCATION_TIME, null);
    }

    private boolean compareCurrentAndLastTimestamp(String currentTimeStamp, SimpleDateFormat sdf)
    {
        String lastLocationTimeStamp = getLastLocationTimeStamp();
        if(lastLocationTimeStamp!= null)
        {
            try {
                Date currentTimestamp = sdf.parse(currentTimeStamp);
                Date lastTimestamp = sdf.parse(lastLocationTimeStamp);
                if(currentTimestamp.compareTo(lastTimestamp) > 0) return true;
                else return false;
            } catch (Exception e) {
                return true;
            }
        }
        return true;
    }

    private void logEventForHealthCheck(Intent intent) {
        if (intent!=null){
            String serviceStartingSource = intent.getStringExtra("StartingSource");
            if (serviceStartingSource!=null){
                if (serviceStartingSource.equals("TRIGGER_SERVICE")){
                    FirebaseAnalytics.getInstance(this).logEvent("service_triggered_by_health_check",new Bundle());
                }
            }
        }
    }

    private void updateStorage(String key, String value){
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString(key, value);
        editor.apply();
    }

}