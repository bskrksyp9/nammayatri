package in.juspay.mobility.utils;

import android.app.Activity;
import android.content.Context;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Handler;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;
import androidx.constraintlayout.widget.ConstraintLayout;
import org.json.JSONException;
import org.json.JSONObject;
import java.util.ArrayList;
import in.juspay.mobility.R;

public class InAppNotification extends AppCompatActivity {
    private ConstraintLayout mainLayout ;
    private ArrayList<String> notificationStack = new ArrayList<String>();
    private JSONObject notificationChannels = new JSONObject();
    private  Activity activity;
    private Context context;

    public InAppNotification(Activity activity) {
        this.activity =  activity;
        this.context = activity.getApplicationContext();
        mainLayout = (ConstraintLayout) activity.findViewById(R.id.main_layout);
    }
    public  void generateNotification(String title , String message , String action1Text , String action2Text , String action1Image , String action2Image , String channelId , int durationInMilliSeconds) throws JSONException {
        Notification notification;
        // if channel id is not in our channels then we will create new channelId and attach layout for this channelId
        if( !notificationChannels.has(channelId)){
            notification = new Notification(channelId);
            notification.attachEventListenerToNotification();

            notificationChannels.put(channelId , notification);
            // adding new notification to the main layout .
            mainLayout.addView(notification.view);
        }
        else{
            notification = (Notification) notificationChannels.get(channelId);
        }

        if(!notification.equals(null)){
            notification.bringToFront();
            // if stack of notification is empty or the notification ( channelId ) which is visible on the front is not equals to new channelId then we will start animation else we will just change the content .
            if(notificationStack.isEmpty() || !notificationStack.get(notificationStack.size()-1).equals(channelId)){
                notification.view.startAnimation(AnimationUtils.loadAnimation(context, R.anim.top_to_bottom));
                notificationStack.remove(channelId);
                notificationStack.add(channelId);
                notification.view.getAnimation().setAnimationListener(new Animation.AnimationListener() {
                    @Override
                    public void onAnimationStart(Animation animation) {

                    }
                    @Override
                    public void onAnimationEnd(Animation animation)  {
                        try {
                            refreshView();
                        } catch (JSONException e) {
                            throw new RuntimeException(e);
                        }
                    }
                    @Override
                    public void onAnimationRepeat(Animation animation) {

                    }
                });
            }
            notification.setContent(title,message,action1Text,action2Text , action1Image,action2Image);
            notification.handleNotificationHandler(durationInMilliSeconds);
            notification.ring();
        }
    }

    private void refreshView() throws JSONException {
        int limit = Integer.min(2,notificationStack.size()-1);
        for(int i=0;i<notificationStack.size();i++){
            Notification curr = (Notification) notificationChannels.get(notificationStack.get(i));
            TextView counterView = curr.view.findViewById(R.id.count);
            counterView.setVisibility(View.GONE);
            int factor = Integer.min(i,limit);
            int finalPaddingTop = curr.paddingTop + 15*(limit-factor);
            int finalLeftPadding = curr.paddingLeft + 8*(limit-factor);
            int finalRightPadding = curr.paddingRight + 8*(limit-factor);
            curr.view.findViewById(R.id.notification_layout).setPadding(finalLeftPadding,finalPaddingTop,finalRightPadding,0);
            if(i==notificationStack.size()-1 && i>0){
                counterView.setVisibility(View.VISIBLE);
                counterView.setText("+ " + (notificationStack.size()-1));
            }
        }
    }

    private class Notification {
        private View view ;
        private Handler handler;
        private String channelId ;
        private int paddingTop ;
        private int paddingLeft ;
        private int paddingRight ;
        private Notification(String channelId){
            this.view= activity.getLayoutInflater().inflate(R.layout.app_notification,null);
            ConstraintLayout.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.WRAP_CONTENT,ConstraintLayout.LayoutParams.WRAP_CONTENT);
            this.view.setLayoutParams(layoutParams);
            this.handler = new Handler();
            this.channelId = channelId;
            this.paddingTop = view.getPaddingBottom();
            this.paddingLeft = view.getPaddingLeft();
            this.paddingRight = view.getPaddingRight();
            TextView counterView = view.findViewById(R.id.count);
            counterView.setVisibility(View.GONE);
        }

        private void setContent(String title , String message , String action1Text , String action2Text , String action1Image , String action2Image){
            TextView titleView = view.findViewById(R.id.title);
            TextView descriptionView = view.findViewById(R.id.desc);
            TextView action1TextView = view.findViewById(R.id.action1_text);
            TextView action2TextView = view.findViewById(R.id.action2_text);
            ImageView action1ImageView = view.findViewById(R.id.action1_image);
            ImageView action2ImageView = view.findViewById(R.id.action2_image);
            View action1View = view.findViewById(R.id.first_action_button);
            View action2View = view.findViewById(R.id.second_action_button);
            if(action1Text.length()>0 && action1Image.length()>0){
                action1TextView.setText(action1Text);
                action1ImageView.setImageResource(activity.getResources().getIdentifier(action1Image, "drawable", context.getPackageName()));
            }else{action1View.setVisibility(View.GONE);}

            if(action2Text.length()>0 && action2Image.length()>0){
                action2TextView.setText(action2Text);
                action2ImageView.setImageResource(activity.getResources().getIdentifier(action2Image, "drawable", context.getPackageName()));
            }else{action2View.setVisibility(View.GONE);}

            titleView.setText(title);
            descriptionView.setText(message);
        }

        private void bringToFront(){
            view.bringToFront();
        }

        private void attachEventListenerToNotification() {
            view.findViewById(R.id.cross).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    view.startAnimation(AnimationUtils.loadAnimation(context, R.anim.bottom_to_top));
                    mainLayout.removeView(view);
                    notificationChannels.remove(channelId);
                    notificationStack.remove(channelId);
                    Handler timerHandler = null;
                    timerHandler = (Handler) handler;
                    timerHandler.removeCallbacksAndMessages(null);
                    try {
                        refreshView();
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                }
            });
            view.findViewById(R.id.first_action_button).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Toast.makeText(context, "First Action Button is Clicked", Toast.LENGTH_SHORT).show();
                }
            });

            view.findViewById(R.id.second_action_button).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Toast.makeText(context, "Second Action Button is Clicked", Toast.LENGTH_SHORT).show();
                }
            });
        }

        private void handleNotificationHandler(int durationInMilliSeconds){
            // removing all previous postDelay .
            handler.removeCallbacksAndMessages(null);

            // adding new postDelay .
            handler.postDelayed(new Runnable() {
                public void run(){
                    if(notificationStack.get(notificationStack.size()-1).equals(channelId)){
                        view.startAnimation(AnimationUtils.loadAnimation(context, R.anim.bottom_to_top));
                    }
                    mainLayout.removeView(view);
                    notificationChannels.remove(channelId);
                    notificationStack.remove(channelId);
                    try {
                        refreshView();
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                }
            }, durationInMilliSeconds);
        }

        private void ring(){
            try {
                Uri notify = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                Ringtone r = RingtoneManager.getRingtone(context, notify);
                r.play();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}